#
# Copyright 2017 National Renewable Energy Laboratory
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

import os
import sys
basepath = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.sep.join([basepath, "lib"]))
import argparse
import numpy as np
import shutil
import subprocess
import rtestlib as rtl
import openfastDrivers
import pass_fail
from errorPlotting import exportCaseSummary
import glob

##### Helper functions
excludeExt=['.out','.outb','.ech','.sum','.log']

##### Main program

### Store the python executable for future python calls
pythonCommand = sys.executable

### Verify input arguments
parser = argparse.ArgumentParser(description="Executes OpenFAST and a regression test for a single test case.")
parser.add_argument("caseName", metavar="Case-Name", type=str, nargs=1, help="The name of the test case.")
parser.add_argument("executable", metavar="OpenFAST", type=str, nargs=1, help="The path to the OpenFAST executable.")
parser.add_argument("sourceDirectory", metavar="path/to/openfast_repo", type=str, nargs=1, help="The path to the OpenFAST repository.")
parser.add_argument("buildDirectory", metavar="path/to/openfast_repo/build", type=str, nargs=1, help="The path to the OpenFAST repository build directory.")
parser.add_argument("rtol", metavar="Relative-Tolerance", type=float, nargs=1, help="Relative tolerance to allow the solution to deviate; expressed as order of magnitudes less than baseline.")
parser.add_argument("atol", metavar="Absolute-Tolerance", type=float, nargs=1, help="Absolute tolerance to allow small values to pass; expressed as order of magnitudes less than baseline.")
parser.add_argument("-p", "-plot", dest="plot", action='store_true', help="bool to include plots in failed cases")
parser.add_argument("-n", "-no-exec", dest="noExec", action='store_true', help="bool to prevent execution of the test cases")
parser.add_argument("-v", "-verbose", dest="verbose", action='store_true', help="bool to include verbose system output")

args = parser.parse_args()

caseName = args.caseName[0]
executable = os.path.abspath(args.executable[0])
sourceDirectory = args.sourceDirectory[0]
buildDirectory = args.buildDirectory[0]
rtol = args.rtol[0]
atol = args.atol[0]
plotError = args.plot
noExec = args.noExec
verbose = args.verbose

# validate inputs
rtl.validateExeOrExit(executable)
rtl.validateDirOrExit(sourceDirectory)
if not os.path.isdir(buildDirectory):
    os.makedirs(buildDirectory, exist_ok=True)

### Build the filesystem navigation variables for running openfast on the test case
rtest = os.path.join(sourceDirectory, "reg_tests", "r-test")
moduleDirectory = os.path.join(rtest, "glue-codes", "openfast-cpp")
openfast_gluecode_directory = os.path.join(rtest, "glue-codes", "openfast")
inputsDirectory = os.path.join(moduleDirectory, caseName)
targetOutputDirectory = os.path.join(inputsDirectory)
testBuildDirectory = os.path.join(buildDirectory, caseName)

# verify all the required directories exist
if not os.path.isdir(rtest):
    rtl.exitWithError("The test data directory, {}, does not exist. If you haven't already, run `git submodule update --init --recursive`".format(rtest))
if not os.path.isdir(targetOutputDirectory):
    rtl.exitWithError("The test data outputs directory, {}, does not exist. Try running `git submodule update`".format(targetOutputDirectory))
if not os.path.isdir(inputsDirectory):
    rtl.exitWithError("The test data inputs directory, {}, does not exist. Verify your local repository is up to date.".format(inputsDirectory))

# create the local output directory if it does not already exist
dst = os.path.join(buildDirectory, "5MW_Baseline")
src = os.path.join(openfast_gluecode_directory, "5MW_Baseline")
if not os.path.isdir(dst):
    rtl.copyTree(src, dst, excludeExt=excludeExt)
else:
    names = os.listdir(src)
    for name in names:
        if name == "ServoData":
            continue
        srcname = os.path.join(src, name)
        dstname = os.path.join(dst, name)
        if os.path.isdir(srcname):
            if not os.path.isdir(dstname):
                rtl.copyTree(srcname, dstname, excludeExt=excludeExt)
        else:
            shutil.copy2(srcname, dstname)

if not os.path.isdir(testBuildDirectory):
    rtl.copyTree(inputsDirectory, testBuildDirectory, excludeExt=excludeExt)

### Run openfast on the test case
if not noExec:
    cwd = os.getcwd()
    caseInputFile = os.path.join(testBuildDirectory, "cDriver.yaml")
    os.chdir(testBuildDirectory)
    returnCode = openfastDrivers.runOpenfastCase(caseInputFile, executable)
    if returnCode != 0:
        sys.exit(returnCode*10)
    os.chdir(cwd)


### If this is a restart test case
if '_Restart' in caseName:
    caseInputFile = os.path.join(testBuildDirectory, "cDriverRestart.yaml")
    os.chdir(testBuildDirectory)
    returnCode = openfastDrivers.runOpenfastCase(caseInputFile, executable)
    if returnCode != 0:
        sys.exit(returnCode*10)

### Build the filesystem navigation variables for running the regression test
localOutFile = os.path.join(testBuildDirectory, "5MW_Land_DLL_WTurb_cpp.outb")
baselineOutFile = os.path.join(inputsDirectory, "5MW_Land_DLL_WTurb_cpp.outb.gold")

rtl.validateFileOrExit(localOutFile)
rtl.validateFileOrExit(baselineOutFile)

testData, testInfo, _ = pass_fail.readFASTOut(localOutFile)
baselineData, baselineInfo, _ = pass_fail.readFASTOut(baselineOutFile)

passing_channels = pass_fail.passing_channels(testData.T, baselineData.T, rtol, atol)
passing_channels = passing_channels.T

norms = pass_fail.calculateNorms(testData, baselineData)

# export all case summaries
channel_names = testInfo["attribute_names"]
exportCaseSummary(testBuildDirectory, caseName, channel_names, passing_channels, norms)

# passing case
if np.all(passing_channels):
    sys.exit(0)

# failing case
if plotError:
    from errorPlotting import finalizePlotDirectory, plotOpenfastError
    for channel in testInfo["attribute_names"]:
        try:
            plotOpenfastError(localOutFile, baselineOutFile, channel, rtol, atol)
        except:
            error = sys.exc_info()[1]
            print("Error generating plots: {}".format(error))
    finalizePlotDirectory(localOutFile, testInfo["attribute_names"], caseName)

sys.exit(1)
