.. _regression_test:

Regression tests
================
The regression test executes a series of test cases which intend to fully
describe OpenFAST and its module's capabilities. Jump to one of the following
sections for instructions on running the regression
tests:

- :ref:`python_driver`
- :ref:`ctest_driver`
- :ref:`regression_test_example`
- :ref:`regression_test_windows`

Each locally computed result is compared to a static set of baseline
results. Floating point arithmetic in computer programs can be inconsistent
between hardware architectures, compilers, and math libraries. The compiler
versions, specific math libraries, and more info on hardware used
to generate the baseline solutions are documented in the
`r-test repository documentation <https://github.com/openFAST/r-test>`__. Currently,
the regression test supports only double precision builds.

The regression test system can be executed with CMake (through its included
test driver, CTest) or manually with
a custom Python driver. Both systems provide similar functionality with
respect to testing, but CTest integration provides access to multithreading,
automation, and test reporting via CDash. Both modes of execution require some
configuration as described in the following sections.

In both modes of execution a directory is created in the build directory
called ``reg_tests`` where all of the input files for the test cases are copied
and all of the locally generated outputs are stored. Ultimately, both CTest and
the manual execution program call a series of Python scripts and libraries in
``reg_tests`` and ``reg_tests/lib``. One such script is ``lib/pass_fail.py``
which reads the output files and determines whether a test passes. The pass/fail
criteria is based on relative and absolute tolerances of the difference
between the locally generated results and the stored baselines on an element-wise
basis. The relative tolerance determines the allowed deviation from the baseline,
and the absolute tolerance sets the smallest values that are considered meaningful.
Both tolerances are expressed as orders of mangitude less than the order of
magnitude of the range of the channel. The default values are `RTOL = 2` and
`ATOL = 1.9` and these have been tuned to the existing r-test suite.
The comparison has the form

.. math::
    | baseline - test |\le atol + rol * | baseline |

    atol = 10^{\log( baseline - \min (baseline) ) - ATOL}

    rtol = 10^{- RTOL}

`atol` is a function of the range of the baseline channel. It sets the level
of precision required to pass. `rtol` is a function of the baseline channel
values themselves, and this can be thought of as the level of accuracy
required to pass the regression test. This comparison allows the threshold
to scale with the magnitude of the baseline so that, for example, a deviation
of 10 passes for a data point at 1e6 but fails for a data point at 1e2.

Dependencies
------------
The following packages are required for regression testing:

- Python 3.7+
- Numpy
- CMake and CTest (Optional)
- Bokeh > 2 (Optional)

.. _python_driver:

Executing with Python driver
----------------------------
The regression test can be executed manually with the included driver at
``openfast/reg_tests/manualRegressionTest.py``. This program reads a case list
file at ``openfast/reg_tests/r-test/glue-codes/openfast/CaseList.md``. Cases
can be removed or ignored by starting that line with a ``#``. The driver
program includes multiple optional flags which can be obtained by
executing with the help option:

::

    >>>$ python manualRegressionTest.py -h
    usage: manualRegressionTest.py [-h] [-p [Plotting-Flag]] [-n [No-Execution]]
                                [-v [Verbose-Flag]] [-case [Case-Name]]
                                OpenFAST System-Name Compiler-Id Test-Tolerance

    Executes OpenFAST and a regression test for a single test case.

    positional arguments:
    OpenFAST              path to the OpenFAST executable
    System-Name           current system's name: [Darwin,Linux,Windows]
    Compiler-Id           compiler's id: [Intel,GNU]
    Test-Tolerance        tolerance defining pass or failure in the regression
                            test

    optional arguments:
    -h, --help            show this help message and exit
    -p [Plotting-Flag], -plot [Plotting-Flag]
                            bool to include plots in failed cases
    -n [No-Execution], -no-exec [No-Execution]
                            bool to prevent execution of the test cases
    -v [Verbose-Flag], -verbose [Verbose-Flag]
                            bool to include verbose system output
    -case [Case-Name]     single case name to execute

.. note::

    For the NREL 5MW turbine test cases, an external ServoDyn controller must
    be compiled and included in the appropriate directory or all NREL 5MW
    cases will fail without starting. More information is available in the
    documentation for the `r-test repository <https://github.com/openfast/r-test#note---servodyn-external-controllers-for-5mw_baseline-cases>`__,
    but be aware that these three DISCON controllers must exist

    .. code-block:: bash

        openfast/build/reg_tests/glue-codes/openfast/5MW_Baseline/ServoData/DISCON.dll
        openfast/build/reg_tests/glue-codes/openfast/5MW_Baseline/ServoData/DISCON_ITIBarge.dll
        openfast/build/reg_tests/glue-codes/openfast/5MW_Baseline/ServoData/DISCON_OC3Hywind.dll

.. _ctest_driver:

Executing with CTest
--------------------
CTest is included with CMake and is primarily a set of preconfigured targets
and commands. To use the CTest driver for the regression test, execute CMake as
described in :ref:`installation`, but with this additional flag:
``-DBUILD_TESTING=ON``.

The regression test specific CMake variables are

::

    BUILD_TESTING
    CTEST_OPENFAST_EXECUTABLE
    CTEST_[MODULE]_EXECUTABLE where [MODULE] is the module name
    CTEST_PLOT_ERRORS
    CTEST_REGRESSION_TOL

Some additional resources that are required for the full regression test suite
are included in the CMake project. Specifically, external ServoDyn controllers
must be compiled for a given system and placed in a particular location. Thus,
be sure to execute the build command with the ``install`` target:

.. code-block:: bash

    # Configure CMake with testing enabled and accept the default
    # values for all other test-specific CMake variables
    cmake .. -DBUILD_TESTING=ON

    # Build and install
    make install

.. note::

    REMINDER: For the NREL 5MW turbine test cases, an external ServoDyn controller must
    be compiled and included in the appropriate directory or all NREL 5MW
    cases will fail without starting. More information is available in the
    documentation for the `r-test repository <https://github.com/openfast/r-test#note---servodyn-external-controllers-for-5mw_baseline-cases>`__,
    but be aware that these three DISCON controllers must exist

    .. code-block:: bash

        openfast/build/reg_tests/glue-codes/openfast/5MW_Baseline/ServoData/DISCON.dll
        openfast/build/reg_tests/glue-codes/openfast/5MW_Baseline/ServoData/DISCON_ITIBarge.dll
        openfast/build/reg_tests/glue-codes/openfast/5MW_Baseline/ServoData/DISCON_OC3Hywind.dll

After CMake configuration and compiling, the automated regression test can be
executed by running either of the commands ``make test`` or ``ctest`` from the
``build`` directory. If the entire OpenFAST package is to be built, CMake will
configure CTest to find the new binary at
``openfast/build/glue-codes/openfast/openfast``. However, if the intention is
to build only the test suite, the OpenFAST binary should be specified in the
CMake configuration under the ``CTEST_OPENFAST_EXECUTABLE`` flag. There is
also a corresponding ``CTEST_[MODULE]_NAME`` flag for each module included in
the regression test.

When driven by CTest, the regression test can be executed by running various
forms of the command ``ctest`` from the build directory. The basic commands
are:

.. code-block:: bash

    # Run the entire regression test
    ctest

    # Disable actual execution of tests;
    # this is helpful in formulating a particular ctest command
    ctest -N

    # Run the entire regression test with verbose output
    ctest -V

    # Run tests by name where TestName is a regular expression (regex)
    ctest -R [TestName]

    # Run all tests with N tests executing in parallel
    ctest -j [N]

Each regression test case contains a series of labels associating all of the
modules used. The labeling can be seen in the test instantiation in
``reg_tests/CTestList.cmake`` or with the command:

.. code-block:: bash

    # Print all available test labels
    ctest --print-labels

The test cases corresponding to a particular label can be executed with this
command:

.. code-block:: bash

    # Filter the test cases corresponding to a particular label
    ctest -L [Label]

Flags can be compounded making useful variations such as

.. code-block:: bash

    # Run all cases that use AeroDyn14 with verbose output
    ctest -V -L aerodyn14

    # Run all cases that use AeroDyn14 in 16 concurrent processes
    ctest -j 16 -L aerodyn14

    # Run the case with name "5MW_DLL_Potential_WTurb" with verbose output
    ctest -V -R 5MW_DLL_Potential_WTurb

    # List all tests with the "beamdyn" label
    ctest -N -L beamdyn

    # List the labels included in all tests matching the regex "bd"
    ctest -N -R bd --print-labels


The automated regression test writes new files only into the build directory.
Specifically, all locally generated solutions are located in the corresponding
glue-code or module within ``openfast/build/reg_tests``. The baseline solutions
contained in ``openfast/reg_tests/r-test`` are strictly read and are not
modified by the automated process.

.. _regression_test_example:

Regression test examples
------------------------
The following examples illustrate methods of running the regression tests
on Unix-based systems. However, similar procedures can be used
on Windows with CMake and CTest. An alternate method of running the
regression tests on Windows is given in :ref:`reg_test_windows`.

Compile OpenFAST and execute with CTest
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The following example assumes the user is starting completely from scratch.
The commands below download the source code, configure the OpenFAST project
with CMake, compile all executables, and execute the full regression test
suite.

.. code-block:: bash

    # Download the source code from GitHub
    #    Note: The default branch is 'main'
    git clone --recursive https://github.com/openfast/openfast.git
    cd openfast

    # If necessary, switch to another target branch and update r-test
    git checkout dev
    git submodule update

    # Create the build and install directories and move into build
    mkdir build install && cd build

    # Configure CMake for testing
    # - BUILD_TESTING - turn ON
    # - CTEST_OPENFAST_EXECUTABLE - accept the default
    # - CTEST_[MODULE]_EXECUTABLE - accept the default
    cmake .. -DBUILD_TESTING=ON

    # Compile and install
    make install

    # Execute the full test suite with 4 concurrent processes
    ctest -j4

Configure with CMake and a given executable
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This example assumes the user has a fully functional OpenFAST executable
available along with any necessary libraries, but does not have the source
code repository downloaded. This might be the case when executables are
distributed within an organization or downloaded from an
`OpenFAST Release <https://github.com/openfast/openfast/releases>`__.
Here, nothing will be compiled, but the test suite will be configured
with CMake for use with the CTest command.

.. code-block:: bash

    # Download the source code from GitHub
    #    Note: The default branch is 'main'
    git clone --recursive https://github.com/openfast/openfast.git
    cd openfast

    # If necessary, switch to another target branch and update r-test
    git checkout dev
    git submodule update

    # Create the build directory and move into it
    mkdir build && cd build

    # Configure CMake with openfast/reg_tests/CMakeLists.txt for testing
    # - BUILD_TESTING - turn ON
    # - CTEST_OPENFAST_EXECUTABLE - provide a path
    # - CTEST_[MODULE]_EXECUTABLE - provide a path
    cmake ../reg_tests \
        -DBUILD_TESTING=ON \
        -DCTEST_OPENFAST_EXECUTABLE=/home/user/Desktop/openfast_executable \
        -DCTEST_BEAMDYN_EXECUTABLE=/home/user/Desktop/beamdyn_driver

    # Install required files
    make install

    # Execute the full test suite with 4 concurrent processes
    ctest -j4

.. _example_python_driver:

Python driver with a given executable
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This example assumes the user has a fully functional OpenFAST executable
available along with any necessary libraries, but does not have the source
code repository downloaded. This might be the case when executables are
distributed within an organization or downloaded from an
`OpenFAST Release <https://github.com/openfast/openfast/releases>`__.
Nothing will be compiled, but the test suite will be executed with the
included Python driver.

.. code-block:: bash

    # Download the source code from GitHub
    #    Note: The default branch is 'main'
    git clone --recursive https://github.com/openfast/openfast.git
    cd openfast

    # If necessary, switch to another target branch and update r-test
    git checkout dev
    git submodule update

    # Execute the Python driver
    cd reg_tests
    python manualRegressionTest.py -h
    # usage: manualRegressionTest.py [-h] [-p [Plotting-Flag]] [-n [No-Execution]]
    #                                [-v [Verbose-Flag]] [-case [Case-Name]]
    #                                OpenFAST System-Name Compiler-Id Test-Tolerance
    # 
    # Executes OpenFAST and a regression test for a single test case.
    # 
    # positional arguments:
    #   OpenFAST              path to the OpenFAST executable
    #   System-Name           current system's name: [Darwin,Linux,Windows]
    #   Compiler-Id           compiler's id: [Intel,GNU]
    #   Test-Tolerance        tolerance defining pass or failure in the regression
    #                         test
    # 
    # optional arguments:
    #   -h, --help            show this help message and exit
    #   -p [Plotting-Flag], -plot [Plotting-Flag]
    #                         bool to include plots in failed cases
    #   -n [No-Execution], -no-exec [No-Execution]
    #                         bool to prevent execution of the test cases
    #   -v [Verbose-Flag], -verbose [Verbose-Flag]
    #                         bool to include verbose system output
    #   -case [Case-Name]     single case name to execute

    python manualRegressionTest.py \
        ..\build\bin\openfast_x64_Double.exe \
        Windows \
        Intel \
        1e-5

.. _reg_test_windows:

Detailed example of running on Windows
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The :ref:`example_python_driver` example can be used for running the
regression tests on a Windows computer. However, a more detailed, step-by-step
description is given in :ref:`regression_test_windows`.

.. toctree::
   :maxdepth: 1
   :hidden:

   regression_test_windows.rst

.. _new_regression_test_case:

Adding test cases
-----------------
In all modes of execution, the regression tests are ultimately driven by a
series of Python scripts located in the ``openfast/reg_tests`` directory
with the naming scheme ``execute<Module>RegressionTest.py``.
The first step to adding a new regression test case is to verify that
a script exists for the target module. If it does not, an issue
should be opened in `OpenFAST Issues <https://github.com/openfast/openfast/issues>`_
to coordinate with the NREL team on creating this script.

The next step is to add the test case in the appropriate location in
the `r-test` submodule. The directory structure in r-test mirrors the
directory structure in OpenFAST, so module-level tests should be placed
in their respective module directories and glue-code tests go in
``r-test/glue-codes/openfast``. Note the naming scheme of files for
existing tests and adapt the new test case files accordingly. Specifically,
the main input file and output file names may be expected in a particular
convention by the Python scripts. Also, consider that any relative paths
within the input deck for the new test case must work within the r-test
directory structure.

Once the test directory exists, the test case must be registered with
the appropriate drivers. For OpenFAST glue-code tests, this happens both in
CMake and a standalone list of test cases. For CMake, edit the file
``openfast/reg_tests/CTestList.cmake``. The additional test should be
added in the section corresponding to the module or driver at the
bottom of that file. For the Python driver, the new test case must
be added to ``openfast/reg_tests/r-test/glue-codes/openfast/CaseList.md``.
At this point, the registration with CTest can be verified:

.. code-block:: bash

    # Move into the build directory
    cd openfast/build

    # Run CMake to take the new changes to the test list
    cmake .. -DBUILD_TESTING=ON  # If the BUILD_TESTING flag was previously enabled, this can be left off

    # List the registered tests, but don't run them
    ctest -N

For module regression tests, the only option for execution is with the
CMake driver, so follow the instructions above to edit ``CTestList.cmake``.

Finally, the new test cases in the r-test submodule must be added to the
r-test repository. To do this, open a new issue in `r-test Issues <https://github.com/openfast/r-test/issues>`_
requesting for support from the NREL team to commit your test.

Updating baselines
------------------
When a code change intentionally changes the output of a simulation,
the impacted regression test baselines should also be updated in order
to maintain a passing test suite. This process involves three primary steps:

1. Generate new baseline results
2. Overwrite the existing baselines
3. Align the repository with git

After the final git-push, double check that everything is working correctly
in your fork's GitHub Actions workflows.

.. tip::

    It can be helpful to run the regression test infrastructure without having
    to run the actual simulation. A CMake flag exists to skip case execution
    but complete the rest of the regression test:
    
    ``cmake .. -DCTEST_NO_RUN_FLAG=ON``
    
    Remember that the simulation results must already exist. Typically,
    you'll have to run the tests once and then enable this flag.


Generate new baseline results
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At it's core, this simply involves running the impacted driver code
on the test suite to generate updated results in either the ascii output
(``.out``) or binary output (``.outb``). However, as a `test`, this process
should be considered a check on the code changes. It is good practice to
first ensure that the tests pass on your system with the branch
that will be merged into; this is ``dev`` in the typical git workflow.
Then, making only the code changes that will be merged, rerun the tests.
The differences here should reflect the differences between the target
branch and the code changes to be merged. It is worthwhile to inspect
the results and determine whether the changes are intentional and expected.

An example of the steps here are given below. For instructions on configuring
and building the test suite, see :ref:`regression_test_example`.

.. code-block:: bash

    git checkout dev
    ctest -j8

    # If all tests pass, proceed.
    git checkout <your branch>
    cmake .. && make -j8
    ctest -j8

    # Inspect failing tests
    # Generate comparison plots; a html file will be created in each failing case directory
    cmake .. -DCTEST_NO_RUN_FLAG=ON -DCTEST_PLOT_ERRORS=ON


Overwrite the existing baselines
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In order to include the new baselines in the automated testing system
and share the updates with other developers, the locally generated results
should be copied into the corresponding directory in the r-test submodule.
The directory structure mirrors the structure of OpenFAST. The OpenFAST
glue code tests are located in ``r-test/glue-codes/`` and the module
tests are location in ``r-test/modules/``. Each driver code has a corresponding
test script, and some scripts require the binary output while others require
ascii output. Check the target r-test case directory for the required output
file type. It is also helpful to include any summary files (`.sum`) and log
files (`.log`) generated by the regression test system.

An example of updating OpenFAST glue code tests is given below. After completing
the update, running the regression tests again should pass since the failing cases
were updated with the locally generated results.

.. code-block:: bash

    cd reg_tests/r-test/glue-codes/openfast/5MW_Land_DLL_WTurb
    cp ../../../../../build/reg_tests/glue-codes/openfast/5MW_Land_DLL_WTurb.out .
    cp ../../../../../build/reg_tests/glue-codes/openfast/5MW_Land_DLL_WTurb.outb .
    cp ../../../../../build/reg_tests/glue-codes/openfast/5MW_Land_DLL_WTurb.*sum .
    cp ../../../../../build/reg_tests/glue-codes/openfast/5MW_Land_DLL_WTurb.log .


Align the repository with git
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Finally, commit all changes. In r-test, commit the changes to the cases
and push to the ``openfast/r-test`` remote. It is best to create a new branch
from the latest ``dev`` branch to house your changes. Don't forget to
``git push``! If this commit doesn't exist on GitHub, the
automated tests will fail since it cannot correctly checkout the repository.

Then, move back in the OpenFAST repository, update the r-test submodule handle,
and push everything to GitHub. See the commands below for reference.

.. code-block:: bash

    cd reg_tests/r-test
    git checkout -b f/my_feature
    git add <files to commit like .out, .outb, .sum, and .log>
    git commit -m "Update XX case baseline for OpenFAST PR #1"
    git push origin f/my_feature

    cd ..  # Should be outside of the r-test directory now
    git add r-test
    git commit -m "Update regression test baselines"
    git push
