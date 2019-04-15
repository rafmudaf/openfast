.. _running-openfast:

Running OpenFAST
================

Both 32- and 64-bit Windows® binaries of the FAST executable in both single and
double precision are distributed in the <FAST8>/bin folder. The 32-bit
single-precision version has a name ending in “win32” and the 64-bit
single-precision version has a name ending in “x64”; the double-precision
versions append these names with “d” e.g. “FAST_x64d.exe” is the 64-bit
executable with double precision. The 32-bit executables can be used on both
32- and 64-bit operating systems and are compiled with “large address aware”,
which allows the executables to use slightly more than 2 GB of RAM, but not
much more, when run on a 64-bit operating system. The 64-bit executables
require a 64-bit operating system and can run simulations that require more
than 2 GB of memory.

Libraries must use the same addressing scheme as the FAST executable.
FAST v8.16.00a-bjj must load the MAP++ library when the program starts. On
Windows® systems, FAST_Win32.exe needs to load MAP_Win32.dll and FAST_x64.exe
needs to load MAP_x64.dll. These dynamic link libraries (DLLs) must be on your
Windows® path. The easiest way to do this is to make sure that the MAP DLLs are
in the same directory as the FAST executables. We distribute the executables
and DLLs in the bin directory of the FAST archive, so this is already done for
you. However, if you choose to move the files or if you compile the code
yourself, you may have to modify your path environment variable or move some
files.

For non-Windows® operating systems e.g. Mac OS or Linux, or when modifying the
source code, users must compile FAST and the MAP++ library themselves.
Compiling instructions and tips for running on non-Windows® systems are
included in the Compiling FAST section below and the FAST archive in the
<FAST8>/Compiling folder.


Normal Simulation: Starting FAST from an input file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To run FAST from a Windows® command prompt, the syntax is:

<name of FAST executable with or without extension> <name of input file with
extension>

To start, it easiest to open up your command window in the directory in which
your FAST primary input file and FAST executable are stored. For example, if
you have an input file named “Input.fst”, along with “FAST_Win32.exe”, stored
in “C:\FileLocation”, you should type:

C:\>cd FileLocation
C:\FileLocation> FAST_Win32 Input.fst

The syntax is the same for different input files. Simply change “Input.fst” to
whatever input file you want.

An installation guide is available that describes how to install FAST (and the
other computer-aided engineering (CAE) tools) in such a way that they will run
from a command window from any folder (without moving or copying the executable
around to different folders). See: Installing NWTC CAE Tools on PCs Running
Windows®.

Restart: Starting FAST from a checkpoint file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If FAST generated a checkpoint file (see section, “Checkpoint Files (Restart
Capability)”), the simulation may be restarted using this syntax from a
Windows® command prompt:

<name of FAST executable > -restart <root name of checkpoint file without
extension>

Note that the syntax for restart uses the checkpoint file root name instead of
the full checkpoint file name. For example, to restart a simulation of
Test18.fst at step 1800, you would type

C:\FileLocation> FAST_Win32 –restart Test18.1800

FAST would then read the file “Test18.1800.chkp” and—because this test uses a
Bladed-style DLL controller—it would also read the file “Test18.1800.dll.chkp”

On restart, FAST does not read the input files again. However, if a
Bladed-style DLL is used for control, the DLL will be reloaded on restart using
the stored name of the DLL file. Thus, if the stored name is a relative path
and you are not in the same directory, it will likely fail.

It is recommended that FAST be restarted from the same directory in which it
was originally run.

Modeling Tips
~~~~~~~~~~~~~

When first making a model, we recommend that you use the visualization
capability to verify that the wind turbine geometry is defined properly.
Stick-figure visualization is useful for seeing the nodes where calculations
take place and element connectivity; surface visualization is useful for seeing
the turbine. Animating the turbine time series can also be useful for
interpreting results and debugging problems. But generating visualization
output files will slow down FAST and take up a lot of disk space, so, we
recommend disabling visualization when running many FAST simulations.

If a model is numerically unstable, you can try these steps

- Add a correction step (NumCrctn).
- Make DT smaller.
- Change InterpOrder.
- Use the recommend value for UJacSclFact.
- Set better initial conditions in the module input files
  (particularly ElastoDyn).
- Simplify models (e.g. disable modules and eliminate DOFs) to debug problems.
- If you are using BeamDyn, we suggest using an executable compiled in double
  precision (not single precision).

Some of models in the FAST archive (e.g., the OC4 Jacket CertTest) require more
than 2GB of memory and may not run on 32-bit Windows® systems. All of the
included models do run using FAST_Win32.exe on a 64-bit Windows® system.
