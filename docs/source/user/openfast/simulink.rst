.. _openfast-simulink:

OpenFAST Interface to Simulink
==============================

FAST v8 has also been implemented as a library that can be called from a
Simulink S-Function block, using predefined inputs from Simulink. Simulink
is a popular simulation tool for controls design that is distributed by The
Mathworks, Inc. in conjunction with MATLAB.

Definition of the FAST v8 Interface to Simulink
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The FAST v8 interface to Simulink is implemented as a Level-2 S-Function called
FAST_SFunc. The interface is written in C, and it calls a DLL of FAST v8
routines, which are written in Fortran. Both 32- and 64-bit Windows® versions
are supported, although the libraries must have the same addressing scheme
(both addressing schemes can be used on 64-bit operating systems, but only the
32-bit version can be used on a 32-bit operating system). The calling sequence
between the libraries is illustrated in Figure 8. The FAST Dynamic Library
binary files distributed in the FAST archive call a MATLAB DLL to print
information to the MATLAB Command Window. This call is not included in
Figure 8.

Figure 8: Libraries in the FAST - Simulink Interface


Please note that because this interface uses static variables, there can be
only one instance of the FAST_SFunc mex file in any instance of MATLAB (i.e.,
you cannot run two different models simultaneously).

S-Function Parameters
---------------------

The FAST_SFunc S-function block is designed to accept exactly three parameters
from Simulink. The values for each of the parameters are required at
initialization, and they must not be changed during the simulation.

Figure 9: FAST_SFunc Block Parameters

FAST_InputFileName
------------------
The first parameter is a string, which contains the name of the FAST v8 primary
input file. In the sample models, this string is contained in a variable called
FAST_InputFileName.

The second parameter is a double-precision real value called TMax. This TMax is
used in place of the TMax specified in the FAST v8 primary input file, except
for the following two cases:

- If TMax in the FAST v8 primary input file is larger than the one in Simulink,
  the TMax from the primary input file will be used by the FAST modules (e.g.,
  HydroDyn’s Waves submodule).
- If TMax in the FAST v8 primary input file is larger than the one in Simulink,
  the TMax from the primary input file will be used to allocate space for the
  binary output file (if OutFileFmt /= 1).

Figure 10: Using TMax to specify simulation end time in Simulink

NumAdditionalInputs
-------------------
The third parameter sent to the S-Function block is NumAdditionalInputs.
Currently, NumAdditionalInputs is 0 for most cases.

S-Function Inputs
-----------------
The inputs to the FAST S-Function are values in an array of size
8 + NumAdditionalInputs. (See section “S-Function Parameters” for an
explanation of NumAdditionalInputs.)
The values in the input array are as follows:

1. Generator torque (Nm)
2. Electrical power (W)
3. Commanded yaw position (radians)
4. Commanded yaw rate (radians/s)
5. Commanded pitch for blade 1 (radians)
6. Commanded pitch for blade 2 (radians)
7. Commanded pitch for blade 3 (radians): note that this input is unused on
   2-bladed turbines, but is always required as input to the DLL
8. Fraction of maximum high-speed shaft braking torque (fractional value
   between 0 and 1)

Note that these inputs are passed from Simulink to ServoDyn. However, these
inputs are used only if the appropriate switches in the ServoDyn input file are
selected. The PCMode, VSContrl, YCMode, and HSSBrMode parameters in the
ServoDyn input file must be set to “4” to allow the inputs from Simulink to be
used for pitch control, variable-speed control, nacelle yaw control, and/or
high-speed shaft braking respectively.

Figure 11: FAST v8 Nonlinear Wind Turbine Block in Simulink

S-Function Outputs
------------------
The outputs from FAST to Simulink are the values that are written to the FAST
output file(s). The values are output to Simulink every time the S-Function is
called, and thus, are not affected by FAST’s DT_Out parameter. These outputs
are the channels defined in module OutList variables; these channel names and
units are also written in the FAST summary file. At the FAST_SFunc block
initialization, FAST_SFunc writes a cell array called “OutList” containing the
names of these output channels to the MATLAB base workspace. Currently, there
is a limit of 1000 outputs from FAST when used with the Simulink Interface.

S-Function States
-----------------
The FAST v8 S-Function does not register any states with Simulink. This
effectively makes the block a discrete-time system which is solved inside
the S-Function block. Whereas the FAST v7 Simulink models frequently required
a time-delay block to eliminate algebraic loops to enable the system to solve,
FAST v8 Simulink models should not need to implement a time-delay block.

Running FAST in Simulink
~~~~~~~~~~~~~~~~~~~~~~~~
To run the FAST S-Function from Simulink, MATLAB must be able to find the
appropriate DLLs. This includes the FAST_SFunc.mex*, FAST_Library_*.dll, and
MAP_*.dll files. All of these files are contained in the FAST archive’s bin
directory, so the easiest way to do this is to add the <FAST8>/bin directory to
the MATLAB path.

Sample Simulink Models for FAST v8
----------------------------------
Two sample models for running FAST v8 with Simulink are provided in the FAST
archive (see the <FAST8>/Simulink/Samples folder). These examples are intended
to help the user understand how to use the FAST_SFunc block. It assumed that
the user is already familiar with the Simulink environment.

OpenLoop
--------
The OpenLoop sample model contains the FAST S-Function block and constant
open-loop control input blocks.

The Run_OpenLoop.m script in the <FAST8>/Simulink/Samples folder allows the
user to run all of the FAST Certification Tests from Simulink using the
OpenLoop model without using any of the control inputs from Simulink.

Figure 12: OpenLoop.mdl Sample Model for Simulink

Test01_SIG
----------
The Test01_SIG.mdl file contains the FAST S-Function block and the simple
induction generator model for FAST certification test #01 implemented within
Simulink. To run this model, change VSControl to “4” in the
<FAST8>/CertTest/AWT27/Test01_ServoDyn.dat file, then run the Run_Test01_SIG.m
file in the <FAST8>/Simulink/Samples folder.

Figure 13: Test01_SIG.mdl Sample Model for Simulink

Error Messages
--------------
If your Simulink model fails to run, please make note of any error, warning,
or informational windows that open. Also make sure to look at any text written
to the MATLAB Command Window, which is where all messages from the
FAST_Library_*.dll file will be written.
