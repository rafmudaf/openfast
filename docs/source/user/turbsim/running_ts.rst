Obtaining TurbSim
=================
The TurbSim software is included in the OpenFAST repository. See the
:ref:`installation` section for details on obtaining the source code
and configuring the system for compiling.

The TurbSim target can be compiled with the Visual Studio project
by building the TurbSim target or with ``make`` through the following
command:

.. code-block:: bash

   make turbsim

Both methods will place an executable at
``openfast/build/modules/turbsim/``.

Retrieving Files from the Archive
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To be able to generate coherent structures with TurbSim, users will also
need to download the coherent structures archive from NREL’s Web server
page. The file is named “TSM_structures.exe.” Create a folder on your
file system and put this file there. Execute the program by double
clicking on it or by typing “TSM_structures” at a command prompt with
the folder you created as the current directory. When executed, this
archive creates the files and folders used to define coherent
structures. It is necessary to type the name of the path to these
folders in TurbSim and AeroDyn v13 input files.

Regression Tests
~~~~~~~~~~~~~~~~
Before using TurbSim for the first time, run the regression tests.
It is a batch file called “CertTest.bat” and is located in the
“Test” folder. To test the installation, edit “CertTest.bat” and set the
environment variables found near the top of the file to settings that
are compatible with your system. You probably will have to change only
the “Editor” variable. Then open a command window, go to the Test
folder, and type “CertTest” or—if you have MATLAB® [12] installed on
your computer and would like to see plots of the data—type “CertTest
MATLAB.”

When the certification testing program is run, TurbSim executes several
times. The test procedure compares the new results to those stored in
the “Test\TstFiles” folder, and it writes the differences between the
output files to a file called “CertTest.out.” If you have specified the
“MATLAB” option, MATLAB opens and plots many results. It might be
necessary to close the MATLAB program before the test procedure can
continue. Before finishing, the test procedure automatically opens the
“CertTest.out” file with the editor you specified with the “Editor”
variable. Scan through the file; the only differences should be the date
and time stamps in the headers of the files and the CPU time in the
summary files. If you recompiled TurbSim with another compiler, some
slight differences could appear in the last digit of many of the
numbers.

Running the software
====================
To begin using TurbSim, a text input file is required. Sample input
files, which serve as a starting point and can be modified, are available
in :ref:`appendixa`. A quick-start guide for using the most basic turbulence
is included in :ref:`appendixb`.

The syntax for running the TurbSim executable is given in the help prompt:

.. code-blocK:: bash

   >>>$ ./turbsim -h

   **************************************************************************************************
   TurbSim

   Copyright (C)  National Renewable Energy Laboratory
   Copyright (C)  Envision Energy USA LTD

   This program is licensed under Apache License Version 2.0 and comes with ABSOLUTELY NO WARRANTY.
   See the "LICENSE" file distributed with this software for details.
   **************************************************************************************************

   Running TurbSim
   a part of OpenFAST - v2.1.0-182-g962d2268-dirty
   linked with NWTC Subroutine Library


   Syntax is:

      TurbSim [-h] [<InputFile>]

   where:

      -h generates this help message.
      <InputFile> is the name of the primary input file.  If omitted, the default file is
      "TurbSim.inp".

   Note: values enclosed in square brackets [] are optional. Do not enter the brackets.

All output files have the specified root file name and different extensions.
