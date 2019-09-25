.. _running-openfast:

Running OpenFAST
================

For obtaining and installing OpenFAST, see the :ref:`installation` section.

`Libraries must use the same addressing scheme as the FAST executable` (what does this mean?).

Normal Simulation: Starting FAST from an input file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The syntax for executing OpenFAST is:

.. code-block:: bash

    # <path to OpenFAST> <path to the input file>
    /usr/local/bin/openfast ./turbine_case/NREL_5MW.fst

.. TODO copy this installation guide to the installation section of sphinx

An installation guide is available that describes how to install FAST (and the
other computer-aided engineering (CAE) tools) in such a way that they will run
from a command window from any folder (without moving or copying the executable
around to different folders). See: Installing NWTC CAE Tools on PCs Running
Windows®.

Restart: Starting OpenFAST from a checkpoint file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.. TODO create the section for CHeckpoint files

If OpenFAST generated a checkpoint file (see section, “Checkpoint Files (Restart
Capability)”), the simulation may be restarted using this syntax:

.. code-block:: bash

    # <path to OpenFAST> -restart <root name of checkpoint file without extension>
    /usr/local/bin/openfast -restart ./turbine_case/NREL_5MW.1800

On restart, OpenFAST does not read the input files again. However, if a
Bladed-style controller is used, the library will be reloaded on restart using
the stored name of the library file. Thus, if the stored name is a relative
path and the files have moved, OpenFAST may encounter an error. It is
recommended that OpenFAST be restarted from the same directory in which it
was originally run.

Modeling Tips
~~~~~~~~~~~~~
When first building a model, we suggest using the visualization
capability to verify that the wind turbine geometry is defined properly.
Stick-figure visualization is useful for seeing the nodes where calculations
take place and element connectivity; surface visualization is useful for seeing
the turbine. Animating the turbine time series can also be useful for
interpreting results and debugging problems. But generating visualization
output files will slow down OpenFAST and take up a lot of disk space, so it
is recommended to disable visualization when running many simulations.

If a model is numerically unstable, the following steps may help:

- Add a correction step (NumCrctn)
- Make DT smaller
- Change InterpOrder
- Use the recommend value for UJacSclFact
- Set better initial conditions in the module input files (particularly
  ElastoDyn)
- Simplify models (e.g. disable modules and eliminate DOFs) to debug problems
- If using BeamDyn, use an executable compiled in double precision
