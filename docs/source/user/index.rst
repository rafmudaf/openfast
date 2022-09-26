.. _user_guide:

User Documentation
==================

We are in the process of transitioning legacy FAST v8 documentation, which can be found at https://www.nrel.gov/wind/nwtc.html.

.. note::

    Much of the documentation here is legacy documentation from FAST v8. While most of it is still
    directly applicable to OpenFAST, portions may be out of date.

>--
**This should be removed or rephrased**
This document is designed to guide you through some of the changes that the FAST
wind turbine multi-physics engineering tool is undergoing, until complete 
documentation is made available. FAST v8.16.00a-bjj is the latest public release 
of FAST under the new modularization framework developed at NREL. 
The architecture of FAST v8 is entirely different from FAST v7.02.00d-bjj. 
These differences are highlighted in Figure 1.

The modules of FAST (AeroDyn, HydroDyn, etc.) correspond to different physical 
domains of the coupled aero-hydro-servo-elastic solution, most of which are 
separated by spatial boundaries. Figure 2 shows the control volumes associated 
with each module for fixed-bottom offshore wind turbines. Though not shown, 
finite-element blade structural dynamics is optionally available through the 
BeamDyn module and loading from surface ice on fixed-bottom offshore wind 
turbines is optionally available through the IceFloe or IceDyn modules. For 
land-based wind turbines, the HydroDyn hydrodynamics module and ice modules 
would not be used and the SubDyn multi-member substructure structural-dynamics 
module is optional. Figure 3 shows the control volumes associated with 
each module for floating offshore wind turbines. Though not shown, 
finite-element blade structural dynamics is optionally available through the
BeamDyn module and mooring and hydrodynamics are optionally available 
through the OrcaFlexInterface module.
--<



.. _general-reference-docs:

General
~~~~~~~
.. toctree::
   :maxdepth: 1

   introduction.rst
   fast_to_openfast.rst
   api_change.rst
   input_file_overview.rst

Workshop material, legacy documentation, and other resources are listed below.

- `Overview of OpenFAST at NAWEA WindTech 2019 <https://drive.google.com/file/d/1wagMTOV_CLxSKzS2EEPFp2CExUo3JLpQ/view?usp=sharing>`_
- `Workshop Presentations <https://drive.google.com/drive/folders/1BDDfcnIyvmZCwf7eFo0ISI7aF_FMAOvt?usp=sharing>`_
- :download:`Old FAST v6 User's Guide <../../OtherSupporting/Old_FAST6_UsersGuide.pdf>`
- :download:`FAST v8 README <../../OtherSupporting/FAST8_README.pdf>`
- `Implementation of Substructure Flexibility and Member-Level Load Capabilities for Floating Offshore Wind Turbines in OpenFAST <https://www.nrel.gov/docs/fy20osti/76822.pdf>`_
- `FAST modularization framework for wind turbine simulation: full-system linearization <https://www.nrel.gov/docs/fy17osti/67015.pdf>`_
- `Full-System Linearization for Floating Offshore Wind Turbines in OpenFAST <https://www.nrel.gov/docs/fy19osti/71865.pdf>`_ 
- :download:`FAST with Labview <../../OtherSupporting/UsingFAST4Labview.pdf>`
- :download:`OutListParameters.xlsx <../../OtherSupporting/OutListParameters.xlsx>` - Contains the full list of outputs for each module.


Module Documentation
~~~~~~~~~~~~~~~~~~~~
This section contains documentation for the OpenFAST module-coupling environment and its underlying modules.
Documentation covers usage of models, underlying theory, and in some cases module verification.

.. toctree::
   :maxdepth: 1

   AeroDyn <aerodyn/index.rst>
   OLAF <aerodyn-olaf/index.rst>
   Aeroacoustics <aerodyn-aeroacoustics/index.rst>
   BeamDyn <beamdyn/index.rst>
   SubDyn <subdyn/index.rst>
   ExtPtfm <extptfm//index.rst>
   ElastoDyn <elastodyn/index.rst>
   HydroDyn <hydrodyn/index.rst>
   InflowWind <inflowwind/index.rst>
   MoorDyn <moordyn/index.rst>
   ServoDyn <servodyn/index.rst>
   Structural Control <servodyn-stc/StC_index.rst>
   TurbSim <turbsim/index.rst>
   C++ API <openfast/cppapi.rst>
   FAST.Farm <fast.farm/index.rst>

The following modules do not currently have formal documentation
or are contributed to OpenFAST from organizations
external to NREL and the core OpenFAST team. As documentation is added,
these resources will be moved to their appropriate location. If newer versions
of the external resources are available, please open a `GitHub Issue <https://github.com/openfast/openfast/issues>`_
with the information for the new documentation.

- MAP++

  - `Official MAP++ documentation <https://map-plus-plus.readthedocs.io/en/latest/index.html>`_
  - :download:`Implementation of a Multi-Segmented, Quasi-Static Cable Model <../../OtherSupporting/MAP/cable_model_development.pdf>`

- FEAMooring

  - :download:`Theory Manual <../../OtherSupporting/FEAMooring/FEAM_Theory_Manual.pdf>`
  - :download:`User's Guide <../../OtherSupporting/FEAMooring/FEAM_Users_Guide.pdf>`

- MoorDyn

  - `Official User's Guide <http://www.matt-hall.ca/files/MoorDyn%20Users%20Guide%202017-08-16.pdf>`_

- OrcaFlex Interface:

  - :download:`User's Guide <../../OtherSupporting/OrcaFlex/User_Guide_OrcaFlexInterface.pdf>`

- IceFloe

  - :download:`Ice Load Project Final Technical Report <../../OtherSupporting/IceFloe/Ice_Load_Final_Report.pdf>`

- IceDyn

  - :download:`Draft: FAST Ice Module Manual <../../OtherSupporting/IceDyn/IceDyn_Manual.pdf>`

- TurbSim

  - :download:`User's Guide <../../OtherSupporting/TurbSim/TurbSim_v2.00.pdf>`

Modularization Framework
~~~~~~~~~~~~~~~~~~~~~~~~

Information specific to the modularization framework of OpenFAST is provided here. These are a collection
of publications, presentations, and past studies on the subject.

- `The New Modularization Framework for the FAST Wind Turbine CAE Tool <https://www.nrel.gov/docs/fy13osti/57228.pdf>`_
- :download:`Example Module Implementation Plans <../../OtherSupporting/ModulePlan_GasmiPaperExamples.doc>`
- :download:`Module and Mesh-Mapping Linearization Implementation Plan <../../OtherSupporting/LinearizationOfMeshMapping_Rev18_Rev2.doc>`
- :download:`Interpolation of DCMs <../../OtherSupporting/DCM_Interpolation/DCM_Interpolation.pdf>` - A summary of the mathematics used in the interpolation of DCM (direction cosine matrices) using logarithmic mapping and matrix exponentials.
- :download:`Set-point Linearization Development Plan <../../OtherSupporting/DevelopmentPlan-SetPoint-Linearization.pdf>`

.. - :download:`OpenFAST Steady State Solution <../../OtherSupporting/OpenFASTSteadyStateSolution_Rev7.doc>`


Glue Code and Mesh Mapping
~~~~~~~~~~~~~~~~~~~~~~~~~~

- `FAST Modular Wind Turbine CAE Tool: Nonmatching Spatial and Temporal Meshes <https://www.nrel.gov/docs/fy14osti/60742.pdf>`_
- `FAST Modular Framework for Wind Turbine Simulation: New Algorithms and Numerical Examples <https://dx.doi.org/10.2514/6.2015-1461>`_
- :download:`OpenFAST Algorithms <../../OtherSupporting/OpenFAST_Algorithms/OpenFAST_Algorithms.pdf>` - A summary of the solve method used in the glue code.
- :download:`Predictor-Corrector Approach <../../OtherSupporting/ProposedPCApproach_Rev4.docx>`


NWTC Subroutine Library
~~~~~~~~~~~~~~~~~~~~~~~

- :download:`NWTC Library - short overview of subroutines and functions <../../OtherSupporting/NWTC_Library_Description.pdf>`
