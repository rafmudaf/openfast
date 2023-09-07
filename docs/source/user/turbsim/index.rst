TurbSim User Guide and Theory Manual
====================================

.. toctree::
   :maxdepth: 2

   introduction.rst
   running_ts.rst
   input_files.rst
   output_files.rst
   modeling_considerations.rst
   future_work.rst
   appendix.rst
..    references.rst

Acknowledgments
~~~~~~~~~~~~~~~
TurbSim was written by Bonnie Jonkman at the National Wind Technology
Center (NWTC).

Analysis of coherent events was performed by Neil Kelley, Bonnie
Jonkman, and George Scott of the National Wind Technology Center, and
Professor Jan Bialasiewicz, and Lisa Redmond of the University of
Colorado at Denver.

The turbulence modeling scaling parameters for the GP_LLJ and NWTCUP
spectral models were developed by Neil Kelley and Bonnie Jonkman. Neil
Kelley developed scaling parameters for the National Renewable Energy
Laboratory wind farm models.

Levi Kilcher of the National Wind Technology Center added the NREL/UW
Tidal Channel (TIDAL) spectral model to extend TurbSim’s use for water
turbulence.

Yi Guo of the National Wind Technology Center added the API model for
hurricane modeling.

List of Acronyms
~~~~~~~~~~~~~~~~

ART Advanced Research Turbine

BLAS Basic Linear Algebra Subprograms

CoRA Colorado Research Associates

CTKE coherent turbulent kinetic energy

CXML Compaq Extended Math Library

DNS direct numerical simulation

ETM Extreme Turbulence Model

EWM Extreme Wind Model

FF full field

FFT Fast Fourier Transform

FFTPACK FFT Package

HH hub height

IEC International Electrotechnical Commission

IFFT Inverse Fast Fourier Transform

LAPACK Linear Algebra Package

LES large-eddy simulation

LIST Long-Term Inflow and Structural Testing

LLLJP Lamar Low-Level Jet Project

MHK marine and hydrokinetic

NCAR National Center for Atmospheric Research

NREL National Renewable Energy Laboratory

NTM Normal Turbulence Model

NWTC National Wind Technology Center

pRNG pseudorandom number generator

SODAR sonic detection and ranging

TI turbulence intensity

TKE turbulent kinetic energy


Caveats

NREL makes no guarantees about the usability or accuracy of TurbSim,
which is essentially a beta code. NREL does not have the resources to
provide full support for this program.

Feedback

If you have questions about TurbSim, please use our
`forums <https://wind.nrel.gov/forum/wind/>`__. We will respond to your
needs if time and resources permit, but please do not expect an
immediate response. You can apply for an account on the forum here:
https://wind.nrel.gov/forum/wind/viewforum.php?f=17







Appendix C: Flow Charts

|image28|

Figure C-1. Overview of the TurbSim simulation method; blue lines
indicate processes influenced by input-file parameters; black lines
indicate internal variables and processes

|image29|

Figure C-2. Parameters in the Runtime Options section of the input file

|image30|

Figure C-3. Parameters in the Turbine/Model Specifications section of
the TurbSim input file

|image31|

Figure C-4. Parameters in the Meteorological Boundary Conditions section
of the TurbSim input file (for IECKAI and IECVKM models only)

|image32|

Figure C-5. Parameters in the Meteorological Boundary Conditions section
of the TurbSim input file (for models other than IECKAI and IECVKM)

|image33|

Figure C-6. Parameters in the Non-IEC Meteorological Boundary Conditions
section of the TurbSim input file

|image34|

Figure C-7. Default input values for the for the Meteorological Boundary
Conditions and Non-IEC Meteorological Boundary Conditions sections of
the TurbSim input file

|image35|

Figure C-8. Parameters for coherent structures and the Coherent
Turbulence Scaling Parameters section of the TurbSim input file; the
SMOOTH model uses the GP_LLJ scaling

Appendix D: Full-Field TurbSim Binary File Format

Table D-1. Full-Field TurbSim Binary File Header Format

=============================== =============== ============================================================================================================================================================= ==================================================================================================================================================================================================
Type (Bytes)                    Parameter       Description                                                                                                                                                  
=============================== =============== ============================================================================================================================================================= ==================================================================================================================================================================================================
Integer (2)                     *ID*            Identifies the file as a TurbSim binary file. *ID* should have the value 7 (not periodic) or 8 (periodic).                                                   
Integer (4)                     *NumGrid_Z*     The number of grid points in the vertical direction.                                                                                                         
Integer (4)                     *NumGrid_Y*     The number of grid points in the horizontal direction.                                                                                                       
Integer (4)                     *n\ tower*      The number of tower points below the grid.                                                                                                                   
Integer (4)                     *n\ t*          The number of time steps.                                                                                                                                    
Real (4)                        *dz*            The distance in meters between two adjacent points in the vertical direction.                                                                                
Real (4)                        *dy*            The distance in meters between two adjacent points in the horizontal direction.                                                                              
Real (4)                        *TimeStep*      The time in seconds between consecutive grids.                                                                                                               
Real (4)                        *u\ hub*        The mean wind speed in m/s at hub height.                                                                                                                    
Real (4)                        *HubHt*         The height in meters of the hub.                                                                                                                             
Real (4)                        *Z\ bottom*     The height in meters of the bottom of the grid.                                                                                                              
*for i = 1, 2, 3*                                                                                                                                                                                            
\                               Real (4)        *V\ slope\ (i)*                                                                                                                                               The slope used to scale the *i\ th* velocity component3F [4]_ from 4-byte reals into 2-byte integers.
\                               Real (4)        *V\ intercept\ (i)*                                                                                                                                           The intercept used to scale the *i\ th* velocity component\ :sup:`4` from 4-byte reals into 2-byte integers.
*end i*                                                                                                                                                                                                      
Integer (4)                     *n\ characters* The number of characters in the ASCII string that gives the TurbSim version number, date, and time the file was generated. This number is no larger than 200.
*for i = 1, 2, … n\ characters*                                                                                                                                                                              
\                               Integer (1)     *Character\ i*                                                                                                                                                The ASCII integer representation of the *i\ th* character of the string that gives the TurbSim version number, date, and time the file was generated. ACHAR(\ *Character\ i)* gives the character.
*end i*                                                                                                                                                                                                      
=============================== =============== ============================================================================================================================================================= ==================================================================================================================================================================================================

Table D-2. FF TurbSim Binary File Grid Format

======================= ============================ ============================ ================= ============================ ==========================================================================================================================
Type (Bytes)            Parameter                    Description                                                                
======================= ============================ ============================ ================= ============================ ==========================================================================================================================
*for it = 1, 2, … n\ t*                                                                                                         
\                       *for iz = 1, 2, … NumGrid_Z*                                                                            
\                                                    *for iy = 1, 2, … NumGrid_Y*                                               
\                                                                                 *for i = 1, 2, 3*                             
\                                                                                 Integer (2)       *V\ grid_norm\ (i,iy,iz,it)* The normalized *i\ th* velocity component4F [5]_ of the wind speed at time step, *it,* and grid location (*y(iy), z(iz)*).
\                                                                                 end i                                         
\                                                    end iy                                                                     
\                       end iz                                                                                                  
\                                                                                                                               
\                       *for iz = 1, 2, … n\ tower*                                                                             
\                                                    *for i = 1, 2, 3*                                                          
\                                                                                 Integer (2)       *V\ tower_norm\ (i,iz,it)*   The normalized *i\ th* -component5 of the wind speed at time step, *it,* and tower height, *z\ tower\ (iz)*.
\                                                    *end i*                                                                    
\                       *end iz*                                                                                                
end it                                                                                                                          
======================= ============================ ============================ ================= ============================ ==========================================================================================================================

To convert the normalized wind in the FF TurbSim binary file to
velocities in units of meters per second, use the following equations:

(D-1)

. (D-2)

The corresponding lateral locations, *Y*, and vertical locations, Z, of
the grid and/or tower points are given in units of meters by

(D-3)

and

. (D-4)

Appendix E: Full-Field Bladed-Style Binary File Format

Table E-1. Full-Field Bladed-Style Binary File Header Format

============ =================== ==================================================================================
Type (Bytes) Parameter           Description
============ =================== ==================================================================================
Integer (2)  *ID*                Identifies the file as a Bladed-style binary file. *ID* should have the value -99.
Integer (2)  *ID2*               *ID2* should have the value 4 to include the next 7 parameters.
Integer (4)  *nc*                The number of wind components. *nc* should be 3.
Real (4)     *Latitude*          This value is not used in AeroDyn.
Real (4)     *Z0*                The surface roughness. This value is not used in AeroDyn.
Real (4)     *Ztmp*              The height at the center of the grid, in meters.
Real (4)     *100 \* TI(u)*      The turbulence intensity of the *u* component, in percent.
Real (4)     *100 \* TI(v)*      The turbulence intensity of the *v* component, in percent.
Real (4)     *100 \* TI(w)*      The turbulence intensity of the *w* component, in percent.
Real (4)     *dz*                The grid spacing in the vertical direction, in meters.
Real (4)     *dy*                The grid spacing in the lateral direction, in meters.
Real (4)     *u\ hub * TimeStep* The longitudinal grid resolution, in meters.
Integer (4)  *nt / 2*            Half the number of points in the longitudinal direction.
Real (4)     *u\ hub*            The mean wind speed (in meters per second) at hub height.
Real (4)     *Unused*            The value 0. This parameter is not used in AeroDyn.
Real (4)     *Unused*            The value 0. This parameter is not used in AeroDyn.
Real (4)     *Unused*            The value 0. This parameter is not used in AeroDyn.
Integer (4)  *Unused*            The value 0. This parameter is not used in AeroDyn.
Integer (4)  *RandSeed1*         This value is not used in AeroDyn.
Integer (4)  *NumGrid_Z*         The number of grid points vertically.
Integer (4)  *NumGrid_Y*         The number of grid points laterally.
Integer (4)  *Unused*            The value 0. This parameter is not used in AeroDyn.
Integer (4)  *Unused*            The value 0. This parameter is not used in AeroDyn.
Integer (4)  *Unused*            The value 0. This parameter is not used in AeroDyn.
Integer (4)  *Unused*            The value 0. This parameter is not used in AeroDyn.
Integer (4)  *Unused*            The value 0. This parameter is not used in AeroDyn.
Integer (4)  *Unused*            The value 0. This parameter is not used in AeroDyn.
============ =================== ==================================================================================

Table E-2. Format of Grid Velocities in Full-Field Bladed-Style Binary
File Format

===================== ============================ ============================ ======================================================================================================
Type (Bytes)          Parameter                    Description                  
===================== ============================ ============================ ======================================================================================================
*for it = 1, 2, … nt*                                                           
\                     *for iz = 1, 2, … NumGrid_Z*                              
\                                                  *for iy = 1, 2, … NumGrid_Y* 
\                                                  Integer (2)                   The normalized *U* component of the wind speed at time step, *it*, and grid location (*y(iy), z(iz)*).
\                                                  Integer (2)                   The normalized *V* component of the wind speed at time step, *it*, and grid location (*y(iy), z(iz)*).
\                                                  Integer (2)                   The normalized *W* component of the wind speed at time step, *it*, and grid location (*y(iy), z(iz)*).
\                                                  end iy                       
\                     end iz                                                    
end it                                                                          
===================== ============================ ============================ ======================================================================================================

To convert the normalized wind in the FF Bladed-style binary file to
velocities in units of meters per second, use the following equations:

, (E-1)

, (E-2)

and

. (E-3)

Here *TI* represents the turbulence intensity as a decimal, not a
percentage.

The corresponding vertical locations, *Z*, of the grid points are given
in units of meters by

(E-4)

using values of *HubHt* and *HeightOffset* from the summary file. The
lateral locations, *Y*, of the grid points depend on the input value
*Clockwise* (read from the summary file) and are given by

. (E-5)

Appendix F: Tower Data Binary File Format

TurbSim tower files have a “.twr” extension. Each file contains a header
of 4-byte real and integer values, followed by 2-byte integer time
series of the three wind components at each point on the tower grid. The
wind components at the tower points are normalized and stored in 2-byte
binary integers, exactly the same way that Bladed-style full-field wind
files are written. The tower files have the same vertical resolution as
the full-field grid, with points going downward from the bottom of the
full grid in a single line at the tower centerline.

Table F-1: Format of Header in TurbSim Binary Tower-Data File

============ ==================== ==========================================================
Type (Bytes) Parameter            Description
============ ==================== ==========================================================
Real (4)     *dz*                 Vertical grid resolution, in meters.
Real (4)     *u\ hub \* TimeStep* Longitudinal grid resolution, in meters.
Real (4)     *Zmax*               The height of the highest tower point, in meters.
Real (4)     *nt*                 The number of points in the longitudinal direction.
Real (4)     *nz*                 The number of vertical tower points.
Real (4)     *u\ hub*             The mean wind speed, in meters per second.
Real (4)     *100 \* TI(u)*       The turbulence intensity of the *u* component, in percent.
Real (4)     *100 \* TI(v)*       The turbulence intensity of the *v* component, in percent.
Real (4)     *100 \* TI(w)*       The turbulence intensity of the *w* component, in percent.
============ ==================== ==========================================================

Table F-2: Format of Grid Velocities in TurbSim Binary Tower-Data File

===================================================================================================================================== ===================== ==================================================================================
For each increasing time step (*nt* points),and starting at the top of the grid, going downward (*nz* points) the data are stored as:                                                                                                         
===================================================================================================================================== ===================== ==================================================================================
Type (Bytes)                                                                                                                          Parameter             Description                                                                       
*for it = 1, 2, … nt*                                                                                                                                                                                                                         
\                                                                                                                                     *for iz = 1, 2, … nz*                                                                                   
Integer (2)                                                                                                                                                 Normalized *U* component of the wind speed at time step, *it,* and height *z(iz)*.
Integer (2)                                                                                                                                                 Normalized *V* component of the wind speed at time step, *it,* and height *z(iz)*.
Integer (2)                                                                                                                                                 Normalized *W* component of the wind speed at time step, *it,* and height *z(iz)*.
\                                                                                                                                     end iz                                                                                                  
end it                                                                                                                                                                                                                                        
===================================================================================================================================== ===================== ==================================================================================

To convert the normalized wind in the tower data binary file to
velocities in units of meters per second, use the following equations:

, (F-1)

, (F-2)

and

. (F-3)

Here *TI* represents the turbulence intensity as a decimal, not a
percentage.

The corresponding lateral locations, *Y*, and vertical locations, *Z*,
of the tower points are given in units of meters using values of *Zmax*
from the file header:

. (F-4)

.

Appendix G: Velocity Spectra Comparison Plots

|Spectra-u-defaultUstar.png|\ |Spectra-v-defaultUstar.png|\ |Spectra-w-defaultUstar.png|

Figure G-1. Neutral velocity spectra for the 8 spectral models available
in TurbSim, using a 15 m/s wind speed at 80 m; IECKAI and IECVKM use NTM
category “B” and 61400‑1 3\ :sup:`rd` ed. scaling; the non-IEC models
use *RICH_NO = *\ 0 and *UStar* = “default”

|Spectra-u-fixedUstar.png|\ |Spectra-v-fixedUstar.png|\ |Spectra-w-fixedUstar.png|

Figure G-2. Neutral velocity spectra for the 8 spectral models available
in TurbSim, using a 15 m/s wind speed at 80 m; IECKAI and IECVKM use NTM
category “B” and 61400‑1 3\ :sup:`rd` ed. scaling; the non-IEC models
use *RICH_NO = *\ 0 and *UStar* = 1.1 m/s

|Spectra-u-Stable-defUstar.png|\ |Spectra-v-Stable-defUstar.png|\ |Spectra-w-Stable-defUstar.png|

Figure G-3. Stable velocity spectra using a 15 m/s wind speed at 80 m;
the non-IEC models use *RICH_NO = *\ 0.05 and *UStar* = “default”; The
IEC models, which are neutral (*RICH_NO* = 0), were added for reference;
they use NTM category “B” and 61400‑1 3\ :sup:`rd` ed. scaling

|Spectra-u-Stable-fixUstar.png|\ |Spectra-v-Stable-fixUstar.png|\ |Spectra-w-Stable-fixUstar.png|

Figure G-4. Stable velocity spectra using a 15 m/s wind speed at 80 m;
the non-IEC models use RICH_NO = 0.05 and UStar = 1.1 m/s; the IEC
models, which are neutral (RICH_NO = 0), were added for reference; they
use NTM category “B” and 61400‑1 3\ :sup:`rd` ed. scaling

|Spectra-u-UnStab-defUstar.png|\ |Spectra-v-UnStab-defUstar.png|\ |Spectra-w-UnStab-defUstar.png|

Figure G-5. Unstable velocity spectra using a 15 m/s wind speed at 80 m;
the non-IEC models use *RICH_NO = -*\ 0.05 and *UStar* = “default”*;*
the IEC models, which are neutral (*RICH_NO* = 0), were added for
reference; they use NTM category “B” and 61400‑1 3\ :sup:`rd` ed.
scaling

|Spectra-u-UnStab-fixUstar.png|\ |Spectra-v-UnStab-fixUstar.png|\ |Spectra-w-UnStab-fixUstar.png|

Figure G-6. Unstable velocity spectra using a 15 m/s wind speed at 80 m;
the non-IEC models use *RICH_NO = -*\ 0.05 and *UStar* = 1.1 m/s; the
IEC models, which are neutral (*RICH_NO* = 0), were added for reference;
they use NTM category “B” and 61400‑1 3\ :sup:`rd` ed. scaling

Appendix H: Sample AeroDyn v13 Coherent Turbulence Parameter Input File

Example Coherent Turbulence Parameter input file (TurbSim_AD.ctp). Valid
with AeroDyn 12.57.

# Parameters that can vary from one turbine simulation to the next:

"H:\x90_i16" \| CTSpath - Path to coherent turbulence data files

"TurbSim.cts" \| CTTSfile - File containing time steps of the coherent
turbulence event files

"TurbSim.wnd" \| CTbackgr - Name of file containing background wind data
(quoted string)

1 \| CT_DF_Y - Decimation factor for wind data in the y direction

1 \| CT_DF_Z - Decimation factor for wind data in the z direction

==================================================

NOTE: Do not add or remove any lines in this file!

==================================================

For decimation factors, 1 = use every point, 2 = use every other point,
etc.

.. [1]
   This model differs slightly from the original neutral spectra defined
   by Kaimal.

.. [2]
   Currently the interpolation in space is limited to *Z* (height). It
   is envisioned that future versions will also interpolate in the
   lateral direction.

.. [3]
   As a general rule of thumb, the number 30 is the dividing line
   between large and small sample statistics.

.. [4]
   The three wind components are defined as *U = 1, V = 2,* and *W = 3.*

.. [5]
   The three wind components are defined as *U = 1, V = 2,* and *W = 3.*

.. |image0| image:: media/image10.png
   :width: 6.5in
   :height: 4.05278in
.. |image1| image:: media/image11.png
   :width: 4.20779in
   :height: 2.90909in
.. |image2| image:: media/image12.png
   :width: 3.47986in
   :height: 3.5in
.. |image3| image:: media/image13.png
   :width: 1.71319in
   :height: 2.46528in
.. |Example grids| image:: media/image24.png
   :width: 4.20682in
   :height: 1.5in
.. |image5| image:: media/image25.emf
   :width: 3.05in
   :height: 2.32in
.. |image6| image:: media/image26.emf
   :width: 3.06528in
   :height: 2.55833in
.. |image7| image:: media/image27.emf
   :width: 3.05278in
   :height: 2.53755in
.. |image8| image:: media/image31.png
   :width: 6.5in
   :height: 3.31389in
.. |image9| image:: media/image32.jpeg
   :width: 6.5in
   :height: 1.9in
.. |image10| image:: media/image39.png
   :width: 3.75in
   :height: 3.13in
.. |image11| image:: media/image40.png
   :width: 2.75in
   :height: 3.13in
.. |image12| image:: media/image53.png
   :width: 2.7in
   :height: 3.1in
.. |image13| image:: media/image54.png
   :width: 3.75in
   :height: 3.1in
.. |image14| image:: media/image76.png
   :width: 6.5in
   :height: 3.44653in
.. |image15| image:: media/image80.png
   :width: 6.5in
   :height: 3.54444in
.. |image16| image:: media/image87.png
   :width: 6.5in
   :height: 3.63264in
.. |image17| image:: media/image88.png
   :width: 6.1563in
   :height: 3.63183in
.. |image18| image:: media/image90.png
   :width: 6.5in
   :height: 9.1625in
.. |image19| image:: media/image111.png
   :width: 6.5in
   :height: 3.85764in
.. |image20| image:: media/image122.png
   :width: 6.5in
   :height: 3.85972in
.. |image21| image:: media/image137.png
   :width: 6.5in
   :height: 3.80233in
.. |image22| image:: media/image139.png
   :width: 6.5in
   :height: 3.8in
.. |image23| image:: media/image145.png
   :width: 6.5in
   :height: 3.85972in
.. |image24| image:: media/image175.png
   :width: 5.96269in
   :height: 4.33333in
.. |image25| image:: media/image189.png
   :width: 6.5in
   :height: 3.42in
.. |image26| image:: media/image192.png
   :width: 6.5in
   :height: 3.09259in
.. |image27| image:: media/image193.png
   :width: 6.5in
   :height: 3.14815in
.. |image28| image:: media/image203.emf
   :width: 6.5in
   :height: 6.41338in
.. |image29| image:: media/image204.emf
   :width: 8.17848in
   :height: 6.02415in
.. |image30| image:: media/image205.emf
   :width: 6.34121in
   :height: 7.2231in
.. |image31| image:: media/image206.emf
   :width: 6.19423in
   :height: 7.60105in
.. |image32| image:: media/image207.emf
   :width: 6.13472in
   :height: 7.53819in
.. |image33| image:: media/image208.emf
   :width: 6.33681in
   :height: 7.66319in
.. |image34| image:: media/image209.emf
   :width: 5.8719in
   :height: 8.25in
.. |image35| image:: media/image210.emf
   :width: 6.1875in
   :height: 8.25in
.. |Spectra-u-defaultUstar.png| image:: media/image230.png
   :width: 5.00044in
   :height: 2.62957in
.. |Spectra-v-defaultUstar.png| image:: media/image231.png
   :width: 5in
   :height: 2.51024in
.. |Spectra-w-defaultUstar.png| image:: media/image232.png
   :width: 5.00044in
   :height: 2.50188in
.. |Spectra-u-fixedUstar.png| image:: media/image233.png
   :width: 5.00044in
   :height: 2.62957in
.. |Spectra-v-fixedUstar.png| image:: media/image234.png
   :width: 5.00044in
   :height: 2.50194in
.. |Spectra-w-fixedUstar.png| image:: media/image235.png
   :width: 5.00044in
   :height: 2.50183in
.. |Spectra-u-Stable-defUstar.png| image:: media/image236.png
   :width: 5.00044in
   :height: 2.62952in
.. |Spectra-v-Stable-defUstar.png| image:: media/image237.png
   :width: 5.00044in
   :height: 2.50188in
.. |Spectra-w-Stable-defUstar.png| image:: media/image238.png
   :width: 5.00044in
   :height: 2.50188in
.. |Spectra-u-Stable-fixUstar.png| image:: media/image239.png
   :width: 5.00044in
   :height: 2.64942in
.. |Spectra-v-Stable-fixUstar.png| image:: media/image240.png
   :width: 5.00044in
   :height: 2.50464in
.. |Spectra-w-Stable-fixUstar.png| image:: media/image241.png
   :width: 5.00044in
   :height: 2.50464in
.. |Spectra-u-UnStab-defUstar.png| image:: media/image242.png
   :width: 5.00044in
   :height: 2.62957in
.. |Spectra-v-UnStab-defUstar.png| image:: media/image243.png
   :width: 5.00044in
   :height: 2.49774in
.. |Spectra-w-UnStab-defUstar.png| image:: media/image244.png
   :width: 5.00044in
   :height: 2.50188in
.. |Spectra-u-UnStab-fixUstar.png| image:: media/image245.png
   :width: 5.00044in
   :height: 2.62957in
.. |Spectra-v-UnStab-fixUstar.png| image:: media/image246.png
   :width: 5.00044in
   :height: 2.50188in
.. |Spectra-w-UnStab-fixUstar.png| image:: media/image247.png
   :width: 5.00044in
   :height: 2.50188in
