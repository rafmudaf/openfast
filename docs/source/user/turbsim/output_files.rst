Output Files

TurbSim can generate several different sets of output files. They have
the root name of the TurbSim input file, and their extensions indicate
what type of files they are. The Runtime Options section in the primary
input file (above) describes how to tell TurbSim which sets to output.

Summary Files

TurbSim generates a summary file for all runs. This summary file is a
text file with a “.sum” extension. The first part of the file tells you
what was specified in the input file. After that, TurbSim prints out
many statistics for the run. These statistics are calculated using the
entire *AnalysisTime* so if a shorter *UsableTime* was requested, the
statistics of the output time series could be different than what is
displayed in the summary file. Also keep in mind that the turbulence
statistics are for the background turbulence only; they do not include
effects of any coherent structures generated in coherent turbulence
time-step files. If a coherent turbulence time-step file is generated,
TurbSim prints the number of events and the total length of those events
in the summary file. If Bladed\ *-*\ style FF files or separate tower
output are requested, TurbSim adds another section that tells InflowWind
how to convert the normalized data to floating-point form.

Hub-Height Binary Files

The hub-height binary files are in a machine-readable form designed to
be read by GenPro, a postprocessor from the National Center for
Atmospheric Research (NCAR). TurbSim gives these files a “.bin”
extension. At each time step, TurbSim writes the values of a series of
parameters in the binary file. The parameters are listed in Table 11 in
the order in which they appear in the file. Each value is stored as a
4-byte floating-point (real) number. A MATLAB® script for reading these
files is included in the TurbSim archive; it is named
“Test\readHHbin.m.”

Hub-Height Formatted Files

The hub-height formatted files contain essentially the same information
as the hub-height binary files, but the parameters are written in
columns in human-readable form. See Table 11 for the list of parameters.
These files have a “.dat” extension.

Hub-Height AeroDyn Formatted Files

These human-readable files are in a format compatible with InflowWind
(formerly AeroDyn). They have the “.hh” extension. See Table 12 for the
file format; the `InflowWind Manual <http://nwtc.nrel.gov/InflowWind>`__
[4] contains a detailed description of the parameters. The horizontal
wind speed and wind direction are equivalent to the vector sum of the
instantaneous *U*- and *V*-component time series from the hub-point, and
the vertical wind speed is the corresponding *W*-component time series.
TurbSim always sets the horizontal wind-shear, vertical linear
wind-shear, and gust-speed parameters to zero in the AeroDyn hub-height
files. The vertical power-law wind-shear exponent is constant for the
entire time series. If the input wind-profile type (*WindProfileType*)
is PL or IEC, the value in the AeroDyn HH file is the *PLExp* parameter;
if *WindProfileType* is JET or LOG, the power law exponent is calculated
based on the mean wind speeds at the top and bottom of the rotor disk:

(21)

The column of plots on the right side of Figure 17 shows how InflowWind
uses the information in these HH files to produce wind speeds at any
part of the volume surrounding the turbine.

Full-Field TurbSim Binary Files

The FF TurbSim binary files are designed to be read by InflowWind. They
have a “.bts” extension. (The column of plots on the left side of Figure
17 shows how InflowWind uses FF data.) TurbSim normalizes the
time-series data (in the inertial reference frame coordinate system) and
encodes them in 2-byte integers stored in these files. The first part of
each file is a header that provides information about the grid and tells
InflowWind how to convert the integers to floating-point values. The
wind speeds for the *NumGrid_Y* × *NumGrid_Z* grids and the tower points
(if specified) follow that. See Appendix D in this document for the file
format. A MATLAB script for reading these files is included in the
TurbSim archive; it is named “Test\readTSgrid.m.”

This binary format has been designed so that InflowWind does not need to
read any other file to properly convert the data to floating-point form.
(In contrast, the FF Bladed-style binary files store scaling information
in the summary file.) This format also provides the maximum resolution
possible in two-byte integers.

Full-Field Bladed-Style Binary Files

The FF Bladed-style binary files are designed to be read by both
InflowWind and GH Bladed. They have a “.wnd” extension. TurbSim
normalizes the data (in the inertial reference frame coordinate system)
and encodes them in 2-byte integers. The first part of the file is a
header that provides information about the grid; the normalized wind
speeds for the *NumGrid_Y* × *NumGrid_Z* grid points follow that. See
Appendix E in this guide for the file format. (The column of plots on
the left side of Figure 17 shows how InflowWind uses FF data.)

When generating these files, TurbSim adds a section to the end of the
summary file that tells InflowWind how to convert the data to
floating-point form. To decode the data, InflowWind must read both the
summary file (with the “.sum” extension) and the binary FF file. TurbSim
uses a newer file format than the format SNwind used. In general, this
updated format retains more resolution in the normalized 2-byte integers
than the previous encoding method did. A MATLAB script that reads these
files is included in the TurbSim archive; it is named
“Test\readBLgrid.m.”

Tower Data Binary Files

The tower data binary files are similar to the FF Bladed\ *-*\ style
binary files, except they contain data for points in a single line at
the grid center—going from the bottom of the grid to the ground—using
the same vertical resolution as the rest of the grid (see Figure 4).
These files have a “.twr” extension. TurbSim normalizes the data (in the
inertial reference frame coordinate system) and encodes them in 2-byte
integers. The first part of the file is a header that provides
information about the location of the tower points and size of the file;
this header is followed by the wind speeds. When generating these files,
TurbSim adds a section to the end of the summary file that indicates how
to convert the data to floating-point form (this is the same section
that is generated for the FF Bladed-style “.wnd” binary files). See
Appendix F in this guide for a more complete description of this binary
format.

If a user requests FF binary files in TurbSim format (*WrADFF* =
“true”), the tower points are normalized and stored as 2-byte integers
along with the full-field grid data in the file with a “.bts” extension.
In that case, a separate file with the “.twr” extension is not
generated.

Full-Field Formatted Files

The FF formatted files are the traditional SNLWIND-3D FF output. These
three files are human readable (text), but use five times more storage
than the binary files. Early versions of AeroDyn could read these files,
but AeroDyn and InflowWind no longer support this format. There is one
file for each component, with “.u,” “.v,” and “.w” file extensions,
respectively.

Each of the files begins with a header containing with some basic
information about the simulation, and blocks of data follow. The first
line in each block includes the time and the hub-height wind speed.
Following that line is a table with the number of rows and columns being
the number of grid points specified in the input file. The tables
contain the wind speeds for the different grid points. Their orientation
is as if you are looking upwind (i.e., *Y* increases from left to right,
and *Z* increases from bottom to top), and all of the velocities are in
the inertial reference frame coordinate system. A MATLAB script for
reading these files is included in the TurbSim archive; it is named
“Test\loadFFtxt.m.”

Coherent Turbulence Time-Step Files

One of the unique features of TurbSim is its ability to add coherent
turbulence events based on data obtained from numerical simulations of a
Kelvin-Helmholtz billow. The data comes from two sources: a large-eddy
simulation from NCAR and a direct numerical simulation from Colorado
Research Associates (CoRA), both of Boulder, Colorado. Because the grid
size of the coherent events is very large (roughly 92 x 92 points),
these events are not added directly to the background turbulence in
TurbSim. Instead, we create coherent turbulence time-step files, which
have a “.cts” extension. These text files contain a header indicating
how to scale the non-dimensional coherent structures; the header is
followed by the times and file numbers of the subset of LES or DNS data
that define the coherent events. AeroDyn v13 reads this file along with
the background wind file and adds the two wind fields together. This
feature can be used only in programs that use AeroDyn v12.57 through
v13.*. See the Using Coherent Turbulence Time-Step Files with AeroDyn
v13 section of this document for more information.

|image18|
