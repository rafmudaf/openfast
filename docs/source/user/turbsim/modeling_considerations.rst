
Coordinate Systems

Wind components are defined in two separate coordinate systems as
described in Table 2 and pictured in Figure 3. TurbSim computes winds in
a coordinate system aligned with the direction of the mean velocity
vector at each point in space. The velocities are rotated to the
inertial reference frame coordinate system before they are written to
output files.

|image2|

Spectral Models

TurbSim uses a modified version of the Sandia method [5] to generate
time series based on spectral representation. Several different spectral
models are available, including two IEC models, the Risø smooth-terrain
model, and several NREL site-specific models (NWTCUP, GP_LLJ, WF_UPW,
WF_07D, and WF_14D). This section describes the velocity spectra used in
each of the models and discusses the measurements used to develop
scaling for the site-specific models. Standard deviations, , have been
calculated by integrating the velocity spectra, *S*:

(22)

Plots comparing the velocity spectra of the different models are
presented in Appendix **G**.

IECKAI: The IEC Kaimal Model

The IEC Kaimal model is defined in IEC 61400‑1 2\ :sup:`nd` ed. [24],
and 3\ :sup:`rd` ed. [21] and assumes neutral atmospheric stability
(*RICH_NO *\ = 0).0F [1]_ The spectra for the three wind components,
*K = u, v, w*, are given by

(23)

where *f* is the cyclic frequency and *L\ K* is an integral scale
parameter. The IEC 61400‑1 standard defines the integral scale parameter
to be

(24)

where the turbulence scale parameter, , is

(25)

(Note that the function in Eq. (25) indicates the minimum of and .) The
relationships between the standard deviations are defined to be

(26)

The velocity spectra (and standard deviations) of the IECKAI model are
assumed to be invariant across the grid. In practice, a small amount of
variation in the *u-*\ component standard deviation occurs due to the
spatial coherence model.

IECVKM: The IEC Von Karman Isotropic Model

This IEC model is defined in IEC 61400‑1 2\ :sup:`nd` ed. [24] for
isotropic turbulence and neutral atmospheric stability. The velocity
spectra for the wind components are given by

(27)

and

(28)

for *K = v, w*. In these equations, *f* is the cyclic frequency and *L*
is an integral scale parameter. *L* is defined using the turbulence
scale parameter, , from Eq. (25):

(29)

The IEC standard defines the relationship between the standard
deviations of the components to be

. (30)

The velocity spectra (and standard deviations) of the IECVKM model are
invariant across the grid. In practice, a small amount of variation in
the *u-*\ component standard deviation occurs due to the spatial
coherence model.

SMOOTH: The Risø Smooth-Terrain Model

TurbSim also offers the Risø smooth-terrain model (SMOOTH), based on
work by Højstrup et al. [29] and Olesen et al. [30]. This spectral model
has separate equations for stable/neutral and for unstable flows. The
SMOOTH model (as well as the site-specific models) defines the velocity
spectra using local height and wind speed; this contrasts with the IEC
models which use the wind speed and height of the hub to define the
spectra at all points. The spectra from the SMOOTH model also form the
basis for the spectra for all the site-specific models.

For stable and neutral conditions (), the SMOOTH-model velocity spectra
for the three wind components, *K*, are given by

(31)

where *f* is the cyclic frequency, *UStar* is the friction velocity
input parameter, is the mean wind speed at height *z*, and and are
functions of the stability parameter, *RICH_NO*. The two scales, *s\ 1*
and *s\ 2*, are defined as follows for each component:

(32)

|image19|\ The theoretical standard deviations of the wind components in
stable and neutral conditions are plotted in Figure 18. These values are
calculated assuming infinite, continuous spectra with no spatial
coherence or time-domain cross-component correlation (i.e*.*, the input
mean hub Reynolds stresses, *PC_UW, PC_UV,* and *PC_VW*, are “none”).
The standard deviations theoretically are constant across the rotor disk
(using the same assumptions); in practice, however, they can appear to
vary with height (depending on the input values used). This variance
should decrease with increased record length. The relationships between
the components’ standard deviations are

(33)

For unstable flows, (), the SMOOTH spectra are modeled as the sum of
low- and high-frequency spectral peaks:

(34)

These two peaks are defined for the three wind components as follows:

(35)

(36)

and

(37)

where *f* is the cyclic frequency, *UStar* and *ZI* are input
parameters, and is the mean wind speed at height *z*. *L* is the
Monin-Obukhov length parameter, which is a function of *RICH_NO* and
*HubHt*.

The standard deviations of the wind components in unstable atmospheric
conditions vary with height, the mixing layer depth (*ZI*), and *L*.
Their approximate values are determined from the following equations:

(38)

(39)

(40)

NWTCUP: The NREL National Wind Technology Center Model

The NWTCUP model, based on measurements from the NWTC/LIST project,
represents turbulent inflow characteristics at the NWTC, downwind of a
major mountain range. In this project, three towers were installed 1.5
rotor diameters upwind of the 600-kW NWTC Advanced Research Turbine
(ART). The central tower contained three-axis sonic anemometers at 15 m,
37 m, and 58 m above ground level; cup anemometers and wind vanes were
located at 3 m, 37 m, and 58 m; and temperature measurements were
obtained at 3 m, 15 m, 37 m, and 58 m. Two additional towers, which were
located 21 m north and south of the central tower, contained three-axis
sonic anemometers at 37 m. Neil Kelley et al. discuss this project and
the instrumentation further [31].

The spectra for this model are based on the 40-Hz time series data
collected by the five sonic anemometers. The default spatial coherence
parameters generated for this model are based on vertical coherence
measured by the sonic anemometers on the central tower.

For neutral and stable flows, the NWTCUP spectra are defined by adding
scaled versions of the SMOOTH-model spectra:

(41)

|image20|\ where *NumPeaks\ K = 2* for all wind components *K = u, v, w*
and the function is defined in Eq. (31). All of the and scaling factors
are functions of *RICH_NO*. Figure 19 shows the standard deviations for
the three wind components and the ratios between the components’
standard deviations.

For unstable flows, the NWTCUP model modifies the SMOOTH-model low- and
high-frequency peaks from Eq. (35) through Eq. (37):

(42)

The scaling factors , , , and , which are empirically derived from
spectra calculated using NWTC/LIST velocity measurements, are functions
of the *RICH_NO* and *UStar* parameters. The standard deviations are
similar to those of the unstable SMOOTH-model, scaled by the and terms.

GP_LLJ: The NREL Great Plains Low-Level Jet Model

The Great Plains model (GP_LLJ) is based on measurements from a 120-m
tower and from an acoustic wind profiler (SODAR [sonic detection and
ranging]) obtained during the Lamar Low-Level Jet Project in
southeastern Colorado. The tower included three-axis sonic anemometers
at 54 m, 67 m, 85 m, and 116 m above the ground; cup anemometers and
direction vanes located at 3, 52, and 113 m; and temperature
measurements obtained at 3 m, 52 m, 83 m, and 113 m. The SODAR provided
measurements of wind speed and direction at 10-m vertical increments
from 20 m to 500 m. The spectra and spatial coherence parameters defined
in this model are based on 20-Hz time-series data collected at the sonic
anemometers. Please refer to Neil Kelley et al. [32] for details of that
project.

The GP_LLJ model defines vertical profiles of stability and of shear
velocity (i.e.*,* stability and shear velocity are functions of height).
The stability profile—related to *RICH_NO*—is a local Monin-Obukhov
stability parameter, , and the shear velocity profile is a local value.
The values used for these profiles are placed in the TurbSim summary
file. Both of these profiles are calculated based on height, wind speed,
and *RICH_NO*. The shear velocity profile also relies on *UStar* and *,*
which is defined in Eq. (14).

The and profiles are used to scale the GP_LLJ velocity spectra (in
contrast, the other models use the *UStar* and *RICH_NO* parameters,
which are averaged values). For stable and neutral flows, the spectra
are defined by adding peaks from the form of the SMOOTH-model spectra:

(43)

where the function is defined in Eq. (31), using the local stability
parameter, , to determine the values of functions and (instead of using
*RICH_NO* as the SMOOTH model does). The *u* and *v* components have two
peaks (*NumPeaks\ K = 2*, *K = u, v*), and the *w* component is modeled
with only one peak (*NumPeaks\ w = 1*). The scaling factors and are
functions of both and . The standard deviations for the three wind
components are plotted in Figure 20. The ratios between the components
satisfy the following inequalities:

(44)

and

(45)

|image21|\ By design, most of the LLLJP data was collected in the stable
atmosphere. As a result, there was not enough data to create a model of
the spectra in unstable flows. Instead, the GP_LLJ spectra for unstable
atmospheric conditions use the same equations as the SMOOTH model
spectra in Eq. (35) through Eq. (37). The one difference is that the
GP_LLJ scales the spectra using the local values instead of the *UStar*
input parameter. The GP_LLJ spectra for unstable flows are thus defined
as

(46)

WF_UPW: The NREL Wind Farm, Upwind Model

The WF_UPW wind-farm model is based on measurements taken from a 50-m
tower upwind of a large wind plant in San Gorgonio Pass, California. The
spectra were calculated using 50-Hz wind-speed measurements from a
three-axis sonic anemometer located 23 m above the ground. The
parameters for spatial coherence were calculated using measurements from
5-Hz cup anemometers and direction vanes located at 5 m, 10 m, 20 m and
50 m above ground level. Please refer to Kelley [6] for details of the
model development and Kelley and Wright [33] for further details on the
measurements.

For neutral and stable flows, the WF_UPW spectra are defined by adding
scaled versions of the SMOOTH-model spectra, using Eq. (41). All of the
wind components use two spectral peaks (*NumPeaks\ K = 2,*
*K = u, v, w*) and each of the scaling factors and are functions of
*RICH_NO*. Figure 21 shows the standard deviations for the three wind
components and the ratios between the components’ standard deviations.

|image22|\ For unstable flows, the WF_UPW model modifies the
SMOOTH-model low- and high-frequency peaks, using Eq. (42). The scaling
factors , , , and are functions of the *RICH_NO* parameter. The
resulting standard deviations are similar to those of the unstable
SMOOTH model, but scaled by the and terms.

WF_14D: The NREL Wind Farm, Downwind Model (14 Rotor Diameters)

The WF_14D wind-farm model is based on measurements taken on a 50-m
tower downwind of a 41-row wind plant in San Gorgonio Pass, California.
The tower was approximately 14-rotor-diameters downwind of the plant,
which consisted of 23-m hub-height Micon 65/13 machines with 16-m rotor
diameters.

The spectra were calculated using 50-Hz wind-speed measurements from a
three-axis sonic anemometer located 23 m above the ground. The
parameters for spatial coherence were calculated using measurements from
5-Hz cup anemometers and direction vanes located at 5 m, 10 m, 20 m, and
50 m above ground. The development of this model is described by Kelley
6, and the measurements are discussed further in Kelley and Wright [33].

For neutral and stable flows, the WF_14D spectra are defined by adding
scaled versions of the SMOOTH-model spectra, also using Eq. (41). All
wind components use two spectral peaks (*NumPeaks\ K = 2,*
*K = u, v, w*) and each of the scaling factors and are functions of
*RICH_NO*. Figure 22 shows the standard deviations for the three wind
components and the ratios between the components’ standard deviations.

For unstable flows, the WF_14D model modifies the SMOOTH-model low- and
high-frequency peaks listed in Eq. (35) through Eq. (37):

(47)

|image23|\ The *u*- and *w*-component spectra have two peaks
(*NumPeaks\ K = 2,* *K = u, w*). For the *v*-component spectra, Kelley
found a third peak (*NumPeaks\ v = 3*), which he attributed to wakes
from the wind turbines upstream. The scaling factors and , , are
functions of the *RICH_NO* parameter. The resulting standard deviations
are similar to those of the unstable SMOOTH-model, scaled by the terms.

WF_07D: The NREL Wind Farm, Downwind Model (7 Rotor Diameters)

The scaling for the WF_07D wind-farm model is based on measurements
taken at row 37 of a 41-row wind plant in San Gorgonio Pass, California
for the SERI Thin-Airfoil Blade Atmospheric Performance Test [34]. The
16-Hz measurements were obtained from a three-axis sonic anemometer 23-m
above the ground, on a tower approximately 7-rotor-diameters downwind of
a row of operating Micon 65/13 wind turbines.

These measurements were used to calculate the scaling for coherent
structures and default input parameters. The measurements used to form
the scaling for the WF_07D model, however, were not sufficient to
develop spectral scaling or spatial coherence. As a result, the WF_07D
model uses the same equations for the velocity spectra and spatial
coherence as the WF_14D model.

TIDAL: The NREL/UW Tidal Channel Model

The TIDAL model for water turbulence is based on measurements taken near
Marrowstone Island in Puget Sound Washington [35]. These measurements
were taken in a tidally-mixed tidal boundary layer 4.6 meters above the
bottom in 18 meters of water. The spectral form is essentially the same
as the SMOOTH spectral model, but the spectral amplitude and shear are
scaled directly based on this tidal channel’s turbulent kinetic energy
(TKE) and shear rather than implicitly from atmospheric boundary layer
theory. In particular, the form is:

(48)

where the empirically determined coefficients are (for frequency, *f*,
in hertz and ∂\ *u*/∂*z* in 1/second):

(49)

The shear, ∂\ *u*/∂*z*, is calculated internally from the specified mean
velocity profile. In the case of a logarithmic velocity profile, the
shear is proportional to *u*/*z* and this form is essentially the same
as the SMOOTH spectral model. The component-TKE levels, *σ\ K\ 2* are
determined based on an exponential profile proportional to
*UStar*\ :sup:`2`:

(50)

where, :math:`\mu_{u} = 4.5,\ \mu_{v} = 2.25,\mu_{w} = 0.9`, are
empirically determined coefficients from the Marrowstone Island site.
*RefHt* is the reference height input parameter at which the input
parameter *Uref* is specified. For simulating fully mixed tidally forced
boundary layers, *RefHt* should be approximately equal to the water
depth, and *Uref* the surface velocity. A simple way to match observed
velocity and TKE (:math:`{\sigma_{K}}^{2}`) profiles is to make minor
adjustments to *Uref* and *RefHt*.

The TurbSim archive includes a “TurbSim_Hydro.inp” sample input file.,
The parameters set in “TurbSim_Hydro.inp” are more appropriate to water
turbulence and tidal channels than the “TurbSim.inp” file, which has
default values appropriate for atmospheric turbulence. Note that many of
the values in the input file are not used for the TIDAL spectral model
(e.g. *Z0*, *RICH_NO*, *ZI*, and all of the Coherent Turbulence Scaling
Parameters).

TIMESR: Time Series Input

The TIMESR model accepts time series input at several points in space.
TurbSim calculates the mean direction at each point (saving the angles
for use with direction profiles), then rotates the velocities each point
so that they are aligned with the mean wind direction, i.e., we now have
*u, v,* and *w* components. The mean velocity is removed from each
point, and the mean values are saved for use with velocity profiles.

TurbSim then performs an FFT of these zero-mean time series and
calculates the spectral amplitudes and phase angles. The resulting
spectra are linearly interpolated in frequency and space1F [2]_ (using
nearest-neighbor extrapolation) to obtain spectral amplitudes for the
simulated points. The phase angles of the simulated points are chosen
from a random uniform distribution; they are then correlated to the
phase angles of the time series at the single point specified by
*RefPtID* (see Input File for User-Defined Time Series) using the
specified coherence model. This ensures that there is coherence between
the simulated points and the user-input *RefPtID* point (when coherence
is requested). Coherence between the simulated points and other
user-input time series is not guaranteed.

USRINP: User-Input Spectra

This spectral model produces uniform spectra for each of the three
velocity components. Three spectra representing the *u, v,* and *w*
velocity components are input in a separate input file (see Input File
for User-Defined Spectra). These spectra are scaled with the scaling
factors listed at the top of the user-defined spectra input file and are
linearly interpolated in frequency (using nearest neighbor
extrapolation) to compute target spectra for the simulated points.

(51)

(52)

(53)

USRVKM: von Karman Model with User-Defined Scaling

The von Karman model with user-defined scaling computes the velocity
spectra for the wind components using local values of standard
deviation, length scale, and wind speed. The velocity spectra for the
wind components are given by:

(54)

and

(55)

for *K = v, w*. In these equations, *f* is the cyclic frequency, is the
time-averaged local wind speed, and are constants whose values are
defined in the input file for user-defined profiles. and are the length
scale and standard deviation values from the user-defined profile input
file; these values are linearly interpolated with height (and, if
necessary, extrapolated using the nearest height) to obtain local values
for these equations.

API: API Spectrum for Hurricane Winds

The API spectral model implements the Frøya model spectral density for
the longitudinal wind component proposed by Andersen and Løvseth as
documented by Det Norske Veritas (DNV) [36]. The Frøya spectral model
was developed for neutral conditions over water in the Norwegian Sea.
Use of the Frøya spectrum can therefore not necessarily be recommended
in regimes where stability effects are important. A frequency of 1/2400
Hz defines the lower bound for the range of application of the spectrum.

(56)

where :math:`\ n = 0.468` and

(57)

:math:`U_{0}` (assumed to be *URef*\ ) is the one-hour mean wind speed
at a height of 10 meters above mean sea level, and *z* is the local
height above sea level.

Spatial Coherence Models

In general, spatial coherence between points *i* and *j* is defined as

(58)

where *f* is the frequency, *S\ ii* is the power spectral density as
defined in the “Spectral Models” section, and *S\ ij* is the
cross-spectral density. This coherence adds correlation between the same
wind components at two spatially separated points (e.g., *u\ i-uj*
correlation, not *u-v* correlation).

The four spatial coherence models that are implemented in TurbSim are
described below.

GENERAL: A general spatial coherence model

TurbSim’s general coherence function for all three of the velocity
components, *K = u, v, w*, is defined as

(59)

where *f* is the frequency, *r* is the distance between points *i* and
*j, z\ m* is the mean height of the two points, and is the mean of the
wind speeds of the two points (over the entire simulation). The
variables *a* and *b* are the input coherence decrement and offset
parameter, respectively, which are defined by the values of the
*IncDec1*, *IncDec2*, and *IncDec3* input parameters (for each of the
components). Their default values are discussed in the “Input Files”
section of this document and are plotted in Figure 13 through Figure 15.

This coherence model is based on the form suggested by Thresher et al.
[37] and implemented in the IEC coherence model. The term has been added
to allow users to implement Solari’s coherence definition [38]. Note
that if *b = 0 and CohExp = 0*, this equation also becomes the Davenport
coherence model [39].

IEC: IEC Coherence Model

The IEC coherence function for all three of the wind components,
*K = u, v, w* is implemented as

| (60)
| where *f* is the frequency, *r* is the distance between points *i* and
  *j* on the grid\ *, and* is the mean hub-height wind speed. The
  variables *a* and *b* are the input coherence decrement and offset
  parameter, respectively, which are defined by the values of the
  *IncDec1*, *IncDec2*, and *IncDec3* input parameters. If *CohExp = 0*,
  the only difference between the general and IEC spatial coherence
  models is the use of mean wind speeds.

To implement the coherence model defined in the IEC 61400‑1 standards
for the *u*-component, define

(61)

where *L\ c* is a coherence scale parameter. For IEC 61400‑1
2\ :sup:`nd` ed. [24], the parameters *a* and *L\ c* are

(62)

where the function is the minimum of 30 meters and *HubHt*. For IEC
61400‑1 3\ :sup:`rd` ed. [21], the parameters are

(63)

The IEC 61400‑1 standard does not specify coherence for the *v* or *w*
wind-speed components.

NONE: Identity Coherence

When using the identity spatial coherence model for velocity component
*K*, the coherence between points *i* and *j* is defined as

(64)

API: API Longitudinal Coherence

The API coherence model implements the Frøya coherence model for wind
over water, and applies only to the longitudinal (*u*) component of the
velocity. The coherence between points *i* and *j* at frequency *f* is
defined as

| (65)
| where :math:`U_{0}` (assumed to be *URef*\ ) is the one-hour mean wind
  speed at a height of 10 meters above mean sea level, and coefficient
  :math:`A_{k}` is defined by

| (66)
| with reference height :math:`H = 10` m, and the coefficients
  :math:`\mathrm{\Delta}_{k}`, :math:`q_{k}`, :math:`p_{k}`,
  :math:`r_{k}`, and :math:`\alpha_{k}` given in Table 13. Note that in
  TurbSim, :math:`\mathrm{\Delta}_{1} = 0`.

Table 13. Coefficients for the API (Frøya) Coherence Model

===== ====================================== =============== =============== =============== ====================
**k** .. math:: \mathrm{\Delta}_{k}          .. math:: q_{k} .. math:: p_{k} .. math:: r_{k} .. math:: \alpha_{k}
===== ====================================== =============== =============== =============== ====================
1     .. math:: \left| x_{j} - x_{i} \right| 1.00            0.4             0.92            2.9
2     .. math:: |y_{j} - y_{i}|              1.00            0.4             0.92            45.0
3     .. math:: |z_{j} - z_{i}|              1.25            0.5             0.85            13.0
===== ====================================== =============== =============== =============== ====================

Velocity and Direction Profiles

TurbSim offers users a choice of mean wind (velocity) profiles. The
velocity profiles determine the mean *u*-component velocity at each
height for the length of the simulation. By definition, the mean *v*-
and *w*-component velocities are zero. Wind-direction profiles determine
the mean horizontal wind direction at each height. A wind-direction
profile is calculated with the low-level jet wind-speed profile and with
the user-defined velocity profiles, but direction profiles are not
calculated with the other velocity profiles.

For velocity profiles that use a reference height and wind speed,
TurbSim uses the inputs *URef* and *RefHt* as the reference point to
calculate the mean velocity at *HubHt*, *. T*\ he velocities at other
heights then are calculated using *and HubHt as the reference point.*
**Figure 23** shows an example of four different types of mean velocity
profiles that were generated using default boundary conditions and
*RICH_NO* = 0.05 with the GP_LLJ turbulence model. For each of the
velocity profiles plotted in the figure, *URef* = 12 m/s and
*RefHt = HubHt* = 90 m.

|image24|

Power-Law Wind Profile

The power-law mean velocity profile uses the *PLExp* input parameter to
calculate the average wind speed at height *z* using the equation

(67)

where is the mean wind speed at *z and z\ ref* is a reference height
above ground where the mean wind speed is known.

Logarithmic Wind Profile

The diabatic (logarithmic) wind profile calculates the average wind
speed at height *z* using the equation

(68)

where is the mean wind speed at *z, z\ ref* is a reference height above
ground where the mean wind speed is known, and *Z0* is the input surface
roughness. The function varies with the *RICH_NO* stability parameter.
When *RICH_NO = *\ 0 (as is the case with the IEC spectral models), .

Logarithmic Water Profile

The “water” logarithmic mean velocity profile calculates the average
flow speed at height *z* using the equation

(69)

where :math:`\kappa = 0.41` is von Karmon’s constant. To specify this
type of mean velocity profile use “H2L” (short for “H2O Log”) as the
*WindProfileType* input parameter. This velocity profile should always
and only be used with the TIDAL spectral model. Note that *z\ ref*
should be far from the inertial boundary layer. In general, *z\ ref*
should be greater than 10 meters and/or equal to the water depth of the
tidal channel.

IEC Wind Profile

The IEC wind profile was the only wind-speed profile available in SNwind
and SNLWIND-3D. This profile uses the power-law wind profile for the
wind speeds at heights on the rotor disk and the logarithmic profile for
heights not on the rotor disk. For example, if *URef* is specified at a
*RefHt* below the rotor disk, the logarithmic profile is used to
calculate the *HubHt* mean wind speed. Then the power-law profile would
be used with the *HubHt* wind speed to calculate winds across the rotor
disk. This profile could cause a discontinuity in the wind profile at
the bottom of the rotor disk (this discontinuity would be noticed with
tower points and with grids where *GridWidth* < *GridHeight*).

Low-Level Jet Wind Profile

The low-level jet wind profile is derived from LLLJP 10-minute SODAR
measurements and is available with only the GP_LLJ spectral model. This
profile type is unique because it generates both wind-speed and
wind-direction profiles.

The low-level jet wind-speed profile is defined using Chebyshev
polynomials,

(70)

where *z* is the height above ground, is the mean wind speed at height
*z*, *T\ n*\ (*z*) is the *n*\ :sup:`th` order Chebyshev polynomial, and
*c\ n* is a Chebyshev coefficient. The Chebyshev coefficients are
derived from LLLJP data and are a linear combination of the jet wind
speed, *, and* input parameters *RICH_NO* and *UStar*:

(71)

The coefficients, , *i = 1, 2, 3, 4*, are determined by the input
parameter *ZJetMax.*

The low-level jet wind-direction profile, like the wind-speed profile,
is a Chebyshev polynomial with coefficients derived from the same
parameters in the LLLJP data. The wind-direction profile is a relative
horizontal direction and is always zero at the hub height. The
*HFlowAng* rotation is added to the relative direction provided from
this profile.

Figure 24 plots example jet wind-speed and wind-direction profiles for
three different jet heights. The profiles have been generated with
*RICH_NO = *\ 0.05, and an 80-m (hub-height) wind speed of 12 m/s. The
*UStar* parameter is 0.411 m/s, which is the default for these GP_LLJ
conditions.

|image25|

API Wind Profile

The API wind profile is defined by the equation

(72)

where *z* is the height above ground, is the mean wind speed at height
*z*, *URef* is the one-hour mean wind speed, and *RefHt* is 10 meters.

User-Defined Velocity Profiles

When *WindProfileType* is USR or TS, TurbSim linearly interpolates the
input velocity and direction profiles. These profiles are input directly
for the USR model; for the TS model, the profiles are calculated from
the mean values of the input time series.

The profiles are extrapolated by using a nearest-neighbor approach: the
profiles are constant at heights above or below the heights where the
input profiles are defined.

Coherent Structures

For analysis purposes, coherent structures have been defined in terms of
CTKE (see Eq. (7) for the CTKE definition). A coherent structure is an
event where the 3-s mean CTKE meets a specified threshold value,
determined by the mean background levels of a particular site. The event
lasts from the time the threshold is first met until the 3-s mean CTKE
falls below the threshold value. For the LLLJP data, the threshold
chosen was 2 m\ :sup:`2`/s:sup:`2`, and for the LIST and wind-farm data,
the threshold chosen was 5 m\ :sup:`2`/s:sup:`2`. Figure 25 gives an
example of CTKE measured in the NWTC LIST experiment and shows the
detected coherent structures.

The background flow that is produced in TurbSim (i.e., the wind speed
data contained in the FF and HH output files) does contain coherent
structures, using the definition above. These wind files, however, do
not always generate as many coherent structures as observed in the
atmosphere. To obtain more events with realistic spatial-temporal
characteristics, sections (in time) of numerical simulations of a
Kelvin-Helmholtz billow are added randomly to the background flow when
the input parameter *WrACT* is “true.” TurbSim generates a coherent
turbulence time-step file (“.cts”) with the information describing how
to scale the billow and where the events should be added. These events
then are superimposed on the background flow in AeroDyn v13.

An example of the superimposed structures is shown in Figure 26. The
black line in the plot shows the 3-s mean CTKE of the background flow at
one point on the grid; the green line shows the 3-s mean CTKE of the
background with the addition of events in the “.cts” file at the same
grid point. It should be noted that the “.cts” files can *decrease* the
CTKE of the background as well as *increase* it.

|image26|

|image27|

Adding and Scaling the Coherent Structures

The Kelvin-Helmholtz billow has been broken up into several different
pieces, which are a fixed non-dimensional size with non-dimensional
velocities. Before adding these pieces to the background flow, they must
be scaled in space (through the *DistScl* input parameter) and in time
to determine the dimensional velocities. TurbSim randomly chooses the
start times of the billow pieces from an exponential distribution; the
choice of which piece of the billow is inserted at those places is
determined from a uniform random distribution.

The coherent structure scaling for the site-specific spectral models has
been determined from analysis of sonic anemometer measurements at each
of the respective sites, which are described in the Spectral Models
section of this guide. The SMOOTH model uses the same scaling as the
GP_LLJ model. Coherent structures are not added to the IEC spectral
models.

The three non-input parameters for scaling the non-dimensional pieces of
the billow and adding them to the background time series are discussed
below. A flow chart with these parameters is included in Appendix C.

Interarrival Times

The interarrival time is the time from the start of one event to the
start of the next event. These times are exponentially distributed
random variables with rate parameters determined from the analyzed
datasets. For the GP_LLJ and SMOOTH models, the random distribution is
influenced by the height and wind speed, , at the center of the billow.
For the NWTCUP and the wind-farm models, the random distribution is
influenced by and *RICH_NO*.

Expected Length of Coherent Structures

The length of coherent structures is the total amount of time that
contains coherent structures in a given record. The expected lengths for
each of the non-IEC spectral models are selected from a random
distribution whose probability density function matches the data from
their respective datasets.

TurbSim concatenates extra pieces of the billow to pieces that already
have been added to the coherent structure file until the total length of
the events is at least the expected length of the coherent structures
from the datasets.

Peak Coherent Turbulent Kinetic Energy

The velocities for the coherent events are scaled to achieve a specific
peak value in CTKE in the set of events added to the background. The
peak CTKE is a function of several different parameters, depending on
the spectral model. These parameters include height, *z*; mean wind
speed of the background flow at the center of the billow, ; shear across
the billow (difference in mean wind speed between the top and bottom of
the billow), ; standard deviation of vertical wind speed at the center
of the billow, ; and input parameters *RICH_NO* and *UStar*. Some models
also include a random component. Table 13 shows which particular
parameters are used for each of the non-IEC spectral models.

Using Coherent Turbulence Time-Step Files with AeroDyn v13

To use the coherent time-step files that TurbSim generates (files with
the “.cts” extension), a coherent turbulence parameter input file must
be created for AeroDyn v13’s InflowWind module. This file must have a
“.ctp” extension, and the name of this “.ctp” file must be entered on
the *WindFile* parameter line in the AeroDyn input file (using v12.57
through v13.*).

See Appendix H in this document for an example of the “.ctp” input file.
Do not add or delete lines from the file because AeroDyn v13 assumes
parameters are on specific lines. The parameters in the file are
discussed below.

CTSpath: Name of path to coherent turbulence binary data files [-]

The *CTSpath* parameter is the name of the path that contains the binary
data files for the coherent structures, which you must get from the
coherent structure archive on the `TurbSim Web
site <http://nwtc.nrel.gov/TurbSim>`__ (in folder x90_i16). This
directory must contain files called “Scales.les” and “Scales.dns,” which
contain scaling parameters for the two event types, and are used to read
and convert normalized 16-bit integer binary data to real numbers. There
should also be three folders in this directory, named “\ *u,”* “\ *v,”*
and “\ *w”* respectively, containing data for the three wind components.
Each of these three directories contains files named something like
“u_i16\_\ *xxxxx*.les.”

CTTSfile: Name of TurbSim CTS file [-]

The parameter *CTTSfile* is the name of the coherent time-step file
generated by TurbSim. It has a “.cts” extension. This file name must be
specified relative to the directory from which AeroDyn v13will be run.

CTbackgr: Name of TurbSim background FF file [-]

The parameter *CTbackgr* is the name of the background turbulence file.
This should be the FF wind file with the “.wnd” or “.bts” extension that
was generated at the same time the “.cts” file was created. This file
name also must be specified relative to the directory from which AeroDyn
will be run. AeroDyn v13 automatically looks for the “.sum” summary file
that goes with a binary “.wnd” file.

CT_DF_Y: Decimation factor in the Y direction [-]

The *CT_DF_Y* parameter is used for decimating the binary coherent
turbulence data in the horizontal, *Y*, direction. Enter the horizontal
decimation factor: A value of 1 uses every point in the *Y* direction, 2
uses every other point, etc. It is *recommended that you always use the
entire grid (i.e., CT_DF_Y = 1).*

CT_DF_Z: Decimation factor in the Z direction [-]

The *CT_DF_Z* parameter is used for decimating the binary coherent
turbulence data in the vertical, Z, direction. Enter the vertical
decimation factor: A value of 1 uses every point in the *Z* direction, 2
uses every other point, etc. It is recommended *that you always use the
entire grid (i.e., CT_DF_Z = 1).*

Suggestions for Generating Coherent Turbulent Structures

Effort has been made in TurbSim to randomize the occurrence and scaling
of coherent event structures that occur in natural, nocturnal boundary
layer flows. Simulations that generate coherent turbulence time-step
files have up to 10 degrees of stochastic freedom—in addition to the
random phases associated with each frequency at each grid point and wind
component—and are designed to give some feel of the expected variability
in the atmosphere. Because of the degree of variability, using more than
30 different random seeds2F [3]_ for a specific set of boundary
conditions is recommended.

To test the effects of a coherent structure (KH billow), we recommend
using the “KHTEST” option in the *IECturbc* input parameter with the
NWTCUP spectral model. This test function superimposes one intense
coherent event in the middle of the output time series, reducing the
number of stochastic degrees of freedom to no more than two (plus the
random phases). The gradient Richardson number (*RICH_NO*) and wind
shear (*PLExp*) of the background flow are overwritten, and TurbSim uses
fixed values to scale the LES-type event. This test function is designed
to generate intense turbulence, and does not necessarily reflect the
variability for given boundary conditions.

The choice of the gradient Richardson number and hub wind speed largely
control the impact of coherent structures on turbine response. It is
recommended that at least one series of runs be made at rated wind speed
and a Richardson number between 0.02 and 0.05. Further discussion on the
impact of coherent turbulent structures on wind turbines is found in
[40].

Warnings

-  AeroDyn v12.57 or a later version is required to read TurbSim files
   correctly.

-  If you compile AeroDyn v13, you must use the compiler option
   “/assume:byterecl” to read the TurbSim coherent structures binary
   files correctly. If you use ADAMS2AD [41], be sure to use v12.17 or
   later so that this compiler option is set.

-  Hub-height time series from HH wind files and FF wind files (without
   *UsableTime* = “ALL”) do not have events happening at the same time
   because InflowWind shifts the FF files (see Figure 17).

-  Because of the way the FFT routine works, extra time may be added to
   the analysis time to get the FFT to run efficiently. Due to this plus
   the fact that the output time could be shorter than the analysis
   time, the mean wind speed for the portion of the run actually used
   could be different from what was specified in the input file.

-  The statistics calculated in the summary file are based on the
   complete time series generated (the analysis time plus any extra time
   added for the FFT calculations). Because the output time can be less
   than the analysis time, these statistics might differ from what can
   be calculated from the output files.

-  Be cautious when using mean flow angle inputs with full-field grids
   for InflowWind. InflowWind marches FF grids through the turbine along
   its Propogation Direction at the mean hub-height wind speed,
   regardless of the flow angles. This can give strange results if the
   mean flow angles are not small.

Limitations

-  The GP_LLJ spectral model is estimated to be applicable up to a
   height of 230 m.

-  The SMOOTH spectral model and the coherent turbulence time-step files
   are both currently estimated to be applicable up to a height of
   120 m.

-  The NWTCUP spectral model is estimated to be applicable up to heights
   of 120 m.

-  The wind farm spectral models (WF_UPW, WF_07D, and WF_14D) are valid
   only up to heights of about 50 m.