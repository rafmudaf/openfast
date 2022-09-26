.. _appendixa:

Appendix A: Sample Input Files
==============================
The following input files are **examples only**, the time series data are not
complete and cannot be used as-is for running TurbSim. However, these files
may be used as a starting point for generating complete files.

Primary input file
~~~~~~~~~~~~~~~~~~
::

    ---------TurbSim v2.00.* Input File------------------------
    Example input file for TurbSim.
    ---------Runtime Options-----------------------------------
    False     Echo        - Echo input data to <RootName>.ech (flag)
    2318573   RandSeed1   - First random seed (-2147483648 to 2147483647)
    RANLUX    RandSeed2   - Second random seed for intrinsic pRNG, or other pRNG: "RanLux" or "RNSNLW"
    False     WrBHHTP     - Output HH turbulence parameters in GenPro-binary form? (Generates RootName.bin)
    False     WrFHHTP     - Output HH turbulence parameters in formatted form? (Generates RootName.dat)
    False     WrADHH      - Output hub-height time-series data in AeroDyn form? (Generates RootName.hh)
    False     WrADFF      - Output FF time-series data in TurbSim/AeroDyn form? (Generates Rootname.bts)
    True      WrBLFF      - Output FF time-series data in BLADED/AeroDyn form? (Generates RootName.wnd)
    False     WrADTWR     - Output tower time-series data? (Generates RootName.twr)
    False     WrFMTFF     - Output FF time-series data in formatted (readable) form? (RootName.u, .v, .w)
    True      WrACT       - Output coherent turbulence time steps in AeroDyn form? (Generates RootName.cts)
    True      Clockwise   - Clockwise rotation looking downwind? (Used only for FF binary files w/ BLADED)
    0         ScaleIEC    - Scale IEC turbulence models to exact target std deviation? [0=none;1=hub;2=all]

    --------Turbine/Model Specifications-----------------------
    13        NumGrid_Z   - Vertical grid-point matrix dimension
    13        NumGrid_Y   - Horizontal grid-point matrix dimension
    0.05      TimeStep    - Time step [s]
    600       AnalysisTime- Length of analysis time series [s] (program will add time if necessary)
    "ALL"     UsableTime  - Usable length of output time series [s] (GridWidth/MeanHHWS s added if not "ALL")
    84.30     HubHt       - Hub height [m] (should be > 0.5*GridHeight)
    80.00     GridHeight  - Grid height [m]
    80.00     GridWidth   - Grid width [m] (should be >= 2*(RotorRadius+ShaftLength))
    0         VFlowAng    - Vertical mean flow (uptilt) angle [degrees]
    0         HFlowAng    - Horizontal mean flow (skew) angle [degrees]

    --------Meteorological Boundary Conditions-------------------
    "SMOOTH"  TurbModel   - Turbulence model (see Table 4 for valid codes)
    "unused"  UserFile    - Name secondary input file for user-defined spectra or time series inputs
    "1-ED2"   IECstandard - Number of the IEC standard (61400-x, x=1,2,3) with optional 61400-1 ed. number
    "A"       IECturbc    - IEC turbulence characteristic ("A", "B", "C" or TI in %) or KHTEST
    "NTM"     IEC_WindType- IEC turbulence type ("NTM", "xETM", "xEWM1", or "xEWM50" for x=class 1, 2, or 3)
    default   ETMc        - IEC Extreme turbulence model "c" parameter [m/s] (or "default")
    "PL"      ProfileType - Wind profile type (see Table 6 for valid codes)
    "unused"  ProfileFile - Name of the file that contains user-defined input profiles
    84.30     RefHt       - Height of the reference wind speed [m]
    18.2      URef        - Mean wind speed at the reference height [m/s] [must be 1-hr mean for API model]
    450       ZJetMax     - Height of the low-level jet [m] (70-490 m or "default", only for "JET" profile)
    default   PLExp       - Power law exponent (or "default")
    default   Z0          - Surface roughness length [m] (or "default")

    --------Non-IEC Meteorological Boundary Conditions------------
    default   Latitude    - Site latitude [degrees] (or "default")
    0.05      RICH_NO     - Gradient Richardson number [-]
    default   UStar       - Friction or shear velocity [m/s] (or "default")
    default   ZI          - Mixing layer depth [m] (or "default")
    default   PC_UW       - Hub mean u'w' Reynolds stress [m^2/s^2] (or "default" or "none")
    default   PC_UV       - Hub mean u'v' Reynolds stress [m^2/s^2] (or "default" or "none")
    default   PC_VW       - Hub mean v'w' Reynolds stress [m^2/s^2] (or "default" or "none")

    --------Spatial Coherence Parameters----------------------------
    default   SCMod1      - u-component coherence model ("GENERAL","IEC","API","NONE", or "default")
    default   SCMod2      - v-component coherence model ("GENERAL","IEC","NONE", or "default")
    default   SCMod3      - w-component coherence model ("GENERAL","IEC","NONE", or "default")
    default   InCDec1     - u-component coherence parameters [-, m^-1] ("a b" in quotes or "default")
    default   InCDec2     - v-component coherence parameters [-, m^-1] ("a b" in quotes or "default")
    default   InCDec3     - w-component coherence parameters [-, m^-1] ("a b" in quotes or "default")
    default   CohExp      - Coherence exponent for general model [-] (or "default")

    --------Coherent Turbulence Scaling Parameters-------------------
    "path/to/coh_events/eventdata"  CTEventPath     - Name of the path where event data files are located
    "Random"  CTEventFile - Type of event files ("LES", "DNS", or "RANDOM")
    True      Randomize   - Randomize the disturbance scale and locations? (true/false)
    1.0       DistScl     - Disturbance scale (ratio of wave height to rotor disk).
    0.5       CTLy        - Fractional location of tower center from right to L of dataset looking downwind
    0.5       CTLz        - Fractional location of hub height from the bottom of the dataset
    30.0      CTStartTime - Minimum start time for coherent structures in RootName.cts [s]

User-defined time series input file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
::

    --------------TurbSim v2.00.* User Time Series Input File------------------
    Time series input 2 sonic anemometers from 01134_16_40_00_013.mat. Using rotated series.
    ---------------------------------------------------------------------------
            3  nComp    - Number of velocity components in the file
            2  nPoints  - Number of time series points contained in this file (-)
            2  RefPtID  - Index of the reference point (1-nPoints)
        Pointyi     Pointzi         ! nPoints listed in order of increasing height
        (m)         (m)
        0.00000    30.00000
        0.00000    76.00000
    --------Time Series--------------------------------------------------------
    Elapsed Time     Point01u        Point01v      Point01w     Point02u      Point02v        Point02w
            (s)         (m/s)         (m/s)         (m/s)         (m/s)         (m/s)         (m/s)
        0.0000       10.0239       -6.5673        0.1700       10.7104       -4.3265       -0.2657
        0.0500        9.8543       -6.6871        0.2014       10.5539       -4.5656       -0.1635
        0.1000        9.6866       -6.9837       -0.0274       10.6105       -4.1738       -0.1907
        0.1500        9.7324       -7.0552       -0.0051       10.6691       -4.4155       -0.0675
        0.2000       10.6893       -6.8507       -0.9577       10.3897       -4.9771        0.2487
        0.2500        9.9231       -7.3007        0.7656       10.4993       -4.6568        0.1041
        0.3000       10.6087       -7.4602        1.1109       10.6404       -4.6216        0.4016
        0.3500       10.7004       -6.5530        1.5361       10.6060       -5.0307        0.2697
        0.4000       10.6239       -6.5870        0.9715       10.2804       -5.5762        0.2131
        0.4500       10.3173       -6.9557        0.7657        9.7826       -5.9725        0.4581
        0.5000       10.1416       -7.2209        0.7567       10.0303       -4.9716        0.6309
        0.5500       10.5047       -6.7512        0.6150        9.2657       -4.9317        0.3516
        0.6000       10.7474       -6.2916        1.0679        9.8545       -4.6793        0.9724
        0.6500       10.0867       -7.4206        0.5036        9.7205       -4.9432        1.1458
        0.7000        9.8459       -7.4542       -1.2710        8.9698       -4.7850        1.0775
        0.7500        9.6427       -7.2455       -1.4315        9.3917       -4.6785        1.1891
        0.8000        9.5695       -7.7153       -0.7343        9.5739       -4.4328        1.1584
        0.8500       10.2921       -6.9918       -0.9979        9.6578       -4.3620        1.2127
        0.9000        9.8191       -6.6210        0.3998        9.8743       -4.2941        1.0936
        0.9500       10.0563       -6.9999        0.2417       10.3157       -4.2559        1.1260
        1.0000        9.2220       -5.9308        0.8000        9.9854       -4.1755        1.2094
        1.0500       10.0784       -5.5374        2.1954        9.5217       -4.6836        0.8753
        1.1000        9.5813       -5.8415        2.4204       10.2011       -4.7455        0.9099
        1.1500       10.1393       -5.7391        1.2873        9.5294       -5.2682        0.6955
        1.2000       10.3018       -6.1910        0.7048        9.3079       -5.5758        0.5641
        1.2500       10.4492       -6.5951        1.0127        9.5492       -6.0838        0.6965
        1.3000        9.7664       -7.2437        0.7676        9.8434       -6.0361        1.7628
        1.3500        8.8919       -7.6760       -0.0979       10.1855       -5.7703        2.1307
        1.4000        8.5238       -7.3008       -0.3770       10.8332       -4.6349        1.7131
        1.4500        8.8623       -7.0775       -0.9606       11.0740       -3.6287        1.5952
        1.5000        8.9728       -7.6597       -1.1552       10.7549       -4.2620        1.7992
        1.5500        8.8930       -7.7153       -1.7600       10.7559       -5.3923        1.5490
        < Lines omitted >
        599.7500       21.6185        1.3266        0.5301       20.7629        1.8099        0.4765
        599.8000       20.6428        2.2662        0.6105       20.6605        2.0787        0.8918
        599.8500       20.0781        2.4219        1.1325       20.0819        2.0141        1.2528
        599.9000       19.9940        1.8457        1.7090       20.2872        2.2371        1.4736
        599.9500       20.6705        2.1299        2.4844       20.4711        2.0164        1.8634

User-defined spectra series input file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
::

    -------- User-Defined Spectra (Used only with USRINP spectral model) ------------------------------------
    -        The Kaimal spectra IEC 61400-1 Ed. 3 for Vhub=12 m/s; Zhub=90 m; Class="B";                    -
    ---------------------------------------------------------------------------------------------------------
    20000           NumUSRf        - Number of Frequencies [determines how many lines to read from this file]
    1.0             SpecScale1     - scaling factor for the input u-component spectrum
    1.0             SpecScale2     - scaling factor for the input v-component spectrum
    1.0             SpecScale3     - scaling factor for the input w-component spectrum
    .........................................................................................................
    Frequency    u-component PSD   v-component PSD      w-component PSD
    (Hz)           (m^2/s)           (m^2/s)             (m^2/s)
    ---------------------------------------------------------------------------------------------------------
    0.001         364.644672       92.196417            9.432145
    0.002         290.820811       84.504829            9.221093
    0.003         238.306635       77.790863            9.017498
    0.004         199.474346       71.891456            8.821001
    0.005         169.860744       66.676649            8.631266
    0.006         146.703651       62.041773            8.447977
    0.007         128.214357       57.901706            8.270838
    0.008         113.190378       54.186616            8.099568
    0.009         100.797523       50.838749            7.933903
    0.010          90.441383       47.809980            7.773593
    0.011          81.688510       45.059936            7.618404
    0.012          74.216397       42.554527            7.468113
    0.013          67.780802       40.264798            7.322511
    0.014          62.193824       38.166019            7.181398
    0.015          57.308865       36.236959            7.044586
    0.016          53.010107       34.459303            6.911897
    0.017          49.205001       32.817181            6.783162
    0.018          45.818825       31.296779            6.658222
    0.019          42.790679       29.886027            6.536923
    0.020          40.070501       28.574339            6.419121
    0.021          37.616811       27.352396            6.304680
    < Lines omitted >
    19.994           0.000616        0.000819            0.000814
    19.995           0.000616        0.000819            0.000814
    19.996           0.000616        0.000819            0.000814
    19.997           0.000616        0.000819            0.000814
    19.998           0.000615        0.000819            0.000814
    19.999           0.000615        0.000818            0.000814
    20.000           0.000615        0.000818            0.000814

User-defined profile series input file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
::

    ---------TurbSim v2.00.* Profile Input File------------------------
    Example file using completely made up profiles
    -------- User-Defined Profiles (Used only with USR wind profile or USRVKM spectral model) -------------
    5               NumUSRz        - Number of Heights
    1.092           StdScale1      - u-component scaling factor for the input standard deviation
    1.0             StdScale2      - v-component scaling factor for the input standard deviation
    0.534           StdScale3      - w-component scaling factor for the input standard deviation
    -----------------------------------------------------------------------------------
    Height    Wind Speed       Wind Direction          Standard Deviation    Length Scale
    (m)        (m/s)       (deg, cntr-clockwise )            (m/s)              (m)
    -----------------------------------------------------------------------------------
    15.0           3            00                            .100                  3
    25.0           4            00                            .200                  4
    35.0           5            00                            .300                  6
    45.0           6            00                            .100                  9
    55.0           7            00                            .500                 13

.. _appendixb:

Appendix B: TurbSim Quick-Start Guidelines for IEC Turbulence
=============================================================
To generate IEC-type turbulence, many of the parameters in the TurbSim
input file can be ignored. Figure B-1 shows a TurbSim input file set up
to generate IEC 61400‑1 3\ :sup:`rd` ed., category “B” turbulence for
the NTM using the Kaimal model. It creates a FF Bladed-style “.wnd” file
containing 600 seconds of (periodic) usable data, using a time step of
0.05 s.

The input parameters that typically might have to be changed are
mentioned below, along with suggestions for typical values. See
the :ref:`ts_input_files` section for more details on the parameters.

The parameters in *blue italics* in Figure B-1 should be changed based
on the particular turbine for which the wind field is being generated:

*ScaleIEC*: Change this parameter to the type of scaling desired. If you
are unsure, use 0.

*NumGrid_Z*: The number of vertical grid points should be set so there
is sufficient vertical grid resolution. A typical value is an odd
integer that is close to the *GridHeight* divided by the mean chord of
the turbine’s blades.

*NumGrid_Y*: The number of lateral grid points should be set so there is
sufficient lateral grid resolution. A typical value is an odd integer
that is close to the *GridWidth* divided by the mean chord of the
turbine’s blades.

*HubHt*: This is the hub height in meters of the turbine for which the
turbulence is being generated.

*GridHeight*: The grid height (in meters) typically is 10% larger than
the turbine rotor diameter. It must be larger for turbines that have
significant displacements.

*GridWidth*: The grid width (in meters) typically is the same as
*GridHeight*.

*IECturbc*: The turbulence category should be “A,” “B,” or “C,”
depending on the desired 61400‑1 category. Category “A” is the most
turbulent.

*RefHt*: The reference height is the height (in meters) where the input
wind speed is defined. It is typically the same as *HubHt*.

The parameters in **bold red** in Figure B-1 typically are changed for
each case when running design load cases:

*RandSeed1*: The random seed, which initializes the pseudo-random number
generator, should be a different number for each simulation. For each
case, several different seeds should be used, keeping *all* other input
parameters constant.

*IEC_WindType*: This is the wind condition for the (turbulent) IEC load
cases. It often is NTM. For other conditions, see Table 5 of this guide.

*URef*: This is the reference wind speed (in meters per second) at the
*RefHt*. It typically ranges from cut-in to cut-out in 2 m/s increments.

::

    ---------TurbSim v2.00.* Input File------------------------
    Example input file for TurbSim.
    ---------Runtime Options-----------------------------------
    False     Echo        - Echo input data to <RootName>.ech (flag)
    1234567   RandSeed1   - First random seed (-2147483648 to 2147483647)
    RANLUX    RandSeed2   - Second random seed for intrinsic pRNG, or other pRNG: "RanLux" or "RNSNLW"
    False     WrBHHTP     - Output HH turbulence parameters in GenPro-binary form? (Generates RootName.bin)
    False     WrFHHTP     - Output HH turbulence parameters in formatted form? (Generates RootName.dat)
    False     WrADHH      - Output hub-height time-series data in AeroDyn form? (Generates RootName.hh)
    False     WrADFF      - Output FF time-series data in TurbSim/AeroDyn form? (Generates Rootname.bts)
    True      WrBLFF      - Output FF time-series data in BLADED/AeroDyn form? (Generates RootName.wnd)
    False     WrADTWR     - Output tower time-series data? (Generates RootName.twr)
    False     WrFMTFF     - Output FF time-series data in formatted (readable) form? (RootName.u, .v, .w)
    False     WrACT       - Output coherent turbulence time steps in AeroDyn form? (Generates RootName.cts)
    True      Clockwise   - Clockwise rotation looking downwind? (Used only for FF binary files w/ BLADED)
    0         ScaleIEC    - Scale IEC turbulence models to exact target std deviation? [0=none;1=hub;2=all]

    --------Turbine/Model Specifications-----------------------
    13        NumGrid_Z   - Vertical grid-point matrix dimension
    13        NumGrid_Y   - Horizontal grid-point matrix dimension
    0.05      TimeStep    - Time step [s]
    600       AnalysisTime- Length of analysis time series [s] (program will add time if necessary)
    "ALL"     UsableTime  - Usable length of output time series [s] (GridWidth/MeanHHWS s added if not "ALL")
    84.30     HubHt       - Hub height [m] (should be > 0.5*GridHeight)
    80.00     GridHeight  - Grid height [m]
    80.00     GridWidth   - Grid width [m] (should be >= 2*(RotorRadius+ShaftLength))
    0         VFlowAng    - Vertical mean flow (uptilt) angle [degrees]
    0         HFlowAng    - Horizontal mean flow (skew) angle [degrees]

    --------Meteorological Boundary Conditions-------------------
    "IECKAI"  TurbModel   - Turbulence model (see Table 4 for valid codes)
    "unused"  UserFile    - Name secondary input file for user-defined spectra or time series inputs
    "1-ED3"   IECstandard - Number of the IEC standard (61400-x, x=1,2,3) with optional 61400-1 ed. number
    "B"       IECturbc    - IEC turbulence characteristic ("A", "B", "C" or TI in %) or KHTEST
    "NTM"     IEC_WindType- IEC turbulence type ("NTM", "xETM", "xEWM1", or "xEWM50" for x=class 1, 2, or 3)
    default   ETMc        - IEC Extreme turbulence model "c" parameter [m/s] (or "default")
    "PL"      ProfileType - Wind profile type (see Table 6 for valid codes)
    "unused"  ProfileFile - Name of the file that contains user-defined input profiles
    84.30     RefHt       - Height of the reference wind speed [m]
    18.2      URef        - Mean wind speed at the reference height [m/s] [must be 1-hr mean for API model]
    450       ZJetMax     - Height of the low-level jet [m] (70-490 m or "default", only for "JET" profile)
    default   PLExp       - Power law exponent (or "default")
    default   Z0          - Surface roughness length [m] (or "default")

    --------Non-IEC Meteorological Boundary Conditions------------
    default   Latitude    - Site latitude [degrees] (or "default")
    0.05      RICH_NO     - Gradient Richardson number [-]
    default   UStar       - Friction or shear velocity [m/s] (or "default")
    default   ZI          - Mixing layer depth [m] (or "default")
    default   PC_UW       - Hub mean u'w' Reynolds stress [m^2/s^2] (or "default" or "none")
    default   PC_UV       - Hub mean u'v' Reynolds stress [m^2/s^2] (or "default" or "none")
    default   PC_VW       - Hub mean v'w' Reynolds stress [m^2/s^2] (or "default" or "none")

    --------Spatial Coherence Parameters----------------------------
    default   SCMod1      - u-component coherence model ("GENERAL","IEC","API","NONE", or "default")
    default   SCMod2      - v-component coherence model ("GENERAL","IEC","NONE", or "default")
    default   SCMod3      - w-component coherence model ("GENERAL","IEC","NONE", or "default")
    default   InCDec1     - u-component coherence parameters [-, m^-1] ("a b" in quotes or "default")
    default   InCDec2     - v-component coherence parameters [-, m^-1] ("a b" in quotes or "default")
    default   InCDec3     - w-component coherence parameters [-, m^-1] ("a b" in quotes or "default")
    default   CohExp      - Coherence exponent for general model [-] (or "default")

    --------Coherent Turbulence Scaling Parameters-------------------
    "path/to/coh_events/eventdata"  CTEventPath     - Name of the path where event data files are located
    "Random"  CTEventFile - Type of event files ("LES", "DNS", or "RANDOM")
    True      Randomize   - Randomize the disturbance scale and locations? (true/false)
    1.0       DistScl     - Disturbance scale (ratio of wave height to rotor disk).
    0.5       CTLy        - Fractional location of tower center from right to L of dataset looking downwind
    0.5       CTLz        - Fractional location of hub height from the bottom of the dataset
    30.0      CTStartTime - Minimum start time for coherent structures in RootName.cts [s]



Figure B-1. Sample TurbSim input file for IEC turbulence: parameters
shown in blue should be changed based on the turbine configuration;
parameters shown in red should be changed for each load case and
simulation. (Note: figure is continued from previous page.)
