!**********************************************************************************************************************************
! FAST_Solver.f90, FAST_Subs.f90, FAST_Lin.f90, and FAST_Mods.f90 make up the FAST glue code in the FAST Modularization Framework.
! FAST_Prog.f90, FAST_Library.f90, FAST_Prog.c are different drivers for this code.
!..................................................................................................................................
! LICENSING
! Copyright (C) 2013-2016  National Renewable Energy Laboratory
!
!    This file is part of FAST.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!**********************************************************************************************************************************
MODULE FAST_IO

   USE FAST_ModTypes
   USE FAST_VTK, ONLY: WriteVTK
   USE ServoDyn
   USE VersionInfo, ONLY: CheckArgs

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------------------------------------------------------------
!> This routine reads in the primary FAST input file, does some validation, and places the values it reads in the
!!   parameter structure (p). It prints to an echo file if requested.
SUBROUTINE FAST_ReadPrimaryFile( InputFile, p, m_FAST, OverrideAbortErrLev, ErrStat, ErrMsg )

   IMPLICIT                        NONE

      ! Passed variables
   TYPE(FAST_ParameterType), INTENT(INOUT) :: p                               !< The parameter data for the FAST (glue-code) simulation
   TYPE(FAST_MiscVarType),   INTENT(INOUT) :: m_FAST                          !< Miscellaneous variables
   CHARACTER(*),             INTENT(IN)    :: InputFile                       !< Name of the file containing the primary input data
   LOGICAL,                  INTENT(IN)    :: OverrideAbortErrLev             !< Determines if we should override AbortErrLev
   INTEGER(IntKi),           INTENT(OUT)   :: ErrStat                         !< Error status
   CHARACTER(*),             INTENT(OUT)   :: ErrMsg                          !< Error message

      ! Local variables:
   REAL(DbKi)                    :: TmpRate                                   ! temporary variable to read VTK_fps before converting to #steps based on DT
   REAL(DbKi)                    :: TmpTime                                   ! temporary variable to read SttsTime and ChkptTime before converting to #steps based on DT
   INTEGER(IntKi)                :: I                                         ! loop counter
   INTEGER(IntKi)                :: UnIn                                      ! Unit number for reading file
   INTEGER(IntKi)                :: UnEc                                      ! I/O unit for echo file. If > 0, file is open for writing.

   INTEGER(IntKi)                :: IOS                                       ! Temporary Error status
   INTEGER(IntKi)                :: ErrStat2                                  ! Temporary Error status
   INTEGER(IntKi)                :: OutFileFmt                                ! An integer that indicates what kind of tabular output should be generated (1=text, 2=binary, 3=both)
   LOGICAL                       :: Echo                                      ! Determines if an echo file should be written
   LOGICAL                       :: TabDelim                                  ! Determines if text output should be delimited by tabs (true) or space (false)
   CHARACTER(ErrMsgLen)          :: ErrMsg2                                   ! Temporary Error message
   CHARACTER(1024)               :: PriPath                                   ! Path name of the primary file

   CHARACTER(10)                 :: AbortLevel                                ! String that indicates which error level should be used to abort the program: WARNING, SEVERE, or FATAL
   CHARACTER(30)                 :: Line                                      ! string for default entry in input file

   CHARACTER(*),   PARAMETER     :: RoutineName = 'FAST_ReadPrimaryFile'


      ! Initialize some variables:
   UnEc = -1
   Echo = .FALSE.                        ! Don't echo until we've read the "Echo" flag
   CALL GetPath( InputFile, PriPath )    ! Input files will be relative to the path where the primary input file is located.


      ! Get an available unit number for the file.

   CALL GetNewUnit( UnIn, ErrStat, ErrMsg )
   IF ( ErrStat >= AbortErrLev ) RETURN


      ! Open the Primary input file.

   CALL OpenFInpFile ( UnIn, InputFile, ErrStat2, ErrMsg2 )
      CALL SetErrStat( ErrStat2, ErrMsg2,ErrStat,ErrMsg,RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if


   ! Read the lines up/including to the "Echo" simulation control variable
   ! If echo is FALSE, don't write these lines to the echo file.
   ! If Echo is TRUE, rewind and write on the second try.

   I = 1 !set the number of times we've read the file
   DO
   !-------------------------- HEADER ---------------------------------------------

      CALL ReadCom( UnIn, InputFile, 'File header: Module Version (line 1)', ErrStat2, ErrMsg2, UnEc )
         CALL SetErrStat( ErrStat2, ErrMsg2,ErrStat,ErrMsg,RoutineName)
         if ( ErrStat >= AbortErrLev ) then
            call cleanup()
            RETURN
         end if

      CALL ReadStr( UnIn, InputFile, p%FTitle, 'FTitle', 'File Header: File Description (line 2)', ErrStat2, ErrMsg2, UnEc )
         CALL SetErrStat( ErrStat2, ErrMsg2,ErrStat,ErrMsg,RoutineName)
         if ( ErrStat >= AbortErrLev ) then
            call cleanup()
            RETURN
         end if


   !---------------------- SIMULATION CONTROL --------------------------------------
      CALL ReadCom( UnIn, InputFile, 'Section Header: Simulation Control', ErrStat2, ErrMsg2, UnEc )
         CALL SetErrStat( ErrStat2, ErrMsg2,ErrStat,ErrMsg,RoutineName)
         if ( ErrStat >= AbortErrLev ) then
            call cleanup()
            RETURN
         end if


         ! Echo - Echo input data to <RootName>.ech (flag):
      CALL ReadVar( UnIn, InputFile, Echo, "Echo", "Echo input data to <RootName>.ech (flag)", ErrStat2, ErrMsg2, UnEc)
         CALL SetErrStat( ErrStat2, ErrMsg2,ErrStat,ErrMsg,RoutineName)
         if ( ErrStat >= AbortErrLev ) then
            call cleanup()
            RETURN
         end if


      IF (.NOT. Echo .OR. I > 1) EXIT !exit this loop

         ! Otherwise, open the echo file, then rewind the input file and echo everything we've read

      I = I + 1         ! make sure we do this only once (increment counter that says how many times we've read this file)

      CALL OpenEcho ( UnEc, TRIM(p%OutFileRoot)//'.ech', ErrStat2, ErrMsg2, FAST_Ver )
         CALL SetErrStat( ErrStat2, ErrMsg2,ErrStat,ErrMsg,RoutineName)
         if ( ErrStat >= AbortErrLev ) then
            call cleanup()
            RETURN
         end if

      IF ( UnEc > 0 )  WRITE (UnEc,'(/,A,/)')  'Data from '//TRIM(FAST_Ver%Name)//' primary input file "'//TRIM( InputFile )//'":'

      REWIND( UnIn, IOSTAT=ErrStat2 )
         IF (ErrStat2 /= 0_IntKi ) THEN
            CALL SetErrStat( ErrID_Fatal, 'Error rewinding file "'//TRIM(InputFile)//'".',ErrStat,ErrMsg,RoutineName)
            call cleanup()
            RETURN
         END IF

   END DO

   CALL WrScr( TRIM(FAST_Ver%Name)//' input file heading:' )
   CALL WrScr( '    '//TRIM( p%FTitle ) )
   CALL WrScr('')


      ! AbortLevel - Error level when simulation should abort:
   CALL ReadVar( UnIn, InputFile, AbortLevel, "AbortLevel", "Error level when simulation should abort (string)", &
                        ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2,ErrStat,ErrMsg,RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      IF (OverrideAbortErrLev) THEN
      ! Let's set the abort level here.... knowing that everything before this aborted only on FATAL errors!
         CALL Conv2UC( AbortLevel ) !convert to upper case
         SELECT CASE( TRIM(AbortLevel) )
            CASE ( "WARNING" )
               AbortErrLev = ErrID_Warn
            CASE ( "SEVERE" )
               AbortErrLev = ErrID_Severe
            CASE ( "FATAL" )
               AbortErrLev = ErrID_Fatal
            CASE DEFAULT
               CALL SetErrStat( ErrID_Fatal, 'Invalid AbortLevel specified in FAST input file. '// &
                                'Valid entries are "WARNING", "SEVERE", or "FATAL".',ErrStat,ErrMsg,RoutineName)
               call cleanup()
               RETURN
         END SELECT
      END IF


      ! TMax - Total run time (s):
   CALL ReadVar( UnIn, InputFile, p%TMax, "TMax", "Total run time (s)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      ! DT - Recommended module time step (s):
   CALL ReadVar( UnIn, InputFile, p%DT, "DT", "Recommended module time step (s)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      if ( EqualRealNos(p%DT, 0.0_DbKi) ) then
         ! add a fatal error here because we're going to divide by DT later in this routine:
         CALL SetErrStat( ErrID_Fatal, 'DT cannot be zero.', ErrStat, ErrMsg, RoutineName)
         call cleanup()
         return
      end if


      ! InterpOrder - Interpolation order for inputs and outputs {0=nearest neighbor ,1=linear, 2=quadratic}
   CALL ReadVar( UnIn, InputFile, p%InterpOrder, "InterpOrder", "Interpolation order "//&
                   "for inputs and outputs {0=nearest neighbor ,1=linear, 2=quadratic} (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      ! NumCrctn - Number of predictor-corrector iterations {1=explicit calculation, i.e., no corrections}
   CALL ReadVar( UnIn, InputFile, p%NumCrctn, "NumCrctn", "Number of corrections"//&
                   "{0=explicit calculation, i.e., no corrections} (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      ! DT_UJac - Time between calls to get Jacobians (s)
   CALL ReadVar( UnIn, InputFile, p%DT_UJac, "DT_UJac", "Time between calls to get Jacobians (s)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      ! UJacSclFact - Scaling factor used in Jacobians (-)
   CALL ReadVar( UnIn, InputFile, p%UJacSclFact, "UJacSclFact", "Scaling factor used in Jacobians (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

   !---------------------- FEATURE SWITCHES AND FLAGS --------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Feature Switches and Flags', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      ! CompElast - Compute structural dynamics (switch) {1=ElastoDyn; 2=ElastoDyn + BeamDyn for blades}:
   CALL ReadVar( UnIn, InputFile, p%CompElast, "CompElast", "Compute structural dynamics (switch) {1=ElastoDyn; 2=ElastoDyn + BeamDyn for blades}", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

          ! immediately convert to values used inside the code:
         IF ( p%CompElast == 1 ) THEN
            p%CompElast = Module_ED
         ELSEIF ( p%CompElast == 2 ) THEN
            p%CompElast = Module_BD
         ELSE
            p%CompElast = Module_Unknown
         END IF

      ! CompInflow - inflow wind velocities (switch) {0=still air; 1=InflowWind}:
   CALL ReadVar( UnIn, InputFile, p%CompInflow, "CompInflow", "inflow wind velocities (switch) {0=still air; 1=InflowWind}", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

          ! immediately convert to values used inside the code:
         IF ( p%CompInflow == 0 ) THEN
            p%CompInflow = Module_NONE
         ELSEIF ( p%CompInflow == 1 ) THEN
            p%CompInflow = Module_IfW
         ELSEIF ( p%CompInflow == 2 ) THEN
            p%CompInflow = Module_OpFM
         ELSE
            p%CompInflow = Module_Unknown
         END IF

      ! CompAero - Compute aerodynamic loads (switch) {0=None; 1=AeroDyn}:
   CALL ReadVar( UnIn, InputFile, p%CompAero, "CompAero", "Compute aerodynamic loads (switch) {0=None; 1=AeroDyn}", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

          ! immediately convert to values used inside the code:
         IF ( p%CompAero == 0 ) THEN
            p%CompAero = Module_NONE
         ELSEIF ( p%CompAero == 1 ) THEN
            p%CompAero = Module_AD14
         ELSEIF ( p%CompAero == 2 ) THEN
            p%CompAero = Module_AD
         ELSE
            p%CompAero = Module_Unknown
         END IF

      ! CompServo - Compute control and electrical-drive dynamics (switch) {0=None; 1=ServoDyn}:
   CALL ReadVar( UnIn, InputFile, p%CompServo, "CompServo", "Compute control and electrical-drive dynamics (switch) {0=None; 1=ServoDyn}", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

          ! immediately convert to values used inside the code:
         IF ( p%CompServo == 0 ) THEN
            p%CompServo = Module_NONE
         ELSEIF ( p%CompServo == 1 ) THEN
            p%CompServo = Module_SrvD
         ELSE
            p%CompServo = Module_Unknown
         END IF


      ! CompHydro - Compute hydrodynamic loads (switch) {0=None; 1=HydroDyn}:
   CALL ReadVar( UnIn, InputFile, p%CompHydro, "CompHydro", "Compute hydrodynamic loads (switch) {0=None; 1=HydroDyn}", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

          ! immediately convert to values used inside the code:
         IF ( p%CompHydro == 0 ) THEN
            p%CompHydro = Module_NONE
         ELSEIF ( p%CompHydro == 1 ) THEN
            p%CompHydro = Module_HD
         ELSE
            p%CompHydro = Module_Unknown
         END IF

      ! CompSub - Compute sub-structural dynamics (switch) {0=None; 1=SubDyn; 2=ExtPtfm_MCKF}:
   CALL ReadVar( UnIn, InputFile, p%CompSub, "CompSub", "Compute sub-structural dynamics (switch) {0=None; 1=SubDyn}", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

          ! immediately convert to values used inside the code:
         IF ( p%CompSub == 0 ) THEN
            p%CompSub = Module_NONE
         ELSEIF ( p%CompSub == 1 ) THEN
            p%CompSub = Module_SD
         ELSEIF ( p%CompSub == 2 ) THEN
            p%CompSub = Module_ExtPtfm
         ELSE
            p%CompSub = Module_Unknown
         END IF

      ! CompMooring - Compute mooring line dynamics (flag):
   CALL ReadVar( UnIn, InputFile, p%CompMooring, "CompMooring", "Compute mooring system (switch) {0=None; 1=MAP; 2=FEAMooring; 3=MoorDyn; 4=OrcaFlex}", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

          ! immediately convert to values used inside the code:
         IF ( p%CompMooring == 0 ) THEN
            p%CompMooring = Module_NONE
         ELSEIF ( p%CompMooring == 1 ) THEN
            p%CompMooring = Module_MAP
         ELSEIF ( p%CompMooring == 2 ) THEN
            p%CompMooring = Module_FEAM
         ELSEIF ( p%CompMooring == 3 ) THEN
            p%CompMooring = Module_MD
         ELSEIF ( p%CompMooring == 4 ) THEN
            p%CompMooring = Module_Orca
         ELSE
            p%CompMooring = Module_Unknown
         END IF

      ! CompIce - Compute ice loads (switch) {0=None; 1=IceFloe}:
   CALL ReadVar( UnIn, InputFile, p%CompIce, "CompIce", "Compute ice loads (switch) {0=None; 1=IceFloe}", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

          ! immediately convert to values used inside the code:
         IF ( p%CompIce == 0 ) THEN
            p%CompIce = Module_NONE
         ELSEIF ( p%CompIce == 1 ) THEN
            p%CompIce = Module_IceF
         ELSEIF ( p%CompIce == 2 ) THEN
            p%CompIce = Module_IceD
         ELSE
            p%CompIce = Module_Unknown
         END IF
               
      ! MHK - MHK turbine type (switch) {0=Not an MHK turbine; 1=Fixed MHK turbine; 2=Floating MHK turbine}:
   CALL ReadVar( UnIn, InputFile, p%MHK, "MHK", "MHK turbine type (switch) {0=Not an MHK turbine; 1=Fixed MHK turbine; 2=Floating MHK turbine}", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN        
      end if

   !---------------------- ENVIRONMENTAL CONDITIONS --------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Environmental Conditions', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN        
      end if   
      
      ! Gravity - Gravitational acceleration (m/s^2):
   CALL ReadVar( UnIn, InputFile, p%Gravity, "Gravity", "Gravitational acceleration (m/s^2)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN        
      end if

         ! AirDens - Air density (kg/m^3):
   CALL ReadVar( UnIn, InputFile, p%AirDens, "AirDens", "Air density (kg/m^3)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN        
      end if

         ! WtrDens - Water density (kg/m^3):
   CALL ReadVar( UnIn, InputFile, p%WtrDens, "WtrDens", "Water density (kg/m^3)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN        
      end if

         ! KinVisc - Kinematic viscosity of working fluid (m^2/s):
   CALL ReadVar( UnIn, InputFile, p%KinVisc, "KinVisc", "Kinematic viscosity of working fluid (m^2/s)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN        
      end if

         ! SpdSound - Speed of sound in working fluid (m/s):
   CALL ReadVar( UnIn, InputFile, p%SpdSound, "SpdSound", "Speed of sound in working fluid (m/s)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN        
      end if

         ! Patm - Atmospheric pressure (Pa):
   CALL ReadVar( UnIn, InputFile, p%Patm, "Patm", "Atmospheric pressure (Pa)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN        
      end if

         ! Pvap - Vapour pressure of working fluid (Pa):
   CALL ReadVar( UnIn, InputFile, p%Pvap, "Pvap", "Vapour pressure of working fluid (Pa)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN        
      end if

         ! WtrDpth - Water depth (m):
   CALL ReadVar( UnIn, InputFile, p%WtrDpth, "WtrDpth", "Water depth (m)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN        
      end if

         ! MSL2SWL - Offset between still-water level and mean sea level (m):
   CALL ReadVar( UnIn, InputFile, p%MSL2SWL, "MSL2SWL", "Offset between still-water level and mean sea level (m)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN        
      end if

   !---------------------- INPUT FILES ---------------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Input Files', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      ! EDFile - Name of file containing ElastoDyn input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%EDFile, "EDFile", "Name of file containing ElastoDyn input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if
   IF ( PathIsRelative( p%EDFile ) ) p%EDFile = TRIM(PriPath)//TRIM(p%EDFile)

DO i=1,MaxNBlades
      ! BDBldFile - Name of file containing BeamDyn blade input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%BDBldFile(i), "BDBldFile("//TRIM(num2LStr(i))//")", "Name of file containing BeamDyn blade "//trim(num2lstr(i))//"input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if
   IF ( PathIsRelative( p%BDBldFile(i) ) ) p%BDBldFile(i) = TRIM(PriPath)//TRIM(p%BDBldFile(i))
END DO

      ! InflowFile - Name of file containing inflow wind input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%InflowFile, "InflowFile", "Name of file containing inflow wind input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if
   IF ( PathIsRelative( p%InflowFile ) ) p%InflowFile = TRIM(PriPath)//TRIM(p%InflowFile)

      ! AeroFile - Name of file containing aerodynamic input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%AeroFile, "AeroFile", "Name of file containing aerodynamic input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if
   IF ( PathIsRelative( p%AeroFile ) ) p%AeroFile = TRIM(PriPath)//TRIM(p%AeroFile)

      ! ServoFile - Name of file containing control and electrical-drive input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%ServoFile, "ServoFile", "Name of file containing control and electrical-drive input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if
   IF ( PathIsRelative( p%ServoFile ) ) p%ServoFile = TRIM(PriPath)//TRIM(p%ServoFile)

      ! HydroFile - Name of file containing hydrodynamic input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%HydroFile, "HydroFile", "Name of file containing hydrodynamic input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if
   IF ( PathIsRelative( p%HydroFile ) ) p%HydroFile = TRIM(PriPath)//TRIM(p%HydroFile)

      ! SubFile - Name of file containing sub-structural input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%SubFile, "SubFile", "Name of file containing sub-structural input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if
   IF ( PathIsRelative( p%SubFile ) ) p%SubFile = TRIM(PriPath)//TRIM(p%SubFile)

      ! MooringFile - Name of file containing mooring system input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%MooringFile, "MooringFile", "Name of file containing mooring system input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if
   IF ( PathIsRelative( p%MooringFile ) ) p%MooringFile = TRIM(PriPath)//TRIM(p%MooringFile)

      ! IceFile - Name of file containing ice input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%IceFile, "IceFile", "Name of file containing ice input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if
   IF ( PathIsRelative( p%IceFile ) ) p%IceFile = TRIM(PriPath)//TRIM(p%IceFile)


   !---------------------- OUTPUT --------------------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Output', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      ! SumPrint - Print summary data to <RootName>.sum (flag):
   CALL ReadVar( UnIn, InputFile, p%SumPrint, "SumPrint", "Print summary data to <RootName>.sum (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      ! SttsTime - Amount of time between screen status messages (s):
   CALL ReadVar( UnIn, InputFile, TmpTime, "SttsTime", "Amount of time between screen status messages (s)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      IF (TmpTime > p%TMax) THEN
         p%n_SttsTime = HUGE(p%n_SttsTime)
      ELSE
         p%n_SttsTime = NINT( TmpTime / p%DT )
      END IF

      ! ChkptTime - Amount of time between creating checkpoint files for potential restart (s):
   CALL ReadVar( UnIn, InputFile, TmpTime, "ChkptTime", "Amount of time between creating checkpoint files for potential restart (s)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      IF (TmpTime > p%TMax) THEN
         p%n_ChkptTime = HUGE(p%n_ChkptTime)
      ELSE
         p%n_ChkptTime = NINT( TmpTime / p%DT )
      END IF

      ! DT_Out - Time step for tabular output (s):
   CALL ReadVar( UnIn, InputFile, Line, "DT_Out", "Time step for tabular output (s)", ErrStat2, ErrMsg2, UnEc)
   !CALL ReadVar( UnIn, InputFile, p%DT_Out, "DT_Out", "Time step for tabular output (s)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      CALL Conv2UC( Line )
      IF ( INDEX(Line, "DEFAULT" ) == 1 ) THEN
         p%DT_Out = p%DT
      ELSE
         ! If it's not "default", read this variable; otherwise use the value in p%DT
         READ( Line, *, IOSTAT=IOS) p%DT_Out
            CALL CheckIOS ( IOS, InputFile, 'DT_Out', NumType, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
            if ( ErrStat >= AbortErrLev ) then
               call cleanup()
               RETURN
            end if
      END IF

      p%n_DT_Out = NINT( p%DT_Out / p%DT )

      ! TStart - Time to begin tabular output (s):
   CALL ReadVar( UnIn, InputFile, p%TStart, "TStart", "Time to begin tabular output (s)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if


      !> OutFileFmt - Format for tabular (time-marching) output file (switch) {1: text file [<RootName>.out], 2: binary file [<RootName>.outb], 4: HDF5 [<RootName>.h5], add for combinations}
      !!
      !!  Combinations of output files are possible by adding the values corresponding to each file.  The possible combination of options are therefore
      !!
      !! | `OutFileFmt` | Description                                                          |
      !! |:------------:|:---------------------------------------------------------------------|
      !! | 1            | Text file only `<RootName>.out`                                      |
      !! | 2            | Binary file only `<RootName>.outb`                                   |
      !! | 3            | Text and binary files                                                |
      !! | 4            | uncompressed binary file `<RootName>.outbu`                          |
      !! | 5            | Text and uncompressed binary files                                   |
      !! | 6  => 4      | Binary (not written) and uncompressed binary files; same as 4        |
      !! | 7  => 5      | Text, Binary (not written), and uncompressed binary files; same as 5 |
      !!

      ! OutFileFmt - Format for tabular (time-marching) output file(s) (1: text file [<RootName>.out], 2: binary file [<RootName>.outb], 3: both) (-):
   CALL ReadVar( UnIn, InputFile, OutFileFmt, "OutFileFmt", "Format for tabular (time-marching) output file(s) {0: uncompressed binary and text file, 1: text file [<RootName>.out], 2: compressed binary file [<RootName>.outb], 3: both text and compressed binary, 4: uncompressed binary <RootName>.outb]; add for combinations) (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

     if (OutFileFmt == 0) OutFileFmt = 5

         ! convert integer to binary representation of which file formats to generate:
      p%WrTxtOutFile = mod(OutFileFmt,2) == 1

      OutFileFmt = OutFileFmt / 2 ! integer division
      p%WrBinOutFile = mod(OutFileFmt,2) == 1

      OutFileFmt = OutFileFmt / 2 ! integer division
      if (mod(OutFileFmt,2) == 1) then
         ! This is a feature for the regression testing system.  It writes binary output stored as uncompressed double floating point data instead of compressed int16 data.
         ! If the compressed binary version was requested, that will not be generated
         if (p%WrBinOutFile) then
            call SetErrStat(ErrID_Warn,'Binary compressed file will not be generated because the uncompressed version was also requested.', ErrStat, ErrMsg, RoutineName)
         else
            p%WrBinOutFile = .true.
         end if
         p%WrBinMod = FileFmtID_NoCompressWithoutTime    ! A format specifier for the binary output file format (3=don't include time channel and do not pack data)
      else
         p%WrBinMod = FileFmtID_ChanLen_In               ! A format specifier for the binary output file format (4=don't include time channel; do include channel width; do pack data)
      end if

      OutFileFmt = OutFileFmt / 2 ! integer division

      if (OutFileFmt /= 0) then
         call SetErrStat( ErrID_Fatal, "OutFileFmt must be 0, 1, 2, or 3.",ErrStat,ErrMsg,RoutineName)
         call cleanup()
         return
      end if

      ! TabDelim - Use tab delimiters in text tabular output file? (flag):
   CALL ReadVar( UnIn, InputFile, TabDelim, "TabDelim", "Use tab delimiters in text tabular output file? (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      IF ( TabDelim ) THEN
         p%Delim = TAB
      ELSE
         p%Delim = ' '
      END IF

      ! OutFmt - Format used for text tabular output (except time).  Resulting field should be 10 characters. (-):
   CALL ReadVar( UnIn, InputFile, p%OutFmt, "OutFmt", "Format used for text tabular output (except time).  Resulting field should be 10 characters. (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if


   !---------------------- LINEARIZATION -----------------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Linearization', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if


      ! Linearize - Linearization analysis (flag)
   CALL ReadVar( UnIn, InputFile, p%Linearize, "Linearize", "Linearization analysis (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if


      ! CalcSteady - Calculate a steady-state periodic operating point before linearization? [unused if Linearize=False] (flag)
   CALL ReadVar( UnIn, InputFile, p%CalcSteady, "CalcSteady", "Calculate a steady-state periodic operating point before linearization? (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

      ! TrimCase - Controller parameter to be trimmed {1:yaw; 2:torque; 3:pitch} [used only if CalcSteady=True] (-)
   CALL ReadVar( UnIn, InputFile, p%TrimCase, "TrimCase", "Controller parameter to be trimmed {1:yaw; 2:torque; 3:pitch} (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

      ! TrimTol - Tolerance for the rotational speed convergence [used only if CalcSteady=True] (-)
   CALL ReadVar( UnIn, InputFile, p%TrimTol, "TrimTol", "Tolerance for the rotational speed convergence (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

      ! TrimGain - Proportional gain for the rotational speed error (>0) [used only if CalcSteady=True] (rad/(rad/s) for yaw or pitch; Nm/(rad/s) for torque)
   CALL ReadVar( UnIn, InputFile, p%TrimGain, "TrimGain", "Proportional gain for the rotational speed error (>0) (rad/(rad/s) for yaw or pitch; Nm/(rad/s) for torque)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

      ! Twr_Kdmp - Damping factor for the tower [used only if CalcSteady=True] (N/(m/s))
   CALL ReadVar( UnIn, InputFile, p%Twr_Kdmp, "Twr_Kdmp", "Damping factor for the tower (N/(m/s))", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

      ! Bld_Kdmp - Damping factor for the blades [used only if CalcSteady=True] (N/(m/s))
   CALL ReadVar( UnIn, InputFile, p%Bld_Kdmp, "Bld_Kdmp", "Damping factor for the blades (N/(m/s))", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      ! NLinTimes - Number of times to linearize (or number of equally spaced azimuth steps in periodic linearized model) (-) [>=1]
   CALL ReadVar( UnIn, InputFile, p%NLinTimes, "NLinTimes", "Number of times to linearize (-) [>=1]", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

   if (.not. p%Linearize) then
      p%CalcSteady = .false.
      p%NLinTimes = 0
   end if

         ! LinTimes - Times to linearize (s) [1 to NLinTimes]
   if (.not. p%CalcSteady .and. p%NLinTimes >= 1 ) then
      call AllocAry( m_FAST%Lin%LinTimes, p%NLinTimes, 'LinTimes', ErrStat2, ErrMsg2 )
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
         if ( ErrStat >= AbortErrLev ) then
            call cleanup()
            RETURN
         end if

      CALL ReadAry( UnIn, InputFile, m_FAST%Lin%LinTimes, p%NLinTimes, "LinTimes", "Times to linearize (s) [1 to NLinTimes]", ErrStat2, ErrMsg2, UnEc)
   else
      CALL ReadCom( UnIn, InputFile, 'Times to linearize (s) [1 to NLinTimes] ', ErrStat2, ErrMsg2, UnEc )
   end if
   CALL SetErrStat( ErrStat2, ErrMsg2,ErrStat,ErrMsg,RoutineName)
   if ( ErrStat >= AbortErrLev ) then
      call cleanup()
      RETURN
   end if

      ! LinInputs - Include inputs in linearization (switch) {0=none; 1=standard; 2=all module inputs (debug)}
   CALL ReadVar( UnIn, InputFile, p%LinInputs, "LinInputs", "Include inputs in linearization (switch) {0=none; 1=standard; 2=all module inputs (debug)}", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      ! LinOutputs - Include outputs in linearization (switch) (0=none; 1=from OutList(s); 2=all module outputs (debug))
   CALL ReadVar( UnIn, InputFile, p%LinOutputs, "LinOutputs", "Include outputs in linearization (switch) (0=none; 1=from OutList(s); 2=all module outputs (debug))", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      ! LinOutJac - Include full Jacabians in linearization output (for debug) (flag)
   CALL ReadVar( UnIn, InputFile, p%LinOutJac, "LinOutJac", "Include full Jacabians in linearization output (for debug) (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      ! LinOutMod - Write module-level linearization output files in addition to output for full system? (flag)
   CALL ReadVar( UnIn, InputFile, p%LinOutMod, "LinOutMod", "Write module-level linearization output files in addition to output for full system? (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

   !---------------------- VISUALIZATION -----------------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Visualization', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      ! WrVTK - VTK Visualization data output: (switch) {0=none; 1=initialization data only; 2=animation; 3=mode shapes}:
   CALL ReadVar( UnIn, InputFile, p%WrVTK, "WrVTK", "Write VTK visualization files (0=none; 1=initialization data only; 2=animation; 3=mode shapes)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      IF ( p%WrVTK < 0 .OR. p%WrVTK > 3 ) THEN
         p%WrVTK = VTK_Unknown
      END IF

      ! VTK_Type - Type of  VTK visualization data: (switch) {1=surfaces; 2=basic meshes (lines/points); 3=all meshes (debug)}:
   CALL ReadVar( UnIn, InputFile, p%VTK_Type, "VTK_Type", "Type of  VTK visualization data: (1=surfaces; 2=basic meshes (lines/points); 3=all meshes)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

          ! immediately convert to values used inside the code:
         IF ( p%VTK_Type == 0 ) THEN
            p%VTK_Type = VTK_None
         ELSEIF ( p%VTK_Type == 1 ) THEN
            p%VTK_Type = VTK_Surf
         ELSEIF ( p%VTK_Type == 2 ) THEN
            p%VTK_Type = VTK_Basic
         ELSEIF ( p%VTK_Type == 3 ) THEN
            p%VTK_Type = VTK_All
         ELSEIF ( p%VTK_Type == 4 ) THEN
            p%VTK_Type = VTK_Old
         ELSE
            p%VTK_Type = VTK_Unknown
         END IF

         !! equivalent:
         !IF ( p%VTK_Type < 0 .OR. p%VTK_Type > 4 ) THEN
         !   p%VTK_Type = VTK_Unknown
         !END IF

      ! VTK_fields - Write mesh fields to VTK data files? (flag) {true/false}:
   CALL ReadVar( UnIn, InputFile, p%VTK_fields, "VTK_fields", "Write mesh fields to VTK data files? (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if

      ! VTK_fps - Frame rate for VTK output (frames per second) {will use closest integer multiple of DT}
   CALL ReadVar( UnIn, InputFile, p%VTK_fps, "VTK_fps", "Frame rate for VTK output(fps)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if ( ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if


      ! convert frames-per-second to seconds per sample:
      if ( EqualRealNos(p%VTK_fps, 0.0_DbKi) ) then
         TmpTime = p%TMax + p%DT
      else
         TmpTime = 1.0_DbKi / p%VTK_fps
      end if

      ! now save the number of time steps between VTK file output:
      IF (p%WrVTK == VTK_ModeShapes) THEN
         p%n_VTKTime = 1
      ELSE IF (TmpTime > p%TMax) THEN
         p%n_VTKTime = HUGE(p%n_VTKTime)
      ELSE
         p%n_VTKTime = NINT( TmpTime / p%DT )
         ! I'll warn if p%n_VTKTime*p%DT is not TmpTime
         IF (p%WrVTK == VTK_Animate) THEN
            TmpRate = p%n_VTKTime*p%DT
            if (.not. EqualRealNos(TmpRate, TmpTime)) then
               call SetErrStat(ErrID_Info, '1/VTK_fps is not an integer multiple of DT. FAST will output VTK information at '//&
                              trim(num2lstr(1.0_DbKi/TmpRate))//' fps, the closest rate possible.',ErrStat,ErrMsg,RoutineName)
            end if
         END IF

      END IF

   call cleanup()
   RETURN

CONTAINS
   !...............................................................................................................................
   subroutine cleanup()
      CLOSE( UnIn )
      IF ( UnEc > 0 ) CLOSE ( UnEc )
   end subroutine cleanup
   !...............................................................................................................................
END SUBROUTINE FAST_ReadPrimaryFile
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine gets the name of the FAST input file from the command line. It also returns a logical indicating if this there
!! was a "DWM" argument after the file name.
SUBROUTINE GetInputFileName(InputFile,UseDWM,ErrStat,ErrMsg)
   CHARACTER(*),             INTENT(OUT)           :: InputFile         !< A CHARACTER string containing the name of the primary FAST input file (if not present, we'll get it from the command line)
   LOGICAL,                  INTENT(OUT)           :: UseDWM            !< whether the last argument from the command line is "DWM"
   INTEGER(IntKi),           INTENT(OUT)           :: ErrStat           !< Error status
   CHARACTER(*),             INTENT(OUT)           :: ErrMsg            !< Error message

   INTEGER(IntKi)                                  :: ErrStat2          ! local error stat
   CHARACTER(1024)                                 :: LastArg           ! A second command-line argument that will allow DWM module to be used in AeroDyn

   ErrStat = ErrID_None
   ErrMsg = ''

   UseDWM = .FALSE.  ! by default, we're not going to use the DWM module
   InputFile = ""  ! initialize to empty string to make sure it's input from the command line
   CALL CheckArgs( InputFile, ErrStat2, LastArg )  ! if ErrStat2 /= ErrID_None, we'll ignore and deal with the problem when we try to read the input file

   IF (LEN_TRIM(InputFile) == 0) THEN ! no input file was specified
      ErrStat = ErrID_Fatal
      ErrMsg  = 'The required input file was not specified on the command line.'
      RETURN
   END IF

   IF (LEN_TRIM(LastArg) > 0) THEN ! see if DWM was specified as the second option
      CALL Conv2UC( LastArg )
      IF ( TRIM(LastArg) == "DWM" ) THEN
         UseDWM    = .TRUE.
      END IF
   END IF

END SUBROUTINE GetInputFileName
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine validates FAST data.
SUBROUTINE ValidateInputData(p, m_FAST, ErrStat, ErrMsg)

   TYPE(FAST_ParameterType), INTENT(INOUT)         :: p                 !< The parameter data for the FAST (glue-code) simulation
   TYPE(FAST_MiscVarType),   INTENT(IN   )         :: m_FAST            !< The misc data for the FAST (glue-code) simulation
   INTEGER(IntKi),           INTENT(  OUT)         :: ErrStat           !< Error status
   CHARACTER(*),             INTENT(  OUT)         :: ErrMsg            !< Error message

   REAL(DbKi)                                      :: TmpTime           ! A temporary variable for error checking

   INTEGER(IntKi)                                  :: i
   INTEGER(IntKi)                                  :: ErrStat2
   CHARACTER(ErrMsgLen)                            :: ErrMsg2
   CHARACTER(*), PARAMETER                         :: RoutineName='ValidateInputData'

   ErrStat = ErrID_None
   ErrMsg  = ""


   IF ( p%TMax < 0.0_DbKi  )  THEN
      CALL SetErrStat( ErrID_Fatal, 'TMax must not be a negative number.', ErrStat, ErrMsg, RoutineName )
   ELSE IF ( p%TMax < p%TStart )  THEN
      CALL SetErrStat( ErrID_Fatal, 'TMax must not be less than TStart.', ErrStat, ErrMsg, RoutineName )
   END IF

   IF ( p%n_ChkptTime < p%n_TMax_m1 ) THEN
      if (.NOT. p%WrBinOutFile) CALL SetErrStat( ErrID_Severe, 'It is highly recommended that time-marching output files be generated in binary format when generating checkpoint files.', ErrStat, ErrMsg, RoutineName )
      if (p%CompMooring==MODULE_Orca) CALL SetErrStat( ErrID_Fatal, 'Restart capability for OrcaFlexInterface is not supported. Set ChkptTime larger than TMax.', ErrStat, ErrMsg, RoutineName )
      ! also check for other features that aren't supported with restart (like ServoDyn's user-defined control routines)
   END IF

   IF ( p%DT <= 0.0_DbKi )  THEN
      CALL SetErrStat( ErrID_Fatal, 'DT must be greater than 0.', ErrStat, ErrMsg, RoutineName )
   ELSE ! Test DT and TMax to ensure numerical stability -- HINT: see the use of OnePlusEps
      TmpTime = p%TMax*EPSILON(p%DT)
      IF ( p%DT <= TmpTime ) THEN
         CALL SetErrStat( ErrID_Fatal, 'DT must be greater than '//TRIM ( Num2LStr( TmpTime ) )//' seconds.', ErrStat, ErrMsg, RoutineName )
      END IF
   END IF

      ! Check that InputFileData%OutFmt is a valid format specifier and will fit over the column headings
   CALL ChkRealFmtStr( p%OutFmt, 'OutFmt', p%FmtWidth, ErrStat2, ErrMsg2 )
      call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   IF ( p%WrTxtOutFile .and. p%FmtWidth < MinChanLen ) CALL SetErrStat( ErrID_Warn, 'OutFmt produces a column width of '// &
         TRIM(Num2LStr(p%FmtWidth))//'), which may be too small.', ErrStat, ErrMsg, RoutineName )

   IF ( p%WrTxtOutFile .AND. p%TChanLen > ChanLen  )  THEN ! ( p%TMax > 9999.999_DbKi )
      CALL SetErrStat( ErrID_Warn, 'TMax is too large for a '//trim(num2lstr(ChanLen))//'-character time column in text tabular (time-marching) output files.'// &
                                   ' Postprocessors with this limitation may not work.', ErrStat, ErrMsg, RoutineName )
   END IF

   IF ( p%TStart      <  0.0_DbKi ) CALL SetErrStat( ErrID_Fatal, 'TStart must not be less than 0 seconds.', ErrStat, ErrMsg, RoutineName )
!  IF ( p%SttsTime    <= 0.0_DbKi ) CALL SetErrStat( ErrID_Fatal, 'SttsTime must be greater than 0 seconds.', ErrStat, ErrMsg, RoutineName )
   IF ( p%n_SttsTime  < 1_IntKi   ) CALL SetErrStat( ErrID_Fatal, 'SttsTime must be greater than 0 seconds.', ErrStat, ErrMsg, RoutineName )
   IF ( p%n_ChkptTime < 1_IntKi   ) CALL SetErrStat( ErrID_Fatal, 'ChkptTime must be greater than 0 seconds.', ErrStat, ErrMsg, RoutineName )
   IF ( p%KMax        < 1_IntKi   ) CALL SetErrStat( ErrID_Fatal, 'KMax must be greater than 0.', ErrStat, ErrMsg, RoutineName )

   IF (p%CompElast   == Module_Unknown) CALL SetErrStat( ErrID_Fatal, 'CompElast must be 1 (ElastoDyn) or 2 (BeamDyn).', ErrStat, ErrMsg, RoutineName )
   IF (p%CompAero    == Module_Unknown) CALL SetErrStat( ErrID_Fatal, 'CompAero must be 0 (None), 1 (AeroDyn14), or 2 (AeroDyn).', ErrStat, ErrMsg, RoutineName )
   IF (p%CompServo   == Module_Unknown) CALL SetErrStat( ErrID_Fatal, 'CompServo must be 0 (None) or 1 (ServoDyn).', ErrStat, ErrMsg, RoutineName )
   IF (p%CompHydro   == Module_Unknown) CALL SetErrStat( ErrID_Fatal, 'CompHydro must be 0 (None) or 1 (HydroDyn).', ErrStat, ErrMsg, RoutineName )
   IF (p%CompSub     == Module_Unknown) CALL SetErrStat( ErrID_Fatal, 'CompSub must be 0 (None), 1 (SubDyn), or 2 (ExtPtfm_MCKF).', ErrStat, ErrMsg, RoutineName )
   IF (p%CompMooring == Module_Unknown) CALL SetErrStat( ErrID_Fatal, 'CompMooring must be 0 (None), 1 (MAP), 2 (FEAMooring), 3 (MoorDyn), or 4 (OrcaFlex).', ErrStat, ErrMsg, RoutineName )
   IF (p%CompIce     == Module_Unknown) CALL SetErrStat( ErrID_Fatal, 'CompIce must be 0 (None) or 1 (IceFloe).', ErrStat, ErrMsg, RoutineName )
   IF (p%CompHydro /= Module_HD) THEN
      IF (p%CompMooring == Module_MAP) THEN
         CALL SetErrStat( ErrID_Fatal, 'HydroDyn must be used when MAP is used. Set CompHydro > 0 or CompMooring = 0 in the FAST input file.', ErrStat, ErrMsg, RoutineName )
      ELSEIF (p%CompMooring == Module_FEAM) THEN
         CALL SetErrStat( ErrID_Fatal, 'HydroDyn must be used when FEAMooring is used. Set CompHydro > 0 or CompMooring = 0 in the FAST input file.', ErrStat, ErrMsg, RoutineName )
      ELSEIF (p%CompMooring == Module_MD) THEN
         CALL SetErrStat( ErrID_Fatal, 'HydroDyn must be used when MoorDyn is used. Set CompHydro > 0 or CompMooring = 0 in the FAST input file.', ErrStat, ErrMsg, RoutineName )
      END IF
   ELSE
      IF (p%CompMooring == Module_Orca) CALL SetErrStat( ErrID_Fatal, 'HydroDyn cannot be used if OrcaFlex is used. Set CompHydro = 0 or CompMooring < 4 in the FAST input file.', ErrStat, ErrMsg, RoutineName )
      IF (p%CompSub == Module_ExtPtfm) CALL SetErrStat( ErrID_Fatal, 'HydroDyn cannot be used if ExtPtfm_MCKF is used. Set CompHydro = 0 or CompSub < 2 in the FAST input file.', ErrStat, ErrMsg, RoutineName )
   END IF


   IF (p%CompIce == Module_IceF) THEN
      IF (p%CompSub   /= Module_SD) CALL SetErrStat( ErrID_Fatal, 'SubDyn must be used when IceFloe is used. Set CompSub > 0 or CompIce = 0 in the FAST input file.', ErrStat, ErrMsg, RoutineName )
      IF (p%CompHydro /= Module_HD) CALL SetErrStat( ErrID_Fatal, 'HydroDyn must be used when IceFloe is used. Set CompHydro > 0 or CompIce = 0 in the FAST input file.', ErrStat, ErrMsg, RoutineName )
   ELSEIF (p%CompIce == Module_IceD) THEN
      IF (p%CompSub   /= Module_SD) CALL SetErrStat( ErrID_Fatal, 'SubDyn must be used when IceDyn is used. Set CompSub > 0 or CompIce = 0 in the FAST input file.', ErrStat, ErrMsg, RoutineName )
      IF (p%CompHydro /= Module_HD) CALL SetErrStat( ErrID_Fatal, 'HydroDyn must be used when IceDyn is used. Set CompHydro > 0 or CompIce = 0 in the FAST input file.', ErrStat, ErrMsg, RoutineName )
   END IF

   IF (p%CompElast == Module_BD .and. p%CompAero == Module_AD14 ) CALL SetErrStat( ErrID_Fatal, 'AeroDyn14 cannot be used when BeamDyn is used. Change CompAero or CompElast in the FAST input file.', ErrStat, ErrMsg, RoutineName )
   
   IF (p%MHK /= 0) CALL SetErrStat( ErrID_Fatal, 'MHK switch must be 0 in the FAST input file. Functionality to model an MHK turbine has not yet been implemented.', ErrStat, ErrMsg, RoutineName ) ! hkr (4/6/21) Remove after MHK functionality is implemented

   IF (p%MHK /= 0 .and. p%MHK /= 1 .and. p%MHK /= 2) CALL SetErrStat( ErrID_Fatal, 'MHK switch is invalid. Set MHK to 0, 1, or 2 in the FAST input file.', ErrStat, ErrMsg, RoutineName )

   IF (p%MHK == 2) CALL SetErrStat( ErrID_Fatal, 'Functionality to model a floating MHK turbine has not yet been implemented.', ErrStat, ErrMsg, RoutineName )

   IF (p%MHK == 1 .and. p%CompAero == Module_AD14 .or. p%MHK == 2 .and. p%CompAero == Module_AD14) CALL SetErrStat( ErrID_Fatal, 'AeroDyn14 cannot be used with an MHK turbine. Change CompAero or MHK in the FAST input file.', ErrStat, ErrMsg, RoutineName )

   IF (p%Gravity < 0.0_ReKi) CALL SetErrStat( ErrID_Fatal, 'Gravity must not be negative.', ErrStat, ErrMsg, RoutineName )

   IF (p%WtrDpth < 0.0_ReKi) CALL SetErrStat( ErrID_Fatal, 'WtrDpth must not be negative.', ErrStat, ErrMsg, RoutineName )

!   IF ( p%InterpOrder < 0 .OR. p%InterpOrder > 2 ) THEN
   IF ( p%InterpOrder < 1 .OR. p%InterpOrder > 2 ) THEN
      CALL SetErrStat( ErrID_Fatal, 'InterpOrder must be 1 or 2.', ErrStat, ErrMsg, RoutineName ) ! 5/13/14 bjj: MAS and JMJ compromise for certain integrators is that InterpOrder cannot be 0
      p%InterpOrder = 1    ! Avoid problems in error handling by setting this to 0
   END IF

   IF ( p%NumCrctn < 0_IntKi ) THEN
      CALL SetErrStat( ErrID_Fatal, 'NumCrctn must be 0 or greater.', ErrStat, ErrMsg, RoutineName )
   END IF


   if ( p%WrVTK == VTK_Unknown ) then
      call SetErrStat(ErrID_Fatal, 'WrVTK must be 0 (none), 1 (initialization only), 2 (animation), or 3 (mode shapes).', ErrStat, ErrMsg, RoutineName)
   else
      if ( p%VTK_type == VTK_Unknown ) then
         call SetErrStat(ErrID_Fatal, 'VTK_type must be 1 (surfaces), 2 (basic meshes:lines/points), or 3 (all meshes).', ErrStat, ErrMsg, RoutineName)
         ! note I'm not going to write that 4 (old) is an option
      end if

      if (p%WrVTK == VTK_ModeShapes .and. .not. p%Linearize) then
         call SetErrStat(ErrID_Fatal, 'WrVTK cannot be 3 (mode shapes) when Linearize is false. (Mode shapes require linearization analysis.)', ErrStat, ErrMsg, RoutineName)
      end if
   end if

   if (p%Linearize) then

      if (p%CalcSteady) then
         if (p%NLinTimes < 1) call SetErrStat(ErrID_Fatal,'NLinTimes must be at least 1 for linearization analysis.',ErrStat, ErrMsg, RoutineName)
         if (p%TrimCase /= TrimCase_yaw .and. p%TrimCase /= TrimCase_torque .and. p%TrimCase /= TrimCase_pitch) then
            call SetErrStat(ErrID_Fatal,'TrimCase must be either 1, 2, or 3.',ErrStat, ErrMsg, RoutineName)
         end if

         if (p%TrimTol <= epsilon(p%TrimTol)) call SetErrStat(ErrID_Fatal,'TrimTol must be larger than '//trim(num2lstr(epsilon(p%TrimTol)))//'.',ErrStat, ErrMsg, RoutineName)
         if (p%Twr_Kdmp < 0.0_ReKi) call SetErrStat(ErrID_Fatal,'Twr_Kdmp must not be negative.',ErrStat, ErrMsg, RoutineName)
         if (p%Bld_Kdmp < 0.0_ReKi) call SetErrStat(ErrID_Fatal,'Bld_Kdmp must not be negative.',ErrStat, ErrMsg, RoutineName)
      else

         if (.not. allocated(m_FAST%Lin%LinTimes)) then
            call SetErrStat(ErrID_Fatal, 'NLinTimes must be at least 1 for linearization analysis.',ErrStat, ErrMsg, RoutineName)
         else
            do i=1,p%NLinTimes
               if (m_FAST%Lin%LinTimes(i) < 0) call SetErrStat(ErrID_Fatal,'LinTimes must be positive values.',ErrStat, ErrMsg, RoutineName)
            end do
            do i=2,p%NLinTimes
               if (m_FAST%Lin%LinTimes(i) <= m_FAST%Lin%LinTimes(i-1)) call SetErrStat(ErrID_Fatal,'LinTimes must be unique values entered in increasing order.',ErrStat, ErrMsg, RoutineName)
            end do

            if (m_FAST%Lin%LinTimes(p%NLinTimes) > p%TMax) call SetErrStat(ErrID_Info, 'Tmax is less than the last linearization time. Linearization analysis will not be performed after TMax.',ErrStat, ErrMsg, RoutineName)
         end if

      end if

      if (p%LinInputs < LIN_NONE .or. p%LinInputs > LIN_ALL) call SetErrStat(ErrID_Fatal,'LinInputs must be 0, 1, or 2.',ErrStat, ErrMsg, RoutineName)
      if (p%LinOutputs < LIN_NONE .or. p%LinOutputs > LIN_ALL) call SetErrStat(ErrID_Fatal,'LinOutputs must be 0, 1, or 2.',ErrStat, ErrMsg, RoutineName)

      if (p%LinOutJac) then
         if ( p%LinInputs /= LIN_ALL .or. p%LinOutputs /= LIN_ALL) then
            call SetErrStat(ErrID_Info,'LinOutJac can be used only when LinInputs=LinOutputs=2.',ErrStat, ErrMsg, RoutineName)
            p%LinOutJac = .false.
         end if
      end if

      ! now, make sure we haven't asked for any modules that we can't yet linearize:
      if (p%CompInflow == MODULE_OpFM) call SetErrStat(ErrID_Fatal,'Linearization is not implemented for the OpenFOAM coupling.',ErrStat, ErrMsg, RoutineName)
      if (p%CompAero == MODULE_AD14) call SetErrStat(ErrID_Fatal,'Linearization is not implemented for the AeroDyn v14 module.',ErrStat, ErrMsg, RoutineName)
      !if (p%CompSub   == MODULE_SD) call SetErrStat(ErrID_Fatal,'Linearization is not implemented for the SubDyn module.',ErrStat, ErrMsg, RoutineName)
      if (p%CompSub /= MODULE_None .and. p%CompSub /= MODULE_SD )     call SetErrStat(ErrID_Fatal,'Linearization is not implemented for the ExtPtfm_MCKF substructure module.',ErrStat, ErrMsg, RoutineName)
      if (p%CompMooring /= MODULE_None .and. p%CompMooring /= MODULE_MAP) call SetErrStat(ErrID_Fatal,'Linearization is not implemented for the FEAMooring or MoorDyn mooring modules.',ErrStat, ErrMsg, RoutineName)
      if (p%CompIce /= MODULE_None) call SetErrStat(ErrID_Fatal,'Linearization is not implemented for any of the ice loading modules.',ErrStat, ErrMsg, RoutineName)

   end if
      
   
   if ( (p%TurbineType == Type_Offshore_Fixed .or. p%TurbineType == Type_Offshore_Floating) .and. .not. EqualRealNos(p%TurbinePos(3), 0.0_SiKi) ) then
    call SetErrStat(ErrID_Fatal, 'Height of turbine location, TurbinePos(3), must be 0 for offshore turbines.', ErrStat, ErrMsg, RoutineName)
   end if

   !...............................................................................................................................

      ! temporary check on p_FAST%DT_out

   IF ( .NOT. EqualRealNos( p%DT_out, p%DT ) ) THEN
      IF ( p%DT_out < p%DT ) THEN
         CALL SetErrStat( ErrID_Fatal, 'DT_out must be at least DT ('//TRIM(Num2LStr(p%DT))//' s).', ErrStat, ErrMsg, RoutineName )
      ELSEIF ( .NOT. EqualRealNos( p%DT_out, p%DT * p%n_DT_Out )  ) THEN
         CALL SetErrStat( ErrID_Fatal, 'DT_out must be an integer multiple of DT.', ErrStat, ErrMsg, RoutineName )
      END IF
   END IF



END SUBROUTINE ValidateInputData
!----------------------------------------------------------------------------------------------------------------------------------
!> Routine that calls FillOutputAry for one instance of a Turbine data structure. This is a separate subroutine so that the FAST
!! driver programs do not need to change or operate on the individual module level. (Called from Simulink interface.)
SUBROUTINE FillOutputAry_T(Turbine, Outputs)

   TYPE(FAST_TurbineType),   INTENT(IN   ) :: Turbine                          !< all data for one instance of a turbine
   REAL(ReKi),               INTENT(  OUT) :: Outputs(:)                       !< single array of output


      CALL FillOutputAry(Turbine%p_FAST, Turbine%y_FAST, Turbine%IfW%y%WriteOutput, Turbine%OpFM%y%WriteOutput, &
                Turbine%ED%y%WriteOutput, Turbine%AD%y, Turbine%SrvD%y%WriteOutput, &
                Turbine%HD%y%WriteOutput, Turbine%SD%y%WriteOutput, Turbine%ExtPtfm%y%WriteOutput, Turbine%MAP%y%WriteOutput, &
                Turbine%FEAM%y%WriteOutput, Turbine%MD%y%WriteOutput, Turbine%Orca%y%WriteOutput, &
                Turbine%IceF%y%WriteOutput, Turbine%IceD%y, Turbine%BD%y, Outputs)

END SUBROUTINE FillOutputAry_T
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine concatenates all of the WriteOutput values from the module Output into one array to be written to the FAST
!! output file.
SUBROUTINE FillOutputAry(p_FAST, y_FAST, IfWOutput, OpFMOutput, EDOutput, y_AD, SrvDOutput, HDOutput, SDOutput, ExtPtfmOutput, &
                        MAPOutput, FEAMOutput, MDOutput, OrcaOutput, IceFOutput, y_IceD, y_BD, OutputAry)

   TYPE(FAST_ParameterType), INTENT(IN)    :: p_FAST                             !< Glue-code simulation parameters
   TYPE(FAST_OutputFileType),INTENT(IN)    :: y_FAST                             !< Glue-code simulation outputs

   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: IfWOutput (:)                      !< InflowWind WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: OpFMOutput (:)                     !< OpenFOAM WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: EDOutput (:)                       !< ElastoDyn WriteOutput values
   TYPE(AD_OutputType),      INTENT(IN)    :: y_AD                               !< AeroDyn outputs (WriteOutput values are subset of allocated Rotors)
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: SrvDOutput (:)                     !< ServoDyn WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: HDOutput (:)                       !< HydroDyn WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: SDOutput (:)                       !< SubDyn WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: ExtPtfmOutput (:)                  !< ExtPtfm_MCKF WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: MAPOutput (:)                      !< MAP WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: FEAMOutput (:)                     !< FEAMooring WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: MDOutput (:)                       !< MoorDyn WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: OrcaOutput (:)                     !< OrcaFlex interface WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: IceFOutput (:)                     !< IceFloe WriteOutput values
   TYPE(IceD_OutputType),    INTENT(IN)    :: y_IceD (:)                         !< IceDyn outputs (WriteOutput values are subset)
   TYPE(BD_OutputType),      INTENT(IN)    :: y_BD (:)                           !< BeamDyn outputs (WriteOutput values are subset)

   REAL(ReKi),               INTENT(OUT)   :: OutputAry(:)                       !< single array of output

   INTEGER(IntKi)                          :: i                                  ! loop counter
   INTEGER(IntKi)                          :: indxLast                           ! The index of the last row value to be written to AllOutData for this time step (column).
   INTEGER(IntKi)                          :: indxNext                           ! The index of the next row value to be written to AllOutData for this time step (column).


            ! store individual module data into one array for output

      indxLast = 0
      indxNext = 1
      
      IF (y_FAST%numOuts(Module_Glue) > 1) THEN ! if we output more than just the time channel....
         indxLast = indxNext + SIZE(y_FAST%DriverWriteOutput) - 1
         OutputAry(indxNext:indxLast) = y_FAST%DriverWriteOutput
         indxNext = IndxLast + 1
      END IF

      IF ( y_FAST%numOuts(Module_IfW) > 0 ) THEN
         indxLast = indxNext + SIZE(IfWOutput) - 1
         OutputAry(indxNext:indxLast) = IfWOutput
         indxNext = IndxLast + 1
      ELSEIF ( y_FAST%numOuts(Module_OpFM) > 0 ) THEN
         indxLast = indxNext + SIZE(OpFMOutput) - 1
         OutputAry(indxNext:indxLast) = OpFMOutput
         indxNext = IndxLast + 1
      END IF

      IF ( y_FAST%numOuts(Module_ED) > 0 ) THEN
         indxLast = indxNext + SIZE(EDOutput) - 1
         OutputAry(indxNext:indxLast) = EDOutput
         indxNext = IndxLast + 1
      END IF

      IF ( y_FAST%numOuts(Module_BD) > 0 ) THEN
         do i=1,SIZE(y_BD)
            indxLast = indxNext + SIZE(y_BD(i)%WriteOutput) - 1
            OutputAry(indxNext:indxLast) = y_BD(i)%WriteOutput
            indxNext = IndxLast + 1
         end do
      END IF

      IF ( y_FAST%numOuts(Module_AD) > 0 ) THEN
         do i=1,SIZE(y_AD%Rotors)
            if (allocated(y_AD%Rotors(i)%WriteOutput)) then
               indxLast = indxNext + SIZE(y_AD%Rotors(i)%WriteOutput) - 1
               OutputAry(indxNext:indxLast) = y_AD%Rotors(i)%WriteOutput
               indxNext = IndxLast + 1
            endif
         end do         
      END IF            
         
      IF ( y_FAST%numOuts(Module_SrvD) > 0 ) THEN
         indxLast = indxNext + SIZE(SrvDOutput) - 1
         OutputAry(indxNext:indxLast) = SrvDOutput
         indxNext = IndxLast + 1
      END IF

      IF ( y_FAST%numOuts(Module_HD) > 0 ) THEN
         indxLast = indxNext + SIZE(HDOutput) - 1
         OutputAry(indxNext:indxLast) = HDOutput
         indxNext = IndxLast + 1
      END IF

      IF ( y_FAST%numOuts(Module_SD) > 0 ) THEN
         indxLast = indxNext + SIZE(SDOutput) - 1
         OutputAry(indxNext:indxLast) = SDOutput
         indxNext = IndxLast + 1
      ELSE IF ( y_FAST%numOuts(Module_ExtPtfm) > 0 ) THEN
         indxLast = indxNext + SIZE(ExtPtfmOutput) - 1
         OutputAry(indxNext:indxLast) = ExtPtfmOutput
         indxNext = IndxLast + 1
      END IF

      IF ( y_FAST%numOuts(Module_MAP) > 0 ) THEN
         indxLast = indxNext + SIZE(MAPOutput) - 1
         OutputAry(indxNext:indxLast) = MAPOutput
         indxNext = IndxLast + 1
      ELSEIF ( y_FAST%numOuts(Module_MD) > 0 ) THEN
         indxLast = indxNext + SIZE(MDOutput) - 1
         OutputAry(indxNext:indxLast) = MDOutput
         indxNext = IndxLast + 1
      ELSEIF ( y_FAST%numOuts(Module_FEAM) > 0 ) THEN
         indxLast = indxNext + SIZE(FEAMOutput) - 1
         OutputAry(indxNext:indxLast) = FEAMOutput
         indxNext = IndxLast + 1
      ELSEIF ( y_FAST%numOuts(Module_Orca) > 0 ) THEN
         indxLast = indxNext + SIZE(OrcaOutput) - 1
         OutputAry(indxNext:indxLast) = OrcaOutput
         indxNext = IndxLast + 1
      END IF

      IF ( y_FAST%numOuts(Module_IceF) > 0 ) THEN
         indxLast = indxNext + SIZE(IceFOutput) - 1
         OutputAry(indxNext:indxLast) = IceFOutput
         indxNext = IndxLast + 1
      ELSEIF ( y_FAST%numOuts(Module_IceD) > 0 ) THEN
         DO i=1,p_FAST%numIceLegs
            indxLast = indxNext + SIZE(y_IceD(i)%WriteOutput) - 1
            OutputAry(indxNext:indxLast) = y_IceD(i)%WriteOutput
            indxNext = IndxLast + 1
         END DO
      END IF

END SUBROUTINE FillOutputAry
!----------------------------------------------------------------------------------------------------------------------------------
!> This writes data to the FAST summary file.
SUBROUTINE FAST_WrSum( p_FAST, y_FAST, MeshMapData, ErrStat, ErrMsg )

   TYPE(FAST_ParameterType), INTENT(IN)    :: p_FAST                             !< Glue-code simulation parameters
   TYPE(FAST_OutputFileType),INTENT(INOUT) :: y_FAST                             !< Glue-code simulation outputs (changes value of UnSum)
   TYPE(FAST_ModuleMapType), INTENT(IN)    :: MeshMapData                        !< Data for mapping between modules
   INTEGER(IntKi),           INTENT(OUT)   :: ErrStat                            !< Error status (level)
   CHARACTER(*),             INTENT(OUT)   :: ErrMsg                             !< Message describing error reported in ErrStat

      ! local variables
   REAL(ReKi)                              :: TmpRate                            ! temporary rate for vtk output
   INTEGER(IntKi)                          :: I                                  ! temporary counter
   INTEGER(IntKi)                          :: J                                  ! temporary counter
   INTEGER(IntKi)                          :: Module_Number                      ! loop counter through the modules
   CHARACTER(200)                          :: Fmt                                ! temporary format string
   CHARACTER(200)                          :: DescStr                            ! temporary string to write text
   CHARACTER(*), PARAMETER                 :: NotUsedTxt = " [not called]"       ! text written if a module is not called
   CHARACTER(ChanLen)                      :: ChanTxt(2)                         ! temp strings to help with formatting with unknown ChanLen size

      ! Get a unit number and open the file:

   CALL GetNewUnit( y_FAST%UnSum, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL OpenFOutFile ( y_FAST%UnSum, TRIM(p_FAST%OutFileRoot)//'.sum', ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) RETURN

         ! Add some file information:

   !.......................... Module Versions .....................................................
   !bjj: modules in this list are ordered by the order they are specified in the FAST input file

   WRITE (y_FAST%UnSum,'(/A)') 'FAST Summary File'
   WRITE (y_FAST%UnSum,'(/A)')  TRIM( y_FAST%FileDescLines(1) )

   WRITE (y_FAST%UnSum,'(2X,A)'   )  'compiled with'
   Fmt = '(4x,A)'
   WRITE (y_FAST%UnSum,Fmt)  TRIM( GetNVD(        NWTC_Ver ) )
   WRITE (y_FAST%UnSum,Fmt)  TRIM( GetNVD( y_FAST%Module_Ver( Module_ED )   ) )

   DescStr = GetNVD( y_FAST%Module_Ver( Module_BD ) )
   IF ( p_FAST%CompElast /= Module_BD ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )

   DescStr = GetNVD( y_FAST%Module_Ver( Module_IfW ) )
   IF ( p_FAST%CompInflow /= Module_IfW ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )

   ! I'm not going to write the openfoam module info to the summary file
   !DescStr = GetNVD( y_FAST%Module_Ver( Module_OpFM ) )
   !IF ( p_FAST%CompInflow /= Module_OpFM ) DescStr = TRIM(DescStr)//NotUsedTxt
   !WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )

   DescStr = GetNVD( y_FAST%Module_Ver( Module_AD14 ) )
   IF ( p_FAST%CompAero /= Module_AD14 ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )

   DescStr = GetNVD( y_FAST%Module_Ver( Module_AD ) )
   IF ( p_FAST%CompAero /= Module_AD ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )

   DescStr = GetNVD( y_FAST%Module_Ver( Module_SrvD ) )
   IF ( p_FAST%CompServo /= Module_SrvD ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )

   DescStr = GetNVD( y_FAST%Module_Ver( Module_HD ) )
   IF ( p_FAST%CompHydro /= Module_HD  ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )

   DescStr = GetNVD( y_FAST%Module_Ver( Module_SD ) )
   IF ( p_FAST%CompSub /= Module_SD ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )

   DescStr = GetNVD( y_FAST%Module_Ver( Module_ExtPtfm ) )
   IF ( p_FAST%CompSub /= Module_ExtPtfm ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )

   DescStr = GetNVD( y_FAST%Module_Ver( Module_MAP ) )
   IF ( p_FAST%CompMooring /= Module_MAP ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )

   DescStr = GetNVD( y_FAST%Module_Ver( Module_FEAM ) )
   IF ( p_FAST%CompMooring /= Module_FEAM ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )

   DescStr = GetNVD( y_FAST%Module_Ver( Module_MD ) )
   IF ( p_FAST%CompMooring /= Module_MD ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )

   DescStr = GetNVD( y_FAST%Module_Ver( Module_Orca ) )
   IF ( p_FAST%CompMooring /= Module_Orca ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )

   DescStr = GetNVD( y_FAST%Module_Ver( Module_IceF ) )
   IF ( p_FAST%CompIce /= Module_IceF ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )

   DescStr = GetNVD( y_FAST%Module_Ver( Module_IceD ) )
   IF ( p_FAST%CompIce /= Module_IceD ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )


   !.......................... Information from FAST input File ......................................
! OTHER information we could print here:
! current working directory
! output file root name
! output file time step
! output file format (text/binary)
! coupling method

   SELECT CASE ( p_FAST%TurbineType )
   CASE ( Type_LandBased )
      DescStr = 'Modeling a land-based turbine'
   CASE ( Type_Offshore_Fixed )
      DescStr = 'Modeling a fixed-bottom offshore turbine'
   CASE ( Type_Offshore_Floating )
      DescStr = 'Modeling a floating offshore turbine'
   CASE ( Type_MHK_Fixed )
      DescStr = 'Modeling a fixed-bottom MHK turbine'
   CASE ( Type_MHK_Floating )
      DescStr = 'Modeling a floating MHK turbine'
   CASE DEFAULT ! This should never happen
      DescStr=""
   END SELECT
   WRITE(y_FAST%UnSum,'(//A)') TRIM(DescStr)

   WRITE (y_FAST%UnSum,'(A)' )   'Description from the FAST input file: '
   WRITE (y_FAST%UnSum,'(2X,A)')  TRIM(p_FAST%FTitle)

   !.......................... Requested Features ...................................................

   SELECT CASE ( p_FAST%InterpOrder )
   CASE (0)
      DescStr = ' (nearest neighbor)'
   CASE (1)
      DescStr = ' (linear)'
   CASE (2)
      DescStr = ' (quadratic)'
   CASE DEFAULT
      DescStr = ' ( )'
   END SELECT

   WRITE(y_FAST%UnSum,'(/A,I1,A)'  ) 'Interpolation order for input/output time histories: ', p_FAST%InterpOrder, TRIM(DescStr)
   WRITE(y_FAST%UnSum,'( A,I2)'    ) 'Number of correction iterations: ', p_FAST%NumCrctn


   !.......................... Information About Coupling ...................................................

   IF ( ALLOCATED( MeshMapData%Jacobian_Opt1 ) ) then ! we're using option 1

      IF ( p_FAST%CompSub /= Module_None .OR. p_FAST%CompElast == Module_BD .OR. p_FAST%CompMooring == Module_Orca ) THEN  ! SubDyn-BeamDyn-HydroDyn-ElastoDyn-ExtPtfm
         DescStr = 'ElastoDyn, SubDyn, HydroDyn, OrcaFlex, ExtPtfm_MCKF, and/or BeamDyn'
      ELSE ! IF ( p_FAST%CompHydro == Module_HD ) THEN
         DescStr = "ElastoDyn to HydroDyn"
      END IF

      WRITE(y_FAST%UnSum,'( A,I6)'  ) 'Number of rows in Jacobian matrix used for coupling '//TRIM(DescStr)//': ', &
                                       SIZE(MeshMapData%Jacobian_Opt1, 1)
   END IF

   !.......................... Time step information: ...................................................

   WRITE (y_FAST%UnSum,'(//,2X,A)') " Requested Time Steps  "
   WRITE (y_FAST%UnSum,   '(2X,A)') "-------------------------------------------------"
   Fmt = '(2X,A17,2X,A15,2X,A13)'
   WRITE (y_FAST%UnSum, Fmt ) "Component        ", "Time Step (s)  ", "Subcycles (-)"
   WRITE (y_FAST%UnSum, Fmt ) "-----------------", "---------------", "-------------"
   Fmt = '(2X,A17,2X,'//TRIM(p_FAST%OutFmt)//',:,T37,2X,I8,:,A)'
   WRITE (y_FAST%UnSum, Fmt ) "FAST (glue code) ", p_FAST%DT
   DO Module_Number=2,NumModules ! assumes glue-code is module number 1 (i.e., MODULE_Glue == 1)
      IF (p_FAST%ModuleInitialized(Module_Number)) THEN
         WRITE (y_FAST%UnSum, Fmt ) y_FAST%Module_Ver(Module_Number)%Name, p_FAST%DT_module(Module_Number), p_FAST%n_substeps(Module_Number)
      END IF
   END DO
   IF ( p_FAST%n_DT_Out  == 1_IntKi ) THEN
      WRITE (y_FAST%UnSum, Fmt ) "FAST output files", p_FAST%DT_out, 1_IntKi   ! we'll write "1" instead of "1^-1"
   ELSE
      WRITE (y_FAST%UnSum, Fmt ) "FAST output files", p_FAST%DT_out, p_FAST%n_DT_Out,"^-1"
   END IF

   IF (p_FAST%WrVTK == VTK_Animate) THEN

      TmpRate = p_FAST%DT*p_FAST%n_VTKTime

      IF ( p_FAST%n_VTKTime == 1_IntKi ) THEN
         WRITE (y_FAST%UnSum, Fmt ) "VTK output files ", p_FAST%DT, 1_IntKi   ! we'll write "1" instead of "1^-1"
      ELSE
         WRITE (y_FAST%UnSum, Fmt ) "VTK output files ", TmpRate, p_FAST%n_VTKTime,"^-1"
      END IF
   ELSE
      TmpRate = p_FAST%VTK_fps
   END IF

      ! bjj: fix this; possibly add names of which files will be generated?
   IF (p_FAST%WrVTK == VTK_Animate .or. p_FAST%WrVTK == VTK_ModeShapes) THEN
      Fmt = '(2X,A17,2X,'//TRIM(p_FAST%OutFmt)//',:,T37,:,A)'

      WRITE (y_FAST%UnSum,'(//,2X,A)') " Requested Visualization Output"
      WRITE (y_FAST%UnSum,   '(2X,A)') "-------------------------------------------------"
      WRITE (y_FAST%UnSum,     Fmt   ) "Frame rate", 1.0_DbKi/TmpRate, " fps"
   END IF


   !.......................... Requested Output Channels ............................................

   WRITE (y_FAST%UnSum,'(//,2X,A)') " Requested Channels in FAST Output File(s)  "
   WRITE (y_FAST%UnSum,   '(2X,A)') "--------------------------------------------"
   Fmt = '(2X,A6,2(2X,A'//TRIM(num2lstr(ChanLen))//'),2X,A)'
   ChanTxt(1) = 'Name'
   ChanTxt(2) = 'Units'
   WRITE (y_FAST%UnSum, Fmt ) "Number", ChanTxt, "Generated by"
   ChanTxt = '--------------------' !this ought to be sufficiently long
   WRITE (y_FAST%UnSum, Fmt ) "------", ChanTxt, "------------"

   Fmt = '(4X,I4,2(2X,A'//TRIM(num2lstr(ChanLen))//'),2X,A)'
   I = 0
   DO Module_Number = 1,NumModules
      DO J = 1,y_FAST%numOuts( Module_Number )
         I = I + 1
         WRITE (y_FAST%UnSum, Fmt ) I, y_FAST%ChannelNames(I), y_FAST%ChannelUnits(I), TRIM(y_FAST%Module_Ver( Module_Number )%Name)
      END DO
   END DO


   !.......................... End of Summary File ............................................

   ! bjj: note that I'm not closing the summary file here, though at the present time we don't write to this file again.
   ! In the future, we may want to write additional information to this file during the simulation.
   ! bjj 4/21/2015: closing the file now because of restart. If it needs to be open later, we can change it again.

   CLOSE( y_FAST%UnSum )
   y_FAST%UnSum = -1

END SUBROUTINE FAST_WrSum
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine initializes the input and output arrays stored for extrapolation. They are initialized after the first input-output solve so that the first
!! extrapolations are used with values from the solution, not just initial guesses. It also creates new copies of the state variables, which need to
!! be stored for the predictor-corrector loop.
SUBROUTINE FAST_InitIOarrays( t_initial, p_FAST, y_FAST, m_FAST, ED, BD, SrvD, AD14, AD, IfW, HD, SD, ExtPtfm, &
   MAPp, FEAM, MD, Orca, IceF, IceD, ErrStat, ErrMsg )

REAL(DbKi),               INTENT(IN   ) :: t_initial           !< start time of the simulation
TYPE(FAST_ParameterType), INTENT(IN   ) :: p_FAST              !< Parameters for the glue code
TYPE(FAST_OutputFileType),INTENT(IN   ) :: y_FAST              !< Output variables for the glue code
TYPE(FAST_MiscVarType),   INTENT(IN   ) :: m_FAST              !< Miscellaneous variables

TYPE(ElastoDyn_Data),     INTENT(INOUT) :: ED                  !< ElastoDyn data
TYPE(BeamDyn_Data),       INTENT(INOUT) :: BD                  !< BeamDyn data
TYPE(ServoDyn_Data),      INTENT(INOUT) :: SrvD                !< ServoDyn data
TYPE(AeroDyn14_Data),     INTENT(INOUT) :: AD14                !< AeroDyn v14 data
TYPE(AeroDyn_Data),       INTENT(INOUT) :: AD                  !< AeroDyn data
TYPE(InflowWind_Data),    INTENT(INOUT) :: IfW                 !< InflowWind data
TYPE(HydroDyn_Data),      INTENT(INOUT) :: HD                  !< HydroDyn data
TYPE(SubDyn_Data),        INTENT(INOUT) :: SD                  !< SubDyn data
TYPE(ExtPtfm_Data),       INTENT(INOUT) :: ExtPtfm             !< ExtPtfm_MCKF data
TYPE(MAP_Data),           INTENT(INOUT) :: MAPp                !< MAP data
TYPE(FEAMooring_Data),    INTENT(INOUT) :: FEAM                !< FEAMooring data
TYPE(MoorDyn_Data),       INTENT(INOUT) :: MD                  !< MoorDyn data
TYPE(OrcaFlex_Data),      INTENT(INOUT) :: Orca                !< OrcaFlex interface data
TYPE(IceFloe_Data),       INTENT(INOUT) :: IceF                !< IceFloe data
TYPE(IceDyn_Data),        INTENT(INOUT) :: IceD                !< All the IceDyn data used in time-step loop

INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None

! local variables
INTEGER(IntKi)                          :: i, j, k             ! loop counters
INTEGER(IntKi)                          :: ErrStat2
CHARACTER(ErrMsgLen)                    :: ErrMsg2
CHARACTER(*), PARAMETER                 :: RoutineName = 'FAST_InitIOarrays'


ErrStat = ErrID_None
ErrMsg  = ""

! We fill ED%InputTimes with negative times, but the ED%Input values are identical for each of those times; this allows
! us to use, e.g., quadratic interpolation that effectively acts as a zeroth-order extrapolation and first-order extrapolation
! for the first and second time steps.  (The interpolation order in the ExtrapInput routines are determined as
! order = SIZE(ED%Input)


DO j = 1, p_FAST%InterpOrder + 1
ED%InputTimes(j) = t_initial - (j - 1) * p_FAST%dt
END DO

DO j = 2, p_FAST%InterpOrder + 1
CALL ED_CopyInput (ED%Input(1),  ED%Input(j),  MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END DO
CALL ED_CopyInput (ED%Input(1),  ED%u,  MESH_NEWCOPY, Errstat2, ErrMsg2) ! do this to initialize meshes/allocatable arrays for output of ExtrapInterp routine
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

! Initialize predicted states for j_pc loop:
CALL ED_CopyContState   (ED%x( STATE_CURR), ED%x( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL ED_CopyDiscState   (ED%xd(STATE_CURR), ED%xd(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL ED_CopyConstrState (ED%z( STATE_CURR), ED%z( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL ED_CopyOtherState (ED%OtherSt( STATE_CURR), ED%OtherSt( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


IF  (p_FAST%CompElast == Module_BD ) THEN

DO k = 1,p_FAST%nBeams

! Copy values for interpolation/extrapolation:
DO j = 1, p_FAST%InterpOrder + 1
BD%InputTimes(j,k) = t_initial - (j - 1) * p_FAST%dt
END DO

DO j = 2, p_FAST%InterpOrder + 1
CALL BD_CopyInput (BD%Input(1,k),  BD%Input(j,k),  MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END DO
CALL BD_CopyInput (BD%Input(1,k),  BD%u(k),  MESH_NEWCOPY, Errstat2, ErrMsg2) ! do this to initialize meshes/allocatable arrays for output of ExtrapInterp routine
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


! Initialize predicted states for j_pc loop:
CALL BD_CopyContState   (BD%x( k,STATE_CURR), BD%x( k,STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL BD_CopyDiscState   (BD%xd(k,STATE_CURR), BD%xd(k,STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL BD_CopyConstrState (BD%z( k,STATE_CURR), BD%z( k,STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL BD_CopyOtherState (BD%OtherSt( k,STATE_CURR), BD%OtherSt( k,STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

END DO ! nBeams

END IF ! CompElast


IF ( p_FAST%CompServo == Module_SrvD ) THEN
! Initialize Input-Output arrays for interpolation/extrapolation:

DO j = 1, p_FAST%InterpOrder + 1
SrvD%InputTimes(j) = t_initial - (j - 1) * p_FAST%dt
!SrvD_OutputTimes(j) = t_initial - (j - 1) * dt
END DO

DO j = 2, p_FAST%InterpOrder + 1
CALL SrvD_CopyInput (SrvD%Input(1),  SrvD%Input(j),  MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END DO
CALL SrvD_CopyInput (SrvD%Input(1),  SrvD%u,  MESH_NEWCOPY, Errstat2, ErrMsg2) ! do this to initialize meshes/allocatable arrays for output of ExtrapInterp routine
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

! Initialize predicted states for j_pc loop:
CALL SrvD_CopyContState   (SrvD%x( STATE_CURR), SrvD%x( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL SrvD_CopyDiscState   (SrvD%xd(STATE_CURR), SrvD%xd(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL SrvD_CopyConstrState (SrvD%z( STATE_CURR), SrvD%z( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL SrvD_CopyOtherState( SrvD%OtherSt(STATE_CURR), SrvD%OtherSt(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

END IF ! CompServo


IF ( p_FAST%CompAero == Module_AD14 ) THEN
! Copy values for interpolation/extrapolation:

DO j = 1, p_FAST%InterpOrder + 1
AD14%InputTimes(j) = t_initial - (j - 1) * p_FAST%dt
END DO

DO j = 2, p_FAST%InterpOrder + 1
CALL AD14_CopyInput (AD14%Input(1),  AD14%Input(j),  MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END DO
CALL AD14_CopyInput (AD14%Input(1),  AD14%u,  MESH_NEWCOPY, Errstat2, ErrMsg2) ! do this to initialize meshes/allocatable arrays for output of ExtrapInterp routine
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


! Initialize predicted states for j_pc loop:
CALL AD14_CopyContState   (AD14%x( STATE_CURR), AD14%x( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL AD14_CopyDiscState   (AD14%xd(STATE_CURR), AD14%xd(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL AD14_CopyConstrState (AD14%z( STATE_CURR), AD14%z( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL AD14_CopyOtherState( AD14%OtherSt(STATE_CURR), AD14%OtherSt(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

ELSEIF ( p_FAST%CompAero == Module_AD ) THEN
! Copy values for interpolation/extrapolation:

DO j = 1, p_FAST%InterpOrder + 1
AD%InputTimes(j) = t_initial - (j - 1) * p_FAST%dt
END DO

DO j = 2, p_FAST%InterpOrder + 1
CALL AD_CopyInput (AD%Input(1),  AD%Input(j),  MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END DO
CALL AD_CopyInput (AD%Input(1),  AD%u,  MESH_NEWCOPY, Errstat2, ErrMsg2) ! do this to initialize meshes/allocatable arrays for output of ExtrapInterp routine
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


! Initialize predicted states for j_pc loop:
CALL AD_CopyContState(AD%x(STATE_CURR), AD%x(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL AD_CopyDiscState(AD%xd(STATE_CURR), AD%xd(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL AD_CopyConstrState(AD%z(STATE_CURR), AD%z(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL AD_CopyOtherState(AD%OtherSt(STATE_CURR), AD%OtherSt(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

END IF ! CompAero == Module_AD



IF ( p_FAST%CompInflow == Module_IfW ) THEN
! Copy values for interpolation/extrapolation:

DO j = 1, p_FAST%InterpOrder + 1
IfW%InputTimes(j) = t_initial - (j - 1) * p_FAST%dt
!IfW%OutputTimes(i) = t_initial - (j - 1) * dt
END DO

DO j = 2, p_FAST%InterpOrder + 1
CALL InflowWind_CopyInput (IfW%Input(1),  IfW%Input(j),  MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END DO
CALL InflowWind_CopyInput (IfW%Input(1),  IfW%u,  MESH_NEWCOPY, Errstat2, ErrMsg2) ! do this to initialize meshes/allocatable arrays for output of ExtrapInterp routine
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


! Initialize predicted states for j_pc loop:
CALL InflowWind_CopyContState   (IfW%x( STATE_CURR), IfW%x( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL InflowWind_CopyDiscState   (IfW%xd(STATE_CURR), IfW%xd(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL InflowWind_CopyConstrState (IfW%z( STATE_CURR), IfW%z( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL InflowWind_CopyOtherState( IfW%OtherSt(STATE_CURR), IfW%OtherSt(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

END IF ! CompInflow == Module_IfW


IF ( p_FAST%CompHydro == Module_HD ) THEN
! Copy values for interpolation/extrapolation:
DO j = 1, p_FAST%InterpOrder + 1
HD%InputTimes(j) = t_initial - (j - 1) * p_FAST%dt
!HD_OutputTimes(i) = t_initial - (j - 1) * dt
END DO

DO j = 2, p_FAST%InterpOrder + 1
CALL HydroDyn_CopyInput (HD%Input(1),  HD%Input(j),  MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END DO
CALL HydroDyn_CopyInput (HD%Input(1),  HD%u,  MESH_NEWCOPY, Errstat2, ErrMsg2) ! do this to initialize meshes/allocatable arrays for output of ExtrapInterp routine
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


! Initialize predicted states for j_pc loop:
CALL HydroDyn_CopyContState   (HD%x( STATE_CURR), HD%x( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL HydroDyn_CopyDiscState   (HD%xd(STATE_CURR), HD%xd(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL HydroDyn_CopyConstrState (HD%z( STATE_CURR), HD%z( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL HydroDyn_CopyOtherState( HD%OtherSt(STATE_CURR), HD%OtherSt(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

END IF !CompHydro


IF  (p_FAST%CompSub == Module_SD ) THEN

! Copy values for interpolation/extrapolation:
DO j = 1, p_FAST%InterpOrder + 1
SD%InputTimes(j) = t_initial - (j - 1) * p_FAST%dt
!SD_OutputTimes(i) = t_initial - (j - 1) * dt
END DO

DO j = 2, p_FAST%InterpOrder + 1
CALL SD_CopyInput (SD%Input(1),  SD%Input(j),  MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END DO
CALL SD_CopyInput (SD%Input(1),  SD%u,  MESH_NEWCOPY, Errstat2, ErrMsg2) ! do this to initialize meshes/allocatable arrays for output of ExtrapInterp routine
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


! Initialize predicted states for j_pc loop:
CALL SD_CopyContState   (SD%x( STATE_CURR), SD%x( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL SD_CopyDiscState   (SD%xd(STATE_CURR), SD%xd(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL SD_CopyConstrState (SD%z( STATE_CURR), SD%z( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL SD_CopyOtherState( SD%OtherSt(STATE_CURR), SD%OtherSt(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

ELSE IF (p_FAST%CompSub == Module_ExtPtfm ) THEN

! Copy values for interpolation/extrapolation:
DO j = 1, p_FAST%InterpOrder + 1
ExtPtfm%InputTimes(j) = t_initial - (j - 1) * p_FAST%dt
END DO

DO j = 2, p_FAST%InterpOrder + 1
CALL ExtPtfm_CopyInput (ExtPtfm%Input(1),  ExtPtfm%Input(j),  MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END DO
CALL ExtPtfm_CopyInput (ExtPtfm%Input(1),  ExtPtfm%u,  MESH_NEWCOPY, Errstat2, ErrMsg2) ! do this to initialize meshes/allocatable arrays for output of ExtrapInterp routine
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


! Initialize predicted states for j_pc loop:
CALL ExtPtfm_CopyContState   (ExtPtfm%x( STATE_CURR), ExtPtfm%x( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL ExtPtfm_CopyDiscState   (ExtPtfm%xd(STATE_CURR), ExtPtfm%xd(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL ExtPtfm_CopyConstrState (ExtPtfm%z( STATE_CURR), ExtPtfm%z( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL ExtPtfm_CopyOtherState( ExtPtfm%OtherSt(STATE_CURR), ExtPtfm%OtherSt(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END IF ! CompSub


IF (p_FAST%CompMooring == Module_MAP) THEN
! Copy values for interpolation/extrapolation:

DO j = 1, p_FAST%InterpOrder + 1
MAPp%InputTimes(j) = t_initial - (j - 1) * p_FAST%dt
!MAP_OutputTimes(i) = t_initial - (j - 1) * dt
END DO

DO j = 2, p_FAST%InterpOrder + 1
CALL MAP_CopyInput (MAPp%Input(1),  MAPp%Input(j),  MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END DO
CALL MAP_CopyInput (MAPp%Input(1),  MAPp%u,  MESH_NEWCOPY, Errstat2, ErrMsg2) ! do this to initialize meshes/allocatable arrays for output of ExtrapInterp routine
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

! Initialize predicted states for j_pc loop:
CALL MAP_CopyContState   (MAPp%x( STATE_CURR), MAPp%x( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL MAP_CopyDiscState   (MAPp%xd(STATE_CURR), MAPp%xd(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL MAP_CopyConstrState (MAPp%z( STATE_CURR), MAPp%z( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
IF ( p_FAST%n_substeps( MODULE_MAP ) > 1 ) THEN
CALL MAP_CopyOtherState( MAPp%OtherSt, MAPp%OtherSt_old, MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END IF

ELSEIF (p_FAST%CompMooring == Module_MD) THEN
! Copy values for interpolation/extrapolation:

DO j = 1, p_FAST%InterpOrder + 1
MD%InputTimes(j) = t_initial - (j - 1) * p_FAST%dt
!MD_OutputTimes(i) = t_initial - (j - 1) * dt
END DO

DO j = 2, p_FAST%InterpOrder + 1
CALL MD_CopyInput (MD%Input(1),  MD%Input(j),  MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END DO
CALL MD_CopyInput (MD%Input(1),  MD%u,  MESH_NEWCOPY, Errstat2, ErrMsg2) ! do this to initialize meshes/allocatable arrays for output of ExtrapInterp routine
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

! Initialize predicted states for j_pc loop:
CALL MD_CopyContState   (MD%x( STATE_CURR), MD%x( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL MD_CopyDiscState   (MD%xd(STATE_CURR), MD%xd(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL MD_CopyConstrState (MD%z( STATE_CURR), MD%z( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL MD_CopyOtherState( MD%OtherSt(STATE_CURR), MD%OtherSt(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

ELSEIF (p_FAST%CompMooring == Module_FEAM) THEN
! Copy values for interpolation/extrapolation:

DO j = 1, p_FAST%InterpOrder + 1
FEAM%InputTimes(j) = t_initial - (j - 1) * p_FAST%dt
!FEAM_OutputTimes(i) = t_initial - (j - 1) * dt
END DO

DO j = 2, p_FAST%InterpOrder + 1
CALL FEAM_CopyInput (FEAM%Input(1),  FEAM%Input(j),  MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END DO
CALL FEAM_CopyInput (FEAM%Input(1),  FEAM%u,  MESH_NEWCOPY, Errstat2, ErrMsg2) ! do this to initialize meshes/allocatable arrays for output of ExtrapInterp routine
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

! Initialize predicted states for j_pc loop:
CALL FEAM_CopyContState   (FEAM%x( STATE_CURR), FEAM%x( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL FEAM_CopyDiscState   (FEAM%xd(STATE_CURR), FEAM%xd(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL FEAM_CopyConstrState (FEAM%z( STATE_CURR), FEAM%z( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL FEAM_CopyOtherState( FEAM%OtherSt(STATE_CURR), FEAM%OtherSt(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

ELSEIF (p_FAST%CompMooring == Module_Orca) THEN
! Copy values for interpolation/extrapolation:

DO j = 1, p_FAST%InterpOrder + 1
Orca%InputTimes(j) = t_initial - (j - 1) * p_FAST%dt
END DO

DO j = 2, p_FAST%InterpOrder + 1
CALL Orca_CopyInput (Orca%Input(1),  Orca%Input(j),  MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END DO
CALL Orca_CopyInput (Orca%Input(1),  Orca%u,  MESH_NEWCOPY, Errstat2, ErrMsg2) ! do this to initialize meshes/allocatable arrays for output of ExtrapInterp routine
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

! Initialize predicted states for j_pc loop:
CALL Orca_CopyContState   (Orca%x( STATE_CURR), Orca%x( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL Orca_CopyDiscState   (Orca%xd(STATE_CURR), Orca%xd(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL Orca_CopyConstrState (Orca%z( STATE_CURR), Orca%z( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL Orca_CopyOtherState( Orca%OtherSt(STATE_CURR), Orca%OtherSt(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END IF ! CompMooring


IF  (p_FAST%CompIce == Module_IceF ) THEN

! Copy values for interpolation/extrapolation:
DO j = 1, p_FAST%InterpOrder + 1
IceF%InputTimes(j) = t_initial - (j - 1) * p_FAST%dt
!IceF_OutputTimes(i) = t_initial - (j - 1) * dt
END DO

DO j = 2, p_FAST%InterpOrder + 1
CALL IceFloe_CopyInput (IceF%Input(1),  IceF%Input(j),  MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END DO
CALL IceFloe_CopyInput (IceF%Input(1),  IceF%u,  MESH_NEWCOPY, Errstat2, ErrMsg2) ! do this to initialize meshes/allocatable arrays for output of ExtrapInterp routine
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


! Initialize predicted states for j_pc loop:
CALL IceFloe_CopyContState   (IceF%x( STATE_CURR), IceF%x( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL IceFloe_CopyDiscState   (IceF%xd(STATE_CURR), IceF%xd(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL IceFloe_CopyConstrState (IceF%z( STATE_CURR), IceF%z( STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL IceFloe_CopyOtherState( IceF%OtherSt(STATE_CURR), IceF%OtherSt(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

ELSEIF  (p_FAST%CompIce == Module_IceD ) THEN

DO i = 1,p_FAST%numIceLegs

! Copy values for interpolation/extrapolation:
DO j = 1, p_FAST%InterpOrder + 1
IceD%InputTimes(j,i) = t_initial - (j - 1) * p_FAST%dt
!IceD%OutputTimes(j,i) = t_initial - (j - 1) * dt
END DO

DO j = 2, p_FAST%InterpOrder + 1
CALL IceD_CopyInput (IceD%Input(1,i),  IceD%Input(j,i),  MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
END DO
CALL IceD_CopyInput (IceD%Input(1,i),  IceD%u(i),  MESH_NEWCOPY, Errstat2, ErrMsg2) ! do this to initialize meshes/allocatable arrays for output of ExtrapInterp routine
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


! Initialize predicted states for j_pc loop:
CALL IceD_CopyContState   (IceD%x( i,STATE_CURR), IceD%x( i,STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL IceD_CopyDiscState   (IceD%xd(i,STATE_CURR), IceD%xd(i,STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL IceD_CopyConstrState (IceD%z( i,STATE_CURR), IceD%z( i,STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
CALL IceD_CopyOtherState( IceD%OtherSt(i,STATE_CURR), IceD%OtherSt(i,STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2)
CALL SetErrStat( Errstat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

END DO ! numIceLegs

END IF ! CompIce


END SUBROUTINE FAST_InitIOarrays

!----------------------------------------------------------------------------------------------------------------------------------
! ROUTINES TO OUTPUT WRITE DATA TO FILE AT EACH REQUSTED TIME STEP
!----------------------------------------------------------------------------------------------------------------------------------
FUNCTION NeedWriteOutput(n_t_global, t_global, p_FAST)
   INTEGER(IntKi),           INTENT(IN   ) :: n_t_global          !< Current global time step
   REAL(DbKi),               INTENT(IN   ) :: t_global            !< Current global time
   TYPE(FAST_ParameterType), INTENT(IN   ) :: p_FAST              !< Parameters for the glue code

   LOGICAL                                 :: NeedWriteOutput     !< Function result; if true, WriteOutput values are needed on this time step

   IF ( t_global >= p_FAST%TStart )  THEN ! note that if TStart isn't an multiple of DT_out, we will not necessarially start output to the file at TStart
      NeedWriteOutput = MOD( n_t_global, p_FAST%n_DT_Out ) == 0
   ELSE
      NeedWriteOutput = .FALSE.
   END IF

END FUNCTION NeedWriteOutput
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine determines if it's time to write to the output files--based on a previous call to fast_subs::needwriteoutput--, and
!! calls the routine to write to the files with the output data. It should be called after all the output solves for a given time
!! have been completed, and assumes y_FAST\%WriteThisStep has been set.
SUBROUTINE WriteOutputToFile(n_t_global, t_global, p_FAST, y_FAST, ED, BD, AD14, AD, IfW, OpFM, HD, SD, ExtPtfm, &
                             SrvD, MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, ErrStat, ErrMsg)
!...............................................................................................................................
   INTEGER(IntKi),           INTENT(IN   ) :: n_t_global          !< Current global time step
   REAL(DbKi),               INTENT(IN   ) :: t_global            !< Current global time
   TYPE(FAST_ParameterType), INTENT(IN   ) :: p_FAST              !< Parameters for the glue code
   TYPE(FAST_OutputFileType),INTENT(INOUT) :: y_FAST              !< Output variables for the glue code

   TYPE(ElastoDyn_Data),     INTENT(IN   ) :: ED                  !< ElastoDyn data
   TYPE(BeamDyn_Data),       INTENT(IN   ) :: BD                  !< BeamDyn data
   TYPE(ServoDyn_Data),      INTENT(IN   ) :: SrvD                !< ServoDyn data
   TYPE(AeroDyn14_Data),     INTENT(IN   ) :: AD14                !< AeroDyn14 data
   TYPE(AeroDyn_Data),       INTENT(IN   ) :: AD                  !< AeroDyn data
   TYPE(InflowWind_Data),    INTENT(IN   ) :: IfW                 !< InflowWind data
   TYPE(OpenFOAM_Data),      INTENT(IN   ) :: OpFM                !< OpenFOAM data
   TYPE(HydroDyn_Data),      INTENT(IN   ) :: HD                  !< HydroDyn data
   TYPE(SubDyn_Data),        INTENT(IN   ) :: SD                  !< SubDyn data
   TYPE(ExtPtfm_Data),       INTENT(IN   ) :: ExtPtfm             !< ExtPtfm_MCKF data
   TYPE(MAP_Data),           INTENT(IN   ) :: MAPp                !< MAP data
   TYPE(FEAMooring_Data),    INTENT(IN   ) :: FEAM                !< FEAMooring data
   TYPE(MoorDyn_Data),       INTENT(IN   ) :: MD                  !< MoorDyn data
   TYPE(OrcaFlex_Data),      INTENT(IN   ) :: Orca                !< OrcaFlex interface data
   TYPE(IceFloe_Data),       INTENT(IN   ) :: IceF                !< IceFloe data
   TYPE(IceDyn_Data),        INTENT(IN   ) :: IceD                !< All the IceDyn data used in time-step loop

   TYPE(FAST_ModuleMapType), INTENT(IN   ) :: MeshMapData         !< Data for mapping between modules
   INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None


   CHARACTER(*), PARAMETER                 :: RoutineName = 'WriteOutputToFile'

   ErrStat = ErrID_None
   ErrMsg  = ""

      ! Write time-series channel data

  !y_FAST%WriteThisStep = NeedWriteOutput(n_t_global, t_global, p_FAST)
   IF ( y_FAST%WriteThisStep )  THEN

         ! Generate glue-code output file
         CALL WrOutputLine( t_global, p_FAST, y_FAST, IfW%y%WriteOutput, OpFM%y%WriteOutput, ED%y%WriteOutput, &
               AD%y, SrvD%y%WriteOutput, HD%y%WriteOutput, SD%y%WriteOutput, ExtPtfm%y%WriteOutput, MAPp%y%WriteOutput, &
               FEAM%y%WriteOutput, MD%y%WriteOutput, Orca%y%WriteOutput, IceF%y%WriteOutput, IceD%y, BD%y, ErrStat, ErrMsg )

   ENDIF

      ! Write visualization data (and also note that we're ignoring any errors that occur doing so)
   IF ( p_FAST%WrVTK == VTK_Animate ) THEN
      IF ( MOD( n_t_global, p_FAST%n_VTKTime ) == 0 ) THEN
         call WriteVTK(t_global, p_FAST, y_FAST, MeshMapData, ED, BD, AD, IfW, OpFM, HD, SD, ExtPtfm, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD)
      END IF
   END IF


END SUBROUTINE WriteOutputToFile
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine writes the module output to the primary output file(s).
SUBROUTINE WrOutputLine( t, p_FAST, y_FAST, IfWOutput, OpFMOutput, EDOutput, y_AD, SrvDOutput, HDOutput, SDOutput, ExtPtfmOutput,&
                        MAPOutput, FEAMOutput, MDOutput, OrcaOutput, IceFOutput, y_IceD, y_BD, ErrStat, ErrMsg)

   IMPLICIT                        NONE

      ! Passed variables
   REAL(DbKi), INTENT(IN)                  :: t                                  !< Current simulation time, in seconds
   TYPE(FAST_ParameterType), INTENT(IN)    :: p_FAST                             !< Glue-code simulation parameters
   TYPE(FAST_OutputFileType),INTENT(INOUT) :: y_FAST                             !< Glue-code simulation outputs


   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: IfWOutput (:)                      !< InflowWind WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: OpFMOutput (:)                     !< OpenFOAM WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: EDOutput (:)                       !< ElastoDyn WriteOutput values
   TYPE(AD_OutputType),      INTENT(IN)    :: y_AD                               !< AeroDyn outputs (WriteOutput values are subset of allocated Rotors)
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: SrvDOutput (:)                     !< ServoDyn WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: HDOutput (:)                       !< HydroDyn WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: SDOutput (:)                       !< SubDyn WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: ExtPtfmOutput (:)                  !< ExtPtfm_MCKF WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: MAPOutput (:)                      !< MAP WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: FEAMOutput (:)                     !< FEAMooring WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: MDOutput (:)                       !< MoorDyn WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: OrcaOutput (:)                     !< OrcaFlex interface WriteOutput values
   REAL(ReKi), ALLOCATABLE,  INTENT(IN)    :: IceFOutput (:)                     !< IceFloe WriteOutput values
   TYPE(IceD_OutputType),    INTENT(IN)    :: y_IceD (:)                         !< IceDyn outputs (WriteOutput values are subset)
   TYPE(BD_OutputType),      INTENT(IN)    :: y_BD (:)                           !< BeamDyn outputs (WriteOutput values are subset)

   INTEGER(IntKi),           INTENT(OUT)   :: ErrStat                            !< Error status
   CHARACTER(*),             INTENT(OUT)   :: ErrMsg                             !< Error message

      ! Local variables.

   CHARACTER(200)                   :: Frmt                                      ! A string to hold a format specifier
   CHARACTER(p_FAST%TChanLen)       :: TmpStr                                    ! temporary string to print the time output as text

   REAL(ReKi)                       :: OutputAry(SIZE(y_FAST%ChannelNames)-1)

   ErrStat = ErrID_None
   ErrMsg  = ''
   
   CALL FillOutputAry(p_FAST, y_FAST, IfWOutput, OpFMOutput, EDOutput, y_AD, SrvDOutput, HDOutput, SDOutput, ExtPtfmOutput, &
                      MAPOutput, FEAMOutput, MDOutput, OrcaOutput, IceFOutput, y_IceD, y_BD, OutputAry)   

   IF (p_FAST%WrTxtOutFile) THEN

         ! Write one line of tabular output:
   !   Frmt = '(F8.3,'//TRIM(Num2LStr(p%NumOuts))//'(:,A,'//TRIM( p%OutFmt )//'))'
      Frmt = '"'//p_FAST%Delim//'"'//p_FAST%OutFmt      ! format for array elements from individual modules

            ! time
      WRITE( TmpStr, '('//trim(p_FAST%OutFmt_t)//')' ) t
      CALL WrFileNR( y_FAST%UnOu, TmpStr )

         ! write the individual module output (convert to SiKi if necessary, so that we don't need to print so many digits in the exponent)
      CALL WrNumAryFileNR ( y_FAST%UnOu, REAL(OutputAry,SiKi), Frmt, ErrStat, ErrMsg )
         !IF ( ErrStat >= AbortErrLev ) RETURN

         ! write a new line (advance to the next line)
      WRITE (y_FAST%UnOu,'()')

   END IF


   IF (p_FAST%WrBinOutFile) THEN

         ! Write data to array for binary output file

      IF ( y_FAST%n_Out == y_FAST%NOutSteps ) THEN
         ErrStat = ErrID_Warn
         ErrMsg = 'Not all data could be written to the binary output file.'
         !CALL ProgWarn( 'Not all data could be written to the binary output file.' )
         !this really would only happen if we have an error somewhere else, right?
         !otherwise, we could allocate a new, larger array and move existing data
      ELSE
         y_FAST%n_Out = y_FAST%n_Out + 1

            ! store time data
         IF ( y_FAST%n_Out == 1_IntKi .OR. p_FAST%WrBinMod == FileFmtID_WithTime ) THEN
            y_FAST%TimeData(y_FAST%n_Out) = t   ! Time associated with these outputs
         END IF

            ! store individual module data
         y_FAST%AllOutData(:, y_FAST%n_Out) = OutputAry

      END IF

   END IF

   RETURN
END SUBROUTINE WrOutputLine
!----------------------------------------------------------------------------------------------------------------------------------
!> This subroutine is called at program termination. It writes any additional output files,
!! deallocates variables for FAST file I/O and closes files.
SUBROUTINE FAST_EndOutput( p_FAST, y_FAST, m_FAST, ErrStat, ErrMsg )

   TYPE(FAST_ParameterType), INTENT(INOUT) :: p_FAST                    !< FAST Parameters
   TYPE(FAST_OutputFileType),INTENT(INOUT) :: y_FAST                    !< FAST Output
   TYPE(FAST_MiscVarType),   INTENT(IN   ) :: m_FAST                    !< Miscellaneous variables (only for the final time)

   INTEGER(IntKi),           INTENT(OUT)   :: ErrStat                   !< Error status
   CHARACTER(*),             INTENT(OUT)   :: ErrMsg                    !< Message associated with errro status

      ! local variables
   CHARACTER(LEN(y_FAST%FileDescLines)*3)  :: FileDesc                  ! The description of the run, to be written in the binary output file


      ! Initialize some values

   ErrStat = ErrID_None
   ErrMsg  = ''

   !-------------------------------------------------------------------------------------------------
   ! Write the binary output file if requested
   !-------------------------------------------------------------------------------------------------

   IF (p_FAST%WrBinOutFile .AND. y_FAST%n_Out > 0) THEN

      FileDesc = TRIM(y_FAST%FileDescLines(1))//' '//TRIM(y_FAST%FileDescLines(2))//'; '//TRIM(y_FAST%FileDescLines(3))

      CALL WrBinFAST(TRIM(p_FAST%OutFileRoot)//'.outb', Int(p_FAST%WrBinMod, B2Ki), TRIM(FileDesc), &
            y_FAST%ChannelNames, y_FAST%ChannelUnits, y_FAST%TimeData, y_FAST%AllOutData(:,1:y_FAST%n_Out), ErrStat, ErrMsg)

      IF ( ErrStat /= ErrID_None ) CALL WrScr( TRIM(GetErrStr(ErrStat))//' when writing binary output file: '//TRIM(ErrMsg) )

   END IF


   !-------------------------------------------------------------------------------------------------
   ! Close the text tabular output file and summary file (if opened)
   !-------------------------------------------------------------------------------------------------
   IF (y_FAST%UnOu  > 0) THEN ! I/O unit number for the tabular output file
      CLOSE( y_FAST%UnOu )
      y_FAST%UnOu = -1
   END IF

   IF (y_FAST%UnSum > 0) THEN ! I/O unit number for the tabular output file
      CLOSE( y_FAST%UnSum )
      y_FAST%UnSum = -1
   END IF

   IF (y_FAST%UnGra > 0) THEN ! I/O unit number for the graphics output file
      CLOSE( y_FAST%UnGra )
      y_FAST%UnGra = -1
   END IF

   !-------------------------------------------------------------------------------------------------
   ! Deallocate arrays
   !-------------------------------------------------------------------------------------------------

      ! Output
   IF ( ALLOCATED(y_FAST%AllOutData                  ) ) DEALLOCATE(y_FAST%AllOutData                  )
   IF ( ALLOCATED(y_FAST%TimeData                    ) ) DEALLOCATE(y_FAST%TimeData                    )
   IF ( ALLOCATED(y_FAST%ChannelNames                ) ) DEALLOCATE(y_FAST%ChannelNames                )
   IF ( ALLOCATED(y_FAST%ChannelUnits                ) ) DEALLOCATE(y_FAST%ChannelUnits                )


END SUBROUTINE FAST_EndOutput
END MODULE