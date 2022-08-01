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
MODULE FAST_Initialization

   USE FAST_ModTypes
   USE FAST_IO,           ONLY: GetInputFileName, FAST_ReadPrimaryFile, FAST_WrSum, ValidateInputData
   USE FAST_Linear,       ONLY: Init_Lin
   USE FAST_VTK,          ONLY: SetVTKParameters_B4HD, SetVTKParameters
   USE AeroDyn,           ONLY: AD_Init
   USE AeroDyn14,         ONLY: AD14_Init
   USE InflowWind,        ONLY: InflowWind_Init
   USE ElastoDyn,         ONLY: ED_Init
   USE BeamDyn,           ONLY: BD_Init
   USE FEAMooring,        ONLY: FEAM_Init
   USE MoorDyn,           ONLY: MD_Init
   USE MAP,               ONLY: MAP_Init
   USE OrcaFlexInterface, ONLY: Orca_Init
   USE HydroDyn,          ONLY: HydroDyn_Init
   USE IceDyn,            ONLY: IceD_Init
   USE IceFloe,           ONLY: IceFloe_Init
   USE ServoDyn,          ONLY: SrvD_Init, Cmpl4SFun, Cmpl4LV, TrimCase_none
   USE SubDyn,            ONLY: SD_Init
   USE OpenFOAM,          ONLY: Init_OpFM
   USE SC_DataEx,         ONLY: SC_DX_Init
   Use ExtPtfm_MCKF,      ONLY: ExtPtfm_Init
   USE VersionInfo,       ONLY: GetVersion, DispCompileRuntimeInfo

   IMPLICIT NONE

CONTAINS
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! INITIALIZATION ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!> a wrapper routine to call FAST_Initialize a the full-turbine simulation level (makes easier to write top-level driver)
SUBROUTINE FAST_InitializeAll_T( t_initial, TurbID, Turbine, ErrStat, ErrMsg, InFile, ExternInitData )

   REAL(DbKi),                        INTENT(IN   ) :: t_initial      !< initial time
   INTEGER(IntKi),                    INTENT(IN   ) :: TurbID         !< turbine Identifier (1-NumTurbines)
   TYPE(FAST_TurbineType),            INTENT(INOUT) :: Turbine        !< all data for one instance of a turbine
   INTEGER(IntKi),                    INTENT(  OUT) :: ErrStat        !< Error status of the operation
   CHARACTER(*),                      INTENT(  OUT) :: ErrMsg         !< Error message if ErrStat /= ErrID_None
   CHARACTER(*),             OPTIONAL,INTENT(IN   ) :: InFile         !< A CHARACTER string containing the name of the primary FAST input file (if not present, we'll get it from the command line)
   TYPE(FAST_ExternInitType),OPTIONAL,INTENT(IN   ) :: ExternInitData !< Initialization input data from an external source (Simulink)

   Turbine%TurbID = TurbID


   IF (PRESENT(InFile)) THEN
      IF (PRESENT(ExternInitData)) THEN
         CALL FAST_InitializeAll( t_initial, Turbine%p_FAST, Turbine%y_FAST, Turbine%m_FAST, &
                     Turbine%ED, Turbine%BD, Turbine%SrvD, Turbine%AD14, Turbine%AD, Turbine%IfW, Turbine%OpFM, Turbine%SC_DX,&
                     Turbine%HD, Turbine%SD, Turbine%ExtPtfm, Turbine%MAP, Turbine%FEAM, Turbine%MD, Turbine%Orca, &
                     Turbine%IceF, Turbine%IceD, Turbine%MeshMapData, ErrStat, ErrMsg, InFile, ExternInitData )
      ELSE
         CALL FAST_InitializeAll( t_initial, Turbine%p_FAST, Turbine%y_FAST, Turbine%m_FAST, &
                     Turbine%ED, Turbine%BD, Turbine%SrvD, Turbine%AD14, Turbine%AD, Turbine%IfW, Turbine%OpFM, Turbine%SC_DX, &
                     Turbine%HD, Turbine%SD, Turbine%ExtPtfm, Turbine%MAP, Turbine%FEAM, Turbine%MD, Turbine%Orca, &
                     Turbine%IceF, Turbine%IceD, Turbine%MeshMapData, ErrStat, ErrMsg, InFile  )
      END IF
   ELSE
      CALL FAST_InitializeAll( t_initial, Turbine%p_FAST, Turbine%y_FAST, Turbine%m_FAST, &
                     Turbine%ED, Turbine%BD, Turbine%SrvD, Turbine%AD14, Turbine%AD, Turbine%IfW, Turbine%OpFM, Turbine%SC_DX, &
                     Turbine%HD, Turbine%SD, Turbine%ExtPtfm, Turbine%MAP, Turbine%FEAM, Turbine%MD, Turbine%Orca, &
                     Turbine%IceF, Turbine%IceD, Turbine%MeshMapData, ErrStat, ErrMsg )
   END IF


END SUBROUTINE FAST_InitializeAll_T
!----------------------------------------------------------------------------------------------------------------------------------
!> Routine to call Init routine for each module. This routine sets all of the init input data for each module.
SUBROUTINE FAST_InitializeAll( t_initial, p_FAST, y_FAST, m_FAST, ED, BD, SrvD, AD14, AD, IfW, OpFM, SC_DX, HD, SD, ExtPtfm, &
                               MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, ErrStat, ErrMsg, InFile, ExternInitData )

   use ElastoDyn_Parameters, only: Method_RK4

   REAL(DbKi),               INTENT(IN   ) :: t_initial           !< initial time
   TYPE(FAST_ParameterType), INTENT(INOUT) :: p_FAST              !< Parameters for the glue code
   TYPE(FAST_OutputFileType),INTENT(INOUT) :: y_FAST              !< Output variables for the glue code
   TYPE(FAST_MiscVarType),   INTENT(INOUT) :: m_FAST              !< Miscellaneous variables

   TYPE(ElastoDyn_Data),     INTENT(INOUT) :: ED                  !< ElastoDyn data
   TYPE(BeamDyn_Data),       INTENT(INOUT) :: BD                  !< BeamDyn data
   TYPE(ServoDyn_Data),      INTENT(INOUT) :: SrvD                !< ServoDyn data
   TYPE(AeroDyn14_Data),     INTENT(INOUT) :: AD14                !< AeroDyn14 data
   TYPE(AeroDyn_Data),       INTENT(INOUT) :: AD                  !< AeroDyn data
   TYPE(InflowWind_Data),    INTENT(INOUT) :: IfW                 !< InflowWind data
   TYPE(OpenFOAM_Data),      INTENT(INOUT) :: OpFM                !< OpenFOAM data
   TYPE(SCDataEx_Data),      INTENT(INOUT) :: SC_DX               !< SuperController exchange data
   TYPE(HydroDyn_Data),      INTENT(INOUT) :: HD                  !< HydroDyn data
   TYPE(SubDyn_Data),        INTENT(INOUT) :: SD                  !< SubDyn data
   TYPE(ExtPtfm_Data),       INTENT(INOUT) :: ExtPtfm             !< ExtPtfm_MCKF data
   TYPE(MAP_Data),           INTENT(INOUT) :: MAPp                !< MAP data
   TYPE(FEAMooring_Data),    INTENT(INOUT) :: FEAM                !< FEAMooring data
   TYPE(MoorDyn_Data),       INTENT(INOUT) :: MD                  !< Data for the MoorDyn module
   TYPE(OrcaFlex_Data),      INTENT(INOUT) :: Orca                !< OrcaFlex interface data

   TYPE(IceFloe_Data),       INTENT(INOUT) :: IceF                !< IceFloe data
   TYPE(IceDyn_Data),        INTENT(INOUT) :: IceD                !< All the IceDyn data used in time-step loop

   TYPE(FAST_ModuleMapType), INTENT(INOUT) :: MeshMapData         !< Data for mapping between modules

   INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None
   CHARACTER(*), OPTIONAL,   INTENT(IN   ) :: InFile              !< A CHARACTER string containing the name of the primary FAST input file (if not present, we'll get it from the command line)

   TYPE(FAST_ExternInitType), OPTIONAL, INTENT(IN) :: ExternInitData !< Initialization input data from an external source (Simulink)

   ! local variables
   CHARACTER(1024)                         :: InputFile           !< A CHARACTER string containing the name of the primary FAST input file
   TYPE(FAST_InitData)                     :: Init                !< Initialization data for all modules


   REAL(ReKi)                              :: AirDens             ! air density for initialization/normalization of OpenFOAM data
   REAL(DbKi)                              :: dt_IceD             ! tmp dt variable to ensure IceDyn doesn't specify different dt values for different legs (IceDyn instances)
   REAL(DbKi)                              :: dt_BD               ! tmp dt variable to ensure BeamDyn doesn't specify different dt values for different instances
   INTEGER(IntKi)                          :: ErrStat2
   INTEGER(IntKi)                          :: IceDim              ! dimension we're pre-allocating for number of IceDyn legs/instances
   INTEGER(IntKi)                          :: I                   ! generic loop counter
   INTEGER(IntKi)                          :: k                   ! blade loop counter
   logical                                 :: CallStart
   
   
   INTEGER(IntKi)                          :: NumBl
   
   CHARACTER(ErrMsgLen)                    :: ErrMsg2

   CHARACTER(*), PARAMETER                 :: RoutineName = 'FAST_InitializeAll'


   !..........
   ErrStat = ErrID_None
   ErrMsg  = ""

   y_FAST%UnSum = -1                                                    ! set the summary file unit to -1 to indicate it's not open
   y_FAST%UnOu  = -1                                                    ! set the text output file unit to -1 to indicate it's not open
   y_FAST%UnGra = -1                                                    ! set the binary graphics output file unit to -1 to indicate it's not open

   p_FAST%WrVTK = VTK_Unknown                                           ! set this so that we can potentially output VTK information on initialization error
   p_FAST%VTK_tWidth = 1                                                ! initialize in case of error before reading the full file
   p_FAST%n_VTKTime  = 1                                                ! initialize in case of error before reading the full file
   y_FAST%VTK_LastWaveIndx = 1                                          ! Start looking for wave data at the first index
   y_FAST%VTK_count = 0                                                 ! first VTK file has 0 as output
   y_FAST%n_Out = 0                                                     ! set the number of ouptut channels to 0 to indicate there's nothing to write to the binary file
   p_FAST%ModuleInitialized = .FALSE.                                   ! (array initialization) no modules are initialized
   
      ! Get the current time
   CALL DATE_AND_TIME ( Values=m_FAST%StrtTime )                        ! Let's time the whole simulation
   CALL CPU_TIME ( m_FAST%UsrTime1 )                                    ! Initial time (this zeros the start time when used as a MATLAB function)
   m_FAST%UsrTime1 = MAX( 0.0_ReKi, m_FAST%UsrTime1 )                   ! CPU_TIME: If a meaningful time cannot be returned, a processor-dependent negative value is returned


   m_FAST%t_global        = t_initial - 20.                             ! initialize this to a number < t_initial for error message in ProgAbort
   m_FAST%calcJacobian    = .TRUE.                                      ! we need to calculate the Jacobian
   m_FAST%NextJacCalcTime = m_FAST%t_global                             ! We want to calculate the Jacobian on the first step
   p_FAST%TDesc           = ''
!   p_FAST%CheckHSSBrTrqC = .false.

   y_FAST%Lin%WindSpeed = 0.0_ReKi

   if (present(ExternInitData)) then
      CallStart = .not. ExternInitData%FarmIntegration ! .and. ExternInitData%TurbineID == 1
      if (ExternInitData%TurbineID > 0) p_FAST%TDesc = 'T'//trim(num2lstr(ExternInitData%TurbineID))
   else
      CallStart = .true.
   end if


      ! Init NWTC_Library, display copyright and version information:
   if (CallStart) then
      AbortErrLev = ErrID_Fatal                                 ! Until we read otherwise from the FAST input file, we abort only on FATAL errors
      CALL FAST_ProgStart( FAST_Ver )
      p_FAST%WrSttsTime = .TRUE.
   else
      ! if we don't call the start data (e.g., from FAST.Farm), we won't override AbortErrLev either
      CALL DispNVD( FAST_Ver )
      p_FAST%WrSttsTime = .FALSE.
   end if

   IF (PRESENT(InFile)) THEN
      p_FAST%UseDWM = .FALSE.
      InputFile = InFile
   ELSE
      CALL GetInputFileName(InputFile,p_FAST%UseDWM,ErrStat2,ErrMsg2)
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         IF (ErrStat >= AbortErrLev) THEN
            CALL Cleanup()
            RETURN
         END IF
   END IF

   ! ... Open and read input files ...
   ! also, set turbine reference position for graphics output
   p_FAST%UseSC = .FALSE.
   if (PRESENT(ExternInitData)) then
      p_FAST%TurbinePos = ExternInitData%TurbinePos
      if( (ExternInitData%NumSC2CtrlGlob .gt. 0) .or. (ExternInitData%NumSC2Ctrl .gt. 0) .or. (ExternInitData%NumCtrl2SC .gt. 0)) then
         p_FAST%UseSC = .TRUE.
      end if

      if (ExternInitData%FarmIntegration) then ! we're integrating with FAST.Farm
         CALL FAST_Init( p_FAST, m_FAST, y_FAST, t_initial, InputFile, ErrStat2, ErrMsg2, ExternInitData%TMax, OverrideAbortLev=.false., RootName=ExternInitData%RootName )
      else
         CALL FAST_Init( p_FAST, m_FAST, y_FAST, t_initial, InputFile, ErrStat2, ErrMsg2, ExternInitData%TMax, ExternInitData%TurbineID )  ! We have the name of the input file and the simulation length from somewhere else (e.g. Simulink)
      end if

   else
      p_FAST%TurbinePos = 0.0_ReKi
      CALL FAST_Init( p_FAST, m_FAST, y_FAST, t_initial, InputFile, ErrStat2, ErrMsg2 )                       ! We have the name of the input file from somewhere else (e.g. Simulink)
   end if

   CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF


   !...............................................................................................................................

   p_FAST%dt_module = p_FAST%dt ! initialize time steps for each module

   ! ........................
   ! initialize ElastoDyn (must be done first)
   ! ........................

   ALLOCATE( ED%Input( p_FAST%InterpOrder+1 ), ED%InputTimes( p_FAST%InterpOrder+1 ),STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating ED%Input and ED%InputTimes.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF

   Init%InData_ED%Linearize = p_FAST%Linearize
   Init%InData_ED%InputFile = p_FAST%EDFile
   IF ( p_FAST%CompAero == Module_AD14 ) THEN
      Init%InData_ED%ADInputFile = p_FAST%AeroFile
   ELSE
      Init%InData_ED%ADInputFile = ""
   END IF

   Init%InData_ED%RootName      = TRIM(p_FAST%OutFileRoot)//'.'//TRIM(y_FAST%Module_Abrev(Module_ED))
   Init%InData_ED%CompElast     = p_FAST%CompElast == Module_ED

   Init%InData_ED%Gravity       = p_FAST%Gravity

   CALL ED_Init( Init%InData_ED, ED%Input(1), ED%p, ED%x(STATE_CURR), ED%xd(STATE_CURR), ED%z(STATE_CURR), ED%OtherSt(STATE_CURR), &
                  ED%y, ED%m, p_FAST%dt_module( MODULE_ED ), Init%OutData_ED, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

   p_FAST%ModuleInitialized(Module_ED) = .TRUE.
   CALL SetModuleSubstepTime(Module_ED, p_FAST, y_FAST, ErrStat2, ErrMsg2)
      CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      ! bjj: added this check per jmj; perhaps it would be better in ElastoDyn, but I'll leave it here for now:
   IF ( p_FAST%TurbineType == Type_Offshore_Floating ) THEN
      IF ( ED%p%TowerBsHt < 0.0_ReKi .AND. .NOT. EqualRealNos( ED%p%TowerBsHt, 0.0_ReKi ) ) THEN
         CALL SetErrStat(ErrID_Fatal,"ElastoDyn TowerBsHt must not be negative for floating offshore systems.",ErrStat,ErrMsg,RoutineName)
      END IF
   END IF

   allocate( y_FAST%Lin%Modules(MODULE_ED)%Instance(1), stat=ErrStat2)
   if (ErrStat2 /= 0 ) then
      call SetErrStat(ErrID_Fatal, "Error allocating Lin%Modules(ED).", ErrStat, ErrMsg, RoutineName )
   else

      if (allocated(Init%OutData_ED%LinNames_y)) call move_alloc(Init%OutData_ED%LinNames_y,y_FAST%Lin%Modules(MODULE_ED)%Instance(1)%Names_y)
      if (allocated(Init%OutData_ED%LinNames_x)) call move_alloc(Init%OutData_ED%LinNames_x,y_FAST%Lin%Modules(MODULE_ED)%Instance(1)%Names_x)
      if (allocated(Init%OutData_ED%LinNames_u)) call move_alloc(Init%OutData_ED%LinNames_u,y_FAST%Lin%Modules(MODULE_ED)%Instance(1)%Names_u)
      if (allocated(Init%OutData_ED%RotFrame_y)) call move_alloc(Init%OutData_ED%RotFrame_y,y_FAST%Lin%Modules(MODULE_ED)%Instance(1)%RotFrame_y)
      if (allocated(Init%OutData_ED%RotFrame_x)) call move_alloc(Init%OutData_ED%RotFrame_x,y_FAST%Lin%Modules(MODULE_ED)%Instance(1)%RotFrame_x)
      if (allocated(Init%OutData_ED%DerivOrder_x)) call move_alloc(Init%OutData_ED%DerivOrder_x,y_FAST%Lin%Modules(MODULE_ED)%Instance(1)%DerivOrder_x)
      if (allocated(Init%OutData_ED%RotFrame_u)) call move_alloc(Init%OutData_ED%RotFrame_u,y_FAST%Lin%Modules(MODULE_ED)%Instance(1)%RotFrame_u)
      if (allocated(Init%OutData_ED%IsLoad_u  )) call move_alloc(Init%OutData_ED%IsLoad_u  ,y_FAST%Lin%Modules(MODULE_ED)%Instance(1)%IsLoad_u  )

      if (allocated(Init%OutData_ED%WriteOutputHdr)) y_FAST%Lin%Modules(MODULE_ED)%Instance(1)%NumOutputs = size(Init%OutData_ED%WriteOutputHdr)
   end if

   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF
      
   NumBl = Init%OutData_ED%NumBl
      
   
   if (p_FAST%CalcSteady) then
      if ( EqualRealNos(Init%OutData_ED%RotSpeed, 0.0_ReKi) ) then
         p_FAST%TrimCase = TrimCase_none
         p_FAST%NLinTimes = 1
         p_FAST%LinInterpOrder = 0 ! constant values
      elseif ( Init%OutData_ED%isFixed_GenDOF ) then
         p_FAST%TrimCase = TrimCase_none
      end if
   end if


   ! ........................
   ! initialize BeamDyn
   ! ........................
   IF ( p_FAST%CompElast == Module_BD ) THEN
      p_FAST%nBeams = Init%OutData_ED%NumBl          ! initialize number of BeamDyn instances = number of blades
   ELSE
      p_FAST%nBeams = 0
   END IF

   ALLOCATE( BD%Input( p_FAST%InterpOrder+1, p_FAST%nBeams ), BD%InputTimes( p_FAST%InterpOrder+1, p_FAST%nBeams ), STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating BD%Input and BD%InputTimes.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF

   ALLOCATE( BD%x(           p_FAST%nBeams,2), &
             BD%xd(          p_FAST%nBeams,2), &
             BD%z(           p_FAST%nBeams,2), &
             BD%OtherSt(     p_FAST%nBeams,2), &
             BD%p(           p_FAST%nBeams  ), &
             BD%u(           p_FAST%nBeams  ), &
             BD%y(           p_FAST%nBeams  ), &
             BD%m(           p_FAST%nBeams  ), &
             Init%OutData_BD(p_FAST%nBeams  ), &
                                             STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating BeamDyn state, input, and output data.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF

   IF (p_FAST%CompElast == Module_BD) THEN

      Init%InData_BD%DynamicSolve = .TRUE.       ! FAST can only couple to BeamDyn when dynamic solve is used.

      Init%InData_BD%Linearize = p_FAST%Linearize
      Init%InData_BD%gravity      = (/ 0.0_ReKi, 0.0_ReKi, -p_FAST%Gravity /)       ! "Gravitational acceleration" m/s^2
      
         ! now initialize BeamDyn for all beams
      dt_BD = p_FAST%dt_module( MODULE_BD )

      Init%InData_BD%HubPos = ED%y%HubPtMotion%Position(:,1)
      Init%InData_BD%HubRot = ED%y%HubPtMotion%RefOrientation(:,:,1)

      p_FAST%BD_OutputSibling = .true.

      allocate( y_FAST%Lin%Modules(MODULE_BD)%Instance(p_FAST%nBeams), stat=ErrStat2)
      if (ErrStat2 /= 0 ) then
         call SetErrStat(ErrID_Fatal, "Error allocating Lin%Modules(BD).", ErrStat, ErrMsg, RoutineName )
         CALL Cleanup()
         RETURN
      end if

      DO k=1,p_FAST%nBeams
         Init%InData_BD%RootName     = TRIM(p_FAST%OutFileRoot)//'.'//TRIM(y_FAST%Module_Abrev(Module_BD))//TRIM( Num2LStr(k) )


         Init%InData_BD%InputFile    = p_FAST%BDBldFile(k)

         Init%InData_BD%GlbPos       = ED%y%BladeRootMotion(k)%Position(:,1)          ! {:}    - - "Initial Position Vector of the local blade coordinate system"
         Init%InData_BD%GlbRot       = ED%y%BladeRootMotion(k)%RefOrientation(:,:,1)  ! {:}{:} - - "Initial direction cosine matrix of the local blade coordinate system"

         Init%InData_BD%RootDisp     = ED%y%BladeRootMotion(k)%TranslationDisp(:,1)   ! {:}    - - "Initial root displacement"
         Init%InData_BD%RootOri      = ED%y%BladeRootMotion(k)%Orientation(:,:,1)     ! {:}{:} - - "Initial root orientation"
         Init%InData_BD%RootVel(1:3) = ED%y%BladeRootMotion(k)%TranslationVel(:,1)    ! {:}    - - "Initial root velocities and angular veolcities"
         Init%InData_BD%RootVel(4:6) = ED%y%BladeRootMotion(k)%RotationVel(:,1)       ! {:}    - - "Initial root velocities and angular veolcities"

         CALL BD_Init( Init%InData_BD, BD%Input(1,k), BD%p(k),  BD%x(k,STATE_CURR), BD%xd(k,STATE_CURR), BD%z(k,STATE_CURR), &
                           BD%OtherSt(k,STATE_CURR), BD%y(k),  BD%m(k), dt_BD, Init%OutData_BD(k), ErrStat2, ErrMsg2 )
            CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

         !bjj: we're going to force this to have the same timestep because I don't want to have to deal with n BD modules with n timesteps.
         IF ( k == 1 ) THEN
            p_FAST%dt_module( MODULE_BD ) = dt_BD

            p_FAST%ModuleInitialized(Module_BD) = .TRUE. ! this really should be once per BD instance, but BD doesn't care so I won't go through the effort to track this
            CALL SetModuleSubstepTime(Module_BD, p_FAST, y_FAST, ErrStat2, ErrMsg2)
               CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
         ELSEIF ( .NOT. EqualRealNos( p_FAST%dt_module( MODULE_BD ),dt_BD )) THEN
            CALL SetErrStat(ErrID_Fatal,"All instances of BeamDyn (one per blade) must have the same time step.",ErrStat,ErrMsg,RoutineName)
         END IF

            ! We're going to do fewer computations if the BD input and output meshes that couple to AD are siblings:
         if (BD%p(k)%BldMotionNodeLoc /= BD_MESH_QP) p_FAST%BD_OutputSibling = .false.

         if (ErrStat>=AbortErrLev) exit !exit this loop so we don't get p_FAST%nBeams of the same errors
         
         if (size(y_FAST%Lin%Modules(MODULE_BD)%Instance) >= k) then ! for aero maps, we only use the first instance:
            if (allocated(Init%OutData_BD(k)%LinNames_y)) call move_alloc(Init%OutData_BD(k)%LinNames_y, y_FAST%Lin%Modules(MODULE_BD)%Instance(k)%Names_y )
            if (allocated(Init%OutData_BD(k)%LinNames_x)) call move_alloc(Init%OutData_BD(k)%LinNames_x, y_FAST%Lin%Modules(MODULE_BD)%Instance(k)%Names_x )
            if (allocated(Init%OutData_BD(k)%LinNames_u)) call move_alloc(Init%OutData_BD(k)%LinNames_u, y_FAST%Lin%Modules(MODULE_BD)%Instance(k)%Names_u )
            if (allocated(Init%OutData_BD(k)%RotFrame_y)) call move_alloc(Init%OutData_BD(k)%RotFrame_y, y_FAST%Lin%Modules(MODULE_BD)%Instance(k)%RotFrame_y )
            if (allocated(Init%OutData_BD(k)%RotFrame_x)) call move_alloc(Init%OutData_BD(k)%RotFrame_x, y_FAST%Lin%Modules(MODULE_BD)%Instance(k)%RotFrame_x )
            if (allocated(Init%OutData_BD(k)%RotFrame_u)) call move_alloc(Init%OutData_BD(k)%RotFrame_u, y_FAST%Lin%Modules(MODULE_BD)%Instance(k)%RotFrame_u )
            if (allocated(Init%OutData_BD(k)%IsLoad_u  )) call move_alloc(Init%OutData_BD(k)%IsLoad_u  , y_FAST%Lin%Modules(MODULE_BD)%Instance(k)%IsLoad_u   )
            if (allocated(Init%OutData_BD(k)%DerivOrder_x)) call move_alloc(Init%OutData_BD(k)%DerivOrder_x, y_FAST%Lin%Modules(MODULE_BD)%Instance(k)%DerivOrder_x )
         
            if (allocated(Init%OutData_BD(k)%WriteOutputHdr)) y_FAST%Lin%Modules(MODULE_BD)%Instance(k)%NumOutputs = size(Init%OutData_BD(k)%WriteOutputHdr)
         end if
         
      END DO
      
      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      END IF
      

   END IF


   ! ........................
   ! initialize AeroDyn
   ! ........................
   ALLOCATE( AD14%Input( p_FAST%InterpOrder+1 ), AD14%InputTimes( p_FAST%InterpOrder+1 ), STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating AD14%Input and AD14%InputTimes.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF

   ALLOCATE( AD%Input( p_FAST%InterpOrder+1 ), AD%InputTimes( p_FAST%InterpOrder+1 ), STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating AD%Input and AD%InputTimes.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF


   IF ( p_FAST%CompAero == Module_AD14 ) THEN

      CALL AD_SetInitInput(Init%InData_AD14, Init%OutData_ED, ED%y, p_FAST, ErrStat2, ErrMsg2)            ! set the values in Init%InData_AD14
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      CALL AD14_Init( Init%InData_AD14, AD14%Input(1), AD14%p, AD14%x(STATE_CURR), AD14%xd(STATE_CURR), AD14%z(STATE_CURR), &
                     AD14%OtherSt(STATE_CURR), AD14%y, AD14%m, p_FAST%dt_module( MODULE_AD14 ), Init%OutData_AD14, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      p_FAST%ModuleInitialized(Module_AD14) = .TRUE.
      CALL SetModuleSubstepTime(Module_AD14, p_FAST, y_FAST, ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

         ! bjj: this really shouldn't be in the FAST glue code, but I'm going to put this check here so people don't use an invalid model
         !    and send me emails to debug numerical issues in their results.
      IF ( AD14%p%TwrProps%PJM_Version .AND. p_FAST%TurbineType == Type_Offshore_Floating ) THEN
         CALL SetErrStat(ErrID_Fatal,'AeroDyn v14 tower influence model "NEWTOWER" is invalid for models of floating offshore turbines.',ErrStat,ErrMsg,RoutineName)
      END IF

      AirDens = Init%OutData_AD14%AirDens

      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      END IF

   ELSEIF ( p_FAST%CompAero == Module_AD ) THEN
   
      allocate(Init%InData_AD%rotors(1), stat=ErrStat2) 
      if (ErrStat2 /= 0 ) then
         call SetErrStat( ErrID_Fatal, 'Allocating rotors', errStat, errMsg, RoutineName )
         call Cleanup()
         return
      end if
   
      Init%InData_AD%rotors(1)%NumBlades  = NumBl
      
      
         ! set initialization data for AD
      CALL AllocAry( Init%InData_AD%rotors(1)%BladeRootPosition,      3, Init%InData_AD%rotors(1)%NumBlades, 'Init%InData_AD%BladeRootPosition', errStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
      CALL AllocAry( Init%InData_AD%rotors(1)%BladeRootOrientation,3, 3, Init%InData_AD%rotors(1)%NumBlades, 'Init%InData_AD%BladeRootOrientation', errStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
         IF (ErrStat >= AbortErrLev) THEN
            CALL Cleanup()
            RETURN
         END IF
      Init%InData_AD%Gravity            = p_FAST%Gravity      
      Init%InData_AD%Linearize          = p_FAST%Linearize
      Init%InData_AD%InputFile          = p_FAST%AeroFile
      Init%InData_AD%RootName           = p_FAST%OutFileRoot
      Init%InData_AD%MHK                = p_FAST%MHK
      if ( p_FAST%MHK == 0 ) then
         Init%InData_AD%defFldDens      = p_FAST%AirDens
      elseif ( p_FAST%MHK == 1 ) then
         Init%InData_AD%defFldDens      = p_FAST%WtrDens
      end if
      Init%InData_AD%defKinVisc         = p_FAST%KinVisc
      Init%InData_AD%defSpdSound        = p_FAST%SpdSound
      Init%InData_AD%defPatm            = p_FAST%Patm
      Init%InData_AD%defPvap            = p_FAST%Pvap
      Init%InData_AD%WtrDpth            = p_FAST%WtrDpth
      Init%InData_AD%MSL2SWL            = p_FAST%MSL2SWL
      
      
      Init%InData_AD%rotors(1)%HubPosition        = ED%y%HubPtMotion%Position(:,1)
      Init%InData_AD%rotors(1)%HubOrientation     = ED%y%HubPtMotion%RefOrientation(:,:,1)
      Init%InData_AD%rotors(1)%NacellePosition    = ED%y%NacelleMotion%Position(:,1)
      Init%InData_AD%rotors(1)%NacelleOrientation = ED%y%NacelleMotion%RefOrientation(:,:,1)
      
      do k=1,NumBl
         Init%InData_AD%rotors(1)%BladeRootPosition(:,k)      = ED%y%BladeRootMotion(k)%Position(:,1)
         Init%InData_AD%rotors(1)%BladeRootOrientation(:,:,k) = ED%y%BladeRootMotion(k)%RefOrientation(:,:,1)
      end do
      
      CALL AD_Init( Init%InData_AD, AD%Input(1), AD%p, AD%x(STATE_CURR), AD%xd(STATE_CURR), AD%z(STATE_CURR), &
                    AD%OtherSt(STATE_CURR), AD%y, AD%m, p_FAST%dt_module( MODULE_AD ), Init%OutData_AD, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      p_FAST%ModuleInitialized(Module_AD) = .TRUE.
      CALL SetModuleSubstepTime(Module_AD, p_FAST, y_FAST, ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      allocate( y_FAST%Lin%Modules(MODULE_AD)%Instance(1), stat=ErrStat2)
      if (ErrStat2 /= 0 ) then
         call SetErrStat(ErrID_Fatal, "Error allocating Lin%Modules(AD).", ErrStat, ErrMsg, RoutineName )
      else
         if (allocated(Init%OutData_AD%rotors(1)%LinNames_u  )) call move_alloc(Init%OutData_AD%rotors(1)%LinNames_u  ,y_FAST%Lin%Modules(MODULE_AD)%Instance(1)%Names_u )
         if (allocated(Init%OutData_AD%rotors(1)%LinNames_y  )) call move_alloc(Init%OutData_AD%rotors(1)%LinNames_y  ,y_FAST%Lin%Modules(MODULE_AD)%Instance(1)%Names_y )
         if (allocated(Init%OutData_AD%rotors(1)%LinNames_x  )) call move_alloc(Init%OutData_AD%rotors(1)%LinNames_x  ,y_FAST%Lin%Modules(MODULE_AD)%Instance(1)%Names_x )
         if (allocated(Init%OutData_AD%rotors(1)%RotFrame_u  )) call move_alloc(Init%OutData_AD%rotors(1)%RotFrame_u  ,y_FAST%Lin%Modules(MODULE_AD)%Instance(1)%RotFrame_u )
         if (allocated(Init%OutData_AD%rotors(1)%RotFrame_y  )) call move_alloc(Init%OutData_AD%rotors(1)%RotFrame_y  ,y_FAST%Lin%Modules(MODULE_AD)%Instance(1)%RotFrame_y )
         if (allocated(Init%OutData_AD%rotors(1)%RotFrame_x  )) call move_alloc(Init%OutData_AD%rotors(1)%RotFrame_x  ,y_FAST%Lin%Modules(MODULE_AD)%Instance(1)%RotFrame_x )
         if (allocated(Init%OutData_AD%rotors(1)%IsLoad_u    )) call move_alloc(Init%OutData_AD%rotors(1)%IsLoad_u    ,y_FAST%Lin%Modules(MODULE_AD)%Instance(1)%IsLoad_u   )
         if (allocated(Init%OutData_AD%rotors(1)%DerivOrder_x)) call move_alloc(Init%OutData_AD%rotors(1)%DerivOrder_x,y_FAST%Lin%Modules(MODULE_AD)%Instance(1)%DerivOrder_x )

         if (allocated(Init%OutData_AD%rotors(1)%WriteOutputHdr)) y_FAST%Lin%Modules(MODULE_AD)%Instance(1)%NumOutputs = size(Init%OutData_AD%rotors(1)%WriteOutputHdr)
      end if

      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      END IF       
      
      AirDens = Init%OutData_AD%rotors(1)%AirDens
      
   ELSE
      AirDens = 0.0_ReKi
   END IF ! CompAero


   ! ........................
   ! initialize InflowWind
   ! ........................
   ALLOCATE( IfW%Input( p_FAST%InterpOrder+1 ), IfW%InputTimes( p_FAST%InterpOrder+1 ), STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating IfW%Input and IfW%InputTimes.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF

   IF ( p_FAST%CompInflow == Module_IfW ) THEN

      Init%InData_IfW%Linearize        = p_FAST%Linearize
      Init%InData_IfW%InputFileName    = p_FAST%InflowFile
      Init%InData_IfW%RootName         = TRIM(p_FAST%OutFileRoot)//'.'//TRIM(y_FAST%Module_Abrev(Module_IfW))
      Init%InData_IfW%UseInputFile     = .TRUE.
      Init%InData_IfW%FixedWindFileRootName = .FALSE.

      Init%InData_IfW%NumWindPoints = 0
      IF ( p_FAST%CompServo == Module_SrvD ) Init%InData_IfW%NumWindPoints = Init%InData_IfW%NumWindPoints + 1
      IF ( p_FAST%CompAero  == Module_AD14 ) THEN
         Init%InData_IfW%NumWindPoints = Init%InData_IfW%NumWindPoints + NumBl * AD14%Input(1)%InputMarkers(1)%NNodes + AD14%Input(1)%Twr_InputMarkers%NNodes
      ELSEIF ( p_FAST%CompAero  == Module_AD ) THEN
         ! Blade
         DO k=1,NumBl
            Init%InData_IfW%NumWindPoints = Init%InData_IfW%NumWindPoints + AD%Input(1)%rotors(1)%BladeMotion(k)%NNodes
         END DO
         ! Tower
         Init%InData_IfW%NumWindPoints = Init%InData_IfW%NumWindPoints + AD%Input(1)%rotors(1)%TowerMotion%NNodes
         ! Nacelle
         if (AD%Input(1)%rotors(1)%NacelleMotion%Committed) then
            Init%InData_IfW%NumWindPoints = Init%InData_IfW%NumWindPoints + AD%Input(1)%rotors(1)%NacelleMotion%NNodes ! 1 point
         endif
         ! Wake
         if (allocated(AD%OtherSt(STATE_CURR)%WakeLocationPoints)) then
            Init%InData_IfW%NumWindPoints = Init%InData_IfW%NumWindPoints + size(AD%OtherSt(STATE_CURR)%WakeLocationPoints,DIM=2)
         end if

      END IF

      ! lidar
      Init%InData_IfW%lidar%Tmax                   = p_FAST%TMax
      Init%InData_IfW%lidar%HubPosition            = ED%y%HubPtMotion%Position(:,1)

      IF ( PRESENT(ExternInitData) ) THEN
         Init%InData_IfW%Use4Dext = ExternInitData%FarmIntegration

         if (Init%InData_IfW%Use4Dext) then
            Init%InData_IfW%FDext%n      = ExternInitData%windGrid_n
            Init%InData_IfW%FDext%delta  = ExternInitData%windGrid_delta
            Init%InData_IfW%FDext%pZero  = ExternInitData%windGrid_pZero
         end if

         ! bjj: these lidar inputs should come from an InflowWind input file; I'm hard coding them here for now
         Init%InData_IfW%lidar%SensorType          = ExternInitData%SensorType
         Init%InData_IfW%lidar%LidRadialVel        = ExternInitData%LidRadialVel
         Init%InData_IfW%lidar%RotorApexOffsetPos  = 0.0
         Init%InData_IfW%lidar%NumPulseGate        = 0
      ELSE
         Init%InData_IfW%lidar%SensorType          = SensorType_None
         Init%InData_IfW%Use4Dext                  = .false.
      END IF

      CALL InflowWind_Init( Init%InData_IfW, IfW%Input(1), IfW%p, IfW%x(STATE_CURR), IfW%xd(STATE_CURR), IfW%z(STATE_CURR),  &
                     IfW%OtherSt(STATE_CURR), IfW%y, IfW%m, p_FAST%dt_module( MODULE_IfW ), Init%OutData_IfW, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      p_FAST%ModuleInitialized(Module_IfW) = .TRUE.
      CALL SetModuleSubstepTime(Module_IfW, p_FAST, y_FAST, ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      allocate( y_FAST%Lin%Modules(MODULE_IfW)%Instance(1), stat=ErrStat2)
      if (ErrStat2 /= 0 ) then
         call SetErrStat(ErrID_Fatal, "Error allocating Lin%Modules(IfW).", ErrStat, ErrMsg, RoutineName )
      else
         if (allocated(Init%OutData_IfW%LinNames_y)) call move_alloc(Init%OutData_IfW%LinNames_y,y_FAST%Lin%Modules(MODULE_IfW)%Instance(1)%Names_y )
         if (allocated(Init%OutData_IfW%LinNames_u)) call move_alloc(Init%OutData_IfW%LinNames_u,y_FAST%Lin%Modules(MODULE_IfW)%Instance(1)%Names_u )
         if (allocated(Init%OutData_IfW%RotFrame_y)) call move_alloc(Init%OutData_IfW%RotFrame_y,y_FAST%Lin%Modules(MODULE_IfW)%Instance(1)%RotFrame_y )
         if (allocated(Init%OutData_IfW%RotFrame_u)) call move_alloc(Init%OutData_IfW%RotFrame_u,y_FAST%Lin%Modules(MODULE_IfW)%Instance(1)%RotFrame_u )
         if (allocated(Init%OutData_IfW%IsLoad_u  )) call move_alloc(Init%OutData_IfW%IsLoad_u  ,y_FAST%Lin%Modules(MODULE_IfW)%Instance(1)%IsLoad_u   )

         if (allocated(Init%OutData_IfW%WriteOutputHdr)) y_FAST%Lin%Modules(MODULE_IfW)%Instance(1)%NumOutputs = size(Init%OutData_IfW%WriteOutputHdr)
         y_FAST%Lin%WindSpeed = Init%OutData_IfW%WindFileInfo%MWS
      end if

      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      END IF

   ELSEIF ( p_FAST%CompInflow == Module_OpFM ) THEN

      IF ( PRESENT(ExternInitData) ) THEN
         Init%InData_OpFM%NumActForcePtsBlade = ExternInitData%NumActForcePtsBlade
         Init%InData_OpFM%NumActForcePtsTower = ExternInitData%NumActForcePtsTower
      ELSE
         CALL SetErrStat( ErrID_Fatal, 'OpenFOAM integration can be used only with external input data (not the stand-alone executable).', ErrStat, ErrMsg, RoutineName )
         CALL Cleanup()
         RETURN
      END IF
      Init%InData_OpFM%BladeLength = Init%OutData_ED%BladeLength
      Init%InData_OpFM%TowerHeight = Init%OutData_ED%TowerHeight
      Init%InData_OpFM%TowerBaseHeight = Init%OutData_ED%TowerBaseHeight
      ALLOCATE(Init%InData_OpFM%StructBldRNodes( SIZE(Init%OutData_ED%BldRNodes)),  STAT=ErrStat2)
      Init%InData_OpFM%StructBldRNodes(:) = Init%OutData_ED%BldRNodes(:)
      ALLOCATE(Init%InData_OpFM%StructTwrHNodes( SIZE(Init%OutData_ED%TwrHNodes)),  STAT=ErrStat2)
      Init%InData_OpFM%StructTwrHNodes(:) = Init%OutData_ED%TwrHNodes(:)
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating OpFM%InitInput.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF
         ! set up the data structures for integration with OpenFOAM
      CALL Init_OpFM( Init%InData_OpFM, p_FAST, AirDens, AD14%Input(1), AD%Input(1), Init%OutData_AD, AD%y, ED%y, OpFM, Init%OutData_OpFM, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      END IF

      !bjj: fix me!!! to do
      Init%OutData_IfW%WindFileInfo%MWS = 0.0_ReKi

   ELSE
      Init%OutData_IfW%WindFileInfo%MWS = 0.0_ReKi
   END IF   ! CompInflow

   ! ........................
   ! initialize SuperController
   ! ........................
      IF ( PRESENT(ExternInitData) ) THEN
            ! set up the data structures for integration with supercontroller
         IF ( p_FAST%UseSC ) THEN
            CALL SC_DX_Init( ExternInitData%NumSC2CtrlGlob, ExternInitData%NumSC2Ctrl, ExternInitData%NumCtrl2SC, SC_DX, ErrStat2, ErrMsg2 )
            CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         ELSE
            SC_DX%u%c_obj%toSC_Len       = 0
            SC_DX%u%c_obj%toSC           = C_NULL_PTR
            SC_DX%y%c_obj%fromSC_Len     = 0
            SC_DX%y%c_obj%fromSC         = C_NULL_PTR
            SC_DX%y%c_obj%fromSCglob_Len = 0
            SC_DX%y%c_obj%fromSCglob     = C_NULL_PTR
         END IF
      END IF

      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      END IF

   ! ........................
   ! some checks for AeroDyn14's Dynamic Inflow with Mean Wind Speed from InflowWind:
   ! (DO NOT COPY THIS CODE!)
   ! bjj: AeroDyn14 should not need this rule of thumb; it should check the instantaneous values when the code runs
   ! ........................

   IF ( p_FAST%CompAero == Module_AD14 ) THEN
      IF (AD14%p%DynInfl) THEN
         IF ( Init%OutData_IfW%WindFileInfo%MWS  < 8.0 ) THEN
            CALL SetErrStat(ErrID_Fatal,'AeroDyn v14 "DYNINFL" InfModel is invalid for models with wind speeds less than 8 m/s.',ErrStat,ErrMsg,RoutineName)
            !CALL SetErrStat(ErrID_Info,'Estimated average inflow wind speed is less than 8 m/s. Dynamic Inflow will be turned off.',ErrStat,ErrMess,RoutineName )
         END IF
      END IF
   END IF


   ! ........................
   ! set some VTK parameters required before HydroDyn init (so we can get wave elevations for visualization)
   ! ........................

      ! get wave elevation data for visualization
   if ( p_FAST%WrVTK > VTK_None ) then
      call SetVTKParameters_B4HD(p_FAST, Init%OutData_ED, Init%InData_HD, BD, ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
         IF (ErrStat >= AbortErrLev) THEN
            CALL Cleanup()
            RETURN
         END IF
   end if


   ! ........................
   ! initialize HydroDyn
   ! ........................
   ALLOCATE( HD%Input( p_FAST%InterpOrder+1 ), HD%InputTimes( p_FAST%InterpOrder+1 ), STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating HD%Input and HD%InputTimes.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF

   IF ( p_FAST%CompHydro == Module_HD ) THEN

      Init%InData_HD%Gravity       = p_FAST%Gravity
      Init%InData_HD%defWtrDens    = p_FAST%WtrDens
      Init%InData_HD%defWtrDpth    = p_FAST%WtrDpth
      Init%InData_HD%defMSL2SWL    = p_FAST%MSL2SWL
      Init%InData_HD%UseInputFile  = .TRUE.
      Init%InData_HD%InputFile     = p_FAST%HydroFile
      Init%InData_HD%OutRootName   = p_FAST%OutFileRoot
      Init%InData_HD%TMax          = p_FAST%TMax
      Init%InData_HD%hasIce        = p_FAST%CompIce /= Module_None
      Init%InData_HD%Linearize     = p_FAST%Linearize

         ! if wave field needs an offset, modify these values (added at request of SOWFA developers):
      Init%InData_HD%PtfmLocationX = p_FAST%TurbinePos(1)
      Init%InData_HD%PtfmLocationY = p_FAST%TurbinePos(2)

      CALL HydroDyn_Init( Init%InData_HD, HD%Input(1), HD%p,  HD%x(STATE_CURR), HD%xd(STATE_CURR), HD%z(STATE_CURR), &
                          HD%OtherSt(STATE_CURR), HD%y, HD%m, p_FAST%dt_module( MODULE_HD ), Init%OutData_HD, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      p_FAST%ModuleInitialized(Module_HD) = .TRUE.
      CALL SetModuleSubstepTime(Module_HD, p_FAST, y_FAST, ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      allocate( y_FAST%Lin%Modules(MODULE_HD)%Instance(1), stat=ErrStat2)
      if (ErrStat2 /= 0 ) then
         call SetErrStat(ErrID_Fatal, "Error allocating Lin%Modules(HD).", ErrStat, ErrMsg, RoutineName )
      else
         if (allocated(Init%OutData_HD%LinNames_y)) call move_alloc(Init%OutData_HD%LinNames_y,y_FAST%Lin%Modules(MODULE_HD)%Instance(1)%Names_y )
         if (allocated(Init%OutData_HD%LinNames_u)) call move_alloc(Init%OutData_HD%LinNames_u,y_FAST%Lin%Modules(MODULE_HD)%Instance(1)%Names_u )
         if (allocated(Init%OutData_HD%LinNames_x)) call move_alloc(Init%OutData_HD%LinNames_x, y_FAST%Lin%Modules(MODULE_HD)%Instance(1)%Names_x )
         if (allocated(Init%OutData_HD%DerivOrder_x)) call move_alloc(Init%OutData_HD%DerivOrder_x,y_FAST%Lin%Modules(MODULE_HD)%Instance(1)%DerivOrder_x)
         if (allocated(Init%OutData_HD%IsLoad_u  )) call move_alloc(Init%OutData_HD%IsLoad_u  ,y_FAST%Lin%Modules(MODULE_HD)%Instance(1)%IsLoad_u   )

         if (allocated(Init%OutData_HD%WriteOutputHdr)) y_FAST%Lin%Modules(MODULE_HD)%Instance(1)%NumOutputs = size(Init%OutData_HD%WriteOutputHdr)
      end if

      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      END IF
   END IF   ! CompHydro

   ! ........................
   ! initialize SubDyn or ExtPtfm_MCKF
   ! ........................
   ALLOCATE( SD%Input( p_FAST%InterpOrder+1 ), SD%InputTimes( p_FAST%InterpOrder+1 ), STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating SD%Input and SD%InputTimes.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF

   ALLOCATE( ExtPtfm%Input( p_FAST%InterpOrder+1 ), ExtPtfm%InputTimes( p_FAST%InterpOrder+1 ), STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating ExtPtfm%Input and ExtPtfm%InputTimes.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF

   IF ( p_FAST%CompSub == Module_SD ) THEN

      IF ( p_FAST%CompHydro == Module_HD ) THEN
         Init%InData_SD%WtrDpth = Init%OutData_HD%WtrDpth
      ELSE
         Init%InData_SD%WtrDpth = 0.0_ReKi
      END IF
            
      Init%InData_SD%Linearize     = p_FAST%Linearize
      Init%InData_SD%g             = p_FAST%Gravity     
      !Ini%tInData_SD%UseInputFile = .TRUE. 
      Init%InData_SD%SDInputFile   = p_FAST%SubFile
      Init%InData_SD%RootName      = p_FAST%OutFileRoot
      Init%InData_SD%TP_RefPoint   = ED%y%PlatformPtMesh%Position(:,1)  ! "Interface point" where loads will be transferred to
      Init%InData_SD%SubRotateZ    = 0.0                                        ! Used by driver to rotate structure around z
      
            
      CALL SD_Init( Init%InData_SD, SD%Input(1), SD%p,  SD%x(STATE_CURR), SD%xd(STATE_CURR), SD%z(STATE_CURR),  &
                    SD%OtherSt(STATE_CURR), SD%y, SD%m, p_FAST%dt_module( MODULE_SD ), Init%OutData_SD, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      p_FAST%ModuleInitialized(Module_SD) = .TRUE.
      CALL SetModuleSubstepTime(Module_SD, p_FAST, y_FAST, ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      allocate( y_FAST%Lin%Modules(MODULE_SD)%Instance(1), stat=ErrStat2)
      if (ErrStat2 /= 0 ) then
         call SetErrStat(ErrID_Fatal, "Error allocating Lin%Modules(SD).", ErrStat, ErrMsg, RoutineName )
      else
         if (allocated(Init%OutData_SD%LinNames_y)) call move_alloc(Init%OutData_SD%LinNames_y,y_FAST%Lin%Modules(MODULE_SD)%Instance(1)%Names_y)
         if (allocated(Init%OutData_SD%LinNames_x)) call move_alloc(Init%OutData_SD%LinNames_x,y_FAST%Lin%Modules(MODULE_SD)%Instance(1)%Names_x)
         if (allocated(Init%OutData_SD%LinNames_u)) call move_alloc(Init%OutData_SD%LinNames_u,y_FAST%Lin%Modules(MODULE_SD)%Instance(1)%Names_u)
         if (allocated(Init%OutData_SD%RotFrame_y)) call move_alloc(Init%OutData_SD%RotFrame_y,y_FAST%Lin%Modules(MODULE_SD)%Instance(1)%RotFrame_y)
         if (allocated(Init%OutData_SD%RotFrame_x)) call move_alloc(Init%OutData_SD%RotFrame_x,y_FAST%Lin%Modules(MODULE_SD)%Instance(1)%RotFrame_x)
         if (allocated(Init%OutData_SD%RotFrame_u)) call move_alloc(Init%OutData_SD%RotFrame_u,y_FAST%Lin%Modules(MODULE_SD)%Instance(1)%RotFrame_u)
         if (allocated(Init%OutData_SD%IsLoad_u  )) call move_alloc(Init%OutData_SD%IsLoad_u  ,y_FAST%Lin%Modules(MODULE_SD)%Instance(1)%IsLoad_u  )
         if (allocated(Init%OutData_SD%WriteOutputHdr)) y_FAST%Lin%Modules(MODULE_SD)%Instance(1)%NumOutputs = size(Init%OutData_SD%WriteOutputHdr)
         if (allocated(Init%OutData_SD%DerivOrder_x)) call move_alloc(Init%OutData_SD%DerivOrder_x,y_FAST%Lin%Modules(MODULE_SD)%Instance(1)%DerivOrder_x)
      end if
               
      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      END IF
   ELSE IF ( p_FAST%CompSub == Module_ExtPtfm ) THEN

      Init%InData_ExtPtfm%InputFile = p_FAST%SubFile
      Init%InData_ExtPtfm%RootName  = trim(p_FAST%OutFileRoot)//'.'//TRIM(y_FAST%Module_Abrev(Module_ExtPtfm))
      Init%InData_ExtPtfm%Linearize = p_FAST%Linearize
      Init%InData_ExtPtfm%PtfmRefzt = ED%p%PtfmRefzt ! Required

      CALL ExtPtfm_Init( Init%InData_ExtPtfm, ExtPtfm%Input(1), ExtPtfm%p,  &
                         ExtPtfm%x(STATE_CURR), ExtPtfm%xd(STATE_CURR), ExtPtfm%z(STATE_CURR),  ExtPtfm%OtherSt(STATE_CURR), &
                         ExtPtfm%y, ExtPtfm%m, p_FAST%dt_module( MODULE_ExtPtfm ), Init%OutData_ExtPtfm, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      p_FAST%ModuleInitialized(MODULE_ExtPtfm) = .TRUE.
      CALL SetModuleSubstepTime(MODULE_ExtPtfm, p_FAST, y_FAST, ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      allocate( y_FAST%Lin%Modules(MODULE_ExtPtfm)%Instance(1), stat=ErrStat2)
      if (ErrStat2 /= 0 ) then
         call SetErrStat(ErrID_Fatal, "Error allocating Lin%Modules(ExtPtfm).", ErrStat, ErrMsg, RoutineName )
      else
         if (allocated(Init%OutData_ExtPtfm%LinNames_y)) call move_alloc(Init%OutData_ExtPtfm%LinNames_y,y_FAST%Lin%Modules(MODULE_ExtPtfm)%Instance(1)%Names_y)
         if (allocated(Init%OutData_ExtPtfm%LinNames_x)) call move_alloc(Init%OutData_ExtPtfm%LinNames_x,y_FAST%Lin%Modules(MODULE_ExtPtfm)%Instance(1)%Names_x)
         if (allocated(Init%OutData_ExtPtfm%LinNames_u)) call move_alloc(Init%OutData_ExtPtfm%LinNames_u,y_FAST%Lin%Modules(MODULE_ExtPtfm)%Instance(1)%Names_u)
         if (allocated(Init%OutData_ExtPtfm%RotFrame_y)) call move_alloc(Init%OutData_ExtPtfm%RotFrame_y,y_FAST%Lin%Modules(MODULE_ExtPtfm)%Instance(1)%RotFrame_y)
         if (allocated(Init%OutData_ExtPtfm%RotFrame_x)) call move_alloc(Init%OutData_ExtPtfm%RotFrame_x,y_FAST%Lin%Modules(MODULE_ExtPtfm)%Instance(1)%RotFrame_x)
         if (allocated(Init%OutData_ExtPtfm%RotFrame_u)) call move_alloc(Init%OutData_ExtPtfm%RotFrame_u,y_FAST%Lin%Modules(MODULE_ExtPtfm)%Instance(1)%RotFrame_u)
         if (allocated(Init%OutData_ExtPtfm%IsLoad_u  )) call move_alloc(Init%OutData_ExtPtfm%IsLoad_u  ,y_FAST%Lin%Modules(MODULE_ExtPtfm)%Instance(1)%IsLoad_u  )
         if (allocated(Init%OutData_ExtPtfm%WriteOutputHdr)) y_FAST%Lin%Modules(MODULE_ExtPtfm)%Instance(1)%NumOutputs = size(Init%OutData_ExtPtfm%WriteOutputHdr)
         if (allocated(Init%OutData_ExtPtfm%DerivOrder_x)) call move_alloc(Init%OutData_ExtPtfm%DerivOrder_x,y_FAST%Lin%Modules(MODULE_ExtPtfm)%Instance(1)%DerivOrder_x)
      end if

      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      END IF

   END IF

   ! ------------------------------
   ! initialize CompMooring modules
   ! ------------------------------
   ALLOCATE( MAPp%Input( p_FAST%InterpOrder+1 ), MAPp%InputTimes( p_FAST%InterpOrder+1 ), STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating MAPp%Input and MAPp%InputTimes.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF
   ALLOCATE( MD%Input( p_FAST%InterpOrder+1 ), MD%InputTimes( p_FAST%InterpOrder+1 ), STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating MD%Input and MD%InputTimes.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF
   ALLOCATE( FEAM%Input( p_FAST%InterpOrder+1 ), FEAM%InputTimes( p_FAST%InterpOrder+1 ), STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating FEAM%Input and FEAM%InputTimes.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF
   ALLOCATE( Orca%Input( p_FAST%InterpOrder+1 ), Orca%InputTimes( p_FAST%InterpOrder+1 ), STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating Orca%Input and Orca%InputTimes.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF

   ! ........................
   ! initialize MAP
   ! ........................
   IF (p_FAST%CompMooring == Module_MAP) THEN
      !bjj: until we modify this, MAP requires HydroDyn to be used. (perhaps we could send air density from AeroDyn or something...)

      CALL WrScr(NewLine) !bjj: I'm printing two blank lines here because MAP seems to be writing over the last line on the screen.

!      Init%InData_MAP%rootname          =  p_FAST%OutFileRoot        ! Output file name 
      Init%InData_MAP%gravity           =  p_FAST%Gravity    ! This need to be according to g from driver
      Init%InData_MAP%sea_density       =  Init%OutData_HD%WtrDens    ! This needs to be set according to seawater density in HydroDyn
      Init%InData_MAP%depth             =  Init%OutData_HD%WtrDpth    ! This need to be set according to the water depth in HydroDyn

   ! differences for MAP++
      Init%InData_MAP%file_name         =  p_FAST%MooringFile        ! This needs to be set according to what is in the FAST input file.
      Init%InData_MAP%summary_file_name =  TRIM(p_FAST%OutFileRoot)//'.MAP.sum'        ! Output file name
      Init%InData_MAP%depth             = -Init%OutData_HD%WtrDpth    ! This need to be set according to the water depth in HydroDyn

      Init%InData_MAP%LinInitInp%Linearize = p_FAST%Linearize

      CALL MAP_Init( Init%InData_MAP, MAPp%Input(1), MAPp%p,  MAPp%x(STATE_CURR), MAPp%xd(STATE_CURR), MAPp%z(STATE_CURR), MAPp%OtherSt, &
                      MAPp%y, p_FAST%dt_module( MODULE_MAP ), Init%OutData_MAP, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      p_FAST%ModuleInitialized(Module_MAP) = .TRUE.
      CALL SetModuleSubstepTime(Module_MAP, p_FAST, y_FAST, ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      allocate( y_FAST%Lin%Modules(Module_MAP)%Instance(1), stat=ErrStat2)
      if (ErrStat2 /= 0 ) then
         call SetErrStat(ErrID_Fatal, "Error allocating Lin%Modules(MAP).", ErrStat, ErrMsg, RoutineName )
      else
         if (allocated(Init%OutData_MAP%LinInitOut%LinNames_y)) call move_alloc(Init%OutData_MAP%LinInitOut%LinNames_y,y_FAST%Lin%Modules(Module_MAP)%Instance(1)%Names_y )
         if (allocated(Init%OutData_MAP%LinInitOut%LinNames_u)) call move_alloc(Init%OutData_MAP%LinInitOut%LinNames_u,y_FAST%Lin%Modules(Module_MAP)%Instance(1)%Names_u )
         if (allocated(Init%OutData_MAP%LinInitOut%IsLoad_u  )) call move_alloc(Init%OutData_MAP%LinInitOut%IsLoad_u  ,y_FAST%Lin%Modules(Module_MAP)%Instance(1)%IsLoad_u   )

         if (allocated(Init%OutData_MAP%WriteOutputHdr)) y_FAST%Lin%Modules(Module_MAP)%Instance(1)%NumOutputs = size(Init%OutData_MAP%WriteOutputHdr)
      end if

      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      END IF
   ! ........................
   ! initialize MoorDyn
   ! ........................
   ELSEIF (p_FAST%CompMooring == Module_MD) THEN

      Init%InData_MD%FileName  = p_FAST%MooringFile         ! This needs to be set according to what is in the FAST input file.
      Init%InData_MD%RootName  = p_FAST%OutFileRoot

      Init%InData_MD%PtfmInit  = Init%OutData_ED%PlatformPos !ED%x(STATE_CURR)%QT(1:6)   ! initial position of the platform !bjj: this should come from Init%OutData_ED, not x_ED
      Init%InData_MD%g         = p_FAST%Gravity     ! This need to be according to g from driver
      Init%InData_MD%rhoW      = Init%OutData_HD%WtrDens     ! This needs to be set according to seawater density in HydroDyn      
      Init%InData_MD%WtrDepth  = Init%OutData_HD%WtrDpth    ! This need to be set according to the water depth in HydroDyn

      CALL MD_Init( Init%InData_MD, MD%Input(1), MD%p, MD%x(STATE_CURR), MD%xd(STATE_CURR), MD%z(STATE_CURR), &
                    MD%OtherSt(STATE_CURR), MD%y, MD%m, p_FAST%dt_module( MODULE_MD ), Init%OutData_MD, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      p_FAST%ModuleInitialized(Module_MD) = .TRUE.
      CALL SetModuleSubstepTime(Module_MD, p_FAST, y_FAST, ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      END IF
   ! ........................
   ! initialize FEAM
   ! ........................
   ELSEIF (p_FAST%CompMooring == Module_FEAM) THEN

      Init%InData_FEAM%InputFile   = p_FAST%MooringFile         ! This needs to be set according to what is in the FAST input file.
      Init%InData_FEAM%RootName    = TRIM(p_FAST%OutFileRoot)//'.'//TRIM(y_FAST%Module_Abrev(Module_FEAM))

      Init%InData_FEAM%PtfmInit    = Init%OutData_ED%PlatformPos !ED%x(STATE_CURR)%QT(1:6)   ! initial position of the platform !bjj: this should come from Init%OutData_ED, not x_ED
      Init%InData_FEAM%NStepWave   = 1                          ! an arbitrary number > 0 (to set the size of the wave data, which currently contains all zero values)     
      Init%InData_FEAM%gravity     = p_FAST%Gravity     ! This need to be according to g from driver
      Init%InData_FEAM%WtrDens     = Init%OutData_HD%WtrDens     ! This needs to be set according to seawater density in HydroDyn      
!      Init%InData_FEAM%depth       =  Init%OutData_HD%WtrDpth    ! This need to be set according to the water depth in HydroDyn

      CALL FEAM_Init( Init%InData_FEAM, FEAM%Input(1), FEAM%p,  FEAM%x(STATE_CURR), FEAM%xd(STATE_CURR), FEAM%z(STATE_CURR), &
                      FEAM%OtherSt(STATE_CURR), FEAM%y, FEAM%m, p_FAST%dt_module( MODULE_FEAM ), Init%OutData_FEAM, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      p_FAST%ModuleInitialized(Module_FEAM) = .TRUE.
      CALL SetModuleSubstepTime(Module_FEAM, p_FAST, y_FAST, ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      END IF
   ! ........................
   ! initialize OrcaFlex Interface
   ! ........................
   ELSEIF (p_FAST%CompMooring == Module_Orca) THEN

      Init%InData_Orca%InputFile = p_FAST%MooringFile
      Init%InData_Orca%RootName  = p_FAST%OutFileRoot
      Init%InData_Orca%TMax      = p_FAST%TMax

      CALL Orca_Init( Init%InData_Orca, Orca%Input(1), Orca%p,  Orca%x(STATE_CURR), Orca%xd(STATE_CURR), Orca%z(STATE_CURR), Orca%OtherSt(STATE_CURR), &
                      Orca%y, Orca%m, p_FAST%dt_module( MODULE_Orca ), Init%OutData_Orca, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      p_FAST%ModuleInitialized(MODULE_Orca) = .TRUE.
      CALL SetModuleSubstepTime(MODULE_Orca, p_FAST, y_FAST, ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      END IF
   END IF

   ! ------------------------------
   ! initialize CompIce modules
   ! ------------------------------
   ALLOCATE( IceF%Input( p_FAST%InterpOrder+1 ), IceF%InputTimes( p_FAST%InterpOrder+1 ), STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating IceF%Input and IceF%InputTimes.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF

      ! We need this to be allocated (else we have issues passing nonallocated arrays and using the first index of Input(),
      !   but we don't need the space of IceD_MaxLegs if we're not using it.
   IF ( p_FAST%CompIce /= Module_IceD ) THEN
      IceDim = 1
   ELSE
      IceDim = IceD_MaxLegs
   END IF

      ! because there may be multiple instances of IceDyn, we'll allocate arrays for that here
      ! we could allocate these after
   ALLOCATE( IceD%Input( p_FAST%InterpOrder+1, IceDim ), IceD%InputTimes( p_FAST%InterpOrder+1, IceDim ), STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating IceD%Input and IceD%InputTimes.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF

     ALLOCATE( IceD%x(           IceDim,2), &
               IceD%xd(          IceDim,2), &
               IceD%z(           IceDim,2), &
               IceD%OtherSt(     IceDim,2), &
               IceD%p(           IceDim  ), &
               IceD%u(           IceDim  ), &
               IceD%y(           IceDim  ), &
               IceD%m(           IceDim  ), &
                                             STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating IceD state, input, and output data.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF


   ! ........................
   ! initialize IceFloe
   ! ........................
   IF ( p_FAST%CompIce == Module_IceF ) THEN

      Init%InData_IceF%InputFile     = p_FAST%IceFile
      Init%InData_IceF%RootName      = TRIM(p_FAST%OutFileRoot)//'.'//TRIM(y_FAST%Module_Abrev(Module_IceF))
      Init%InData_IceF%simLength     = p_FAST%TMax  !bjj: IceFloe stores this as single-precision (ReKi) TMax is DbKi
      Init%InData_IceF%MSL2SWL       = Init%OutData_HD%MSL2SWL
      Init%InData_IceF%gravity       = p_FAST%Gravity
      
      CALL IceFloe_Init( Init%InData_IceF, IceF%Input(1), IceF%p,  IceF%x(STATE_CURR), IceF%xd(STATE_CURR), IceF%z(STATE_CURR), &
                         IceF%OtherSt(STATE_CURR), IceF%y, IceF%m, p_FAST%dt_module( MODULE_IceF ), Init%OutData_IceF, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      p_FAST%ModuleInitialized(Module_IceF) = .TRUE.
      CALL SetModuleSubstepTime(Module_IceF, p_FAST, y_FAST, ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      END IF
   ! ........................
   ! initialize IceDyn
   ! ........................
   ELSEIF ( p_FAST%CompIce == Module_IceD ) THEN

      Init%InData_IceD%InputFile     = p_FAST%IceFile
      Init%InData_IceD%RootName      = TRIM(p_FAST%OutFileRoot)//'.'//TRIM(y_FAST%Module_Abrev(Module_IceD))//'1'     
      Init%InData_IceD%MSL2SWL       = Init%OutData_HD%MSL2SWL      
      Init%InData_IceD%WtrDens       = Init%OutData_HD%WtrDens    
      Init%InData_IceD%gravity       = p_FAST%Gravity
      Init%InData_IceD%TMax          = p_FAST%TMax
      Init%InData_IceD%LegNum        = 1

      CALL IceD_Init( Init%InData_IceD, IceD%Input(1,1), IceD%p(1),  IceD%x(1,STATE_CURR), IceD%xd(1,STATE_CURR), IceD%z(1,STATE_CURR), &
                      IceD%OtherSt(1,STATE_CURR), IceD%y(1), IceD%m(1), p_FAST%dt_module( MODULE_IceD ), Init%OutData_IceD, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      p_FAST%ModuleInitialized(Module_IceD) = .TRUE.
      CALL SetModuleSubstepTime(Module_IceD, p_FAST, y_FAST, ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

         ! now initialize IceD for additional legs (if necessary)
      dt_IceD           = p_FAST%dt_module( MODULE_IceD )
      p_FAST%numIceLegs = Init%OutData_IceD%numLegs

      IF (p_FAST%numIceLegs > IceD_MaxLegs) THEN
         CALL SetErrStat(ErrID_Fatal,'IceDyn-FAST coupling is supported for up to '//TRIM(Num2LStr(IceD_MaxLegs))//' legs, but ' &
                           //TRIM(Num2LStr(p_FAST%numIceLegs))//' legs were specified.',ErrStat,ErrMsg,RoutineName)
      END IF


      DO i=2,p_FAST%numIceLegs  ! basically, we just need IceDyn to set up its meshes for inputs/outputs and possibly initial values for states
         Init%InData_IceD%LegNum = i
         Init%InData_IceD%RootName = TRIM(p_FAST%OutFileRoot)//'.'//TRIM(y_FAST%Module_Abrev(Module_IceD))//TRIM(Num2LStr(i))

         CALL IceD_Init( Init%InData_IceD, IceD%Input(1,i), IceD%p(i),  IceD%x(i,STATE_CURR), IceD%xd(i,STATE_CURR), IceD%z(i,STATE_CURR), &
                            IceD%OtherSt(i,STATE_CURR), IceD%y(i), IceD%m(i), dt_IceD, Init%OutData_IceD, ErrStat2, ErrMsg2 )
            CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

         !bjj: we're going to force this to have the same timestep because I don't want to have to deal with n IceD modules with n timesteps.
         IF (.NOT. EqualRealNos( p_FAST%dt_module( MODULE_IceD ),dt_IceD )) THEN
            CALL SetErrStat(ErrID_Fatal,"All instances of IceDyn (one per support-structure leg) must be the same",ErrStat,ErrMsg,RoutineName)
         END IF
      END DO

      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      END IF

   END IF


   ! ........................
   ! initialize ServoDyn 
   ! ........................
   ALLOCATE( SrvD%Input( p_FAST%InterpOrder+1 ), SrvD%InputTimes( p_FAST%InterpOrder+1 ), STAT = ErrStat2 )
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal,"Error allocating SrvD%Input and SrvD%InputTimes.",ErrStat,ErrMsg,RoutineName)
         CALL Cleanup()
         RETURN
      END IF
      
   IF ( p_FAST%CompServo == Module_SrvD ) THEN
      Init%InData_SrvD%InputFile     = p_FAST%ServoFile
      Init%InData_SrvD%RootName      = TRIM(p_FAST%OutFileRoot)//'.'//TRIM(y_FAST%Module_Abrev(Module_SrvD))
      Init%InData_SrvD%NumBl         = Init%OutData_ED%NumBl
      Init%InData_SrvD%Gravity       = (/ 0.0_ReKi, 0.0_ReKi, -p_FAST%Gravity /)       ! "Gravitational acceleration vector" m/s^2
      Init%InData_SrvD%NacRefPos(1:3)        = ED%y%NacelleMotion%Position(1:3,1)
      Init%InData_SrvD%NacTransDisp(1:3)     = ED%y%NacelleMotion%TranslationDisp(1:3,1)     ! R8Ki
      Init%InData_SrvD%NacRefOrient(1:3,1:3) = ED%y%NacelleMotion%RefOrientation(1:3,1:3,1)  ! R8Ki
      Init%InData_SrvD%NacOrient(1:3,1:3)    = ED%y%NacelleMotion%Orientation(1:3,1:3,1)     ! R8Ki
      Init%InData_SrvD%TwrBaseRefPos         = Init%OutData_ED%TwrBaseRefPos
      Init%InData_SrvD%TwrBaseTransDisp      = Init%OutData_ED%TwrBaseTransDisp
      Init%InData_SrvD%TwrBaseRefOrient      = Init%OutData_ED%TwrBaseRefOrient              ! R8Ki
      Init%InData_SrvD%TwrBaseOrient         = Init%OutData_ED%TwrBaseOrient                 ! R8Ki
      Init%InData_SrvD%PtfmRefPos(1:3)       = ED%y%PlatformPtMesh%Position(1:3,1)
      Init%InData_SrvD%PtfmTransDisp(1:3)    = ED%y%PlatformPtMesh%TranslationDisp(1:3,1)
      Init%InData_SrvD%PtfmRefOrient(1:3,1:3)= ED%y%PlatformPtMesh%RefOrientation(1:3,1:3,1) ! R8Ki
      Init%InData_SrvD%PtfmOrient(1:3,1:3)   = ED%y%PlatformPtMesh%Orientation(1:3,1:3,1)    ! R8Ki
      Init%InData_SrvD%TMax          = p_FAST%TMax
      Init%InData_SrvD%AirDens       = AirDens
      Init%InData_SrvD%AvgWindSpeed  = Init%OutData_IfW%WindFileInfo%MWS
      Init%InData_SrvD%Linearize     = p_FAST%Linearize
      Init%InData_SrvD%TrimCase      = p_FAST%TrimCase
      Init%InData_SrvD%TrimGain      = p_FAST%TrimGain
      Init%InData_SrvD%RotSpeedRef   = Init%OutData_ED%RotSpeed
      Init%InData_SrvD%InterpOrder   = p_FAST%InterpOrder

      CALL AllocAry( Init%InData_SrvD%BladeRootRefPos,         3, Init%OutData_ED%NumBl, 'Init%InData_SrvD%BladeRootRefPos',     errStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
      CALL AllocAry( Init%InData_SrvD%BladeRootTransDisp,      3, Init%OutData_ED%NumBl, 'Init%InData_SrvD%BladeRootTransDisp',  errStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
      CALL AllocAry( Init%InData_SrvD%BladeRootRefOrient,   3, 3, Init%OutData_ED%NumBl, 'Init%InData_SrvD%BladeRootRefOrient',  errStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
      CALL AllocAry( Init%InData_SrvD%BladeRootOrient,      3, 3, Init%OutData_ED%NumBl, 'Init%InData_SrvD%BladeRootOrient',     errStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
         IF (ErrStat >= AbortErrLev) THEN
            CALL Cleanup()
            RETURN
         END IF
      do k=1,Init%OutData_ED%NumBl
         Init%InData_SrvD%BladeRootRefPos(:,k)     = ED%y%BladeRootMotion(k)%Position(:,1)
         Init%InData_SrvD%BladeRootTransDisp(:,k)  = ED%y%BladeRootMotion(k)%TranslationDisp(:,1)
         Init%InData_SrvD%BladeRootRefOrient(:,:,k)= ED%y%BladeRootMotion(k)%RefOrientation(:,:,1)
         Init%InData_SrvD%BladeRootOrient(:,:,k)   = ED%y%BladeRootMotion(k)%Orientation(:,:,1)
      enddo

      
      IF ( PRESENT(ExternInitData) ) THEN
         Init%InData_SrvD%NumSC2CtrlGlob = ExternInitData%NumSC2CtrlGlob
         IF ( (Init%InData_SrvD%NumSC2CtrlGlob > 0) ) THEN
            CALL AllocAry( Init%InData_SrvD%fromSCGlob, Init%InData_SrvD%NumSC2CtrlGlob, 'Init%InData_SrvD%fromSCGlob', ErrStat2, ErrMsg2)
               CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
               IF (ErrStat >= AbortErrLev) THEN
                  CALL Cleanup()
                  RETURN
               END IF
               
            do i=1,Init%InData_SrvD%NumSC2CtrlGlob
               Init%InData_SrvD%fromSCGlob(i) = ExternInitData%fromSCGlob(i)
            end do
         END IF

         Init%InData_SrvD%NumSC2Ctrl = ExternInitData%NumSC2Ctrl
         IF ( (Init%InData_SrvD%NumSC2Ctrl > 0) ) THEN
            CALL AllocAry( Init%InData_SrvD%fromSC, Init%InData_SrvD%NumSC2Ctrl, 'Init%InData_SrvD%fromSC', ErrStat2, ErrMsg2)
               CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
               IF (ErrStat >= AbortErrLev) THEN
                  CALL Cleanup()
                  RETURN
               END IF
            
            do i=1,Init%InData_SrvD%NumSC2Ctrl
               Init%InData_SrvD%fromSC(i) = ExternInitData%fromSC(i)
            end do
         END IF

         Init%InData_SrvD%NumCtrl2SC = ExternInitData%NumCtrl2SC

      ELSE
         Init%InData_SrvD%NumSC2CtrlGlob = 0
         Init%InData_SrvD%NumSC2Ctrl = 0
         Init%InData_SrvD%NumCtrl2SC = 0
      END IF     

      ! Set cable controls inputs (if requested by other modules)  -- There is probably a nicer way to do this, but this will work for now.
      call SetSrvDCableControls()
  
            
      CALL AllocAry(Init%InData_SrvD%BlPitchInit, Init%OutData_ED%NumBl, 'BlPitchInit', ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      if (ErrStat >= abortErrLev) then ! make sure allocatable arrays are valid before setting them
         CALL Cleanup()
         RETURN
      end if

      Init%InData_SrvD%BlPitchInit   = Init%OutData_ED%BlPitch
      CALL SrvD_Init( Init%InData_SrvD, SrvD%Input(1), SrvD%p, SrvD%x(STATE_CURR), SrvD%xd(STATE_CURR), SrvD%z(STATE_CURR), &
                      SrvD%OtherSt(STATE_CURR), SrvD%y, SrvD%m, p_FAST%dt_module( MODULE_SrvD ), Init%OutData_SrvD, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
      p_FAST%ModuleInitialized(Module_SrvD) = .TRUE.

      !IF ( Init%OutData_SrvD%CouplingScheme == ExplicitLoose ) THEN ...  bjj: abort if we're doing anything else!

      CALL SetModuleSubstepTime(Module_SrvD, p_FAST, y_FAST, ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      !! initialize SrvD%y%ElecPwr and SrvD%y%GenTq because they are one timestep different (used as input for the next step)?
                  
      allocate( y_FAST%Lin%Modules(MODULE_SrvD)%Instance(1), stat=ErrStat2)
      if (ErrStat2 /= 0 ) then
         call SetErrStat(ErrID_Fatal, "Error allocating Lin%Modules(SrvD).", ErrStat, ErrMsg, RoutineName )
      else
         if (allocated(Init%OutData_SrvD%LinNames_y)) call move_alloc(Init%OutData_SrvD%LinNames_y,y_FAST%Lin%Modules(MODULE_SrvD)%Instance(1)%Names_y )
         if (allocated(Init%OutData_SrvD%LinNames_u)) call move_alloc(Init%OutData_SrvD%LinNames_u,y_FAST%Lin%Modules(MODULE_SrvD)%Instance(1)%Names_u )
         if (allocated(Init%OutData_SrvD%LinNames_x)) call move_alloc(Init%OutData_SrvD%LinNames_x,y_FAST%Lin%Modules(MODULE_SrvD)%Instance(1)%Names_x )
         if (allocated(Init%OutData_SrvD%RotFrame_y)) call move_alloc(Init%OutData_SrvD%RotFrame_y,y_FAST%Lin%Modules(MODULE_SrvD)%Instance(1)%RotFrame_y )
         if (allocated(Init%OutData_SrvD%RotFrame_u)) call move_alloc(Init%OutData_SrvD%RotFrame_u,y_FAST%Lin%Modules(MODULE_SrvD)%Instance(1)%RotFrame_u )
         if (allocated(Init%OutData_SrvD%RotFrame_x)) call move_alloc(Init%OutData_SrvD%RotFrame_x,y_FAST%Lin%Modules(MODULE_SrvD)%Instance(1)%RotFrame_x )
         if (allocated(Init%OutData_SrvD%IsLoad_u  )) call move_alloc(Init%OutData_SrvD%IsLoad_u  ,y_FAST%Lin%Modules(MODULE_SrvD)%Instance(1)%IsLoad_u   )
         if (allocated(Init%OutData_SrvD%DerivOrder_x)) call move_alloc(Init%OutData_SrvD%DerivOrder_x,y_FAST%Lin%Modules(MODULE_SrvD)%Instance(1)%DerivOrder_x)

         if (allocated(Init%OutData_SrvD%WriteOutputHdr)) y_FAST%Lin%Modules(MODULE_SrvD)%Instance(1)%NumOutputs = size(Init%OutData_SrvD%WriteOutputHdr)
      end if
      
      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      END IF
      
   ! ........................
   ! some checks for AeroDyn and ElastoDyn inputs with the high-speed shaft brake hack in ElastoDyn:
   ! (DO NOT COPY THIS CODE!)
   ! ........................   
         ! bjj: this is a hack to get high-speed shaft braking in FAST v8
      
      IF ( Init%OutData_SrvD%UseHSSBrake ) THEN
         IF ( p_FAST%CompAero == Module_AD14 ) THEN
            IF ( AD14%p%DYNINFL ) THEN
               CALL SetErrStat(ErrID_Fatal,'AeroDyn v14 "DYNINFL" InfModel is invalid for models with high-speed shaft braking.',ErrStat,ErrMsg,RoutineName)
            END IF
         END IF
         

         IF ( ED%p%method == Method_RK4 ) THEN ! bjj: should be using ElastoDyn's Method_ABM4 Method_AB4 parameters
            CALL SetErrStat(ErrID_Fatal,'ElastoDyn must use the AB4 or ABM4 integration method to implement high-speed shaft braking.',ErrStat,ErrMsg,RoutineName)
         ENDIF
      END IF ! Init%OutData_SrvD%UseHSSBrake
      
      
   END IF


   ! ........................
   ! Set up output for glue code (must be done after all modules are initialized so we have their WriteOutput information)
   ! ........................

   CALL FAST_InitOutput( p_FAST, y_FAST, Init, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)


   ! -------------------------------------------------------------------------
   ! Initialize mesh-mapping data
   ! -------------------------------------------------------------------------

   CALL InitModuleMappings(p_FAST, ED, BD, AD14, AD, HD, SD, ExtPtfm, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, ErrStat2, ErrMsg2)
      CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

      IF (ErrStat >= AbortErrLev) THEN
         CALL Cleanup()
         RETURN
      ELSEIF (ErrStat /= ErrID_None) THEN
         ! a little work-around in case the mesh mapping info messages get too long
         CALL WrScr( NewLine//TRIM(ErrMsg)//NewLine )
         ErrStat = ErrID_None
         ErrMsg = ""
      END IF

   ! -------------------------------------------------------------------------
   ! Initialize for linearization:
   ! -------------------------------------------------------------------------
   if ( p_FAST%Linearize ) then      
      ! NOTE: In the following call, we use Init%OutData_AD%BladeProps(1)%NumBlNds as the number of aero nodes on EACH blade, which 
      !       is consistent with the current AD implementation, but if AD changes this, then it must be handled here, too!
      if (p_FAST%CompAero == MODULE_AD) then
         call Init_Lin(p_FAST, y_FAST, m_FAST, AD, ED, NumBl, Init%OutData_AD%rotors(1)%BladeProps(1)%NumBlNds, ErrStat2, ErrMsg2) 
      else
         call Init_Lin(p_FAST, y_FAST, m_FAST, AD, ED, NumBl, -1, ErrStat2, ErrMsg2) 
      endif     
         call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

         if (ErrStat >= AbortErrLev) then
            call Cleanup()
            return
         end if
   end if


   ! -------------------------------------------------------------------------
   ! Initialize data for VTK output
   ! -------------------------------------------------------------------------
   if ( p_FAST%WrVTK > VTK_None ) then
      call SetVTKParameters(p_FAST, Init%OutData_ED, Init%OutData_AD, Init%InData_HD, Init%OutData_HD, ED, BD, AD, HD, ErrStat2, ErrMsg2)
         call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
   end if

   ! -------------------------------------------------------------------------
   ! Write initialization data to FAST summary file:
   ! -------------------------------------------------------------------------
   if (p_FAST%SumPrint)  then
       CALL FAST_WrSum( p_FAST, y_FAST, MeshMapData, ErrStat2, ErrMsg2 )
          CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
   endif


   ! -------------------------------------------------------------------------
   ! other misc variables initialized here:
   ! -------------------------------------------------------------------------

   m_FAST%t_global        = t_initial

   ! Initialize external inputs for first step
   if ( p_FAST%CompServo == MODULE_SrvD ) then
      m_FAST%ExternInput%GenTrq     = SrvD%Input(1)%ExternalGenTrq !0.0_ReKi
      m_FAST%ExternInput%ElecPwr    = SrvD%Input(1)%ExternalElecPwr
      m_FAST%ExternInput%YawPosCom  = SrvD%Input(1)%ExternalYawPosCom
      m_FAST%ExternInput%YawRateCom = SrvD%Input(1)%ExternalYawRateCom
      m_FAST%ExternInput%HSSBrFrac  = SrvD%Input(1)%ExternalHSSBrFrac

      do i=1,SIZE(SrvD%Input(1)%ExternalBlPitchCom)
         m_FAST%ExternInput%BlPitchCom(i) = SrvD%Input(1)%ExternalBlPitchCom(i)
      end do

      do i=1,SIZE(SrvD%Input(1)%ExternalBlAirfoilCom)
         m_FAST%ExternInput%BlAirfoilCom(i) = SrvD%Input(1)%ExternalBlAirfoilCom(i)
      end do

         ! Cable Controls (only 20 channels are passed to simulink, but may be less or more in SrvD)
      if (allocated(SrvD%Input(1)%ExternalCableDeltaL)) then
         do i=1,min(SIZE(m_FAST%ExternInput%CableDeltaL),SIZE(SrvD%Input(1)%ExternalCableDeltaL))
            m_FAST%ExternInput%CableDeltaL(i) = SrvD%Input(1)%ExternalCableDeltaL(i)
         end do
      else  ! Initialize to zero for consistency
         m_FAST%ExternInput%CableDeltaL = 0.0_Reki
      endif
      if (allocated(SrvD%Input(1)%ExternalCableDeltaLdot)) then
         do i=1,min(SIZE(m_FAST%ExternInput%CableDeltaLdot),SIZE(SrvD%Input(1)%ExternalCableDeltaLdot))
            m_FAST%ExternInput%CableDeltaLdot(i) = SrvD%Input(1)%ExternalCableDeltaLdot(i)
         end do
      else  ! Initialize to zero for consistency
         m_FAST%ExternInput%CableDeltaLdot = 0.0_Reki
      endif
   end if

   m_FAST%ExternInput%LidarFocus = 1.0_ReKi  ! make this non-zero (until we add the initial position in the InflowWind input file)


   !...............................................................................................................................
   ! Destroy initializion data
   !...............................................................................................................................
   CALL Cleanup()

CONTAINS
   SUBROUTINE Cleanup()
   !...............................................................................................................................
   ! Destroy initializion data
   !...............................................................................................................................
      CALL FAST_DestroyInitData( Init, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)

   END SUBROUTINE Cleanup

   SUBROUTINE SetSrvDCableControls()
      ! There is probably a better method for doint this, but this will work for now.  Kind of an ugly bit of hacking.
      Init%InData_SrvD%NumCableControl = 0
      if (allocated(Init%OutData_SD%CableCChanRqst)) then
         Init%InData_SrvD%NumCableControl = max(Init%InData_SrvD%NumCableControl, size(Init%OutData_SD%CableCChanRqst))
      endif
      if (allocated(Init%OutData_MD%CableCChanRqst)) then
         Init%InData_SrvD%NumCableControl = max(Init%InData_SrvD%NumCableControl, size(Init%OutData_MD%CableCChanRqst))
      endif
      ! Set an array listing which modules requested which channels.
      !     They may not all be requested, so check the arrays returned from them during initialization.
      if (Init%InData_SrvD%NumCableControl > 0) then
         call AllocAry(Init%InData_SrvD%CableControlRequestor, Init%InData_SrvD%NumCableControl, 'CableControlRequestor', ErrStat2, ErrMsg2)
            call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
         if (ErrStat >= abortErrLev) then ! make sure allocatable arrays are valid before setting them
            call Cleanup()
            return
         endif
         !  Fill a string array that we pass to SrvD containing info about which module is using which of the
         !  requested channels.  This is not strictly necessary, but will greatly simplify troubleshooting erros
         !  with the setup later.
         Init%InData_SrvD%CableControlRequestor = ''
         do I=1,Init%InData_SrvD%NumCableControl
            ! SD -- lots of logic here since we don't know if SD did the requesting of the channels
            if (allocated(Init%OutData_SD%CableCChanRqst)) then
               if (I <= size(Init%OutData_SD%CableCChanRqst)) then
                  if (Init%OutData_SD%CableCChanRqst(I)) then
                     if (len_trim(Init%InData_SrvD%CableControlRequestor(I))>0) Init%InData_SrvD%CableControlRequestor(I) = trim(Init%InData_SrvD%CableControlRequestor(I))//', '
                     Init%InData_SrvD%CableControlRequestor(I) = trim(Init%InData_SrvD%CableControlRequestor(I))//trim(y_FAST%Module_Ver( Module_SD )%Name)
                  endif
               endif
            endif
            ! MD -- lots of logic here since we don't know if MD did the requesting of the channels
            if (allocated(Init%OutData_MD%CableCChanRqst)) then
               if (I <= size(Init%OutData_MD%CableCChanRqst)) then
                  if (Init%OutData_MD%CableCChanRqst(I)) then
                     if (len_trim(Init%InData_SrvD%CableControlRequestor(I))>0) Init%InData_SrvD%CableControlRequestor(I) = trim(Init%InData_SrvD%CableControlRequestor(I))//', '
                     Init%InData_SrvD%CableControlRequestor(I) = trim(Init%InData_SrvD%CableControlRequestor(I))//trim(y_FAST%Module_Ver( Module_MD )%Name)
                  endif
               endif
            endif
         enddo
      endif

      !  Now that we actually know which channels are requested, resize the arrays sent into SD and MD.  They can both handle
      !  larger and sparse arrays. They will simply ignore the channels they aren't looking for.,
      if (Init%InData_SrvD%NumCableControl > 0) then
         !  SD has one array (CableDeltaL)
         if (allocated(SD%Input)) then
            if (allocated(SD%Input(1)%CableDeltaL)) then
               if (size(SD%Input(1)%CableDeltaL)<Init%InData_SrvD%NumCableControl) then
                  deallocate(SD%Input(1)%CableDeltaL)
                  call AllocAry(SD%Input(1)%CableDeltaL,Init%InData_SrvD%NumCableControl,'SD%Input(1)%CableDeltaL', ErrStat2, ErrMsg2)
                     call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
                  if (ErrStat >= abortErrLev) then ! make sure allocatable arrays are valid before setting them
                     call Cleanup()
                     return
                  endif
                  SD%Input(1)%CableDeltaL = 0.0_ReKi
               endif
            endif
         endif
         ! Resize the MD arrays as needed -- They may have requested different inputs, but we are passing larger arrays if necessary.
         !  MD has two arrays (DeltaL, DeltaLdot)
         if (allocated(MD%Input)) then
            if (allocated(MD%Input(1)%DeltaL)) then
               if (size(MD%Input(1)%DeltaL)<Init%InData_SrvD%NumCableControl) then
                  deallocate(MD%Input(1)%DeltaL)
                  call AllocAry(MD%Input(1)%DeltaL,Init%InData_SrvD%NumCableControl,'MD%Input(1)%DeltaL', ErrStat2, ErrMsg2)
                     call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
                  if (ErrStat >= abortErrLev) then ! make sure allocatable arrays are valid before setting them
                     call Cleanup()
                     return
                  endif
                  MD%Input(1)%DeltaL = 0.0_ReKi
               endif
            endif
         endif
         if (allocated(MD%Input)) then
            if (allocated(MD%Input(1)%DeltaLdot)) then
               if (size(MD%Input(1)%DeltaLdot)<Init%InData_SrvD%NumCableControl) then
                  deallocate(MD%Input(1)%DeltaLdot)
                  call AllocAry(MD%Input(1)%DeltaLdot,Init%InData_SrvD%NumCableControl,'MD%Input(1)%DeltaLdot', ErrStat2, ErrMsg2)
                     call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
                  if (ErrStat >= abortErrLev) then ! make sure allocatable arrays are valid before setting them
                     call Cleanup()
                     return
                  endif
                  MD%Input(1)%DeltaLdot = 0.0_ReKi
               endif
            endif
         endif
      endif
   END SUBROUTINE SetSrvDCableControls

END SUBROUTINE FAST_InitializeAll
!----------------------------------------------------------------------------------------------------------------------------------
!> This subroutine checks for command-line arguments, gets the root name of the input files
!! (including full path name), and creates the names of the output files.
SUBROUTINE FAST_Init( p, m_FAST, y_FAST, t_initial, InputFile, ErrStat, ErrMsg, TMax, TurbID, OverrideAbortLev, RootName )

      IMPLICIT                        NONE

   ! Passed variables

   TYPE(FAST_ParameterType), INTENT(INOUT)         :: p                 !< The parameter data for the FAST (glue-code) simulation
   TYPE(FAST_MiscVarType),   INTENT(INOUT)         :: m_FAST            !< Miscellaneous variables
   TYPE(FAST_OutputFileType),INTENT(INOUT)         :: y_FAST            !< The output data for the FAST (glue-code) simulation
   REAL(DbKi),               INTENT(IN)            :: t_initial         !< the beginning time of the simulation
   INTEGER(IntKi),           INTENT(OUT)           :: ErrStat           !< Error status
   CHARACTER(*),             INTENT(OUT)           :: ErrMsg            !< Error message
   CHARACTER(*),             INTENT(IN)            :: InputFile         !< A CHARACTER string containing the name of the primary FAST input file (if not present, we'll get it from the command line)
   REAL(DbKi),               INTENT(IN), OPTIONAL  :: TMax              !< the length of the simulation (from Simulink or FAST.Farm)
   INTEGER(IntKi),           INTENT(IN), OPTIONAL  :: TurbID            !< an ID for naming the tubine output file
   LOGICAL,                  INTENT(IN), OPTIONAL  :: OverrideAbortLev  !< whether or not we should override the abort error level (e.g., FAST.Farm)
   CHARACTER(*),             INTENT(IN), OPTIONAL  :: RootName          !< A CHARACTER string containing the root name of FAST output files, overriding normal naming convention
      ! Local variables

   INTEGER                      :: i                                    ! loop counter
   !CHARACTER(1024)              :: DirName                              ! A CHARACTER string containing the path of the current working directory


   LOGICAL                      :: OverrideAbortErrLev
   CHARACTER(*), PARAMETER      :: RoutineName = "FAST_Init"

   INTEGER(IntKi)               :: ErrStat2
   CHARACTER(ErrMsgLen)         :: ErrMsg2

      ! Initialize some variables
   ErrStat = ErrID_None
   ErrMsg = ''

   IF (PRESENT(OverrideAbortLev)) THEN
      OverrideAbortErrLev = OverrideAbortLev
   ELSE
      OverrideAbortErrLev = .true.
   END IF



   !...............................................................................................................................
   ! Set the root name of the output files based on the input file name
   !...............................................................................................................................

   if (present(RootName)) then
      p%OutFileRoot = RootName
   else
         ! Determine the root name of the primary file (will be used for output files)
      CALL GetRoot( InputFile, p%OutFileRoot )
      IF ( Cmpl4SFun )  p%OutFileRoot = TRIM( p%OutFileRoot )//'.SFunc'
      IF ( PRESENT(TurbID) ) THEN
         IF ( TurbID > 0 ) THEN
            p%OutFileRoot = TRIM( p%OutFileRoot )//'.T'//TRIM(Num2LStr(TurbID))
         END IF
      END IF

   end if
   p%VTK_OutFileRoot = p%OutFileRoot !initialize this here in case of error before it is set later


   !...............................................................................................................................
   ! Initialize the module name/date/version info:
   !...............................................................................................................................

   y_FAST%Module_Ver( Module_Glue   ) = FAST_Ver
   
   DO i=2,NumModules
      y_FAST%Module_Ver(i)%Date = 'unknown date'
      y_FAST%Module_Ver(i)%Ver  = 'unknown version'
   END DO
   y_FAST%Module_Ver( Module_IfW    )%Name = 'InflowWind'
   y_FAST%Module_Ver( Module_OpFM   )%Name = 'OpenFOAM integration'
   y_FAST%Module_Ver( Module_ED     )%Name = 'ElastoDyn'
   y_FAST%Module_Ver( Module_BD     )%Name = 'BeamDyn'
   y_FAST%Module_Ver( Module_AD14   )%Name = 'AeroDyn14'
   y_FAST%Module_Ver( Module_AD     )%Name = 'AeroDyn'
   y_FAST%Module_Ver( Module_SrvD   )%Name = 'ServoDyn'
   y_FAST%Module_Ver( Module_HD     )%Name = 'HydroDyn'
   y_FAST%Module_Ver( Module_SD     )%Name = 'SubDyn'
   y_FAST%Module_Ver( Module_ExtPtfm)%Name = 'ExtPtfm_MCKF'
   y_FAST%Module_Ver( Module_MAP    )%Name = 'MAP'
   y_FAST%Module_Ver( Module_FEAM   )%Name = 'FEAMooring'
   y_FAST%Module_Ver( Module_MD     )%Name = 'MoorDyn'
   y_FAST%Module_Ver( Module_Orca   )%Name = 'OrcaFlexInterface'
   y_FAST%Module_Ver( Module_IceF   )%Name = 'IceFloe'
   y_FAST%Module_Ver( Module_IceD   )%Name = 'IceDyn'
         
   y_FAST%Module_Abrev( Module_Glue   ) = 'FAST'
   y_FAST%Module_Abrev( Module_IfW    ) = 'IfW'
   y_FAST%Module_Abrev( Module_OpFM   ) = 'OpFM'
   y_FAST%Module_Abrev( Module_ED     ) = 'ED'
   y_FAST%Module_Abrev( Module_BD     ) = 'BD'
   y_FAST%Module_Abrev( Module_AD14   ) = 'AD'
   y_FAST%Module_Abrev( Module_AD     ) = 'AD'
   y_FAST%Module_Abrev( Module_SrvD   ) = 'SrvD'
   y_FAST%Module_Abrev( Module_HD     ) = 'HD'
   y_FAST%Module_Abrev( Module_SD     ) = 'SD'
   y_FAST%Module_Abrev( Module_ExtPtfm) = 'ExtPtfm'
   y_FAST%Module_Abrev( Module_MAP    ) = 'MAP'
   y_FAST%Module_Abrev( Module_FEAM   ) = 'FEAM'
   y_FAST%Module_Abrev( Module_MD     ) = 'MD'
   y_FAST%Module_Abrev( Module_Orca   ) = 'Orca'
   y_FAST%Module_Abrev( Module_IceF   ) = 'IceF'
   y_FAST%Module_Abrev( Module_IceD   ) = 'IceD'

   p%n_substeps = 1                                                ! number of substeps for between modules and global/FAST time
   p%BD_OutputSibling = .false.

   !...............................................................................................................................
   ! Read the primary file for the glue code:
   !...............................................................................................................................
   CALL FAST_ReadPrimaryFile( InputFile, p, m_FAST, OverrideAbortErrLev, ErrStat2, ErrMsg2 )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

      ! make sure some linearization variables are consistant
   if (.not. p%Linearize)  p%CalcSteady = .false.
   if (.not. p%CalcSteady) p%TrimCase = TrimCase_none
   m_FAST%Lin%FoundSteady = .false.
   p%LinInterpOrder = p%InterpOrder ! 1 ! always use linear (or constant) interpolation on rotor?

      ! overwrite TMax if necessary)
   IF (PRESENT(TMax)) THEN
      p%TMax = TMax
      !p%TMax = MAX( TMax, p%TMax )
   END IF

   IF ( ErrStat >= AbortErrLev ) RETURN


   p%KMax = 1                 ! after more checking, we may put this in the input file...
   !IF (p%CompIce == Module_IceF) p%KMax = 2
   p%SizeJac_Opt1 = 0  ! initialize this vector to zero; after we figure out what size the ED/SD/HD/BD meshes are, we'll fill this

   p%numIceLegs = 0           ! initialize number of support-structure legs in contact with ice (IceDyn will set this later)

   p%nBeams = 0               ! initialize number of BeamDyn instances (will be set later)

      ! determine what kind of turbine we're modeling:
   IF ( p%CompHydro == Module_HD .and. p%MHK == 0) THEN
      IF ( p%CompSub == Module_SD ) THEN
         p%TurbineType = Type_Offshore_Fixed
      ELSE
         p%TurbineType = Type_Offshore_Floating
      END IF
   ELSEIF ( p%CompMooring == Module_Orca .and. p%MHK == 0) THEN
      p%TurbineType = Type_Offshore_Floating
   ELSEIF ( p%CompSub == Module_ExtPtfm .and. p%MHK == 0) THEN
      p%TurbineType = Type_Offshore_Fixed
   ELSEIF ( p%MHK == 1 ) THEN
         p%TurbineType = Type_MHK_Fixed
   ELSEIF ( p%MHK == 2 ) THEN
         p%TurbineType = Type_MHK_Floating
   ELSE      
      p%TurbineType = Type_LandBased
   END IF


   p%n_TMax_m1  = CEILING( ( (p%TMax - t_initial) / p%DT ) ) - 1 ! We're going to go from step 0 to n_TMax (thus the -1 here)

   if (p%TMax < 1.0_DbKi) then ! log10(0) gives floating point divide-by-zero error
      p%TChanLen = MinChanLen
   else
      p%TChanLen = max( MinChanLen, int(log10(p%TMax))+7 )
   end if
   p%OutFmt_t = 'F'//trim(num2lstr( p%TChanLen ))//'.4' ! 'F10.4'

   !...............................................................................................................................
   ! Do some error checking on the inputs (validation):
   !...............................................................................................................................
   call ValidateInputData(p, m_FAST, ErrStat2, ErrMsg2)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )



   IF ( ErrStat >= AbortErrLev ) RETURN


   RETURN
END SUBROUTINE FAST_Init
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine initializes all of the mapping data structures needed between the various modules.
SUBROUTINE InitModuleMappings(p_FAST, ED, BD, AD14, AD, HD, SD, ExtPtfm, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, ErrStat, ErrMsg)
!...............................................................................................................................
   
   TYPE(FAST_ParameterType),   INTENT(INOUT) :: p_FAST              !< Parameters for the glue code

   TYPE(ElastoDyn_Data),TARGET,INTENT(INOUT) :: ED                  !< ElastoDyn data
   TYPE(BeamDyn_Data),         INTENT(INOUT) :: BD                  !< BeamDyn data
   TYPE(ServoDyn_Data),        INTENT(INOUT) :: SrvD                !< ServoDyn data
   TYPE(AeroDyn_Data),         INTENT(INOUT) :: AD                  !< AeroDyn data
   TYPE(AeroDyn14_Data),       INTENT(INOUT) :: AD14                !< AeroDyn14 data
   TYPE(HydroDyn_Data),        INTENT(INOUT) :: HD                  !< HydroDyn data
   TYPE(SubDyn_Data),          INTENT(INOUT) :: SD                  !< SubDyn data
   TYPE(ExtPtfm_Data),         INTENT(INOUT) :: ExtPtfm             !< ExtPtfm data
   TYPE(MAP_Data),             INTENT(INOUT) :: MAPp                !< MAP data
   TYPE(FEAMooring_Data),      INTENT(INOUT) :: FEAM                !< FEAMooring data
   TYPE(MoorDyn_Data),         INTENT(INOUT) :: MD                  !< MoorDyn data
   TYPE(OrcaFlex_Data),        INTENT(INOUT) :: Orca                !< OrcaFlex interface data
   TYPE(IceFloe_Data),         INTENT(INOUT) :: IceF                !< IceFloe data
   TYPE(IceDyn_Data),          INTENT(INOUT) :: IceD                !< All the IceDyn data used in time-step loop

   TYPE(FAST_ModuleMapType),   INTENT(INOUT) :: MeshMapData         !< Data for mapping between modules
   
   
   INTEGER(IntKi),             INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),               INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None
   

   INTEGER                                   :: K, i    ! loop counters
   INTEGER                                   :: j       ! loop counter for StC instance
   INTEGER                                   :: NumBl   ! number of blades
   INTEGER(IntKi)                            :: ErrStat2
   CHARACTER(ErrMsgLen)                      :: ErrMSg2
   CHARACTER(*), PARAMETER                   :: RoutineName = 'InitModuleMappings'
   
   TYPE(MeshType), POINTER                   :: PlatformMotion
   TYPE(MeshType), POINTER                   :: PlatformLoads
   !............................................................................................................................
   
   ErrStat = ErrID_None
   ErrMsg  = ""
   
   NumBl   = SIZE(ED%y%BladeRootMotion,1)
   PlatformMotion => ED%y%PlatformPtMesh
   PlatformLoads  => ED%Input(1)%PlatformPtMesh

   !............................................................................................................................
   ! Create the data structures and mappings in MeshMapType 
   !............................................................................................................................
   
!-------------------------
!  ElastoDyn <-> BeamDyn
!-------------------------
   IF ( p_FAST%CompElast == Module_BD ) THEN
      
      ! Blade meshes: (allocate two mapping data structures to number of blades, then allocate data inside the structures)
      ALLOCATE( MeshMapData%ED_P_2_BD_P(NumBl), MeshMapData%BD_P_2_ED_P(NumBl), STAT=ErrStat2 )
         IF ( ErrStat2 /= 0 ) THEN
            CALL SetErrStat( ErrID_Fatal, 'Error allocating MeshMapData%ED_P_2_BD_P and MeshMapData%BD_P_2_ED_P.', &
                            ErrStat, ErrMsg, RoutineName )
            RETURN
         END IF
         
      DO K=1,NumBl
         CALL MeshMapCreate( ED%y%BladeRootMotion(K), BD%Input(1,k)%RootMotion, MeshMapData%ED_P_2_BD_P(K), ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':ED_2_BD_BladeRootMotion('//TRIM(Num2LStr(K))//')' )
         CALL MeshMapCreate( BD%y(k)%ReactionForce, ED%Input(1)%HubPtLoad,  MeshMapData%BD_P_2_ED_P(K), ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':BD_2_ED_ReactionLoad('//TRIM(Num2LStr(K))//')' )
      END DO      
      
      ! Hub meshes:
      ALLOCATE( MeshMapData%ED_P_2_BD_P_Hub(NumBl), STAT=ErrStat2 )
         IF ( ErrStat2 /= 0 ) THEN
            CALL SetErrStat( ErrID_Fatal, 'Error allocating MeshMapData%ED_P_2_BD_P_Hub.', ErrStat, ErrMsg, RoutineName )
            RETURN
         END IF
         
      DO K=1,NumBl
         CALL MeshMapCreate( ED%y%HubPtMotion, BD%Input(1,k)%HubMotion, MeshMapData%ED_P_2_BD_P_Hub(K), ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':ED_2_BD_HubMotion('//TRIM(Num2LStr(K))//')' )
      END DO      
            
   END IF
   

   IF ( p_FAST%CompServo == Module_SrvD ) THEN
!-------------------------
!  ServoDyn <-> ElastoDyn
!-------------------------
         !  Nacelle TMD
      IF ( ALLOCATED(SrvD%Input(1)%NStCMotionMesh) ) THEN
         j=size(SrvD%Input(1)%NStCMotionMesh)
         ALLOCATE( MeshMapData%ED_P_2_NStC_P_N(j), MeshMapData%NStC_P_2_ED_P_N(j), STAT=ErrStat2 )
            IF ( ErrStat2 /= 0 ) THEN
               CALL SetErrStat( ErrID_Fatal, 'Error allocating MeshMapData%ED_P_2_NStC_P_N and MeshMapData%NStC_P_2_ED_P_N.', &
                               ErrStat, ErrMsg, RoutineName )
               RETURN
            END IF
         do j=1,size(SrvD%Input(1)%NStCMotionMesh)
            IF ( SrvD%Input(1)%NStCMotionMesh(j)%Committed ) THEN
               CALL MeshMapCreate( ED%y%NacelleMotion, SrvD%Input(1)%NStCMotionMesh(j), MeshMapData%ED_P_2_NStC_P_N(j), ErrStat2, ErrMsg2 )
                  CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':ED_2_SrvD_NacelleMotion' )
               CALL MeshMapCreate( SrvD%y%NStCLoadMesh(j), ED%Input(1)%NacelleLoads,  MeshMapData%NStC_P_2_ED_P_N(j), ErrStat2, ErrMsg2 )
                  CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':SrvD_2_ED_NacelleLoads' )
            ENDIF
         enddo
         CALL MeshCopy( ED%Input(1)%NacelleLoads, MeshMapData%u_ED_NacelleLoads, MESH_NEWCOPY, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_ED_NacelleLoads' )
      END IF
 
         !  Tower TMD
      IF ( ALLOCATED(SrvD%Input(1)%TStCMotionMesh) ) THEN
         j=size(SrvD%Input(1)%TStCMotionMesh)
         ALLOCATE( MeshMapData%ED_L_2_TStC_P_T(j), MeshMapData%TStC_P_2_ED_P_T(j), STAT=ErrStat2 )
            IF ( ErrStat2 /= 0 ) THEN
               CALL SetErrStat( ErrID_Fatal, 'Error allocating MeshMapData%ED_L_2_TStC_P_T and MeshMapData%TStC_P_2_ED_P_T.', &
                               ErrStat, ErrMsg, RoutineName )
               RETURN
            END IF
         do j=1,size(SrvD%Input(1)%TStCMotionMesh)
            IF ( SrvD%Input(1)%TStCMotionMesh(j)%Committed ) THEN
               CALL MeshMapCreate( ED%y%TowerLn2Mesh, SrvD%Input(1)%TStCMotionMesh(j), MeshMapData%ED_L_2_TStC_P_T(j), ErrStat2, ErrMsg2 )
                  CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':ED_2_SrvD_TowerMotion' )
               CALL MeshMapCreate( SrvD%y%TStCLoadMesh(j), ED%Input(1)%TowerPtLoads,  MeshMapData%TStC_P_2_ED_P_T(j), ErrStat2, ErrMsg2 )
                  CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':SrvD_2_ED_TowerLoad' )
               CALL MeshCopy ( ED%Input(1)%TowerPtLoads, MeshMapData%u_ED_TowerPtLoads, MESH_NEWCOPY, ErrStat2, ErrMsg2 )      
                  CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_ED_TowerPtLoads' )                 
            ENDIF
         enddo
      ENDIF

!-------------------------
!  ServoDyn <-> Blades
!-------------------------
      IF ( ALLOCATED(SrvD%Input(1)%BStCMotionMesh) ) THEN
         IF ( p_FAST%CompElast == Module_ED ) then       ! ElastoDyn Blades
            j=size(SrvD%Input(1)%BStCMotionMesh,2)
            ALLOCATE( MeshMapData%ED_L_2_BStC_P_B(NumBl,j), MeshMapData%BStC_P_2_ED_P_B(NumBl,j), MeshMapData%u_ED_BladePtLoads(NumBl), STAT=ErrStat2 )
               IF ( ErrStat2 /= 0 ) THEN
                  CALL SetErrStat( ErrID_Fatal, 'Error allocating MeshMapData%ED_L_2_BStC_P_B and MeshMapData%BStC_P_2_ED_P_B and MeshMapData%u_ED_BladePtLoads.', &
                                  ErrStat, ErrMsg, RoutineName )
                  RETURN
               END IF
            do j=1,size(SrvD%Input(1)%BStCMotionMesh,2)
               DO K = 1,NumBl
                  IF ( SrvD%Input(1)%BStCMotionMesh(K,j)%Committed ) THEN
                     CALL MeshMapCreate( ED%y%BladeLn2Mesh(K), SrvD%Input(1)%BStCMotionMesh(K,j), MeshMapData%ED_L_2_BStC_P_B(K,j), ErrStat2, ErrMsg2 )
                        CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':ED_L_2_BStC_P_B' )
                     CALL MeshMapCreate( SrvD%y%BStCLoadMesh(K,j), ED%Input(1)%BladePtLoads(K),  MeshMapData%BStC_P_2_ED_P_B(K,j), ErrStat2, ErrMsg2 )
                        CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':BStC_P_2_ED_P_B' )
                  END IF
               ENDDO
            enddo
            do K = 1,NumBl
               CALL MeshCopy ( ED%Input(1)%BladePtLoads(K), MeshMapData%u_ED_BladePtLoads(K), MESH_NEWCOPY, ErrStat2, ErrMsg2 )
                  CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_ED_BladePtLoads('//trim(num2lstr(j))//','//trim(num2lstr(k))//')' )
            enddo
         ELSEIF ( p_FAST%CompElast == Module_BD ) THEN      ! BeamDyn Blades
            j=size(SrvD%Input(1)%BStCMotionMesh,2)
            ALLOCATE( MeshMapData%BD_L_2_BStC_P_B(NumBl,j), MeshMapData%BStC_P_2_BD_P_B(NumBl,j), MeshMapData%u_BD_DistrLoad(NumBl), STAT=ErrStat2 )
               IF ( ErrStat2 /= 0 ) THEN
                  CALL SetErrStat( ErrID_Fatal, 'Error allocating MeshMapData%BD_L_2_BStC_P_B and MeshMapData%BStC_P_2_BD_P_B and MeshMapData%u_BD_DistrLoad.', &
                                  ErrStat, ErrMsg, RoutineName )
                  RETURN
               END IF
            do j=1,size(SrvD%Input(1)%BStCMotionMesh,2)
               DO K = 1,NumBl
                  IF ( SrvD%Input(1)%BStCMotionMesh(K,j)%Committed ) THEN
                     CALL MeshMapCreate( BD%y(k)%BldMotion, SrvD%Input(1)%BStCMotionMesh(K,j), MeshMapData%BD_L_2_BStC_P_B(K,j), ErrStat2, ErrMsg2 )
                        CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':BD_L_2_BStC_P_B' )
                     CALL MeshMapCreate( SrvD%y%BStCLoadMesh(K,j), BD%Input(1,k)%DistrLoad,  MeshMapData%BStC_P_2_BD_P_B(K,j), ErrStat2, ErrMsg2 )
                        CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':BStC_P_2_BD_P_B' )
                  END IF
               ENDDO
            enddo
            do K = 1,NumBl
               CALL MeshCopy ( BD%Input(1,k)%DistrLoad, MeshMapData%u_BD_DistrLoad(k), MESH_NEWCOPY, ErrStat2, ErrMsg2 )
                  CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_BD_DistrLoad('//trim(num2lstr(k))//')' )
            enddo
         ENDIF
      ENDIF

!-------------------------
!  ServoDyn <-> Platform
!-------------------------
      ! ServoDyn platform point mesh from ElastoDyn platform point mesh -- Motions passed to DLL
      IF ( SrvD%Input(1)%PtfmMotionMesh%Committed ) THEN
         CALL MeshMapCreate( PlatformMotion, SrvD%Input(1)%PtfmMotionMesh, MeshMapData%ED_P_2_SrvD_P_P, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':ED_P_2_SrvD_P_P' )
      ENDIF


      IF ( ALLOCATED(SrvD%Input(1)%SStCMotionMesh) ) THEN
         IF ( p_FAST%CompSub /= Module_SD ) THEN ! all of these get mapped to ElastoDyn ! (offshore floating with rigid substructure)
            j=size(SrvD%Input(1)%SStCMotionMesh)
            ALLOCATE( MeshMapData%SStC_P_P_2_ED_P(j), MeshMapData%ED_P_2_SStC_P_P(j), STAT=ErrStat2 )
               IF ( ErrStat2 /= 0 ) THEN
                  CALL SetErrStat( ErrID_Fatal, 'Error allocating MeshMapData%SStC_P_P_2_ED_P and MeshMapData%ED_P_2_SStC_P_P.', &
                                  ErrStat, ErrMsg, RoutineName )
                  RETURN
               END IF
            do j=1,size(SrvD%Input(1)%SStCMotionMesh)
               IF ( SrvD%Input(1)%SStCMotionMesh(j)%Committed ) THEN      ! Single point per SStC instance
                  ! ServoDyn SStC point mesh to/from ElastoDyn point mesh
                  CALL MeshMapCreate( PlatformMotion, SrvD%Input(1)%SStCMotionMesh(j), MeshMapData%ED_P_2_SStC_P_P(j), ErrStat2, ErrMsg2 )
                     CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':ED_P_2_SStC_P_P' )
                  CALL MeshMapCreate( SrvD%y%SStCLoadMesh(j), PlatformLoads, MeshMapData%SStC_P_P_2_ED_P(j), ErrStat2, ErrMsg2 )
                     CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':SStC_P_P_2_ED_P' )       
               ENDIF
            enddo
         ELSE  ! SubDyn is used
            j=size(SrvD%Input(1)%SStCMotionMesh)
            ALLOCATE( MeshMapData%SStC_P_P_2_SD_P(j), MeshMapData%SDy3_P_2_SStC_P_P(j), STAT=ErrStat2 )
               IF ( ErrStat2 /= 0 ) THEN
                  CALL SetErrStat( ErrID_Fatal, 'Error allocating MeshMapData%SStC_P_P_2_SD_P and MeshMapData%SDy3_P_2_SStC_P_P.', &
                                  ErrStat, ErrMsg, RoutineName )
                  RETURN
               END IF
            do j=1,size(SrvD%Input(1)%SStCMotionMesh)
               IF ( SrvD%Input(1)%SStCMotionMesh(j)%Committed ) THEN      ! Single point per SStC instance
                  ! ServoDyn SStC point mesh to/from SubDyn point mesh
                  CALL MeshMapCreate( SrvD%y%SStCLoadMesh(j), SD%Input(1)%LMesh, MeshMapData%SStC_P_P_2_SD_P(j), ErrStat2, ErrMsg2 )
                     CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':SStC_P_P_2_SD_P' )       
                  CALL MeshMapCreate( SD%y%y3Mesh, SrvD%Input(1)%SStCMotionMesh(j), MeshMapData%SDy3_P_2_SStC_P_P(j), ErrStat2, ErrMsg2 )
                     CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':SDy3_P_2_SStC_P_P' )
               ENDIF
            enddo
          ENDIF
      ENDIF
   ENDIF



!-------------------------
!  ElastoDyn <-> AeroDyn14
!-------------------------
   
   IF ( p_FAST%CompAero == Module_AD14 ) THEN ! ED-AD14
         
      ! Blade meshes: (allocate two mapping data structures to number of blades, then allocate data inside the structures)
      ! AD14 does not properly set up its blade meshes, so we can't use this
      !ALLOCATE( MeshMapData%BDED_L_2_AD_L_B(NumBl), MeshMapData%AD_L_2_BDED_B(NumBl), STAT=ErrStat2 )
      !   IF ( ErrStat2 /= 0 ) THEN
      !      CALL SetErrStat( ErrID_Fatal, 'Error allocating MeshMapData%BDED_L_2_AD_L_B and MeshMapData%AD_L_2_BDED_B.', &
      !                      ErrStat, ErrMsg, RoutineName )
      !      RETURN
      !   END IF
      !   
      !DO K=1,NumBl         
      !   CALL MeshMapCreate( AD14%y%OutputLoads(K), ED%Input(1)%BladePtLoads(K),  MeshMapData%AD_L_2_BDED_B(K), ErrStat2, ErrMsg2 )
      !      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':AD_L_2_BDED_B('//TRIM(Num2LStr(K))//')' )
      !END DO
         
      ! Tower mesh:
      IF ( AD14%Input(1)%Twr_InputMarkers%Committed ) THEN
         CALL MeshMapCreate( ED%y%TowerLn2Mesh, AD14%Input(1)%Twr_InputMarkers, MeshMapData%ED_L_2_AD_L_T, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':ED_2_AD_TowerMotion' )
         CALL MeshMapCreate( AD14%y%Twr_OutputLoads, ED%Input(1)%TowerPtLoads,  MeshMapData%AD_L_2_ED_P_T, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':AD_2_ED_TowerLoad' )
      END IF
               
      IF (ErrStat >= AbortErrLev ) RETURN
      
   ELSEIF ( p_FAST%CompAero == Module_AD ) THEN ! ED-AD and/or BD-AD

      ! allocate per-blade space for mapping to structural module
      
         ! Blade root meshes
      ALLOCATE( MeshMapData%ED_P_2_AD_P_R(NumBl), STAT=ErrStat2 )
         IF ( ErrStat2 /= 0 ) THEN
            CALL SetErrStat( ErrID_Fatal, 'Error allocating MeshMapData%ED_P_2_AD_P_R.', ErrStat, ErrMsg, RoutineName )
            RETURN
         END IF      
         
      ! Blade meshes: (allocate two mapping data structures to number of blades, then allocate data inside the structures)
      ALLOCATE( MeshMapData%BDED_L_2_AD_L_B(NumBl), MeshMapData%AD_L_2_BDED_B(NumBl), STAT=ErrStat2 )
         IF ( ErrStat2 /= 0 ) THEN
            CALL SetErrStat( ErrID_Fatal, 'Error allocating MeshMapData%BDED_L_2_AD_L_B and MeshMapData%AD_L_2_BDED_B.', &
                              ErrStat, ErrMsg, RoutineName )
            RETURN
         END IF
         
         
         
!-------------------------
!  ElastoDyn <-> AeroDyn
!-------------------------
         
         ! blade root meshes
      DO K=1,NumBl         
         CALL MeshMapCreate( ED%y%BladeRootMotion(K), AD%Input(1)%rotors(1)%BladeRootMotion(K), MeshMapData%ED_P_2_AD_P_R(K), ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':ED_2_AD_RootMotion('//TRIM(Num2LStr(K))//')' )
      END DO
      
      
         ! Hub point mesh
      CALL MeshMapCreate( ED%y%HubPtMotion, AD%Input(1)%rotors(1)%HubMotion, MeshMapData%ED_P_2_AD_P_H, ErrStat2, ErrMsg2 )
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':ED_2_AD_HubMotion' )
      
      
         ! Tower mesh:
      IF ( AD%Input(1)%rotors(1)%TowerMotion%Committed ) THEN
         CALL MeshMapCreate( ED%y%TowerLn2Mesh, AD%Input(1)%rotors(1)%TowerMotion, MeshMapData%ED_L_2_AD_L_T, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':ED_2_AD_TowerMotion' )
            
         IF ( AD%y%rotors(1)%TowerLoad%Committed ) THEN            
            CALL MeshMapCreate( AD%y%rotors(1)%TowerLoad, ED%Input(1)%TowerPtLoads,  MeshMapData%AD_L_2_ED_P_T, ErrStat2, ErrMsg2 )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':AD_2_ED_TowerLoad' )
         END IF         
      END IF
            
         ! Nacelle mesh:
      IF ( AD%Input(1)%rotors(1)%NacelleMotion%Committed ) THEN
         CALL MeshMapCreate( ED%y%NacelleMotion, AD%Input(1)%rotors(1)%NacelleMotion, MeshMapData%ED_P_2_AD_P_N, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':ED_2_AD_NacelleMotion' )
         CALL MeshMapCreate( AD%y%rotors(1)%NacelleLoad, ED%Input(1)%NacelleLoads,  MeshMapData%AD_P_2_ED_P_N, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':AD_2_ED_NacelleLoads' )
         if (.not. MeshMapData%u_ED_NacelleLoads%Committed ) then    ! May have been set for NStC intance
            CALL MeshCopy( ED%Input(1)%NacelleLoads, MeshMapData%u_ED_NacelleLoads, MESH_NEWCOPY, ErrStat2, ErrMsg2 )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_ED_NacelleLoads' )
         endif
      endif
      
      IF ( p_FAST%CompElast == Module_ED ) then
         
            ! Blade meshes: 
         DO K=1,NumBl         
            CALL MeshMapCreate( ED%y%BladeLn2Mesh(K), AD%Input(1)%rotors(1)%BladeMotion(K), MeshMapData%BDED_L_2_AD_L_B(K), ErrStat2, ErrMsg2 )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':ED_2_AD_BladeMotion('//TRIM(Num2LStr(K))//')' )
            CALL MeshMapCreate( AD%y%rotors(1)%BladeLoad(K), ED%Input(1)%BladePtLoads(K),  MeshMapData%AD_L_2_BDED_B(K), ErrStat2, ErrMsg2 )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':AD_2_ED_BladeLoad('//TRIM(Num2LStr(K))//')' )
         END DO
         
      ELSEIF ( p_FAST%CompElast == Module_BD ) then
         
!-------------------------
!  BeamDyn <-> AeroDyn
!-------------------------
            
         ! connect AD mesh with BeamDyn
         DO K=1,NumBl         
            CALL MeshMapCreate( BD%y(k)%BldMotion, AD%Input(1)%rotors(1)%BladeMotion(K), MeshMapData%BDED_L_2_AD_L_B(K), ErrStat2, ErrMsg2 )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':BD_2_AD_BladeMotion('//TRIM(Num2LStr(K))//')' )
            CALL MeshMapCreate( AD%y%rotors(1)%BladeLoad(K), BD%Input(1,k)%DistrLoad,  MeshMapData%AD_L_2_BDED_B(K), ErrStat2, ErrMsg2 )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':AD_2_BD_BladeLoad('//TRIM(Num2LStr(K))//')' )
         END DO
         
!-------------------------
!  BeamDyn <-> BeamDyn
!-------------------------
         if (.not. p_FAST%BD_OutputSibling) then            
            
            ! Blade meshes for load transfer: (allocate meshes at BD input locations for motions transferred from BD output locations)                  
            ALLOCATE( MeshMapData%BD_L_2_BD_L(NumBl), MeshMapData%y_BD_BldMotion_4Loads(NumBl), STAT=ErrStat2 )
               IF ( ErrStat2 /= 0 ) THEN
                  CALL SetErrStat( ErrID_Fatal, 'Error allocating MeshMapData%BD_L_2_BD_L and MeshMapData%y_BD_BldMotion_4Loads.', &
                                    ErrStat, ErrMsg, RoutineName )
                  RETURN
               END IF
         
            DO K=1,NumBl         
                  ! create the new mesh:
               CALL MeshCopy ( SrcMesh  = BD%Input(1,k)%DistrLoad &
                              , DestMesh = MeshMapData%y_BD_BldMotion_4Loads(k) &
                              , CtrlCode = MESH_SIBLING     &
                              , IOS      = COMPONENT_OUTPUT &
                              , TranslationDisp = .TRUE.    &
                              , Orientation     = .TRUE.    &
                              , RotationVel     = .TRUE.    &
                              , TranslationVel  = .TRUE.    &
                              , RotationAcc     = .TRUE.    &
                              , TranslationAcc  = .TRUE.    &
                              , ErrStat  = ErrStat2         &
                              , ErrMess  = ErrMsg2          ) 
                  CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )         
                  IF (ErrStat >= AbortErrLev) RETURN
                                    
                  ! create the mapping:
               CALL MeshMapCreate( BD%y(k)%BldMotion, MeshMapData%y_BD_BldMotion_4Loads(k), MeshMapData%BD_L_2_BD_L(K), ErrStat2, ErrMsg2 )
                  CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':BD_2_BD_BladeMotion('//TRIM(Num2LStr(K))//')' )         
            END DO
            
         end if !.not. p_FAST%BD_OutputSibling
      
      END IF ! CompElast
      
   END IF ! AeroDyn/AeroDyn14 to structural code
   
      
      
   IF ( p_FAST%CompHydro == Module_HD ) THEN ! HydroDyn-{ElastoDyn or SubDyn}
    
      ! Regardless of the offshore configuration, ED platform motions will be mapped to the PRPMesh of HD
      ! we're just going to assume PlatformLoads and PlatformMotion are committed
      CALL MeshMapCreate( PlatformMotion, HD%Input(1)%PRPMesh, MeshMapData%ED_P_2_HD_PRP_P, ErrStat2, ErrMsg2 )
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':ED_P_2_HD_PRP_P' )
         
!-------------------------
!  HydroDyn <-> ElastoDyn
!-------------------------
      IF ( p_FAST%CompSub /= Module_SD ) THEN ! all of these get mapped to ElastoDyn ! (offshore floating with rigid substructure)
      
         IF ( HD%y%WAMITMesh%Committed  ) THEN ! meshes for floating
               ! HydroDyn WAMIT point mesh to/from ElastoDyn point mesh
            CALL MeshMapCreate( HD%y%WAMITMesh, PlatformLoads, MeshMapData%HD_W_P_2_ED_P, ErrStat2, ErrMsg2 )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':HD_W_P_2_ED_P' )       
            CALL MeshMapCreate( PlatformMotion, HD%Input(1)%WAMITMesh, MeshMapData%ED_P_2_HD_W_P, ErrStat2, ErrMsg2 )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':ED_P_2_HD_W_P' )
         END IF            
            
            ! ElastoDyn point mesh HydroDyn Morison point mesh (ED sets inputs, but gets outputs from HD%y%WAMITMesh in floating case)
         IF ( HD%Input(1)%Morison%Mesh%Committed  ) THEN  
            CALL MeshMapCreate( HD%y%Morison%Mesh, PlatformLoads, MeshMapData%HD_M_P_2_ED_P, ErrStat2, ErrMsg2 )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':HD_M_P_2_ED_P' )
            CALL MeshMapCreate( PlatformMotion,  HD%Input(1)%Morison%Mesh, MeshMapData%ED_P_2_HD_M_P, ErrStat2, ErrMsg2 )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':ED_P_2_HD_M_P' )                  
         END IF
                        
      ELSE ! these get mapped to ElastoDyn AND SubDyn (in ED_SD_HD coupling)  ! offshore with substructure flexibility
   
             
            
!-------------------------
!  HydroDyn <-> SubDyn
!-------------------------                     
                     
            ! HydroDyn Morison point mesh to SubDyn point mesh
         IF ( HD%y%Morison%Mesh%Committed ) THEN
            
            CALL MeshMapCreate( HD%y%Morison%Mesh, SD%Input(1)%LMesh,  MeshMapData%HD_M_P_2_SD_P, ErrStat2, ErrMsg2 )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':HD_M_P_2_SD_P' )                  
            CALL MeshMapCreate( SD%y%y2Mesh,  HD%Input(1)%Morison%Mesh, MeshMapData%SD_P_2_HD_M_P, ErrStat2, ErrMsg2 )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':SD_P_2_HD_M_P' )                                           
         END IF
        
            ! HydroDyn WAMIT point mesh to SD point mesh 
         IF ( HD%y%WAMITMesh%Committed  ) THEN
  
            CALL MeshMapCreate( HD%y%WAMITMesh, SD%Input(1)%LMesh, MeshMapData%HD_W_P_2_SD_P, ErrStat2, ErrMsg2 )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':HD_W_P_2_SD_P' ) 
            CALL MeshMapCreate( SD%y%y2Mesh,  HD%Input(1)%WAMITMesh, MeshMapData%SD_P_2_HD_W_P, ErrStat2, ErrMsg2 )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':SD_P_2_HD_W_P' )
                  
         END IF             
         
      END IF ! HydroDyn-SubDyn
      
      IF (ErrStat >= AbortErrLev ) RETURN
    
   END IF !HydroDyn-{ElastoDyn or SubDyn}

      
!-------------------------
!  ElastoDyn <-> SubDyn
!-------------------------
   IF ( p_FAST%CompSub == Module_SD ) THEN
                           
      ! NOTE: the MeshMapCreate routine returns fatal errors if either mesh is not committed
      
         ! SubDyn transition piece point mesh to/from ElastoDyn point mesh
      CALL MeshMapCreate( SD%y%Y1mesh, PlatformLoads,  MeshMapData%SD_TP_2_ED_P, ErrStat2, ErrMsg2 )
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':SD_TP_2_Ptfm' )                  
      CALL MeshMapCreate( PlatformMotion, SD%Input(1)%TPMesh,  MeshMapData%ED_P_2_SD_TP, ErrStat2, ErrMsg2 )
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':Ptfm_2_SD_TP' )                  
   
!-------------------------
!  ElastoDyn <-> ExtPtfm
!-------------------------
   ELSE IF ( p_FAST%CompSub == Module_ExtPtfm ) THEN
                           
      ! NOTE: the MeshMapCreate routine returns fatal errors if either mesh is not committed
      
         ! ExtPtfm PtfmMesh point mesh to/from ElastoDyn point mesh
      CALL MeshMapCreate( ExtPtfm%y%PtfmMesh, PlatformLoads,  MeshMapData%SD_TP_2_ED_P, ErrStat2, ErrMsg2 )
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':SD_TP_2_Ptfm' )                  
      CALL MeshMapCreate( PlatformMotion, ExtPtfm%Input(1)%PtfmMesh,  MeshMapData%ED_P_2_SD_TP, ErrStat2, ErrMsg2 )
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':Ptfm_2_SD_TP' )                  
   
   END IF ! SubDyn-ElastoDyn      
      
      
   IF ( p_FAST%CompMooring == Module_MAP ) THEN
      
      IF ( p_FAST%CompSub == Module_SD ) THEN
!-------------------------
!  SubDyn <-> MAP
!-------------------------              
      ! MAP point mesh to/from SubDyn point mesh
         CALL MeshMapCreate( MAPp%y%PtFairleadLoad, SD%Input(1)%LMesh,  MeshMapData%Mooring_P_2_SD_P, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':Mooring_P_2_SD_P' )                  
         CALL MeshMapCreate( SD%y%y3Mesh, MAPp%Input(1)%PtFairDisplacement,  MeshMapData%SDy3_P_2_Mooring_P, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':SDy3_P_2_Mooring_P' )                
      ELSE        
!-------------------------
!  ElastoDyn <-> MAP
!-------------------------            
            ! MAP point mesh to/from ElastoDyn point mesh
         CALL MeshMapCreate( MAPp%y%PtFairleadLoad, PlatformLoads,  MeshMapData%Mooring_P_2_ED_P, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':Mooring_P_2_Ptfm' )                  
         CALL MeshMapCreate( PlatformMotion, MAPp%Input(1)%PtFairDisplacement,  MeshMapData%ED_P_2_Mooring_P, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':Ptfm_2_Mooring_P' )              
      END IF ! p_FAST%CompSub == Module_SD
      
   ELSEIF ( p_FAST%CompMooring == Module_MD ) THEN
      IF ( p_FAST%CompSub == Module_SD ) THEN
!-------------------------
!  SubDyn <-> MoorDyn
!-------------------------              
      ! MoorDyn point mesh to/from SubDyn point mesh
         CALL MeshMapCreate( MD%y%PtFairleadLoad, SD%Input(1)%LMesh,  MeshMapData%Mooring_P_2_SD_P, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':Mooring_P_2_SD_P' )                  
         CALL MeshMapCreate( SD%y%y3Mesh, MD%Input(1)%PtFairleadDisplacement,  MeshMapData%SDy3_P_2_Mooring_P, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':SDy3_P_2_Mooring_P' )              
      ELSE        
!-------------------------
!  ElastoDyn <-> MoorDyn
!-------------------------          
            ! MoorDyn point mesh to/from ElastoDyn point mesh
         CALL MeshMapCreate( MD%y%PtFairleadLoad, PlatformLoads,  MeshMapData%Mooring_P_2_ED_P, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':Mooring_P_2_Ptfm' )                  
         CALL MeshMapCreate( PlatformMotion, MD%Input(1)%PtFairleadDisplacement,  MeshMapData%ED_P_2_Mooring_P, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':Ptfm_2_Mooring_P' )                  
      END IF ! p_FAST%CompSub == Module_SD
      
   ELSEIF ( p_FAST%CompMooring == Module_FEAM ) THEN
      IF ( p_FAST%CompSub == Module_SD ) THEN
!-------------------------
!  SubDyn <-> FEAMooring
!-------------------------              
         ! FEAMooring point mesh to/from SubDyn point mesh
         CALL MeshMapCreate( FEAM%y%PtFairleadLoad, SD%Input(1)%LMesh,  MeshMapData%Mooring_P_2_SD_P, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':Mooring_P_2_SD_P' )                  
         CALL MeshMapCreate( SD%y%y3Mesh, FEAM%Input(1)%PtFairleadDisplacement,  MeshMapData%SDy3_P_2_Mooring_P, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':SDy3_P_2_Mooring_P' )              
      ELSE  
!-------------------------
!  ElastoDyn <-> FEAMooring
!-------------------------          
         ! FEAMooring point mesh to/from ElastoDyn point mesh
         CALL MeshMapCreate( FEAM%y%PtFairleadLoad, PlatformLoads,  MeshMapData%Mooring_P_2_ED_P, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':Mooring_P_2_Ptfm' )                  
         CALL MeshMapCreate( PlatformMotion, FEAM%Input(1)%PtFairleadDisplacement,  MeshMapData%ED_P_2_Mooring_P, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':Ptfm_2_Mooring_P' )                           
      END IF ! p_FAST%CompSub == Module_SD  
      
   ELSEIF ( p_FAST%CompMooring == Module_Orca ) THEN
      
!-------------------------
!  ElastoDyn <-> OrcaFlex
!-------------------------      
      
         ! OrcaFlex point mesh to/from ElastoDyn point mesh
      CALL MeshMapCreate( Orca%y%PtfmMesh, PlatformLoads,  MeshMapData%Mooring_P_2_ED_P, ErrStat2, ErrMsg2 )
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':Mooring_P_2_Ptfm' )                  
      CALL MeshMapCreate( PlatformMotion, Orca%Input(1)%PtfmMesh,  MeshMapData%ED_P_2_Mooring_P, ErrStat2, ErrMsg2 )
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':Ptfm_2_Mooring_P' )                           

   END IF   ! MAP-ElastoDyn ; FEAM-ElastoDyn; Orca-ElastoDyn
            
         
!-------------------------
!  SubDyn <-> IceFloe
!-------------------------      
      
   IF ( p_FAST%CompIce == Module_IceF ) THEN
   
         ! IceFloe iceMesh point mesh to SubDyn LMesh point mesh              
      CALL MeshMapCreate( IceF%y%iceMesh, SD%Input(1)%LMesh,  MeshMapData%IceF_P_2_SD_P, ErrStat2, ErrMsg2 )
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':IceF_P_2_SD_P' )                  
         ! SubDyn y3Mesh point mesh to IceFloe iceMesh point mesh 
      CALL MeshMapCreate( SD%y%y3Mesh, IceF%Input(1)%iceMesh,  MeshMapData%SDy3_P_2_IceF_P, ErrStat2, ErrMsg2 )
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':SDy3_P_2_IceF_P' )                  
                              
!-------------------------
!  SubDyn <-> IceDyn
!-------------------------      
      
   ELSEIF ( p_FAST%CompIce == Module_IceD ) THEN
   
      ALLOCATE( MeshMapData%IceD_P_2_SD_P(   p_FAST%numIceLegs )  , & 
                MeshMapData%SDy3_P_2_IceD_P( p_FAST%numIceLegs )  , Stat=ErrStat2 )
      IF (ErrStat2 /= 0 ) THEN
         CALL SetErrStat( ErrID_Fatal, 'Unable to allocate IceD_P_2_SD_P and SDy3_P_2_IceD_P', ErrStat, ErrMsg, RoutineName )                  
         RETURN
      END IF
         
      DO i = 1,p_FAST%numIceLegs
            
            ! IceDyn PointMesh point mesh to SubDyn LMesh point mesh              
         CALL MeshMapCreate( IceD%y(i)%PointMesh, SD%Input(1)%LMesh,  MeshMapData%IceD_P_2_SD_P(i), ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':IceD_P_2_SD_P('//TRIM(num2LStr(i))//')' )                  
            ! SubDyn y3Mesh point mesh to IceDyn PointMesh point mesh 
         CALL MeshMapCreate( SD%y%y3Mesh, IceD%Input(1,i)%PointMesh,  MeshMapData%SDy3_P_2_IceD_P(i), ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':SDy3_P_2_IceD_P('//TRIM(num2LStr(i))//')' )                  
               
      END DO
                        
   END IF   ! SubDyn-IceFloe
      
   IF (ErrStat >= AbortErrLev ) RETURN   
      
   !............................................................................................................................
   ! Initialize the Jacobian structures:
   !............................................................................................................................
   !IF ( p_FAST%TurbineType == Type_Offshore_Fixed ) THEN 
   IF ( p_FAST%CompSub /= Module_None .OR. (p_FAST%CompElast == Module_BD .and. BD_Solve_Option1) .or. p_FAST%CompMooring == Module_Orca) THEN  !.OR. p_FAST%CompHydro == Module_HD ) THEN         
      CALL Init_FullOpt1_Jacobian( p_FAST, MeshMapData, ED%Input(1)%PlatformPtMesh, SD%Input(1)%TPMesh, SD%Input(1)%LMesh, &
                                    HD%Input(1)%Morison%Mesh, HD%Input(1)%WAMITMesh, &
                                    ED%Input(1)%HubPtLoad, BD%Input(1,:), Orca%Input(1)%PtfmMesh, ExtPtfm%Input(1)%PtfmMesh, ErrStat2, ErrMsg2)
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )                 
   ELSEIF ( p_FAST%CompHydro == Module_HD ) THEN
         CALL AllocAry( MeshMapData%Jacobian_Opt1, SizeJac_ED_HD, SizeJac_ED_HD, 'Jacobian for Ptfm-HD coupling', ErrStat2, ErrMsg2 )
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )                 
   END IF
   
   IF ( ALLOCATED( MeshMapData%Jacobian_Opt1 ) ) THEN   
      CALL AllocAry( MeshMapData%Jacobian_pivot, SIZE(MeshMapData%Jacobian_Opt1), 'Pivot array for Jacobian LU decomposition', ErrStat2, ErrMsg2 )
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )                 
   END IF
   
   IF (ErrStat >= AbortErrLev ) RETURN   
   
   !............................................................................................................................
   ! reset the remap flags (do this before making the copies else the copies will always have remap = true)
   !............................................................................................................................
   CALL ResetRemapFlags(p_FAST, ED, BD, AD14, AD, HD, SD, ExtPtfm, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD )      
            
   !............................................................................................................................
   ! initialize the temporary input meshes (for input-output solves in Solve Option 1):
   ! (note that we do this after ResetRemapFlags() so that the copies have remap=false)
   !............................................................................................................................
   IF ( p_FAST%CompHydro == Module_HD .OR. p_FAST%CompSub /= Module_None .OR. (p_FAST%CompElast == Module_BD .and. BD_Solve_Option1) &
         .or. p_FAST%CompMooring == Module_Orca) THEN
                  
         ! Temporary meshes for transfering inputs to ED, HD, BD, Orca, and SD
      CALL MeshCopy ( ED%Input(1)%HubPtLoad, MeshMapData%u_ED_HubPtLoad, MESH_NEWCOPY, ErrStat2, ErrMsg2 )      
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_ED_HubPtLoad' )                 
            
      CALL MeshCopy ( ED%Input(1)%PlatformPtMesh, MeshMapData%u_ED_PlatformPtMesh, MESH_NEWCOPY, ErrStat2, ErrMsg2 )      
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_ED_PlatformPtMesh' )                 

      CALL MeshCopy ( ED%Input(1)%PlatformPtMesh, MeshMapData%u_ED_PlatformPtMesh_2, MESH_NEWCOPY, ErrStat2, ErrMsg2 )      
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_ED_PlatformPtMesh_2' )                 

             
      IF ( p_FAST%CompElast == Module_BD ) THEN
      
            ! Temporary meshes for transfering inputs to ED and BD
         CALL MeshCopy ( ED%Input(1)%HubPtLoad, MeshMapData%u_ED_HubPtLoad_2, MESH_NEWCOPY, ErrStat2, ErrMsg2 )      
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_ED_HubPtLoad_2' )     
            
         allocate( MeshMapData%u_BD_RootMotion( p_FAST%nBeams ), STAT = ErrStat2 )
         if (ErrStat2 /= 0) then
            CALL SetErrStat( ErrID_Fatal, "Error allocating u_BD_RootMotion", ErrStat, ErrMsg, RoutineName )     
            return
         end if
         
         do k=1,p_FAST%nBeams
            CALL MeshCopy ( BD%Input(1,k)%RootMotion, MeshMapData%u_BD_RootMotion(k), MESH_NEWCOPY, ErrStat2, ErrMsg2 )      
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_BD_RootMotion('//trim(num2lstr(k))//')' )                 
         end do
         
                              
      END IF         
         
      IF ( p_FAST%CompSub == Module_SD ) THEN
         
         CALL MeshCopy ( SD%Input(1)%TPMesh, MeshMapData%u_SD_TPMesh, MESH_NEWCOPY, ErrStat2, ErrMsg2 )      
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_SD_TPMesh' )                 
               
         IF ( p_FAST%CompHydro == Module_HD ) THEN
               
            CALL MeshCopy ( SD%Input(1)%LMesh, MeshMapData%u_SD_LMesh, MESH_NEWCOPY, ErrStat2, ErrMsg2 )      
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_SD_LMesh' )                 

            CALL MeshCopy ( SD%Input(1)%LMesh, MeshMapData%u_SD_LMesh_2, MESH_NEWCOPY, ErrStat2, ErrMsg2 )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_SD_LMesh_2' )
                              
         END IF

      ELSE IF ( p_FAST%CompSub == Module_ExtPtfm ) THEN
         
         CALL MeshCopy ( ExtPtfm%Input(1)%PtfmMesh, MeshMapData%u_ExtPtfm_PtfmMesh, MESH_NEWCOPY, ErrStat2, ErrMsg2 )      
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_ExtPtfm_PtfmMesh' ) 
            
      END IF
         
      IF ( p_FAST%CompHydro == Module_HD ) THEN
         
         !TODO: GJH Is this needed, I created it as a place holder, 5/11/2020
         !CALL MeshCopy ( HD%Input(1)%PRPMesh, MeshMapData%u_HD_PRP_Mesh, MESH_NEWCOPY, ErrStat2, ErrMsg2 )      
         !   CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_HD_PRP_Mesh' )
            
         CALL MeshCopy ( HD%Input(1)%WAMITMesh, MeshMapData%u_HD_W_Mesh, MESH_NEWCOPY, ErrStat2, ErrMsg2 )      
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_HD_W_Mesh' )                 
                  
         CALL MeshCopy ( HD%Input(1)%Morison%Mesh, MeshMapData%u_HD_M_Mesh, MESH_NEWCOPY, ErrStat2, ErrMsg2 )      
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_HD_M_Mesh' )                         
                                    
      END IF
          
      IF ( p_FAST%CompMooring == Module_Orca ) THEN
         
         CALL MeshCopy ( Orca%Input(1)%PtfmMesh, MeshMapData%u_Orca_PtfmMesh, MESH_NEWCOPY, ErrStat2, ErrMsg2 )      
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_Orca_PtfmMesh' )                 
                              
      END IF
      
      
   ELSEIF ( p_FAST%CompSub /= Module_SD ) THEN     ! Platform loads from SrvD Structural control (TMDs) if not SD
      IF ( ALLOCATED(SrvD%Input(1)%SStCMotionMesh) ) THEN ! Platform TMD loads
         CALL MeshCopy ( ED%Input(1)%PlatformPtMesh, MeshMapData%u_ED_PlatformPtMesh, MESH_NEWCOPY, ErrStat2, ErrMsg2 )
            CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName//':u_ED_PlatformPtMesh' )
      ENDIF

   END IF
   
   

   !............................................................................................................................

      
END SUBROUTINE InitModuleMappings
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine initializes the array that maps rows/columns of the Jacobian to specific mesh fields.
!! Do not change the order of this packing without changing subroutine Create_FullOpt1_UVector()!
SUBROUTINE Init_FullOpt1_Jacobian( p_FAST, MeshMapData, ED_PlatformPtMesh, SD_TPMesh, SD_LMesh, HD_M_Mesh,  &
                                   HD_WAMIT_Mesh, ED_HubPtLoad, u_BD, Orca_PtfmMesh, ExtPtfm_PtfmMesh, ErrStat, ErrMsg)

   TYPE(FAST_ParameterType)          , INTENT(INOUT) :: p_FAST                !< FAST parameters               
   TYPE(FAST_ModuleMapType)          , INTENT(INOUT) :: MeshMapData           !< data that maps meshes together
   
      ! input meshes for each of the 4 modules:
   TYPE(MeshType)                    , INTENT(IN   ) :: ED_PlatformPtMesh     !< ElastoDyn's PlatformPtMesh
   TYPE(MeshType)                    , INTENT(IN   ) :: ED_HubPtLoad          !< ElastoDyn's HubPtLoad mesh
   TYPE(MeshType)                    , INTENT(IN   ) :: SD_TPMesh             !< SubDyn's TP (transition piece) mesh
   TYPE(MeshType)                    , INTENT(IN   ) :: SD_LMesh              !< SubDyn's LMesh
   TYPE(MeshType)                    , INTENT(IN   ) :: HD_M_Mesh             !< HydroDyn's Morison Lumped Mesh
   TYPE(MeshType)                    , INTENT(IN   ) :: HD_WAMIT_Mesh         !< HydroDyn's WAMIT mesh
   TYPE(BD_InputType)                , INTENT(IN   ) :: u_BD(:)               !< inputs for each instance of the BeamDyn module (for the RootMotion meshes)
   TYPE(MeshType)                    , INTENT(IN   ) :: Orca_PtfmMesh         !< OrcaFlex interface PtfmMesh
   TYPE(MeshType)                    , INTENT(IN   ) :: ExtPtfm_PtfmMesh      !< ExtPtfm_MCKF interface PtfmMesh
   
   INTEGER(IntKi)                    , INTENT(  OUT) :: ErrStat               !< Error status of the operation
   CHARACTER(*)                      , INTENT(  OUT) :: ErrMsg                !< Error message if ErrStat /= ErrID_None
   
   CHARACTER(*), PARAMETER                           :: RoutineName = 'Init_FullOpt1_Jacobian'
   
      ! local variables:
   INTEGER(IntKi)                :: i, j, k, index
   
   ErrStat = ErrID_None
   ErrMsg  = ""
   
      ! determine how many inputs there are between the 6 modules (ED, SD, HD, BD, Orca, ExtPtfm)
   p_FAST%SizeJac_Opt1 = 0 ! initialize whole array
   
   if (p_FAST%CompHydro == Module_HD .or. p_FAST%CompSub /= Module_None .or. p_FAST%CompMooring == Module_Orca) then
      p_FAST%SizeJac_Opt1(2) = ED_PlatformPtMesh%NNodes*6        ! ED inputs: 3 forces and 3 moments per node (only 1 node)
   else
      p_FAST%SizeJac_Opt1(2) = 0
   end if
   
                  
   p_FAST%SizeJac_Opt1(3) = SD_TPMesh%NNodes*6                    ! SD inputs: 6 accelerations per node (size of SD input from ED) 
   IF ( p_FAST%CompHydro == Module_HD ) THEN   
      p_FAST%SizeJac_Opt1(3) = p_FAST%SizeJac_Opt1(3) &   
                                    + SD_LMesh%NNodes *6          ! SD inputs: 6 loads per node (size of SD input from HD)       
   END IF
               
   p_FAST%SizeJac_Opt1(4) = HD_M_Mesh%NNodes *6 &                 ! HD inputs: 6 accelerations per node (on each Morison mesh) 
                                 + HD_WAMIT_Mesh%NNodes*6         ! HD inputs: 6 accelerations per node (on the WAMIT mesh)      
   
   IF ( p_FAST%CompElast == Module_BD .and. BD_Solve_Option1) THEN   
      p_FAST%SizeJac_Opt1(2) = p_FAST%SizeJac_Opt1(2) &   
                                     + ED_HubPtLoad%NNodes *6     ! ED inputs: 6 loads per node (size of ED input from BD)
      
      p_FAST%SizeJac_Opt1(5:7) = 0 ! assumes a max of 3 blades
      do k=1,size(u_BD)
         p_FAST%SizeJac_Opt1(4+k) = u_BD(k)%RootMotion%NNodes *6   ! BD inputs: 6 accelerations per node (size of BD input from ED)         
      end do
            
   END IF
        
   if ( p_FAST%CompMooring == Module_Orca ) then   
      p_FAST%SizeJac_Opt1(8) = Orca_PtfmMesh%NNodes*6
   else
      p_FAST%SizeJac_Opt1(8) = 0
   end if
   
   if ( p_FAST%CompSub == Module_ExtPtfm ) then   
      p_FAST%SizeJac_Opt1(9) = ExtPtfm_PtfmMesh%NNodes*6
   else
      p_FAST%SizeJac_Opt1(9) = 0
   end if
   
                       
                              
   p_FAST%SizeJac_Opt1(1) = sum( p_FAST%SizeJac_Opt1 )   ! all the inputs from these modules
                  

      ! allocate matrix to store jacobian 
   CALL AllocAry( MeshMapData%Jacobian_Opt1, p_FAST%SizeJac_Opt1(1), p_FAST%SizeJac_Opt1(1), "Jacobian for full option 1", ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
         
      ! allocate matrix to store index to help us figure out what the ith value of the u vector really means
   ALLOCATE ( MeshMapData%Jac_u_indx( p_FAST%SizeJac_Opt1(1), 3 ), STAT = ErrStat )
      IF ( ErrStat /= 0 ) THEN
         ErrStat = ErrID_Fatal
         ErrMsg = 'Cannot allocate Jac_u_indx.'
         RETURN
      END IF
         
   ! fill matrix to store index to help us figure out what the ith value of the u vector really means
   ! ( see Create_FullOpt1_UVector() ... these MUST match )
   ! column 1 indicates module's mesh and field
   ! column 2 indicates the first index of the acceleration/load field
   ! column 3 is the node
      
   !...............
   ! ED inputs:   
   !...............
   
   index = 1
   if (p_FAST%CompHydro == Module_HD .or. p_FAST%CompSub /= Module_None .or. p_FAST%CompMooring == Module_Orca) then
   
      do i=1,ED_PlatformPtMesh%NNodes
         do j=1,3
            MeshMapData%Jac_u_indx(index,1) =  1 !Module/Mesh/Field: u_ED%PlatformPtMesh%Force = 1
            MeshMapData%Jac_u_indx(index,2) =  j !index:  j
            MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
            index = index + 1
         end do !j      
      end do !i
   
      do i=1,ED_PlatformPtMesh%NNodes
         do j=1,3
            MeshMapData%Jac_u_indx(index,1) =  2 !Module/Mesh/Field: u_ED%PlatformPtMesh%Moment = 2
            MeshMapData%Jac_u_indx(index,2) =  j !index:  j
            MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
            index = index + 1
         end do !j      
      end do !i
      
   end if
   
   
   if (p_FAST%CompElast == Module_BD .and. BD_Solve_Option1) then
      
      do i=1,ED_HubPtLoad%NNodes
         do j=1,3
            MeshMapData%Jac_u_indx(index,1) =  3 !Module/Mesh/Field: u_ED%HubPtMesh%Force = 3
            MeshMapData%Jac_u_indx(index,2) =  j !index:  j
            MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
            index = index + 1
         end do !j      
      end do !i
      
      
      do i=1,ED_HubPtLoad%NNodes
         do j=1,3
            MeshMapData%Jac_u_indx(index,1) =  4 !Module/Mesh/Field: u_ED%HubPtMesh%Moment = 4
            MeshMapData%Jac_u_indx(index,2) =  j !index:  j
            MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
            index = index + 1
         end do !j      
      end do !i

   end if
   
      
   !...............
   ! SD inputs:   
   !...............
      
   ! SD_TPMesh                        
   do i=1,SD_TPMesh%NNodes
      do j=1,3
         MeshMapData%Jac_u_indx(index,1) =  5 !Module/Mesh/Field: u_SD%TPMesh%TranslationAcc = 5
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1                  
      end do !j                             
   end do !i                                
                                            
   do i=1,SD_TPMesh%NNodes                  
      do j=1,3                              
         MeshMapData%Jac_u_indx(index,1) =  6 !Module/Mesh/Field:  u_SD%TPMesh%RotationAcc = 6
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1
      end do !j      
   end do !i   
   
   IF ( p_FAST%CompHydro == Module_HD ) THEN   ! this SD mesh linked only when HD is enabled
   
      ! SD_LMesh
      do i=1,SD_LMesh%NNodes
         do j=1,3
            MeshMapData%Jac_u_indx(index,1) =  7 !Module/Mesh/Field: u_SD%LMesh%Force = 7
            MeshMapData%Jac_u_indx(index,2) =  j !index:  j
            MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
            index = index + 1                  
         end do !j                             
      end do !i                                
                                            
      do i=1,SD_LMesh%NNodes                   
         do j=1,3                              
            MeshMapData%Jac_u_indx(index,1) =  8 !Module/Mesh/Field: u_SD%LMesh%Moment = 8
            MeshMapData%Jac_u_indx(index,2) =  j !index:  j
            MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
            index = index + 1
         end do !j      
      end do !i 
      
   END IF
   
   !...............
   ! HD inputs:
   !...............
         
   !(Morison%Mesh)
   do i=1,HD_M_Mesh%NNodes
      do j=1,3
         MeshMapData%Jac_u_indx(index,1) =  9 !Module/Mesh/Field: u_HD%Morison%Mesh%TranslationAcc = 9
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1
      end do !j      
   end do !i
   
   do i=1,HD_M_Mesh%NNodes
      do j=1,3
         MeshMapData%Jac_u_indx(index,1) = 10 !Module/Mesh/Field:  u_HD%Morison%Mesh%RotationAcc = 10
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1
      end do !j      
   end do !i     
   
   
   !(Mesh)
   do i=1,HD_WAMIT_Mesh%NNodes
      do j=1,3
         MeshMapData%Jac_u_indx(index,1) = 11 !Module/Mesh/Field: u_HD%WAMITMesh%TranslationAcc = 11
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1
      end do !j      
   end do !i
   
   do i=1,HD_WAMIT_Mesh%NNodes
      do j=1,3
         MeshMapData%Jac_u_indx(index,1) = 12 !Module/Mesh/Field:  u_HD%WAMITMesh%RotationAcc = 12
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1
      end do !j      
   end do !i        
   
   !...............
   ! BD inputs:
   !...............
   
   if (p_FAST%CompElast == Module_BD .and. BD_Solve_Option1) then
                 
      do k=1,size(u_BD)
         
         do i=1,u_BD(k)%RootMotion%NNodes
            do j=1,3
               MeshMapData%Jac_u_indx(index,1) =  11 + 2*k !Module/Mesh/Field: u_BD(k)%RootMotion%TranslationAcc = 13 (k=1), 15 (k=2), 17 (k=3)
               MeshMapData%Jac_u_indx(index,2) =  j !index:  j
               MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
               index = index + 1
            end do !j      
         end do !i
      
         do i=1,u_BD(k)%RootMotion%NNodes
            do j=1,3
               MeshMapData%Jac_u_indx(index,1) =  12 + 2*k !Module/Mesh/Field: u_BD(k)%RootMotion%RotationAcc = 14 (k=1), 16 (k=2), 18 (k=3)
               MeshMapData%Jac_u_indx(index,2) =  j !index:  j
               MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
               index = index + 1
            end do !j      
         end do !i
                  
      end do !k
                  
   end if
   
   !...............
   ! Orca inputs:   
   !...............
      
   ! Orca_PtfmMesh
   do i=1,Orca_PtfmMesh%NNodes
      do j=1,3
         MeshMapData%Jac_u_indx(index,1) =  19 !Module/Mesh/Field: u_Orca%PtfmMesh%TranslationAcc = 19
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1                  
      end do !j                             
   end do !i                                
                                            
   do i=1,Orca_PtfmMesh%NNodes                  
      do j=1,3                              
         MeshMapData%Jac_u_indx(index,1) =  20 !Module/Mesh/Field:  u_Orca%PtfmMesh%RotationAcc = 20
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1
      end do !j      
   end do !i      
   
   !...............
   ! ExtPtfm inputs:   
   !...............
      
   ! ExtPtfm_PtfmMesh
   do i=1,ExtPtfm_PtfmMesh%NNodes
      do j=1,3
         MeshMapData%Jac_u_indx(index,1) =  21 !Module/Mesh/Field: u_ExtPtfm%PtfmMesh%TranslationAcc = 21
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1                  
      end do !j                             
   end do !i                                
                                            
   do i=1,ExtPtfm_PtfmMesh%NNodes                  
      do j=1,3                              
         MeshMapData%Jac_u_indx(index,1) =  22 !Module/Mesh/Field:  u_ExtPtfm%PtfmMesh%RotationAcc = 22
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1
      end do !j      
   end do !i
   
   
END SUBROUTINE Init_FullOpt1_Jacobian
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine initializes the output for the glue code, including writing the header for the primary output file.
SUBROUTINE FAST_InitOutput( p_FAST, y_FAST, Init, ErrStat, ErrMsg )

   IMPLICIT NONE

      ! Passed variables
   TYPE(FAST_ParameterType),       INTENT(IN)           :: p_FAST                                !< Glue-code simulation parameters
   TYPE(FAST_OutputFileType),      INTENT(INOUT)        :: y_FAST                                !< Glue-code simulation outputs
   TYPE(FAST_InitData),            INTENT(IN)           :: Init                                  !< Initialization data for all modules

   INTEGER(IntKi),                 INTENT(OUT)          :: ErrStat                               !< Error status
   CHARACTER(*),                   INTENT(OUT)          :: ErrMsg                                !< Error message corresponding to ErrStat


      ! Local variables.

   INTEGER(IntKi)                   :: I, J                                            ! Generic index for DO loops.
   INTEGER(IntKi)                   :: indxNext                                        ! The index of the next value to be written to an array
   INTEGER(IntKi)                   :: NumOuts                                         ! number of channels to be written to the output file(s)



   !......................................................
   ! Set the description lines to be printed in the output file
   !......................................................
   y_FAST%FileDescLines(1)  = 'Predictions were generated on '//CurDate()//' at '//CurTime()//' using '//TRIM(GetVersion(FAST_Ver, Cmpl4SFun, Cmpl4LV))
   y_FAST%FileDescLines(2)  = 'linked with ' //' '//TRIM(GetNVD(NWTC_Ver            ))  ! we'll get the rest of the linked modules in the section below
   y_FAST%FileDescLines(3)  = 'Description from the FAST input file: '//TRIM(p_FAST%FTitle)

   !......................................................
   ! We'll fill out the rest of FileDescLines(2),
   ! and save the module version info for later use, too:
   !......................................................

   y_FAST%Module_Ver( Module_ED ) = Init%OutData_ED%Ver
   y_FAST%FileDescLines(2) = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_ED )  ))

   IF ( p_FAST%CompElast == Module_BD )  THEN
      y_FAST%Module_Ver( Module_BD ) = Init%OutData_BD(1)%Ver ! call copy routine for this type if it every uses dynamic memory
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_BD )))
   END IF


   IF ( p_FAST%CompInflow == Module_IfW )  THEN
      y_FAST%Module_Ver( Module_IfW ) = Init%OutData_IfW%Ver ! call copy routine for this type if it every uses dynamic memory
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_IfW )))
   ELSEIF ( p_FAST%CompInflow == Module_OpFM )  THEN
      y_FAST%Module_Ver( Module_OpFM ) = Init%OutData_OpFM%Ver ! call copy routine for this type if it every uses dynamic memory
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_OpFM )))
   END IF

   IF ( p_FAST%CompAero == Module_AD14 )  THEN
      y_FAST%Module_Ver( Module_AD14  ) = Init%OutData_AD14%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_AD14  ) ))
   ELSEIF ( p_FAST%CompAero == Module_AD )  THEN
      y_FAST%Module_Ver( Module_AD  ) = Init%OutData_AD%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_AD  ) ))
   END IF

   IF ( p_FAST%CompServo == Module_SrvD ) THEN
      y_FAST%Module_Ver( Module_SrvD ) = Init%OutData_SrvD%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_SrvD )))
   END IF

   IF ( p_FAST%CompHydro == Module_HD ) THEN
      y_FAST%Module_Ver( Module_HD )   = Init%OutData_HD%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_HD )))
   END IF

   IF ( p_FAST%CompSub == Module_SD ) THEN
      y_FAST%Module_Ver( Module_SD )   = Init%OutData_SD%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_SD )))
   ELSE IF ( p_FAST%CompSub == Module_ExtPtfm ) THEN
      y_FAST%Module_Ver( Module_ExtPtfm )   = Init%OutData_ExtPtfm%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_ExtPtfm )))
   END IF

   IF ( p_FAST%CompMooring == Module_MAP ) THEN
      y_FAST%Module_Ver( Module_MAP )   = Init%OutData_MAP%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_MAP )))
   ELSEIF ( p_FAST%CompMooring == Module_MD ) THEN
      y_FAST%Module_Ver( Module_MD )   = Init%OutData_MD%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_MD )))
   ELSEIF ( p_FAST%CompMooring == Module_FEAM ) THEN
      y_FAST%Module_Ver( Module_FEAM )   = Init%OutData_FEAM%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_FEAM )))
   ELSEIF ( p_FAST%CompMooring == Module_Orca ) THEN
      y_FAST%Module_Ver( Module_Orca )   = Init%OutData_Orca%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_Orca)))
   END IF

   IF ( p_FAST%CompIce == Module_IceF ) THEN
      y_FAST%Module_Ver( Module_IceF )   = Init%OutData_IceF%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_IceF )))
   ELSEIF ( p_FAST%CompIce == Module_IceD ) THEN
      y_FAST%Module_Ver( Module_IceD )   = Init%OutData_IceD%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_IceD )))
   END IF

   !......................................................
   ! Set the number of output columns from each module
   !......................................................
   y_FAST%numOuts = 0    ! Inintialize entire array
   
   IF ( ALLOCATED( Init%OutData_IfW%WriteOutputHdr  ) ) y_FAST%numOuts(Module_IfW)  = SIZE(Init%OutData_IfW%WriteOutputHdr)
   IF ( ALLOCATED( Init%OutData_OpFM%WriteOutputHdr ) ) y_FAST%numOuts(Module_OpFM) = SIZE(Init%OutData_OpFM%WriteOutputHdr)
   IF ( ALLOCATED( Init%OutData_ED%WriteOutputHdr   ) ) y_FAST%numOuts(Module_ED)   = SIZE(Init%OutData_ED%WriteOutputHdr)
do i=1,p_FAST%nBeams
   IF ( ALLOCATED( Init%OutData_BD(i)%WriteOutputHdr) ) y_FAST%numOuts(Module_BD)   = y_FAST%numOuts(Module_BD) + SIZE(Init%OutData_BD(i)%WriteOutputHdr)
end do
!ad14 doesn't have outputs:
                                                       y_FAST%numOuts(Module_AD14) = 0
                                                       
   IF ( ALLOCATED( Init%OutData_AD%rotors)) then
      IF ( ALLOCATED( Init%OutData_AD%rotors(1)%WriteOutputHdr)) y_FAST%numOuts(Module_AD) = SIZE(Init%OutData_AD%rotors(1)%WriteOutputHdr)
   ENDIF
   IF ( ALLOCATED( Init%OutData_SrvD%WriteOutputHdr   ) ) y_FAST%numOuts(Module_SrvD)   = SIZE(Init%OutData_SrvD%WriteOutputHdr)
   IF ( ALLOCATED( Init%OutData_HD%WriteOutputHdr     ) ) y_FAST%numOuts(Module_HD)     = SIZE(Init%OutData_HD%WriteOutputHdr)
   IF ( ALLOCATED( Init%OutData_SD%WriteOutputHdr     ) ) y_FAST%numOuts(Module_SD)     = SIZE(Init%OutData_SD%WriteOutputHdr)
   IF ( ALLOCATED( Init%OutData_ExtPtfm%WriteOutputHdr) ) y_FAST%numOuts(Module_ExtPtfm)= SIZE(Init%OutData_ExtPtfm%WriteOutputHdr)
   IF ( ALLOCATED( Init%OutData_MAP%WriteOutputHdr    ) ) y_FAST%numOuts(Module_MAP)    = SIZE(Init%OutData_MAP%WriteOutputHdr)
   IF ( ALLOCATED( Init%OutData_FEAM%WriteOutputHdr   ) ) y_FAST%numOuts(Module_FEAM)   = SIZE(Init%OutData_FEAM%WriteOutputHdr)
   IF ( ALLOCATED( Init%OutData_MD%WriteOutputHdr     ) ) y_FAST%numOuts(Module_MD)     = SIZE(Init%OutData_MD%WriteOutputHdr)
   IF ( ALLOCATED( Init%OutData_Orca%WriteOutputHdr   ) ) y_FAST%numOuts(Module_Orca)   = SIZE(Init%OutData_Orca%WriteOutputHdr)
   IF ( ALLOCATED( Init%OutData_IceF%WriteOutputHdr   ) ) y_FAST%numOuts(Module_IceF)   = SIZE(Init%OutData_IceF%WriteOutputHdr)
   IF ( ALLOCATED( Init%OutData_IceD%WriteOutputHdr   ) ) y_FAST%numOuts(Module_IceD)   = SIZE(Init%OutData_IceD%WriteOutputHdr)*p_FAST%numIceLegs

   !......................................................
   ! Initialize the output channel names and units
   !......................................................
      y_FAST%numOuts(Module_Glue) = 1 ! time

   
   NumOuts   = SUM( y_FAST%numOuts )

   CALL AllocAry( y_FAST%ChannelNames,NumOuts, 'ChannelNames', ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( y_FAST%ChannelUnits,NumOuts, 'ChannelUnits', ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN

      ! Glue outputs: 
   y_FAST%ChannelNames(1) = 'Time'
   y_FAST%ChannelUnits(1) = '(s)'

   
   indxNext = y_FAST%numOuts(Module_Glue) + 1
   
   DO i=1,y_FAST%numOuts(Module_IfW) !InflowWind
      y_FAST%ChannelNames(indxNext) = Init%OutData_IfW%WriteOutputHdr(i)
      y_FAST%ChannelUnits(indxNext) = Init%OutData_IfW%WriteOutputUnt(i)
      indxNext = indxNext + 1
   END DO

   DO i=1,y_FAST%numOuts(Module_OpFM) !OpenFOAM
      y_FAST%ChannelNames(indxNext) = Init%OutData_OpFM%WriteOutputHdr(i)
      y_FAST%ChannelUnits(indxNext) = Init%OutData_OpFM%WriteOutputUnt(i)
      indxNext = indxNext + 1
   END DO

   DO i=1,y_FAST%numOuts(Module_ED) !ElastoDyn
      y_FAST%ChannelNames(indxNext) = Init%OutData_ED%WriteOutputHdr(i)
      y_FAST%ChannelUnits(indxNext) = Init%OutData_ED%WriteOutputUnt(i)
      indxNext = indxNext + 1
   END DO

   IF ( y_FAST%numOuts(Module_BD) > 0_IntKi ) THEN !BeamDyn
      do i=1,p_FAST%nBeams
         if ( allocated(Init%OutData_BD(i)%WriteOutputHdr) ) then
            do j=1,size(Init%OutData_BD(i)%WriteOutputHdr)
               y_FAST%ChannelNames(indxNext) = 'B'//TRIM(Num2Lstr(i))//trim(Init%OutData_BD(i)%WriteOutputHdr(j))
               y_FAST%ChannelUnits(indxNext) = Init%OutData_BD(i)%WriteOutputUnt(j)
               indxNext = indxNext + 1
            end do ! j
         end if
      end do
   END IF


   ! none for AeroDyn14

   DO i=1,y_FAST%numOuts(Module_AD) !AeroDyn
      y_FAST%ChannelNames(indxNext) = Init%OutData_AD%rotors(1)%WriteOutputHdr(i)
      y_FAST%ChannelUnits(indxNext) = Init%OutData_AD%rotors(1)%WriteOutputUnt(i)
      indxNext = indxNext + 1
   END DO

   DO i=1,y_FAST%numOuts(Module_SrvD) !ServoDyn
      y_FAST%ChannelNames(indxNext) = Init%OutData_SrvD%WriteOutputHdr(i)
      y_FAST%ChannelUnits(indxNext) = Init%OutData_SrvD%WriteOutputUnt(i)
      indxNext = indxNext + 1
   END DO

   DO i=1,y_FAST%numOuts(Module_HD) !HydroDyn
      y_FAST%ChannelNames(indxNext) = Init%OutData_HD%WriteOutputHdr(i)
      y_FAST%ChannelUnits(indxNext) = Init%OutData_HD%WriteOutputUnt(i)
      indxNext = indxNext + 1
   END DO

   DO i=1,y_FAST%numOuts(Module_SD) !SubDyn
      y_FAST%ChannelNames(indxNext) = Init%OutData_SD%WriteOutputHdr(i)
      y_FAST%ChannelUnits(indxNext) = Init%OutData_SD%WriteOutputUnt(i)
      indxNext = indxNext + 1
   END DO

   DO i=1,y_FAST%numOuts(Module_ExtPtfm) !ExtPtfm_MCKF
      y_FAST%ChannelNames(indxNext) = Init%OutData_ExtPtfm%WriteOutputHdr(i)
      y_FAST%ChannelUnits(indxNext) = Init%OutData_ExtPtfm%WriteOutputUnt(i)
      indxNext = indxNext + 1
   END DO

   DO i=1,y_FAST%numOuts(Module_MAP) !MAP
      y_FAST%ChannelNames(indxNext) = Init%OutData_MAP%WriteOutputHdr(i)
      y_FAST%ChannelUnits(indxNext) = Init%OutData_MAP%WriteOutputUnt(i)
      indxNext = indxNext + 1
   END DO

   DO i=1,y_FAST%numOuts(Module_MD) !MoorDyn
      y_FAST%ChannelNames(indxNext) = Init%OutData_MD%WriteOutputHdr(i)
      y_FAST%ChannelUnits(indxNext) = Init%OutData_MD%WriteOutputUnt(i)
      indxNext = indxNext + 1
   END DO

   DO i=1,y_FAST%numOuts(Module_FEAM) !FEAMooring
      y_FAST%ChannelNames(indxNext) = Init%OutData_FEAM%WriteOutputHdr(i)
      y_FAST%ChannelUnits(indxNext) = Init%OutData_FEAM%WriteOutputUnt(i)
      indxNext = indxNext + 1
   END DO

   DO i=1,y_FAST%numOuts(Module_Orca) !OrcaFlex
      y_FAST%ChannelNames(indxNext) = Init%OutData_Orca%WriteOutputHdr(i)
      y_FAST%ChannelUnits(indxNext) = Init%OutData_Orca%WriteOutputUnt(i)
      indxNext = indxNext + 1
   END DO

   DO i=1,y_FAST%numOuts(Module_IceF) !IceFloe
      y_FAST%ChannelNames(indxNext) = Init%OutData_IceF%WriteOutputHdr(i)
      y_FAST%ChannelUnits(indxNext) = Init%OutData_IceF%WriteOutputUnt(i)
      indxNext = indxNext + 1
   END DO

   IF ( y_FAST%numOuts(Module_IceD) > 0_IntKi ) THEN !IceDyn
      DO I=1,p_FAST%numIceLegs
         DO J=1,SIZE(Init%OutData_IceD%WriteOutputHdr)
            y_FAST%ChannelNames(indxNext) =TRIM(Init%OutData_IceD%WriteOutputHdr(J))//'L'//TRIM(Num2Lstr(I))  !bjj: do we want this "Lx" at the end?
            y_FAST%ChannelUnits(indxNext) = Init%OutData_IceD%WriteOutputUnt(J)
            indxNext = indxNext + 1
         END DO ! J
      END DO ! I
   END IF


   !......................................................
   ! Open the text output file and print the headers
   !......................................................

   IF (p_FAST%WrTxtOutFile) THEN

      y_FAST%ActualChanLen = max( MinChanLen, p_FAST%FmtWidth )
      DO I=1,NumOuts
         y_FAST%ActualChanLen = max( y_FAST%ActualChanLen, LEN_TRIM(y_FAST%ChannelNames(I)) )
         y_FAST%ActualChanLen = max( y_FAST%ActualChanLen, LEN_TRIM(y_FAST%ChannelUnits(I)) )
      ENDDO ! I

      y_FAST%OutFmt_a = '"'//p_FAST%Delim//'"'//p_FAST%OutFmt      ! format for array elements from individual modules
      if (p_FAST%FmtWidth < y_FAST%ActualChanLen) then
         y_FAST%OutFmt_a = trim(y_FAST%OutFmt_a)//','//trim(num2lstr(y_FAST%ActualChanLen - p_FAST%FmtWidth))//'x'
      end if

      CALL GetNewUnit( y_FAST%UnOu, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN

      CALL OpenFOutFile ( y_FAST%UnOu, TRIM(p_FAST%OutFileRoot)//'.out', ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN

         ! Add some file information:

      WRITE (y_FAST%UnOu,'(/,A)')  TRIM( y_FAST%FileDescLines(1) )
      WRITE (y_FAST%UnOu,'(1X,A)') TRIM( y_FAST%FileDescLines(2) )
      WRITE (y_FAST%UnOu,'()' )    !print a blank line
      WRITE (y_FAST%UnOu,'(A)'   ) TRIM( y_FAST%FileDescLines(3) )
      WRITE (y_FAST%UnOu,'()' )    !print a blank line


         !......................................................
         ! Write the names of the output parameters on one line:
         !......................................................
      if (p_FAST%Delim /= " ") then ! trim trailing spaces if not space delimited:

         CALL WrFileNR ( y_FAST%UnOu, trim(y_FAST%ChannelNames(1)) ) ! first one is time, with a special format

         DO I=2,NumOuts
            CALL WrFileNR ( y_FAST%UnOu, p_FAST%Delim//trim(y_FAST%ChannelNames(I)) )
         ENDDO ! I
      else

         CALL WrFileNR ( y_FAST%UnOu, y_FAST%ChannelNames(1)(1:p_FAST%TChanLen) ) ! first one is time, with a special format

         DO I=2,NumOuts
            CALL WrFileNR ( y_FAST%UnOu, p_FAST%Delim//y_FAST%ChannelNames(I)(1:y_FAST%ActualChanLen) )
         ENDDO ! I
      end if

      WRITE (y_FAST%UnOu,'()')

         !......................................................
         ! Write the units of the output parameters on one line:
         !......................................................

      if (p_FAST%Delim /= " ") then

         CALL WrFileNR ( y_FAST%UnOu, trim(y_FAST%ChannelUnits(1)) )

         DO I=2,NumOuts
            CALL WrFileNR ( y_FAST%UnOu, p_FAST%Delim//trim(y_FAST%ChannelUnits(I)) )
         ENDDO ! I
      else

         CALL WrFileNR ( y_FAST%UnOu, y_FAST%ChannelUnits(1)(1:p_FAST%TChanLen) )

         DO I=2,NumOuts
            CALL WrFileNR ( y_FAST%UnOu, p_FAST%Delim//y_FAST%ChannelUnits(I)(1:y_FAST%ActualChanLen) )
         ENDDO ! I
      end if

      WRITE (y_FAST%UnOu,'()')

   END IF

   !......................................................
   ! Allocate data for binary output file
   !......................................................
   IF (p_FAST%WrBinOutFile) THEN

         ! calculate the size of the array of outputs we need to store
      y_FAST%NOutSteps = CEILING ( (p_FAST%TMax - p_FAST%TStart) / p_FAST%DT_OUT ) + 1

      CALL AllocAry( y_FAST%AllOutData, NumOuts-1, y_FAST%NOutSteps, 'AllOutData', ErrStat, ErrMsg ) ! this does not include the time channel
      IF ( ErrStat >= AbortErrLev ) RETURN
      y_FAST%AllOutData = 0.0_ReKi

      IF ( p_FAST%WrBinMod == FileFmtID_WithTime ) THEN   ! we store the entire time array
         CALL AllocAry( y_FAST%TimeData, y_FAST%NOutSteps, 'TimeData', ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      ELSE
         CALL AllocAry( y_FAST%TimeData, 2_IntKi, 'TimeData', ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN

         y_FAST%TimeData(1) = 0.0_DbKi           ! This is the first output time, which we will set later
         y_FAST%TimeData(2) = p_FAST%DT_out      ! This is the (constant) time between subsequent writes to the output file
      END IF

      y_FAST%n_Out = 0  !number of steps actually written to the file

   END IF

   y_FAST%VTK_count = 0  ! first VTK file has 0 as output

RETURN
END SUBROUTINE FAST_InitOutput
!----------------------------------------------------------------------------------------------------------------------------------
!> This subroutine sets up the information needed to initialize AeroDyn, then initializes AeroDyn
SUBROUTINE AD_SetInitInput(InitInData_AD14, InitOutData_ED, y_ED, p_FAST, ErrStat, ErrMsg)

   ! Passed variables:
   TYPE(AD14_InitInputType),INTENT(INOUT) :: InitInData_AD14  !< The initialization input to AeroDyn14
   TYPE(ED_InitOutputType), INTENT(IN)    :: InitOutData_ED   !< The initialization output from structural dynamics module
   TYPE(ED_OutputType),     INTENT(IN)    :: y_ED             !< The outputs of the structural dynamics module (meshes with position/RefOrientation set)
   TYPE(FAST_ParameterType),INTENT(IN)    :: p_FAST           !< The parameters of the glue code
   INTEGER(IntKi)                         :: ErrStat          !< Error status of the operation
   CHARACTER(*)                           :: ErrMsg           !< Error message if ErrStat /= ErrID_None

      ! Local variables

   !TYPE(AD_InitOptions)       :: ADOptions                  ! Options for AeroDyn

   INTEGER                    :: K


   ErrStat = ErrID_None
   ErrMsg  = ""
   
   
      ! Set up the AeroDyn parameters
   InitInData_AD14%ADFileName   = p_FAST%AeroFile
   InitInData_AD14%OutRootName  = p_FAST%OutFileRoot
   InitInData_AD14%WrSumFile    = p_FAST%SumPrint      
   InitInData_AD14%NumBl        = InitOutData_ED%NumBl
   InitInData_AD14%UseDWM       = p_FAST%UseDWM
   
   InitInData_AD14%DWM%IfW%InputFileName   = p_FAST%InflowFile
   
      ! Hub position and orientation (relative here, but does not need to be)

   InitInData_AD14%TurbineComponents%Hub%Position(:)      = y_ED%HubPtMotion14%Position(:,1) - y_ED%HubPtMotion14%Position(:,1)  ! bjj: was 0; mesh was changed by adding p_ED%HubHt to 3rd component
   InitInData_AD14%TurbineComponents%Hub%Orientation(:,:) = y_ED%HubPtMotion14%RefOrientation(:,:,1)
   InitInData_AD14%TurbineComponents%Hub%TranslationVel   = 0.0_ReKi ! bjj: we don't need this field
   InitInData_AD14%TurbineComponents%Hub%RotationVel      = 0.0_ReKi ! bjj: we don't need this field

      ! Blade root position and orientation (relative here, but does not need to be)

   IF (.NOT. ALLOCATED( InitInData_AD14%TurbineComponents%Blade ) ) THEN
      ALLOCATE( InitInData_AD14%TurbineComponents%Blade( InitInData_AD14%NumBl ), STAT = ErrStat )
      IF ( ErrStat /= 0 ) THEN
         ErrStat = ErrID_Fatal
         ErrMsg = ' Error allocating space for InitInData_AD%TurbineComponents%Blade.'
         RETURN
      ELSE
         ErrStat = ErrID_None !reset to ErrID_None, just in case ErrID_None /= 0
      END IF
   END IF

   DO K=1, InitInData_AD14%NumBl
      InitInData_AD14%TurbineComponents%Blade(K)%Position        = y_ED%BladeRootMotion14%Position(:,K)
      InitInData_AD14%TurbineComponents%Blade(K)%Orientation     = y_ED%BladeRootMotion14%RefOrientation(:,:,K)
      InitInData_AD14%TurbineComponents%Blade(K)%TranslationVel  = 0.0_ReKi ! bjj: we don't need this field
      InitInData_AD14%TurbineComponents%Blade(K)%RotationVel     = 0.0_ReKi ! bjj: we don't need this field      
   END DO
  

      ! Blade length
   IF (p_FAST%CompElast == Module_ED) THEN  ! note, we can't get here if we're using BeamDyn....
      InitInData_AD14%TurbineComponents%BladeLength = InitOutData_ED%BladeLength
   END IF
   
   
      ! Tower mesh ( here only because we currently need line2 meshes to contain the same nodes/elements )
      
   InitInData_AD14%NumTwrNodes = y_ED%TowerLn2Mesh%NNodes - 2
   IF (.NOT. ALLOCATED( InitInData_AD14%TwrNodeLocs ) ) THEN
      ALLOCATE( InitInData_AD14%TwrNodeLocs( 3, InitInData_AD14%NumTwrNodes ), STAT = ErrStat )
      IF ( ErrStat /= 0 ) THEN
         ErrStat = ErrID_Fatal
         ErrMsg = ' Error allocating space for InitInData_AD%TwrNodeLocs.'
         RETURN
      ELSE
         ErrStat = ErrID_None
      END IF
   END IF   
   
   IF ( InitInData_AD14%NumTwrNodes > 0 ) THEN
      InitInData_AD14%TwrNodeLocs = y_ED%TowerLn2Mesh%Position(:,1:InitInData_AD14%NumTwrNodes)  ! ED has extra nodes at beginning and top and bottom of tower
   END IF
   
      ! hub height         
   InitInData_AD14%HubHt = InitOutData_ED%HubHt
             

   RETURN
END SUBROUTINE AD_SetInitInput
!----------------------------------------------------------------------------------------------------------------------------------
!> This subroutine is called at the start (or restart) of a FAST program (or FAST.Farm). It initializes the NWTC subroutine library,
!! displays the copyright notice, and displays some version information (including addressing scheme and precision).
SUBROUTINE FAST_ProgStart(ThisProgVer)
   TYPE(ProgDesc), INTENT(IN) :: ThisProgVer     !< program name/date/version description

   ! ... Initialize NWTC Library
   ! sets the pi constants, open console for output, etc...
   CALL NWTC_Init( ProgNameIN=ThisProgVer%Name, EchoLibVer=.FALSE. )

   ! Display the copyright notice and compile info:
   CALL DispCopyrightLicense( ThisProgVer%Name )
   CALL DispCompileRuntimeInfo( ThisProgVer%Name )

END SUBROUTINE FAST_ProgStart
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine sets the number of subcycles (substeps) for modules at initialization, checking to make sure that their requested
!! time step is valid.
SUBROUTINE SetModuleSubstepTime(ModuleID, p_FAST, y_FAST, ErrStat, ErrMsg)
   INTEGER(IntKi),           INTENT(IN   ) :: ModuleID            !< ID of the module to check time step and set
   TYPE(FAST_ParameterType), INTENT(INOUT) :: p_FAST              !< Parameters for the glue code
   TYPE(FAST_OutputFileType),INTENT(IN   ) :: y_FAST              !< Output variables for the glue code
   INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None


   ErrStat = ErrID_None
   ErrMsg  = ""

   IF ( EqualRealNos( p_FAST%dt_module( ModuleID ), p_FAST%dt ) ) THEN
      p_FAST%n_substeps(ModuleID) = 1
   ELSE
      IF ( p_FAST%dt_module( ModuleID ) > p_FAST%dt ) THEN
         ErrStat = ErrID_Fatal
         ErrMsg = "The "//TRIM(y_FAST%Module_Ver(ModuleID)%Name)//" module time step ("//&
                          TRIM(Num2LStr(p_FAST%dt_module( ModuleID )))// &
                    " s) cannot be larger than FAST time step ("//TRIM(Num2LStr(p_FAST%dt))//" s)."
      ELSE
            ! calculate the number of subcycles:
         p_FAST%n_substeps(ModuleID) = NINT( p_FAST%dt / p_FAST%dt_module( ModuleID ) )

            ! let's make sure THE module DT is an exact integer divisor of the global (FAST) time step:
         IF ( .NOT. EqualRealNos( p_FAST%dt, p_FAST%dt_module( ModuleID ) * p_FAST%n_substeps(ModuleID) )  ) THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = "The "//TRIM(y_FAST%Module_Ver(ModuleID)%Name)//" module time step ("//&
                              TRIM(Num2LStr(p_FAST%dt_module( ModuleID )))// &
                              " s) must be an integer divisor of the FAST time step ("//TRIM(Num2LStr(p_FAST%dt))//" s)."
         END IF

      END IF
   END IF

   RETURN

END SUBROUTINE SetModuleSubstepTime

END MODULE
