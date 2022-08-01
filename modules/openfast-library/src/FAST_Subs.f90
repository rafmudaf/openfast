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
MODULE FAST_Subs

   USE FAST_ModTypes
   USE FAST_Initialization, ONLY: FAST_ProgStart
   USE FAST_IO, ONLY: FAST_EndOutput, FAST_InitIOarrays, WriteOutputToFile, NeedWriteOutput
   USE FAST_Linear, ONLY: FAST_CalcSteady, FAST_Linearize_OP, PerturbOP, SetOperatingPoint
   USE FAST_Solver
   USE FAST_VTK, ONLY: WriteVTK, WrVTK_AllMeshes, ReadModeShapeFile, ReadModeShapeMatlabFile
   USE SC_DataEx, ONLY: SC_DX_SetInputs, SC_DX_SetOutputs

   IMPLICIT NONE

CONTAINS

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

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! TIME-STEP SOLVER ROUTINES (includes initialization after first call to calcOutput at t=0)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!> Routine that calls FAST_Solution0 for one instance of a Turbine data structure. This is a separate subroutine so that the FAST
!! driver programs do not need to change or operate on the individual module level.
SUBROUTINE FAST_Solution0_T(Turbine, ErrStat, ErrMsg)

   TYPE(FAST_TurbineType),   INTENT(INOUT) :: Turbine             !< all data for one instance of a turbine
   INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None


   CALL FAST_Solution0(Turbine%p_FAST, Turbine%y_FAST, Turbine%m_FAST, &
                     Turbine%ED, Turbine%BD, Turbine%SrvD, Turbine%AD14, Turbine%AD, Turbine%IfW, Turbine%OpFM, Turbine%SC_DX,&
                     Turbine%HD, Turbine%SD, Turbine%ExtPtfm, Turbine%MAP, Turbine%FEAM, Turbine%MD, Turbine%Orca, &
                     Turbine%IceF, Turbine%IceD, Turbine%MeshMapData, ErrStat, ErrMsg )

END SUBROUTINE FAST_Solution0_T
!----------------------------------------------------------------------------------------------------------------------------------
!> Routine that calls CalcOutput for the first time of the simulation (at t=0). After the initial solve, data arrays are initialized.
SUBROUTINE FAST_Solution0(p_FAST, y_FAST, m_FAST, ED, BD, SrvD, AD14, AD, IfW, OpFM, SC_DX, HD, SD, ExtPtfm, &
                          MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, ErrStat, ErrMsg )

   TYPE(FAST_ParameterType), INTENT(IN   ) :: p_FAST              !< Parameters for the glue code
   TYPE(FAST_OutputFileType),INTENT(INOUT) :: y_FAST              !< Output variables for the glue code
   TYPE(FAST_MiscVarType),   INTENT(INOUT) :: m_FAST              !< Miscellaneous variables

   TYPE(ElastoDyn_Data),     INTENT(INOUT) :: ED                  !< ElastoDyn data
   TYPE(BeamDyn_Data),       INTENT(INOUT) :: BD                  !< BeamDyn data
   TYPE(ServoDyn_Data),      INTENT(INOUT) :: SrvD                !< ServoDyn data
   TYPE(AeroDyn14_Data),     INTENT(INOUT) :: AD14                !< AeroDyn14 data
   TYPE(AeroDyn_Data),       INTENT(INOUT) :: AD                  !< AeroDyn data
   TYPE(InflowWind_Data),    INTENT(INOUT) :: IfW                 !< InflowWind data
   TYPE(OpenFOAM_Data),      INTENT(INOUT) :: OpFM                !< OpenFOAM data
   TYPE(SCDataEx_Data),      INTENT(INOUT) :: SC_DX               !< Supercontroller exchange data
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

   ! local variables
   INTEGER(IntKi), PARAMETER               :: n_t_global = -1     ! loop counter
   INTEGER(IntKi), PARAMETER               :: n_t_global_next = 0 ! loop counter
   REAL(DbKi)                              :: t_initial           ! next simulation time (t_global_next)

   INTEGER(IntKi)                          :: ErrStat2
   CHARACTER(ErrMsgLen)                    :: ErrMsg2
   CHARACTER(*), PARAMETER                 :: RoutineName = 'FAST_Solution0'


   !NOTE: m_FAST%t_global is t_initial in this routine

   ErrStat = ErrID_None
   ErrMsg  = ""

   t_initial = m_FAST%t_global ! which is used in place of t_global_next
   y_FAST%WriteThisStep = NeedWriteOutput(n_t_global_next, t_initial, p_FAST)

   IF (p_FAST%WrSttsTime) then
      CALL SimStatus_FirstTime( m_FAST%TiLstPrn, m_FAST%PrevClockTime, m_FAST%SimStrtTime, m_FAST%UsrTime2, t_initial, p_FAST%TMax, p_FAST%TDesc )
   END IF


   ! Solve input-output relations; this section of code corresponds to Eq. (35) in Gasmi et al. (2013)
   ! This code will be specific to the underlying modules

      ! the initial ServoDyn and IfW/Lidar inputs from Simulink:
   IF ( p_FAST%CompServo == Module_SrvD ) CALL SrvD_SetExternalInputs( p_FAST, m_FAST, SrvD%Input(1) )
   IF ( p_FAST%CompInflow == Module_IfW ) CALL IfW_SetExternalInputs( IfW%p, m_FAST, ED%y, IfW%Input(1) )

   CALL CalcOutputs_And_SolveForInputs(  n_t_global, t_initial,  STATE_CURR, m_FAST%calcJacobian, m_FAST%NextJacCalcTime, &
                        p_FAST, m_FAST, y_FAST%WriteThisStep, ED, BD, SrvD, AD14, AD, IfW, OpFM, HD, SD, ExtPtfm, &
                        MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

   if (p_FAST%UseSC ) then
      call SC_DX_SetInputs(p_FAST, SrvD%y, SC_DX, ErrStat2, ErrMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   end if

   !----------------------------------------------------------------------------------------
   ! Check to see if we should output data this time step:
   !----------------------------------------------------------------------------------------

   CALL WriteOutputToFile(n_t_global_next, t_initial, p_FAST, y_FAST, ED, BD, AD14, AD, IfW, OpFM, HD, SD, ExtPtfm, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, ErrStat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

      ! turn off VTK output when
   if (p_FAST%WrVTK == VTK_InitOnly) then
      ! Write visualization data for initialization (and also note that we're ignoring any errors that occur doing so)

      call WriteVTK(t_initial, p_FAST, y_FAST, MeshMapData, ED, BD, AD, IfW, OpFM, HD, SD, ExtPtfm, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD)

   end if


   !...............
   ! Copy values of these initial guesses for interpolation/extrapolation and
   ! initialize predicted states for j_pc loop (use MESH_NEWCOPY here so we can use MESH_UPDATE copy later)
   !...............

   ! Initialize Input-Output arrays for interpolation/extrapolation:

   CALL FAST_InitIOarrays( m_FAST%t_global, p_FAST, y_FAST, m_FAST, ED, BD, SrvD, AD14, AD, IfW, HD, SD, ExtPtfm, &
                           MAPp, FEAM, MD, Orca, IceF, IceD, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


END SUBROUTINE FAST_Solution0
!----------------------------------------------------------------------------------------------------------------------------------
!> Routine that calls FAST_Solution for one instance of a Turbine data structure. This is a separate subroutine so that the FAST
!! driver programs do not need to change or operate on the individual module level.
SUBROUTINE FAST_Solution_T(t_initial, n_t_global, Turbine, ErrStat, ErrMsg )

   REAL(DbKi),               INTENT(IN   ) :: t_initial           !< initial time
   INTEGER(IntKi),           INTENT(IN   ) :: n_t_global          !< loop counter
   TYPE(FAST_TurbineType),   INTENT(INOUT) :: Turbine             !< all data for one instance of a turbine
   INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None

   CALL FAST_Solution(t_initial, n_t_global, Turbine%p_FAST, Turbine%y_FAST, Turbine%m_FAST, &
                  Turbine%ED, Turbine%BD, Turbine%SrvD, Turbine%AD14, Turbine%AD, Turbine%IfW, Turbine%OpFM, Turbine%SC_DX, &
                  Turbine%HD, Turbine%SD, Turbine%ExtPtfm, Turbine%MAP, Turbine%FEAM, Turbine%MD, Turbine%Orca, &
                  Turbine%IceF, Turbine%IceD, Turbine%MeshMapData, ErrStat, ErrMsg )

END SUBROUTINE FAST_Solution_T
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine takes data from n_t_global and gets values at n_t_global + 1
SUBROUTINE FAST_Solution(t_initial, n_t_global, p_FAST, y_FAST, m_FAST, ED, BD, SrvD, AD14, AD, IfW, OpFM, SC_DX, HD, SD, ExtPtfm, &
                         MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, ErrStat, ErrMsg )

   REAL(DbKi),               INTENT(IN   ) :: t_initial           !< initial time
   INTEGER(IntKi),           INTENT(IN   ) :: n_t_global          !< loop counter

   TYPE(FAST_ParameterType), INTENT(IN   ) :: p_FAST              !< Parameters for the glue code
   TYPE(FAST_OutputFileType),INTENT(INOUT) :: y_FAST              !< Output variables for the glue code
   TYPE(FAST_MiscVarType),   INTENT(INOUT) :: m_FAST              !< Miscellaneous variables

   TYPE(ElastoDyn_Data),     INTENT(INOUT) :: ED                  !< ElastoDyn data
   TYPE(BeamDyn_Data),       INTENT(INOUT) :: BD                  !< BeamDyn data
   TYPE(ServoDyn_Data),      INTENT(INOUT) :: SrvD                !< ServoDyn data
   TYPE(AeroDyn14_Data),     INTENT(INOUT) :: AD14                !< AeroDyn14 data
   TYPE(AeroDyn_Data),       INTENT(INOUT) :: AD                  !< AeroDyn data
   TYPE(InflowWind_Data),    INTENT(INOUT) :: IfW                 !< InflowWind data
   TYPE(OpenFOAM_Data),      INTENT(INOUT) :: OpFM                !< OpenFOAM data
   TYPE(SCDataEx_Data),      INTENT(INOUT) :: SC_DX               !< Supercontroller Exchange data
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

   ! local variables
   REAL(DbKi)                              :: t_global_next       ! next simulation time (m_FAST%t_global + p_FAST%dt)
   INTEGER(IntKi)                          :: n_t_global_next     ! n_t_global + 1
   INTEGER(IntKi)                          :: j_pc                ! predictor-corrector loop counter
   INTEGER(IntKi)                          :: NumCorrections      ! number of corrections for this time step
   INTEGER(IntKi), parameter               :: MaxCorrections = 20 ! maximum number of corrections allowed
   LOGICAL                                 :: WriteThisStep       ! Whether WriteOutput values will be printed

   INTEGER(IntKi)                          :: I, k                ! generic loop counters

   !REAL(ReKi)                              :: ControlInputGuess   ! value of controller inputs


   INTEGER(IntKi)                          :: ErrStat2
   CHARACTER(ErrMsgLen)                    :: ErrMsg2
   CHARACTER(*), PARAMETER                 :: RoutineName = 'FAST_Solution'


   ErrStat  = ErrID_None
   ErrMsg   = ""
   ErrStat2 = ErrID_None
   ErrMsg2  = ""

   n_t_global_next = n_t_global+1
   t_global_next = t_initial + n_t_global_next*p_FAST%DT  ! = m_FAST%t_global + p_FAST%dt

   y_FAST%WriteThisStep = NeedWriteOutput(n_t_global_next, t_global_next, p_FAST)

      !! determine if the Jacobian should be calculated this time
   IF ( m_FAST%calcJacobian ) THEN ! this was true (possibly at initialization), so we'll advance the time for the next calculation of the Jacobian

      if (p_FAST%CompMooring == Module_Orca .and. n_t_global < 5) then
         m_FAST%NextJacCalcTime = m_FAST%t_global + p_FAST%DT  ! the jacobian calculated with OrcaFlex at t=0 is incorrect, but is okay on the 2nd step (it's not okay for OrcaFlex version 10, so I increased this to 5)
      else
         m_FAST%NextJacCalcTime = m_FAST%t_global + p_FAST%DT_UJac
      end if

   END IF

      ! set number of corrections to be used for this time step:
   IF ( p_FAST%CompElast == Module_BD ) THEN ! BD accelerations have fewer spikes with these corrections on the first several time steps
      if (n_t_global > 2) then ! this 2 should probably be related to p_FAST%InterpOrder
         NumCorrections = p_FAST%NumCrctn
      elseif (n_t_global == 0) then
         NumCorrections = max(p_FAST%NumCrctn,16)
      else
         NumCorrections = max(p_FAST%NumCrctn,1)
      end if
   ELSE
      NumCorrections = p_FAST%NumCrctn
   END IF

      ! the ServoDyn inputs from Simulink are for t, not t+dt, so we're going to overwrite the inputs from
      ! the previous step before we extrapolate these inputs:
   IF ( p_FAST%CompServo == Module_SrvD ) CALL SrvD_SetExternalInputs( p_FAST, m_FAST, SrvD%Input(1) )

   IF ( p_FAST%UseSC ) THEN
      CALL SC_DX_SetOutputs(p_FAST, SrvD%Input(1), SC_DX, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   END IF

   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   !! ## Step 1.a: Extrapolate Inputs
   !!
   !! gives predicted values at t+dt
   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   CALL FAST_ExtrapInterpMods( t_global_next, p_FAST, m_FAST, ED, BD, SrvD, AD14, AD, IfW, HD, SD, ExtPtfm, &
                               MAPp, FEAM, MD, Orca, IceF, IceD, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


   !! predictor-corrector loop:
   j_pc = 0
   do while (j_pc <= NumCorrections)
      WriteThisStep = y_FAST%WriteThisStep .AND. j_pc==NumCorrections

   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   !! ## Step 1.b: Advance states (yield state and constraint values at t_global_next)
   !!
   !! STATE_CURR values of x, xd, z, and OtherSt contain values at m_FAST%t_global;
   !! STATE_PRED values contain values at t_global_next.
   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      CALL FAST_AdvanceStates( t_initial, n_t_global, p_FAST, m_FAST, ED, BD, SrvD, AD14, AD, IfW, OpFM, HD, SD, ExtPtfm, &
                               MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, ErrStat2, ErrMsg2, WriteThisStep )
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         IF (ErrStat >= AbortErrLev) RETURN

   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   !! ## Step 1.c: Input-Output Solve
   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! save predicted inputs for comparison with corrected value later
      !IF (p_FAST%CheckHSSBrTrqC) THEN
      !   ControlInputGuess = ED%Input(1)%HSSBrTrqC
      !END IF

      CALL CalcOutputs_And_SolveForInputs( n_t_global, t_global_next,  STATE_PRED, m_FAST%calcJacobian, m_FAST%NextJacCalcTime, &
         p_FAST, m_FAST, WriteThisStep, ED, BD, SrvD, AD14, AD, IfW, OpFM, HD, SD, ExtPtfm, MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         IF (ErrStat >= AbortErrLev) RETURN

   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   !! ## Step 2: Correct (continue in loop)
   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      j_pc = j_pc + 1

      !   ! Check if the predicted inputs were significantly different than the corrected inputs
      !   ! (values before and after CalcOutputs_And_SolveForInputs)
      !if (j_pc > NumCorrections) then
      !
      !   !if (p_FAST%CheckHSSBrTrqC) then
      !   !   if ( abs(ControlInputGuess - ED%Input(1)%HSSBrTrqC) > 50.0_ReKi ) then ! I randomly picked 50 N-m
      !   !      NumCorrections = min(p_FAST%NumCrctn + 1, MaxCorrections)
      !   !      ! print *, 'correction:', t_global_next, NumCorrections
      !   !      cycle
      !   !   end if
      !   !end if
      !
      !   ! check pitch position input to structural code (not implemented, yet)
      !end if

   enddo ! j_pc

   if (p_FAST%UseSC ) then
      call SC_DX_SetInputs(p_FAST, SrvD%y, SC_DX, ErrStat2, ErrMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   end if

   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   !! ## Step 3: Save all final variables (advance to next time)
   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   !----------------------------------------------------------------------------------------
   !! copy the final predicted states from step t_global_next to actual states for that step
   !----------------------------------------------------------------------------------------

      ! ElastoDyn: copy final predictions to actual states
   CALL ED_CopyContState   (ED%x( STATE_PRED), ED%x( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   CALL ED_CopyDiscState   (ED%xd(STATE_PRED), ED%xd(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   CALL ED_CopyConstrState (ED%z( STATE_PRED), ED%z( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   CALL ED_CopyOtherState (ED%OtherSt( STATE_PRED), ED%OtherSt( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


      ! BeamDyn: copy final predictions to actual states
   IF ( p_FAST%CompElast == Module_BD ) THEN
      DO k=1,p_FAST%nBeams
         CALL BD_CopyContState   (BD%x( k,STATE_PRED), BD%x( k,STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
            CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         CALL BD_CopyDiscState   (BD%xd(k,STATE_PRED), BD%xd(k,STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
            CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         CALL BD_CopyConstrState (BD%z( k,STATE_PRED), BD%z( k,STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
            CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         CALL BD_CopyOtherState (BD%OtherSt( k,STATE_PRED), BD%OtherSt( k,STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
            CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      END DO
   END IF


      ! AeroDyn: copy final predictions to actual states; copy current outputs to next
   IF ( p_FAST%CompAero == Module_AD14 ) THEN
      CALL AD14_CopyContState   (AD14%x( STATE_PRED), AD14%x( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL AD14_CopyDiscState   (AD14%xd(STATE_PRED), AD14%xd(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL AD14_CopyConstrState (AD14%z( STATE_PRED), AD14%z( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL AD14_CopyOtherState (AD14%OtherSt(STATE_PRED), AD14%OtherSt(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   ELSEIF ( p_FAST%CompAero == Module_AD ) THEN
      CALL AD_CopyContState   (AD%x( STATE_PRED), AD%x( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL AD_CopyDiscState   (AD%xd(STATE_PRED), AD%xd(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL AD_CopyConstrState (AD%z( STATE_PRED), AD%z( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL AD_CopyOtherState (AD%OtherSt(STATE_PRED), AD%OtherSt(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   END IF


   ! InflowWind: copy final predictions to actual states; copy current outputs to next
   IF ( p_FAST%CompInflow == Module_IfW ) THEN
      CALL InflowWind_CopyContState   (IfW%x( STATE_PRED), IfW%x( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL InflowWind_CopyDiscState   (IfW%xd(STATE_PRED), IfW%xd(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL InflowWind_CopyConstrState (IfW%z( STATE_PRED), IfW%z( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL InflowWind_CopyOtherState (IfW%OtherSt( STATE_PRED), IfW%OtherSt( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   END IF


   ! ServoDyn: copy final predictions to actual states; copy current outputs to next
   IF ( p_FAST%CompServo == Module_SrvD ) THEN
      CALL SrvD_CopyContState   (SrvD%x( STATE_PRED), SrvD%x( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL SrvD_CopyDiscState   (SrvD%xd(STATE_PRED), SrvD%xd(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL SrvD_CopyConstrState (SrvD%z( STATE_PRED), SrvD%z( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL SrvD_CopyOtherState (SrvD%OtherSt( STATE_PRED), SrvD%OtherSt( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   END IF


   ! HydroDyn: copy final predictions to actual states
   IF ( p_FAST%CompHydro == Module_HD ) THEN
      CALL HydroDyn_CopyContState   (HD%x( STATE_PRED), HD%x( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL HydroDyn_CopyDiscState   (HD%xd(STATE_PRED), HD%xd(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL HydroDyn_CopyConstrState (HD%z( STATE_PRED), HD%z( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL HydroDyn_CopyOtherState (HD%OtherSt(STATE_PRED), HD%OtherSt(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   END IF


   ! SubDyn: copy final predictions to actual states
   IF ( p_FAST%CompSub == Module_SD ) THEN
      CALL SD_CopyContState   (SD%x( STATE_PRED), SD%x( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL SD_CopyDiscState   (SD%xd(STATE_PRED), SD%xd(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL SD_CopyConstrState (SD%z( STATE_PRED), SD%z( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL SD_CopyOtherState (SD%OtherSt(STATE_PRED), SD%OtherSt(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   ELSE IF ( p_FAST%CompSub == Module_ExtPtfm ) THEN
      CALL ExtPtfm_CopyContState   (ExtPtfm%x( STATE_PRED), ExtPtfm%x( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL ExtPtfm_CopyDiscState   (ExtPtfm%xd(STATE_PRED), ExtPtfm%xd(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL ExtPtfm_CopyConstrState (ExtPtfm%z( STATE_PRED), ExtPtfm%z( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL ExtPtfm_CopyOtherState (ExtPtfm%OtherSt(STATE_PRED), ExtPtfm%OtherSt(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   END IF


   ! MAP: copy final predictions to actual states
   IF (p_FAST%CompMooring == Module_MAP) THEN
      CALL MAP_CopyContState   (MAPp%x( STATE_PRED), MAPp%x( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL MAP_CopyDiscState   (MAPp%xd(STATE_PRED), MAPp%xd(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL MAP_CopyConstrState (MAPp%z( STATE_PRED), MAPp%z( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      !CALL MAP_CopyOtherState (MAPp%OtherSt(STATE_PRED), MAPp%OtherSt(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
      !   CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   ELSEIF (p_FAST%CompMooring == Module_MD) THEN
      CALL MD_CopyContState   (MD%x( STATE_PRED), MD%x( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL MD_CopyDiscState   (MD%xd(STATE_PRED), MD%xd(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL MD_CopyConstrState (MD%z( STATE_PRED), MD%z( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL MD_CopyOtherState (MD%OtherSt(STATE_PRED), MD%OtherSt(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   ELSEIF (p_FAST%CompMooring == Module_FEAM) THEN
      CALL FEAM_CopyContState   (FEAM%x( STATE_PRED), FEAM%x( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL FEAM_CopyDiscState   (FEAM%xd(STATE_PRED), FEAM%xd(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL FEAM_CopyConstrState (FEAM%z( STATE_PRED), FEAM%z( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL FEAM_CopyOtherState (FEAM%OtherSt( STATE_PRED), FEAM%OtherSt( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   ELSEIF (p_FAST%CompMooring == Module_Orca) THEN
      CALL Orca_CopyContState   (Orca%x( STATE_PRED), Orca%x( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL Orca_CopyDiscState   (Orca%xd(STATE_PRED), Orca%xd(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL Orca_CopyConstrState (Orca%z( STATE_PRED), Orca%z( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL Orca_CopyOtherState (Orca%OtherSt( STATE_PRED), Orca%OtherSt( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   END IF

         ! IceFloe: copy final predictions to actual states
   IF ( p_FAST%CompIce == Module_IceF ) THEN
      CALL IceFloe_CopyContState   (IceF%x( STATE_PRED), IceF%x( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL IceFloe_CopyDiscState   (IceF%xd(STATE_PRED), IceF%xd(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL IceFloe_CopyConstrState (IceF%z( STATE_PRED), IceF%z( STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL IceFloe_CopyOtherState (IceF%OtherSt(STATE_PRED), IceF%OtherSt(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   ELSEIF ( p_FAST%CompIce == Module_IceD ) THEN
      DO i=1,p_FAST%numIceLegs
         CALL IceD_CopyContState   (IceD%x( i,STATE_PRED), IceD%x( i,STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
            CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         CALL IceD_CopyDiscState   (IceD%xd(i,STATE_PRED), IceD%xd(i,STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
            CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         CALL IceD_CopyConstrState (IceD%z( i,STATE_PRED), IceD%z( i,STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
            CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         CALL IceD_CopyOtherState (IceD%OtherSt( i,STATE_PRED), IceD%OtherSt( i,STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2)
            CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      END DO
   END IF


   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   !! We've advanced everything to the next time step:
   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   !! update the global time

   m_FAST%t_global = t_global_next


   !----------------------------------------------------------------------------------------
   !! Check to see if we should output data this time step:
   !----------------------------------------------------------------------------------------

   CALL WriteOutputToFile(n_t_global_next, t_global_next, p_FAST, y_FAST, ED, BD, AD14, AD, IfW, OpFM, HD, SD, ExtPtfm, &
                          SrvD, MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, ErrStat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

   !----------------------------------------------------------------------------------------
   !! Display simulation status every SttsTime-seconds (i.e., n_SttsTime steps):
   !----------------------------------------------------------------------------------------

   IF (p_FAST%WrSttsTime) then
      IF ( MOD( n_t_global_next, p_FAST%n_SttsTime ) == 0 ) THEN
            CALL SimStatus( m_FAST%TiLstPrn, m_FAST%PrevClockTime, m_FAST%t_global, p_FAST%TMax, p_FAST%TDesc )

      ENDIF
   ENDIF

END SUBROUTINE FAST_Solution
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE GetTimeConstants(DampedFreq_Hz, VTK_fps, nt, dt, VTK_tWidth )
   REAL(R8Ki),     INTENT(IN   ) :: DampedFreq_Hz
   REAL(DbKi),     INTENT(IN   ) :: VTK_fps
   INTEGER(IntKi), INTENT(  OUT) :: nt  !< number of steps
   REAL(DbKi),     INTENT(  OUT) :: dt  !< time step
   INTEGER(IntKi), INTENT(  OUT) :: VTK_tWidth

   REAL(DbKi)                              :: cycle_time          ! time for one cycle of mode
   INTEGER(IntKi)                          :: NCycles
   INTEGER(IntKi), PARAMETER               :: MinFrames = 5

   if (DampedFreq_Hz <= 0.0_DbKi) then
      nt = huge(nt)
      dt = epsilon(dt)
      VTK_tWidth = 1
      return
   end if

   nt = 1
   NCycles = 0
   do while (nt<MinFrames)
      NCycles = NCycles + 1
      cycle_time = NCycles * 1.0_DbKi / DampedFreq_Hz

      nt = NINT( max(1.0_DbKi, VTK_fps) * cycle_time )
   end do

   dt = cycle_time / nt

   VTK_tWidth = CEILING( log10( real(nt) ) ) + 1

END SUBROUTINE GetTimeConstants
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Linerization routines
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!> Routine that calls FAST_Linearize_T for an array of Turbine data structures if the linearization flag is set for each individual turbine.
SUBROUTINE FAST_Linearize_Tary(t_initial, n_t_global, Turbine, ErrStat, ErrMsg)

   REAL(DbKi),               INTENT(IN   ) :: t_initial           !< initial simulation time (almost always 0)
   INTEGER(IntKi),           INTENT(IN   ) :: n_t_global          !< integer time step
   TYPE(FAST_TurbineType),   INTENT(INOUT) :: Turbine(:)          !< all data for one instance of a turbine
   INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None

      ! local variables
   INTEGER(IntKi)                          :: i_turb, NumTurbines
   INTEGER(IntKi)                          :: ErrStat2            ! local error status
   CHARACTER(ErrMsgLen)                    :: ErrMsg2             ! local error message
   CHARACTER(*),             PARAMETER     :: RoutineName = 'FAST_Linearize_Tary'


   NumTurbines = SIZE(Turbine)
   ErrStat = ErrID_None
   ErrMsg  = ""

   DO i_turb = 1,NumTurbines

      CALL FAST_Linearize_T(t_initial, n_t_global, Turbine(i_turb), ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         IF (ErrStat >= AbortErrLev) RETURN

   END DO


END SUBROUTINE FAST_Linearize_Tary
!----------------------------------------------------------------------------------------------------------------------------------
!> Routine that performs lineaization at an operating point for a turbine. This is a separate subroutine so that the FAST
!! driver programs do not need to change or operate on the individual module level.
SUBROUTINE FAST_Linearize_T(t_initial, n_t_global, Turbine, ErrStat, ErrMsg)

   REAL(DbKi),               INTENT(IN   ) :: t_initial           !< initial simulation time (almost always 0)
   INTEGER(IntKi),           INTENT(IN   ) :: n_t_global          !< integer time step
   TYPE(FAST_TurbineType),   INTENT(INOUT) :: Turbine             !< all data for one instance of a turbine
   INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None

      ! local variables
   REAL(DbKi)                              :: t_global            ! current simulation time
   REAL(DbKi)                              :: next_lin_time       ! next simulation time where linearization analysis should be performed
   INTEGER(IntKi)                          :: iLinTime            ! loop counter
   INTEGER(IntKi)                          :: ErrStat2            ! local error status
   CHARACTER(ErrMsgLen)                    :: ErrMsg2             ! local error message
   CHARACTER(*),             PARAMETER     :: RoutineName = 'FAST_Linearize_T'


   ErrStat = ErrID_None
   ErrMsg  = ""

   if ( .not. Turbine%p_FAST%Linearize ) return

   if (.not. Turbine%p_FAST%CalcSteady) then

      if ( Turbine%m_FAST%Lin%NextLinTimeIndx <= Turbine%p_FAST%NLinTimes ) then  !bjj: maybe this logic should go in FAST_Linearize_OP???

         next_lin_time = Turbine%m_FAST%Lin%LinTimes( Turbine%m_FAST%Lin%NextLinTimeIndx )
         t_global      = t_initial + n_t_global*Turbine%p_FAST%dt

         if ( EqualRealNos( t_global, next_lin_time ) .or. t_global > next_lin_time ) then

            CALL FAST_Linearize_OP(t_global, Turbine%p_FAST, Turbine%y_FAST, Turbine%m_FAST, &
                     Turbine%ED, Turbine%BD, Turbine%SrvD, Turbine%AD, Turbine%IfW, Turbine%OpFM, &
                     Turbine%HD, Turbine%SD, Turbine%ExtPtfm, Turbine%MAP, Turbine%FEAM, Turbine%MD, Turbine%Orca, &
                     Turbine%IceF, Turbine%IceD, Turbine%MeshMapData, ErrStat2, ErrMsg2 )
               CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
               IF (ErrStat >= AbortErrLev) RETURN

            if (Turbine%p_FAST%WrVTK == VTK_ModeShapes) then
               if (Turbine%m_FAST%Lin%NextLinTimeIndx > Turbine%p_FAST%NLinTimes) call WrVTKCheckpoint()
            end if

         end if

      end if

   else ! CalcSteady

      t_global      = t_initial + n_t_global*Turbine%p_FAST%dt

      call FAST_CalcSteady( n_t_global, t_global, Turbine%p_FAST, Turbine%y_FAST, Turbine%m_FAST, Turbine%ED, Turbine%BD, Turbine%SrvD, &
                      Turbine%AD, Turbine%IfW, Turbine%OpFM, Turbine%HD, Turbine%SD, Turbine%ExtPtfm, Turbine%MAP, Turbine%FEAM, Turbine%MD, &
                      Turbine%Orca, Turbine%IceF, Turbine%IceD, ErrStat2, ErrMsg2 )
            call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

      if (Turbine%m_FAST%Lin%FoundSteady) then
         if (Turbine%m_FAST%Lin%ForceLin) then
            Turbine%p_FAST%NLinTimes=1
         endif

         do iLinTime=1,Turbine%p_FAST%NLinTimes
            t_global = Turbine%m_FAST%Lin%LinTimes(iLinTime)

            call SetOperatingPoint(iLinTime, Turbine%p_FAST, Turbine%y_FAST, Turbine%m_FAST, Turbine%ED, Turbine%BD, Turbine%SrvD, &
                                      Turbine%AD, Turbine%IfW, Turbine%OpFM, Turbine%HD, Turbine%SD, Turbine%ExtPtfm, &
                                    Turbine%MAP, Turbine%FEAM, Turbine%MD, Turbine%Orca, Turbine%IceF, Turbine%IceD, ErrStat2, ErrMsg2 )
               CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

            if (Turbine%p_FAST%DT_UJac < Turbine%p_FAST%TMax) then
               Turbine%m_FAST%calcJacobian = .true.
               Turbine%m_FAST%NextJacCalcTime = t_global
            end if

            CALL CalcOutputs_And_SolveForInputs( -1,  t_global,  STATE_CURR, Turbine%m_FAST%calcJacobian, Turbine%m_FAST%NextJacCalcTime, &
               Turbine%p_FAST, Turbine%m_FAST, .false., Turbine%ED, Turbine%BD, Turbine%SrvD, Turbine%AD14, Turbine%AD, Turbine%IfW, Turbine%OpFM, &
               Turbine%HD, Turbine%SD, Turbine%ExtPtfm, Turbine%MAP, Turbine%FEAM, Turbine%MD, Turbine%Orca, Turbine%IceF, Turbine%IceD, Turbine%MeshMapData, ErrStat2, ErrMsg2 )
               CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
               IF (ErrStat >= AbortErrLev) RETURN

            CALL FAST_Linearize_OP(t_global, Turbine%p_FAST, Turbine%y_FAST, Turbine%m_FAST, &
                     Turbine%ED, Turbine%BD, Turbine%SrvD, Turbine%AD, Turbine%IfW, Turbine%OpFM, &
                     Turbine%HD, Turbine%SD, Turbine%ExtPtfm, Turbine%MAP, Turbine%FEAM, Turbine%MD, Turbine%Orca, &
                     Turbine%IceF, Turbine%IceD, Turbine%MeshMapData, ErrStat2, ErrMsg2 )
               CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
               IF (ErrStat >= AbortErrLev) RETURN

         end do

         if (Turbine%p_FAST%WrVTK == VTK_ModeShapes) CALL WrVTKCheckpoint()

         if (Turbine%m_FAST%Lin%ForceLin) then
            ErrStat2 = ErrID_Warn
            ErrMsg2  = 'Linearization was forced at simulation end. The linearized model may not be sufficiently representative of the solution in steady state.'
            CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         endif

      end if

   end if
   return

contains
   subroutine WrVTKCheckpoint()
         ! we are creating a checkpoint file for each turbine, so setting NumTurbines=1 in the file
      CALL FAST_CreateCheckpoint_T(t_initial, Turbine%p_FAST%n_TMax_m1+1, 1, Turbine, TRIM(Turbine%p_FAST%OutFileRoot)//'.ModeShapeVTK', ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   end subroutine WrVTKCheckpoint
END SUBROUTINE FAST_Linearize_T
!----------------------------------------------------------------------------------------------------------------------------------

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! PROGRAM EXIT ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!> Routine that calls ExitThisProgram for one instance of a Turbine data structure. This is a separate subroutine so that the FAST
!! driver programs do not need to change or operate on the individual module level.
!! This routine should be called from glue code only (e.g., FAST_Prog.f90). It should not be called in any of these driver routines.
SUBROUTINE ExitThisProgram_T( Turbine, ErrLevel_in, StopTheProgram, ErrLocMsg, SkipRunTimeMsg )

   TYPE(FAST_TurbineType),   INTENT(INOUT) :: Turbine             !< Data for one turbine instance
   INTEGER(IntKi),           INTENT(IN)    :: ErrLevel_in         !< Error level when Error == .TRUE. (required when Error is .TRUE.)
   LOGICAL,                  INTENT(IN)    :: StopTheProgram      !< flag indicating if the program should end (false if there are more turbines to end)
   CHARACTER(*), OPTIONAL,   INTENT(IN)    :: ErrLocMsg           !< an optional message describing the location of the error
   LOGICAL,      OPTIONAL,   INTENT(IN)    :: SkipRunTimeMsg      !< an optional message describing run-time stats

   LOGICAL                                 :: SkipRunTimes

   IF (PRESENT(SkipRunTimeMsg)) THEN
      SkipRunTimes = SkipRunTimeMsg
   ELSE
      SkipRunTimes = .FALSE.
   END IF


   IF (PRESENT(ErrLocMsg)) THEN

      CALL ExitThisProgram( Turbine%p_FAST, Turbine%y_FAST, Turbine%m_FAST, &
                     Turbine%ED, Turbine%BD, Turbine%SrvD, Turbine%AD14, Turbine%AD, Turbine%IfW, Turbine%OpFM, &
                     Turbine%HD, Turbine%SD, Turbine%ExtPtfm, Turbine%MAP, Turbine%FEAM, Turbine%MD, Turbine%Orca, &
                     Turbine%IceF, Turbine%IceD, Turbine%MeshMapData, ErrLevel_in, StopTheProgram, ErrLocMsg, SkipRunTimes )

   ELSE

      CALL ExitThisProgram( Turbine%p_FAST, Turbine%y_FAST, Turbine%m_FAST, &
                     Turbine%ED, Turbine%BD, Turbine%SrvD, Turbine%AD14, Turbine%AD, Turbine%IfW, Turbine%OpFM, &
                     Turbine%HD, Turbine%SD, Turbine%ExtPtfm, Turbine%MAP, Turbine%FEAM, Turbine%MD, Turbine%Orca, &
                     Turbine%IceF, Turbine%IceD, Turbine%MeshMapData, ErrLevel_in, StopTheProgram, SkipRunTimeMsg=SkipRunTimes )

   END IF

END SUBROUTINE ExitThisProgram_T
!----------------------------------------------------------------------------------------------------------------------------------
!> This subroutine is called when FAST exits. It calls all the modules' end routines and cleans up variables declared in the
!! main program. If there was an error, it also aborts. Otherwise, it prints the run times and performs a normal exit.
!! This routine should not be called from glue code (e.g., FAST_Prog.f90) or ExitThisProgram_T only. It should not be called in any
!! of these driver routines.
SUBROUTINE ExitThisProgram( p_FAST, y_FAST, m_FAST, ED, BD, SrvD, AD14, AD, IfW, OpFM, HD, SD, ExtPtfm, &
                            MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, ErrLevel_in, StopTheProgram, ErrLocMsg, SkipRunTimeMsg )

      ! Passed arguments
   TYPE(FAST_ParameterType), INTENT(INOUT) :: p_FAST              !< Parameters for the glue code
   TYPE(FAST_OutputFileType),INTENT(INOUT) :: y_FAST              !< Output variables for the glue code
   TYPE(FAST_MiscVarType),   INTENT(INOUT) :: m_FAST              !< Miscellaneous variables

   TYPE(ElastoDyn_Data),     INTENT(INOUT) :: ED                  !< ElastoDyn data
   TYPE(BeamDyn_Data),       INTENT(INOUT) :: BD                  !< BeamDyn data
   TYPE(ServoDyn_Data),      INTENT(INOUT) :: SrvD                !< ServoDyn data
   TYPE(AeroDyn14_Data),     INTENT(INOUT) :: AD14                !< AeroDyn v14 data
   TYPE(AeroDyn_Data),       INTENT(INOUT) :: AD                  !< AeroDyn data
   TYPE(InflowWind_Data),    INTENT(INOUT) :: IfW                 !< InflowWind data
   TYPE(OpenFOAM_Data),      INTENT(INOUT) :: OpFM                !< OpenFOAM data
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

   INTEGER(IntKi),           INTENT(IN)    :: ErrLevel_in         !< Error level when Error == .TRUE. (required when Error is .TRUE.)
   LOGICAL,                  INTENT(IN)    :: StopTheProgram      !< flag indicating if the program should end (false if there are more turbines to end)
   CHARACTER(*), OPTIONAL,   INTENT(IN)    :: ErrLocMsg           !< an optional message describing the location of the error
   LOGICAL,      OPTIONAL,   INTENT(IN)    :: SkipRunTimeMsg      !< an optional message describing run-time stats


      ! Local variables:
   INTEGER(IntKi)                          :: ErrorLevel
   LOGICAL                                 :: PrintRunTimes

   INTEGER(IntKi)                          :: ErrStat2            ! Error status
   CHARACTER(ErrMsgLen)                    :: ErrMsg2             ! Error message
   CHARACTER(1224)                         :: SimMsg              ! optional message to print about where the error took place in the simulation

   CHARACTER(*), PARAMETER                 :: RoutineName = 'ExitThisProgram'


   ErrorLevel = ErrLevel_in

      ! for debugging, let's output the meshes and all of their fields
   IF ( ErrorLevel >= AbortErrLev .AND. p_FAST%WrVTK > VTK_None .and. .not. m_FAST%Lin%FoundSteady) THEN
      p_FAST%VTK_OutFileRoot = trim(p_FAST%VTK_OutFileRoot)//'.DebugError'
      p_FAST%VTK_fields = .true.
      CALL WrVTK_AllMeshes(p_FAST, y_FAST, MeshMapData, ED, BD, AD, IfW, OpFM, HD, SD, ExtPtfm, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD)
   end if



      ! End all modules
   CALL FAST_EndMods( p_FAST, y_FAST, m_FAST, ED, BD, SrvD, AD14, AD, IfW, HD, SD, ExtPtfm, MAPp, FEAM, MD, Orca, IceF, IceD, ErrStat2, ErrMsg2 )
      IF (ErrStat2 /= ErrID_None) THEN
         CALL WrScr( NewLine//RoutineName//':'//TRIM(ErrMsg2)//NewLine )
         ErrorLevel = MAX(ErrorLevel,ErrStat2)
      END IF

      ! Destroy all data associated with FAST variables:

   CALL FAST_DestroyAll( p_FAST, y_FAST, m_FAST, ED, BD, SrvD, AD14, AD, IfW, OpFM, HD, SD, ExtPtfm, MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, ErrStat2, ErrMsg2 )
      IF (ErrStat2 /= ErrID_None) THEN
         CALL WrScr( NewLine//RoutineName//':'//TRIM(ErrMsg2)//NewLine )
         ErrorLevel = MAX(ErrorLevel,ErrStat2)
      END IF


   !............................................................................................................................
   ! Set exit error code if there was an error;
   !............................................................................................................................
   IF ( ErrorLevel >= AbortErrLev ) THEN

      IF (PRESENT(ErrLocMsg)) THEN
         SimMsg = ErrLocMsg
      ELSE
         SimMsg = 'after the simulation completed'
      END IF

      IF (y_FAST%UnSum > 0) THEN
         CLOSE(y_FAST%UnSum)
         y_FAST%UnSum = -1
      END IF
      
                         
      SimMsg = TRIM(FAST_Ver%Name)//' encountered an error '//TRIM(SimMsg)//'.'//NewLine//' Simulation error level: '//TRIM(GetErrStr(ErrorLevel))
      if (StopTheProgram) then
         CALL ProgAbort( trim(SimMsg), TrapErrors=.FALSE., TimeWait=3._ReKi )  ! wait 3 seconds (in case they double-clicked and got an error)
      else
         CALL WrScr(trim(SimMsg))
      end if

   END IF

   !............................................................................................................................
   !  Write simulation times and stop
   !............................................................................................................................
   if (present(SkipRunTimeMsg)) then
      PrintRunTimes = .not. SkipRunTimeMsg
   else
      PrintRunTimes = .true.
   end if

   IF (p_FAST%WrSttsTime .and. PrintRunTimes) THEN
      CALL RunTimes( m_FAST%StrtTime, m_FAST%UsrTime1, m_FAST%SimStrtTime, m_FAST%UsrTime2, m_FAST%t_global, UnSum=y_FAST%UnSum, DescStrIn=p_FAST%TDesc )
   END IF
   IF (y_FAST%UnSum > 0) THEN
      CLOSE(y_FAST%UnSum)
      y_FAST%UnSum = -1
   END IF

   if (StopTheProgram) then
#if (defined COMPILE_SIMULINK || defined COMPILE_LABVIEW)
      ! for Simulink, this may not be a normal stop. It might call this after an error in the model.
      CALL WrScr( NewLine//' '//TRIM(FAST_Ver%Name)//' completed.'//NewLine )
#else
      CALL NormStop( )
#endif
   end if


END SUBROUTINE ExitThisProgram
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine calls the end routines for each module that was previously initialized.
SUBROUTINE FAST_EndMods( p_FAST, y_FAST, m_FAST, ED, BD, SrvD, AD14, AD, IfW, HD, SD, ExtPtfm, MAPp, FEAM, MD, Orca, IceF, IceD, ErrStat, ErrMsg )

   TYPE(FAST_ParameterType), INTENT(INOUT) :: p_FAST              !< Parameters for the glue code
   TYPE(FAST_OutputFileType),INTENT(INOUT) :: y_FAST              !< Output variables for the glue code
   TYPE(FAST_MiscVarType),   INTENT(INOUT) :: m_FAST              !< Miscellaneous variables

   TYPE(ElastoDyn_Data),     INTENT(INOUT) :: ED                  !< ElastoDyn data
   TYPE(BeamDyn_Data),       INTENT(INOUT) :: BD                  !< BeamDyn data
   TYPE(ServoDyn_Data),      INTENT(INOUT) :: SrvD                !< ServoDyn data
   TYPE(AeroDyn14_Data),     INTENT(INOUT) :: AD14                !< AeroDyn v14 data
   TYPE(AeroDyn_Data),       INTENT(INOUT) :: AD                  !< AeroDyn data
   TYPE(InflowWind_Data),    INTENT(INOUT) :: IfW                 !< InflowWind data
   TYPE(HydroDyn_Data),      INTENT(INOUT) :: HD                  !< HydroDyn data
   TYPE(SubDyn_Data),        INTENT(INOUT) :: SD                  !< SubDyn data
   TYPE(ExtPtfm_Data),       INTENT(INOUT) :: ExtPtfm             !< ExtPtfm data
   TYPE(MAP_Data),           INTENT(INOUT) :: MAPp                !< MAP data
   TYPE(FEAMooring_Data),    INTENT(INOUT) :: FEAM                !< FEAMooring data
   TYPE(MoorDyn_Data),       INTENT(INOUT) :: MD                  !< Data for the MoorDyn module
   TYPE(OrcaFlex_Data),      INTENT(INOUT) :: Orca                !< OrcaFlex interface data
   TYPE(IceFloe_Data),       INTENT(INOUT) :: IceF                !< IceFloe data
   TYPE(IceDyn_Data),        INTENT(INOUT) :: IceD                !< All the IceDyn data used in time-step loop

   INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None

   ! local variables
   INTEGER(IntKi)                          :: i, k                ! loop counter

   INTEGER(IntKi)                          :: ErrStat2
   CHARACTER(ErrMsgLen)                    :: ErrMsg2
   CHARACTER(*), PARAMETER                 :: RoutineName = 'FAST_EndMods'

      !...............................................................................................................................
      ! End all modules (and write binary FAST output file)
      !...............................................................................................................................

   ErrStat = ErrID_None
   ErrMsg  = ""


   CALL FAST_EndOutput( p_FAST, y_FAST, m_FAST, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   IF ( p_FAST%ModuleInitialized(Module_ED) ) THEN
      CALL ED_End(   ED%Input(1),   ED%p,   ED%x(STATE_CURR),   ED%xd(STATE_CURR),   ED%z(STATE_CURR),   ED%OtherSt(STATE_CURR),   &
                     ED%y,          ED%m,  ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   END IF

   IF ( p_FAST%ModuleInitialized(Module_BD) ) THEN

      DO k=1,p_FAST%nBeams
         CALL BD_End(BD%Input(1,k),  BD%p(k),  BD%x(k,STATE_CURR),  BD%xd(k,STATE_CURR),  BD%z(k,STATE_CURR), &
                        BD%OtherSt(k,STATE_CURR),  BD%y(k),  BD%m(k), ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      END DO

   END IF


   IF ( p_FAST%ModuleInitialized(Module_AD14) ) THEN
      CALL AD14_End( AD14%Input(1), AD14%p, AD14%x(STATE_CURR), AD14%xd(STATE_CURR), AD14%z(STATE_CURR), &
                     AD14%OtherSt(STATE_CURR), AD14%y, AD14%m, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   ELSEIF ( p_FAST%ModuleInitialized(Module_AD) ) THEN
      CALL AD_End(   AD%Input(1), AD%p, AD%x(STATE_CURR), AD%xd(STATE_CURR), AD%z(STATE_CURR), &
                     AD%OtherSt(STATE_CURR), AD%y, AD%m,  ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   END IF

   IF ( p_FAST%ModuleInitialized(Module_IfW) ) THEN
      CALL InflowWind_End( IfW%Input(1), IfW%p, IfW%x(STATE_CURR), IfW%xd(STATE_CURR), IfW%z(STATE_CURR), IfW%OtherSt(STATE_CURR),   &
                           IfW%y, IfW%m, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   END IF

   IF ( p_FAST%ModuleInitialized(Module_SrvD) ) THEN
      CALL SrvD_End( SrvD%Input(1), SrvD%p, SrvD%x(STATE_CURR), SrvD%xd(STATE_CURR), SrvD%z(STATE_CURR), SrvD%OtherSt(STATE_CURR), &
                     SrvD%y, SrvD%m, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   END IF

   IF ( p_FAST%ModuleInitialized(Module_HD) ) THEN
      CALL HydroDyn_End( HD%Input(1), HD%p, HD%x(STATE_CURR), HD%xd(STATE_CURR), HD%z(STATE_CURR), HD%OtherSt(STATE_CURR),  &
                         HD%y, HD%m, ErrStat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   END IF

   IF ( p_FAST%ModuleInitialized(Module_SD) ) THEN
      CALL SD_End( SD%Input(1), SD%p, SD%x(STATE_CURR), SD%xd(STATE_CURR), SD%z(STATE_CURR), SD%OtherSt(STATE_CURR),   &
                   SD%y, SD%m, ErrStat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   ELSE IF ( p_FAST%ModuleInitialized(Module_ExtPtfm) ) THEN
      CALL ExtPtfm_End( ExtPtfm%Input(1), ExtPtfm%p, ExtPtfm%x(STATE_CURR), ExtPtfm%xd(STATE_CURR), ExtPtfm%z(STATE_CURR), &
                        ExtPtfm%OtherSt(STATE_CURR), ExtPtfm%y, ExtPtfm%m, ErrStat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   END IF

   IF ( p_FAST%ModuleInitialized(Module_MAP) ) THEN
      CALL MAP_End(    MAPp%Input(1),   MAPp%p,   MAPp%x(STATE_CURR),   MAPp%xd(STATE_CURR),   MAPp%z(STATE_CURR),   MAPp%OtherSt,   &
                        MAPp%y,   ErrStat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   ELSEIF ( p_FAST%ModuleInitialized(Module_MD) ) THEN
      CALL MD_End(  MD%Input(1), MD%p, MD%x(STATE_CURR), MD%xd(STATE_CURR), MD%z(STATE_CURR), MD%OtherSt(STATE_CURR), &
                    MD%y, MD%m, ErrStat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   ELSEIF ( p_FAST%ModuleInitialized(Module_FEAM) ) THEN
      CALL FEAM_End( FEAM%Input(1), FEAM%p, FEAM%x(STATE_CURR), FEAM%xd(STATE_CURR), FEAM%z(STATE_CURR),   &
                     FEAM%OtherSt(STATE_CURR), FEAM%y, FEAM%m, ErrStat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   ELSEIF ( p_FAST%ModuleInitialized(Module_Orca) ) THEN
      CALL Orca_End(   Orca%Input(1),  Orca%p,  Orca%x(STATE_CURR),  Orca%xd(STATE_CURR),  Orca%z(STATE_CURR),  Orca%OtherSt(STATE_CURR),  &
                        Orca%y,  Orca%m, ErrStat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   END IF

   IF ( p_FAST%ModuleInitialized(Module_IceF) ) THEN
      CALL IceFloe_End(IceF%Input(1), IceF%p, IceF%x(STATE_CURR), IceF%xd(STATE_CURR), IceF%z(STATE_CURR),  &
                       IceF%OtherSt(STATE_CURR), IceF%y, IceF%m, ErrStat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   ELSEIF ( p_FAST%ModuleInitialized(Module_IceD) ) THEN

      DO i=1,p_FAST%numIceLegs
         CALL IceD_End(IceD%Input(1,i),  IceD%p(i),  IceD%x(i,STATE_CURR),  IceD%xd(i,STATE_CURR),  IceD%z(i,STATE_CURR), &
                        IceD%OtherSt(i,STATE_CURR),  IceD%y(i),  IceD%m(i), ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      END DO

   END IF

END SUBROUTINE FAST_EndMods
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine calls the destroy routines for each module. (It is basically a duplicate of FAST_DestroyTurbineType().)
SUBROUTINE FAST_DestroyAll( p_FAST, y_FAST, m_FAST, ED, BD, SrvD, AD14, AD, IfW, OpFM, HD, SD, ExtPtfm, &
                            MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, ErrStat, ErrMsg )

   TYPE(FAST_ParameterType), INTENT(INOUT) :: p_FAST              !< Parameters for the glue code
   TYPE(FAST_OutputFileType),INTENT(INOUT) :: y_FAST              !< Output variables for the glue code
   TYPE(FAST_MiscVarType),   INTENT(INOUT) :: m_FAST              !< Miscellaneous variables

   TYPE(ElastoDyn_Data),     INTENT(INOUT) :: ED                  !< ElastoDyn data
   TYPE(BeamDyn_Data),       INTENT(INOUT) :: BD                  !< BeamDyn data
   TYPE(ServoDyn_Data),      INTENT(INOUT) :: SrvD                !< ServoDyn data
   TYPE(AeroDyn14_Data),     INTENT(INOUT) :: AD14                !< AeroDyn v14 data
   TYPE(AeroDyn_Data),       INTENT(INOUT) :: AD                  !< AeroDyn data
   TYPE(InflowWind_Data),    INTENT(INOUT) :: IfW                 !< InflowWind data
   TYPE(OpenFOAM_Data),      INTENT(INOUT) :: OpFM                !< OpenFOAM data
   TYPE(HydroDyn_Data),      INTENT(INOUT) :: HD                  !< HydroDyn data
   TYPE(SubDyn_Data),        INTENT(INOUT) :: SD                  !< SubDyn data
   TYPE(ExtPtfm_Data),       INTENT(INOUT) :: ExtPtfm             !< ExtPtfm data
   TYPE(MAP_Data),           INTENT(INOUT) :: MAPp                !< MAP data
   TYPE(FEAMooring_Data),    INTENT(INOUT) :: FEAM                !< FEAMooring data
   TYPE(MoorDyn_Data),       INTENT(INOUT) :: MD                  !< Data for the MoorDyn module
   TYPE(OrcaFlex_Data),      INTENT(INOUT) :: Orca                !< OrcaFlex interface data
   TYPE(IceFloe_Data),       INTENT(INOUT) :: IceF                !< IceFloe data
   TYPE(IceDyn_Data),        INTENT(INOUT) :: IceD                !< All the IceDyn data used in time-step loop

   TYPE(FAST_ModuleMapType), INTENT(INOUT) :: MeshMapData         !< Data for mapping between modules

   INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None

   ! local variables
   INTEGER(IntKi)                          :: ErrStat2
   CHARACTER(ErrMsgLen)                    :: ErrMsg2
   CHARACTER(*), PARAMETER                 :: RoutineName = 'FAST_DestroyAll'



   ! -------------------------------------------------------------------------
   ! Deallocate/Destroy structures associated with mesh mapping
   ! -------------------------------------------------------------------------

   ErrStat = ErrID_None
   ErrMsg  = ""


   ! FAST
   CALL FAST_DestroyParam( p_FAST, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   CALL FAST_DestroyOutputFileType( y_FAST, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   CALL FAST_DestroyMisc( m_FAST, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   ! ElastoDyn
   CALL FAST_DestroyElastoDyn_Data( ED, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   ! BeamDyn
   CALL FAST_DestroyBeamDyn_Data( BD, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   ! ServoDyn
   CALL FAST_DestroyServoDyn_Data( SrvD, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   ! AeroDyn14
   CALL FAST_DestroyAeroDyn14_Data( AD14, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   ! AeroDyn
   CALL FAST_DestroyAeroDyn_Data( AD, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   ! InflowWind
   CALL FAST_DestroyInflowWind_Data( IfW, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   ! OpenFOAM
   CALL FAST_DestroyOpenFOAM_Data( OpFM, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   ! HydroDyn
   CALL FAST_DestroyHydroDyn_Data( HD, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   ! SubDyn
   CALL FAST_DestroySubDyn_Data( SD, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   ! ExtPtfm
   CALL FAST_DestroyExtPtfm_Data( ExtPtfm, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)


   ! MAP
   CALL FAST_DestroyMAP_Data( MAPp, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   ! FEAMooring
   CALL FAST_DestroyFEAMooring_Data( FEAM, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   ! MoorDyn
   CALL FAST_DestroyMoorDyn_Data( MD, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   ! Orca
   CALL FAST_DestroyOrcaFlex_Data( Orca, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)


   ! IceFloe
   CALL FAST_DestroyIceFloe_Data( IceF, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   ! IceDyn
   CALL FAST_DestroyIceDyn_Data( IceD, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   ! Module (Mesh) Mapping data
   CALL FAST_DestroyModuleMapType( MeshMapData, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)



   END SUBROUTINE FAST_DestroyAll
!----------------------------------------------------------------------------------------------------------------------------------


!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! CHECKPOINT/RESTART ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!> Routine that calls FAST_CreateCheckpoint_T for an array of Turbine data structures.
SUBROUTINE FAST_CreateCheckpoint_Tary(t_initial, n_t_global, Turbine, CheckpointRoot, ErrStat, ErrMsg)

   REAL(DbKi),               INTENT(IN   ) :: t_initial           !< initial time
   INTEGER(IntKi),           INTENT(IN   ) :: n_t_global          !< loop counter
   TYPE(FAST_TurbineType),   INTENT(INOUT) :: Turbine(:)          !< all data for all turbines
   CHARACTER(*),             INTENT(IN   ) :: CheckpointRoot      !< Rootname of checkpoint file
   INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None

      ! local variables
   INTEGER(IntKi)                          :: NumTurbines         ! Number of turbines in this simulation
   INTEGER(IntKi)                          :: i_turb
   INTEGER                                 :: Unit
   INTEGER(IntKi)                          :: ErrStat2            ! local error status
   CHARACTER(ErrMsgLen)                    :: ErrMsg2             ! local error message
   CHARACTER(*),             PARAMETER     :: RoutineName = 'FAST_CreateCheckpoint_Tary'


   NumTurbines = SIZE(Turbine)
   ErrStat = ErrID_None
   ErrMsg  = ""

   ! TRIM(CheckpointRoot)//'.'//TRIM(Num2LStr(Turbine%TurbID))//

      !! This allows us to put all the turbine data in one file.
   Unit = -1
   DO i_turb = 1,NumTurbines
      CALL FAST_CreateCheckpoint_T(t_initial, n_t_global, NumTurbines, Turbine(i_turb), CheckpointRoot, ErrStat2, ErrMsg2, Unit )
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         if (ErrStat >= AbortErrLev ) then
            if (Unit > 0) close(Unit)
            RETURN
         end if

   END DO


END SUBROUTINE FAST_CreateCheckpoint_Tary
!----------------------------------------------------------------------------------------------------------------------------------
!> Routine that packs all of the data from one turbine instance into arrays and writes checkpoint files. If Unit is present and
!! greater than 0, it will append the data to an already open file. Otherwise, it opens a new file and writes header information
!! before writing the turbine data to the file.
SUBROUTINE FAST_CreateCheckpoint_T(t_initial, n_t_global, NumTurbines, Turbine, CheckpointRoot, ErrStat, ErrMsg, Unit )

   USE BladedInterface, ONLY: CallBladedDLL  ! Hack for Bladed-style DLL
   USE BladedInterface, ONLY: GH_DISCON_STATUS_CHECKPOINT

   REAL(DbKi),               INTENT(IN   ) :: t_initial           !< initial time
   INTEGER(IntKi),           INTENT(IN   ) :: n_t_global          !< loop counter
   INTEGER(IntKi),           INTENT(IN   ) :: NumTurbines         !< Number of turbines in this simulation
   TYPE(FAST_TurbineType),   INTENT(INOUT) :: Turbine             !< all data for one instance of a turbine (INTENT(OUT) only because of hack for Bladed DLL)
   CHARACTER(*),             INTENT(IN   ) :: CheckpointRoot      !< Rootname of checkpoint file
   INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None
   INTEGER(IntKi), OPTIONAL, INTENT(INOUT) :: Unit                !< unit number for output file

      ! local variables:
   REAL(ReKi),               ALLOCATABLE   :: ReKiBuf(:)
   REAL(DbKi),               ALLOCATABLE   :: DbKiBuf(:)
   INTEGER(IntKi),           ALLOCATABLE   :: IntKiBuf(:)

   INTEGER(B4Ki)                           :: ArraySizes(3)

   INTEGER(IntKi)                          :: unOut               ! unit number for output file
   INTEGER(IntKi)                          :: old_avrSwap1        ! previous value of avrSwap(1) !hack for Bladed DLL checkpoint/restore
   INTEGER(IntKi)                          :: ErrStat2            ! local error status
   CHARACTER(ErrMsgLen)                    :: ErrMsg2             ! local error message
   CHARACTER(*),             PARAMETER     :: RoutineName = 'FAST_CreateCheckpoint_T'

   CHARACTER(1024)                         :: FileName            ! Name of the (output) checkpoint file
   CHARACTER(1024)                         :: DLLFileName         ! Name of the (output) checkpoint file

      ! init error status
   ErrStat = ErrID_None
   ErrMsg  = ""

      ! Get the arrays of data to be stored in the output file
   CALL FAST_PackTurbineType( ReKiBuf, DbKiBuf, IntKiBuf, Turbine, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      if (ErrStat >= AbortErrLev ) then
         call cleanup()
         RETURN
      end if


   ArraySizes = 0
   IF ( ALLOCATED(ReKiBuf)  ) ArraySizes(1) = SIZE(ReKiBuf)
   IF ( ALLOCATED(DbKiBuf)  ) ArraySizes(2) = SIZE(DbKiBuf)
   IF ( ALLOCATED(IntKiBuf) ) ArraySizes(3) = SIZE(IntKiBuf)

   FileName    = TRIM(CheckpointRoot)//'.chkp'
   DLLFileName = TRIM(CheckpointRoot)//'.dll.chkp'

   unOut=-1
   IF (PRESENT(Unit)) unOut = Unit

   IF ( unOut < 0 ) THEN

      CALL GetNewUnit( unOut, ErrStat2, ErrMsg2 )
      CALL OpenBOutFile ( unOut, FileName, ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         if (ErrStat >= AbortErrLev ) then
            call cleanup()
            IF (.NOT. PRESENT(Unit)) THEN
               CLOSE(unOut)
               unOut = -1
            END IF

            RETURN
         end if

         ! checkpoint file header:
      WRITE (unOut, IOSTAT=ErrStat2)   INT(ReKi              ,B4Ki)     ! let's make sure we've got the correct number of bytes for reals on restart.
      WRITE (unOut, IOSTAT=ErrStat2)   INT(DbKi              ,B4Ki)     ! let's make sure we've got the correct number of bytes for doubles on restart.
      WRITE (unOut, IOSTAT=ErrStat2)   INT(IntKi             ,B4Ki)     ! let's make sure we've got the correct number of bytes for integers on restart.
      WRITE (unOut, IOSTAT=ErrStat2)   AbortErrLev
      WRITE (unOut, IOSTAT=ErrStat2)   NumTurbines                      ! Number of turbines
      WRITE (unOut, IOSTAT=ErrStat2)   t_initial                        ! initial time
      WRITE (unOut, IOSTAT=ErrStat2)   n_t_global                       ! current time step

   END IF


      ! data from current turbine at time step:
   WRITE (unOut, IOSTAT=ErrStat2)   ArraySizes                       ! Number of reals, doubles, and integers written to file
   WRITE (unOut, IOSTAT=ErrStat2)   ReKiBuf                          ! Packed reals
   WRITE (unOut, IOSTAT=ErrStat2)   DbKiBuf                          ! Packed doubles
   WRITE (unOut, IOSTAT=ErrStat2)   IntKiBuf                         ! Packed integers


   IF ( ALLOCATED(ReKiBuf)  ) DEALLOCATE(ReKiBuf)
   IF ( ALLOCATED(DbKiBuf)  ) DEALLOCATE(DbKiBuf)
   IF ( ALLOCATED(IntKiBuf) ) DEALLOCATE(IntKiBuf)

      !CALL FAST_CreateCheckpoint(t_initial, n_t_global, Turbine%p_FAST, Turbine%y_FAST, Turbine%m_FAST, &
      !            Turbine%ED, Turbine%SrvD, Turbine%AD, Turbine%IfW, &
      !            Turbine%HD, Turbine%SD, Turbine%MAP, Turbine%FEAM, Turbine%MD, &
      !            Turbine%IceF, Turbine%IceD, Turbine%MeshMapData, ErrStat, ErrMsg )


   IF (Turbine%TurbID == NumTurbines .OR. .NOT. PRESENT(Unit)) THEN
      CLOSE(unOut)
      unOut = -1
   END IF

   IF (PRESENT(Unit)) Unit = unOut

      ! A hack to pack Bladed-style DLL data
   IF (Turbine%SrvD%p%UseBladedInterface) THEN
      if (Turbine%SrvD%m%dll_data%avrSWAP( 1) > 0   ) then
            ! store value to be overwritten
         old_avrSwap1 = Turbine%SrvD%m%dll_data%avrSWAP( 1)
         FileName     = Turbine%SrvD%m%dll_data%DLL_InFile
            ! overwrite values:
         Turbine%SrvD%m%dll_data%DLL_InFile = DLLFileName
         Turbine%SrvD%m%dll_data%avrSWAP(50) = REAL( LEN_TRIM(DLLFileName) ) +1 ! No. of characters in the "INFILE"  argument (-) (we add one for the C NULL CHARACTER)
         Turbine%SrvD%m%dll_data%avrSWAP( 1) = GH_DISCON_STATUS_CHECKPOINT
         Turbine%SrvD%m%dll_data%SimStatus = Turbine%SrvD%m%dll_data%avrSWAP( 1)
         CALL CallBladedDLL(Turbine%SrvD%Input(1), Turbine%SrvD%p, Turbine%SrvD%m%dll_data, ErrStat2, ErrMsg2)
            CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

            ! put values back:
         Turbine%SrvD%m%dll_data%DLL_InFile = FileName
         Turbine%SrvD%m%dll_data%avrSWAP(50) = REAL( LEN_TRIM(FileName) ) +1 ! No. of characters in the "INFILE"  argument (-) (we add one for the C NULL CHARACTER)
         Turbine%SrvD%m%dll_data%avrSWAP( 1) = old_avrSwap1
         Turbine%SrvD%m%dll_data%SimStatus = Turbine%SrvD%m%dll_data%avrSWAP( 1)
      end if
   END IF

   call cleanup()

contains
   subroutine cleanup()
      IF ( ALLOCATED(ReKiBuf)  ) DEALLOCATE(ReKiBuf)
      IF ( ALLOCATED(DbKiBuf)  ) DEALLOCATE(DbKiBuf)
      IF ( ALLOCATED(IntKiBuf) ) DEALLOCATE(IntKiBuf)
   end subroutine cleanup
END SUBROUTINE FAST_CreateCheckpoint_T
!----------------------------------------------------------------------------------------------------------------------------------
!> Routine that calls FAST_RestoreFromCheckpoint_T for an array of Turbine data structures.
SUBROUTINE FAST_RestoreFromCheckpoint_Tary(t_initial, n_t_global, Turbine, CheckpointRoot, ErrStat, ErrMsg  )

   REAL(DbKi),               INTENT(IN   ) :: t_initial           !< initial time (for comparing with time from checkpoint file)
   INTEGER(IntKi),           INTENT(  OUT) :: n_t_global          !< loop counter
   TYPE(FAST_TurbineType),   INTENT(INOUT) :: Turbine(:)          !< all data for one instance of a turbine !intent(INOUT) instead of (IN) to attempt to avoid memory warnings in gnu compilers
   CHARACTER(*),             INTENT(IN   ) :: CheckpointRoot      !< Rootname of checkpoint file
   INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None

      ! local variables
   REAL(DbKi)                              :: t_initial_out
   INTEGER(IntKi)                          :: NumTurbines_out
   INTEGER(IntKi)                          :: NumTurbines         ! Number of turbines in this simulation
   INTEGER(IntKi)                          :: i_turb
   INTEGER                                 :: Unit
   INTEGER(IntKi)                          :: ErrStat2            ! local error status
   CHARACTER(ErrMsgLen)                    :: ErrMsg2             ! local error message
   CHARACTER(*),             PARAMETER     :: RoutineName = 'FAST_RestoreFromCheckpoint_Tary'


   NumTurbines = SIZE(Turbine)
   ErrStat = ErrID_None
   ErrMsg  = ""

      ! Init NWTC_Library, display copyright and version information:
   CALL FAST_ProgStart( FAST_Ver )

      ! Restore data from checkpoint file
   Unit = -1
   DO i_turb = 1,NumTurbines
      CALL FAST_RestoreFromCheckpoint_T(t_initial_out, n_t_global, NumTurbines_out, Turbine(i_turb), CheckpointRoot, ErrStat2, ErrMsg2, Unit )
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

         IF (t_initial_out /= t_initial) CALL SetErrStat(ErrID_Fatal, "invalid value of t_initial.", ErrStat, ErrMsg, RoutineName )
         IF (NumTurbines_out /= NumTurbines) CALL SetErrStat(ErrID_Fatal, "invalid value of NumTurbines.", ErrStat, ErrMsg, RoutineName )
         IF (ErrStat >= AbortErrLev) RETURN
   END DO

   CALL WrScr( ' Restarting simulation at '//TRIM(Num2LStr(n_t_global*Turbine(1)%p_FAST%DT))//' seconds.' )


END SUBROUTINE FAST_RestoreFromCheckpoint_Tary
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine is the inverse of FAST_CreateCheckpoint_T. It reads data from a checkpoint file and populates data structures for
!! the turbine instance.
SUBROUTINE FAST_RestoreFromCheckpoint_T(t_initial, n_t_global, NumTurbines, Turbine, CheckpointRoot, ErrStat, ErrMsg, Unit )
   USE BladedInterface, ONLY: CallBladedDLL  ! Hack for Bladed-style DLL
   USE BladedInterface, ONLY: GH_DISCON_STATUS_RESTARTING

   REAL(DbKi),               INTENT(INOUT) :: t_initial           !< initial time
   INTEGER(IntKi),           INTENT(INOUT) :: n_t_global          !< loop counter
   INTEGER(IntKi),           INTENT(INOUT) :: NumTurbines         !< Number of turbines in this simulation
   TYPE(FAST_TurbineType),   INTENT(INOUT) :: Turbine             !< all data for one instance of a turbine (bjj: note that is intent INOUT instead of OUT only because of a gfortran compiler memory issue)
   CHARACTER(*),             INTENT(IN   ) :: CheckpointRoot      !< Rootname of checkpoint file
   INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None
   INTEGER(IntKi), OPTIONAL, INTENT(INOUT) :: Unit                !< unit number for output file

      ! local variables:
   REAL(ReKi),               ALLOCATABLE   :: ReKiBuf(:)
   REAL(DbKi),               ALLOCATABLE   :: DbKiBuf(:)
   INTEGER(IntKi),           ALLOCATABLE   :: IntKiBuf(:)

   INTEGER(B4Ki)                           :: ArraySizes(3)

   INTEGER(IntKi)                          :: unIn                ! unit number for input file
   INTEGER(IntKi)                          :: old_avrSwap1        ! previous value of avrSwap(1) !hack for Bladed DLL checkpoint/restore
   INTEGER(IntKi)                          :: ErrStat2            ! local error status
   CHARACTER(ErrMsgLen)                    :: ErrMsg2             ! local error message
   CHARACTER(*),             PARAMETER     :: RoutineName = 'FAST_RestoreFromCheckpoint_T'

   CHARACTER(1024)                         :: FileName            ! Name of the (input) checkpoint file
   CHARACTER(1024)                         :: DLLFileName         ! Name of the (input) checkpoint file


   ErrStat=ErrID_None
   ErrMsg=""

   FileName    = TRIM(CheckpointRoot)//'.chkp'
   DLLFileName = TRIM(CheckpointRoot)//'.dll.chkp'
   ! FileName = TRIM(CheckpointRoot)//'.cp'
   unIn=-1
   IF (PRESENT(Unit)) unIn = Unit

   IF ( unIn < 0 ) THEN

      CALL GetNewUnit( unIn, ErrStat2, ErrMsg2 )

      CALL OpenBInpFile ( unIn, FileName, ErrStat2, ErrMsg2)
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         IF (ErrStat >= AbortErrLev ) RETURN

         ! checkpoint file header:
      READ (unIn, IOSTAT=ErrStat2)   ArraySizes     ! let's make sure we've got the correct number of bytes for reals, doubles, and integers on restart.

      IF ( ArraySizes(1) /= ReKi  ) CALL SetErrStat(ErrID_Fatal,"ReKi on restart is different than when checkpoint file was created.",ErrStat,ErrMsg,RoutineName)
      IF ( ArraySizes(2) /= DbKi  ) CALL SetErrStat(ErrID_Fatal,"DbKi on restart is different than when checkpoint file was created.",ErrStat,ErrMsg,RoutineName)
      IF ( ArraySizes(3) /= IntKi ) CALL SetErrStat(ErrID_Fatal,"IntKi on restart is different than when checkpoint file was created.",ErrStat,ErrMsg,RoutineName)
      IF (ErrStat >= AbortErrLev) THEN
         CLOSE(unIn)
         unIn = -1
         IF (PRESENT(Unit)) Unit = unIn
         RETURN
      END IF

      READ (unIn, IOSTAT=ErrStat2)   AbortErrLev
      READ (unIn, IOSTAT=ErrStat2)   NumTurbines                      ! Number of turbines
      READ (unIn, IOSTAT=ErrStat2)   t_initial                        ! initial time
      READ (unIn, IOSTAT=ErrStat2)   n_t_global                       ! current time step

   END IF

      ! in case the Turbine data structure isn't empty on entry of this routine:
   call FAST_DestroyTurbineType( Turbine, ErrStat2, ErrMsg2 )

      ! data from current time step:
   READ (unIn, IOSTAT=ErrStat2)   ArraySizes                       ! Number of reals, doubles, and integers written to file

   ALLOCATE(ReKiBuf( ArraySizes(1)), STAT=ErrStat2)
      IF (ErrStat2 /=0) CALL SetErrStat(ErrID_Fatal, "Could not allocate ReKiBuf", ErrStat, ErrMsg, RoutineName )
   ALLOCATE(DbKiBuf( ArraySizes(2)), STAT=ErrStat2)
      IF (ErrStat2 /=0) CALL SetErrStat(ErrID_Fatal, "Could not allocate DbKiBuf", ErrStat, ErrMsg, RoutineName )
   ALLOCATE(IntKiBuf(ArraySizes(3)), STAT=ErrStat2)
      IF (ErrStat2 /=0) CALL SetErrStat(ErrID_Fatal, "Could not allocate IntKiBuf", ErrStat, ErrMsg, RoutineName )

      ! Read the packed arrays
   IF (ErrStat < AbortErrLev) THEN

      READ (unIn, IOSTAT=ErrStat2)   ReKiBuf    ! Packed reals
         IF (ErrStat2 /=0) CALL SetErrStat(ErrID_Fatal, "Could not read ReKiBuf", ErrStat, ErrMsg, RoutineName )
      READ (unIn, IOSTAT=ErrStat2)   DbKiBuf    ! Packed doubles
         IF (ErrStat2 /=0) CALL SetErrStat(ErrID_Fatal, "Could not read DbKiBuf", ErrStat, ErrMsg, RoutineName )
      READ (unIn, IOSTAT=ErrStat2)   IntKiBuf   ! Packed integers
         IF (ErrStat2 /=0) CALL SetErrStat(ErrID_Fatal, "Could not read IntKiBuf", ErrStat, ErrMsg, RoutineName )

   END IF

      ! Put the arrays back in the data types
   IF (ErrStat < AbortErrLev) THEN
      CALL FAST_UnpackTurbineType( ReKiBuf, DbKiBuf, IntKiBuf, Turbine, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   END IF


      ! close file if necessary (do this after unpacking turbine data, so that TurbID is set)
   IF (Turbine%TurbID == NumTurbines .OR. .NOT. PRESENT(Unit)) THEN
      CLOSE(unIn)
      unIn = -1
   END IF

   IF (PRESENT(Unit)) Unit = unIn


   IF ( ALLOCATED(ReKiBuf)  ) DEALLOCATE(ReKiBuf)
   IF ( ALLOCATED(DbKiBuf)  ) DEALLOCATE(DbKiBuf)
   IF ( ALLOCATED(IntKiBuf) ) DEALLOCATE(IntKiBuf)


      ! A sort-of hack to restore MAP DLL data (in particular Turbine%MAP%OtherSt%C_Obj%object)
    ! these must be the same variables that are used in MAP_Init because they get allocated in the DLL and
    ! destroyed in MAP_End (also, inside the DLL)
   IF (Turbine%p_FAST%CompMooring == Module_MAP) THEN
      CALL MAP_Restart( Turbine%MAP%Input(1), Turbine%MAP%p, Turbine%MAP%x(STATE_CURR), Turbine%MAP%xd(STATE_CURR), &
                        Turbine%MAP%z(STATE_CURR), Turbine%MAP%OtherSt, Turbine%MAP%y, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   END IF


      ! A hack to restore Bladed-style DLL data
   if (Turbine%SrvD%p%UseBladedInterface) then
      if (Turbine%SrvD%m%dll_data%avrSWAP( 1) > 0   ) then ! this isn't allocated if UseBladedInterface is FALSE
            ! store value to be overwritten
         old_avrSwap1 = Turbine%SrvD%m%dll_data%avrSWAP( 1)
         FileName     = Turbine%SrvD%m%dll_data%DLL_InFile
            ! overwrite values before calling DLL:
         Turbine%SrvD%m%dll_data%DLL_InFile = DLLFileName
         Turbine%SrvD%m%dll_data%avrSWAP(50) = REAL( LEN_TRIM(DLLFileName) ) +1 ! No. of characters in the "INFILE"  argument (-) (we add one for the C NULL CHARACTER)
         Turbine%SrvD%m%dll_data%avrSWAP( 1) = GH_DISCON_STATUS_RESTARTING
         Turbine%SrvD%m%dll_data%SimStatus = Turbine%SrvD%m%dll_data%avrSWAP( 1)
         CALL CallBladedDLL(Turbine%SrvD%Input(1), Turbine%SrvD%p,  Turbine%SrvD%m%dll_data, ErrStat2, ErrMsg2)
            CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
            ! put values back:
         Turbine%SrvD%m%dll_data%DLL_InFile = FileName
         Turbine%SrvD%m%dll_data%avrSWAP(50) = REAL( LEN_TRIM(FileName) ) +1 ! No. of characters in the "INFILE"  argument (-) (we add one for the C NULL CHARACTER)
         Turbine%SrvD%m%dll_data%avrSWAP( 1) = old_avrSwap1
         Turbine%SrvD%m%dll_data%SimStatus = Turbine%SrvD%m%dll_data%avrSWAP( 1)
      end if
   end if

      ! deal with sibling meshes here:
   ! (ignoring for now; they are not going to be siblings on restart)

   ! deal with files that were open:
   IF (Turbine%p_FAST%WrTxtOutFile) THEN
      CALL OpenFunkFileAppend ( Turbine%y_FAST%UnOu, TRIM(Turbine%p_FAST%OutFileRoot)//'.out', ErrStat2, ErrMsg2)
      IF ( ErrStat2 >= AbortErrLev ) RETURN
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      CALL WrFileNR ( Turbine%y_FAST%UnOu, '#Restarting here')
      WRITE(Turbine%y_FAST%UnOu, '()')
   END IF
   ! (ignoring for now; will have fort.x files if any were open [though I printed a warning about not outputting binary files earlier])


END SUBROUTINE FAST_RestoreFromCheckpoint_T
!----------------------------------------------------------------------------------------------------------------------------------
!> Routine that calls FAST_RestoreForVTKModeShape_T for an array of Turbine data structures.
SUBROUTINE FAST_RestoreForVTKModeShape_Tary(t_initial, Turbine, InputFileName, ErrStat, ErrMsg  )

   REAL(DbKi),               INTENT(IN   ) :: t_initial           !< initial time (for comparing with time from checkpoint file)
   TYPE(FAST_TurbineType),   INTENT(INOUT) :: Turbine(:)          !< all data for one instance of a turbine !intent(INOUT) instead of (IN) to attempt to avoid memory warnings in gnu compilers
   CHARACTER(*),             INTENT(IN   ) :: InputFileName       !< Name of the input file
   INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None

      ! local variables
   INTEGER(IntKi)                          :: i_turb
   INTEGER(IntKi)                          :: n_t_global          !< loop counter
   INTEGER(IntKi)                          :: NumTurbines         ! Number of turbines in this simulation
   INTEGER(IntKi)                          :: ErrStat2            ! local error status
   CHARACTER(ErrMsgLen)                    :: ErrMsg2             ! local error message
   CHARACTER(*),             PARAMETER     :: RoutineName = 'FAST_RestoreForVTKModeShape_Tary'


   ErrStat = ErrID_None
   ErrMsg  = ""

   NumTurbines = SIZE(Turbine)
   if (NumTurbines /=1) then
      call SetErrStat(ErrID_Fatal, "Mode-shape visualization is not available for multiple turbines.", ErrStat, ErrMsg, RoutineName)
      return
   end if


   CALL ReadModeShapeFile( Turbine(1)%p_FAST, trim(InputFileName), ErrStat2, ErrMsg2, checkpointOnly=.true. )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      if (ErrStat >= AbortErrLev) return

   CALL FAST_RestoreFromCheckpoint_Tary( t_initial, n_t_global, Turbine, trim(Turbine(1)%p_FAST%VTK_modes%CheckpointRoot), ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


   DO i_turb = 1,NumTurbines
      if (.not. allocated(Turbine(i_turb)%m_FAST%Lin%LinTimes)) then
         call SetErrStat(ErrID_Fatal, "Mode-shape visualization requires a checkpoint file from a simulation with linearization analysis, but NLinTimes is 0.", ErrStat, ErrMsg, RoutineName)
         return
      end if

      CALL FAST_RestoreForVTKModeShape_T(t_initial, Turbine(i_turb)%p_FAST, Turbine(i_turb)%y_FAST, Turbine(i_turb)%m_FAST, &
                  Turbine(i_turb)%ED, Turbine(i_turb)%BD, Turbine(i_turb)%SrvD, Turbine(i_turb)%AD14, Turbine(i_turb)%AD, Turbine(i_turb)%IfW, Turbine(i_turb)%OpFM, &
                  Turbine(i_turb)%HD, Turbine(i_turb)%SD, Turbine(i_turb)%ExtPtfm, Turbine(i_turb)%MAP, Turbine(i_turb)%FEAM, Turbine(i_turb)%MD, Turbine(i_turb)%Orca, &
                  Turbine(i_turb)%IceF, Turbine(i_turb)%IceD, Turbine(i_turb)%MeshMapData, trim(InputFileName), ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   END DO


END SUBROUTINE FAST_RestoreForVTKModeShape_Tary
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine calculates the motions generated by mode shapes and outputs VTK data for it
SUBROUTINE FAST_RestoreForVTKModeShape_T(t_initial, p_FAST, y_FAST, m_FAST, ED, BD, SrvD, AD14, AD, IfW, OpFM, HD, SD, ExtPtfm, &
                         MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, InputFileName, ErrStat, ErrMsg )

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
   CHARACTER(*),             INTENT(IN   ) :: InputFileName       !< Name of the input file

   INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None

   ! local variables
   REAL(DbKi)                              :: dt                  ! time
   REAL(DbKi)                              :: tprime              ! time
   INTEGER(IntKi)                          :: nt

   INTEGER(IntKi)                          :: iLinTime            ! generic loop counters
   INTEGER(IntKi)                          :: it                  ! generic loop counters
   INTEGER(IntKi)                          :: iMode               ! generic loop counters
   INTEGER(IntKi)                          :: ModeNo              ! mode number
   INTEGER(IntKi)                          :: NLinTimes

   INTEGER(IntKi)                          :: ErrStat2
   CHARACTER(ErrMsgLen)                    :: ErrMsg2
   CHARACTER(*), PARAMETER                 :: RoutineName = 'FAST_RestoreForVTKModeShape_T'
   CHARACTER(1024)                         :: VTK_RootName


   ErrStat = ErrID_None
   ErrMsg  = ""

   CALL ReadModeShapeFile( p_FAST, trim(InputFileName), ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      if (ErrStat >= AbortErrLev) return

   call ReadModeShapeMatlabFile( p_FAST, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      if (ErrStat >= AbortErrLev ) return

   y_FAST%WriteThisStep = .true.
   y_FAST%UnSum = -1

   NLinTimes = min( p_FAST%VTK_modes%VTKNLinTimes, size(p_FAST%VTK_modes%x_eig_magnitude,2), p_FAST%NLinTimes )

   VTK_RootName = p_FAST%VTK_OutFileRoot

   select case (p_FAST%VTK_modes%VTKLinTim)
   case (1)

      do iMode = 1,p_FAST%VTK_modes%VTKLinModes
         ModeNo = p_FAST%VTK_modes%VTKModes(iMode)

         call  GetTimeConstants(p_FAST%VTK_modes%DampedFreq_Hz(ModeNo), p_FAST%VTK_fps, nt, dt, p_FAST%VTK_tWidth )
         if (nt > 500) cycle

         p_FAST%VTK_OutFileRoot = trim(VTK_RootName)//'.Mode'//trim(num2lstr(ModeNo))
         y_FAST%VTK_count = 1  ! we are skipping the reference meshes by starting at 1
         do iLinTime = 1,NLinTimes
            tprime = m_FAST%Lin%LinTimes(iLinTime) - m_FAST%Lin%LinTimes(1)

            if (p_FAST%DT_UJac < p_FAST%TMax) then
               m_FAST%calcJacobian = .true.
               m_FAST%NextJacCalcTime = m_FAST%Lin%LinTimes(iLinTime)
            end if

            call SetOperatingPoint(iLinTime, p_FAST, y_FAST, m_FAST, ED, BD, SrvD, AD, IfW, OpFM, HD, SD, ExtPtfm, &
                                    MAPp, FEAM, MD, Orca, IceF, IceD, ErrStat2, ErrMsg2 )
               CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

               ! set perturbation of states based on x_eig magnitude and phase
            call PerturbOP(tprime, iLinTime, ModeNo, p_FAST, y_FAST, ED, BD, SrvD, AD, IfW, OpFM, HD, SD, ExtPtfm, MAPp, FEAM, MD, Orca, &
                        IceF, IceD, ErrStat2, ErrMsg2 )
               CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
               IF (ErrStat >= AbortErrLev) RETURN

            CALL CalcOutputs_And_SolveForInputs( -1,  m_FAST%Lin%LinTimes(iLinTime),  STATE_CURR, m_FAST%calcJacobian, m_FAST%NextJacCalcTime, &
               p_FAST, m_FAST, .true., ED, BD, SrvD, AD14, AD, IfW, OpFM, HD, SD, ExtPtfm, MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, ErrStat2, ErrMsg2 )
               CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
               IF (ErrStat >= AbortErrLev) RETURN

            call WriteVTK(m_FAST%Lin%LinTimes(iLinTime), p_FAST, y_FAST, MeshMapData, ED, BD, AD, IfW, OpFM, HD, SD, ExtPtfm, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD)

         end do ! iLinTime
      end do ! iMode

   case (2)

      do iMode = 1,p_FAST%VTK_modes%VTKLinModes
         ModeNo = p_FAST%VTK_modes%VTKModes(iMode)

         call  GetTimeConstants(p_FAST%VTK_modes%DampedFreq_Hz(ModeNo), p_FAST%VTK_fps, nt, dt, p_FAST%VTK_tWidth )
         if (nt > 500) cycle

         do iLinTime = 1,NLinTimes
            p_FAST%VTK_OutFileRoot = trim(VTK_RootName)//'.Mode'//trim(num2lstr(ModeNo))//'.LinTime'//trim(num2lstr(iLinTime))
            y_FAST%VTK_count = 1  ! we are skipping the reference meshes by starting at 1

            if (p_FAST%DT_UJac < p_FAST%TMax) then
               m_FAST%calcJacobian = .true.
               m_FAST%NextJacCalcTime = m_FAST%Lin%LinTimes(iLinTime)
            end if

            do it = 1,nt
               tprime = (it-1)*dt

               call SetOperatingPoint(iLinTime, p_FAST, y_FAST, m_FAST, ED, BD, SrvD, AD, IfW, OpFM, HD, SD, ExtPtfm, &
                                     MAPp, FEAM, MD, Orca, IceF, IceD, ErrStat2, ErrMsg2 )
                  CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

                  ! set perturbation of states based on x_eig magnitude and phase
               call PerturbOP(tprime, iLinTime, ModeNo, p_FAST, y_FAST, ED, BD, SrvD, AD, IfW, OpFM, HD, SD, ExtPtfm, MAPp, FEAM, MD, Orca, &
                         IceF, IceD, ErrStat2, ErrMsg2 )
                  CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
                  IF (ErrStat >= AbortErrLev) RETURN

               CALL CalcOutputs_And_SolveForInputs( -1, m_FAST%Lin%LinTimes(iLinTime),  STATE_CURR, m_FAST%calcJacobian, m_FAST%NextJacCalcTime, &
                  p_FAST, m_FAST, .true., ED, BD, SrvD, AD14, AD, IfW, OpFM, HD, SD, ExtPtfm, MAPp, FEAM, MD, Orca, IceF, IceD, MeshMapData, ErrStat2, ErrMsg2 )
                  CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
                  IF (ErrStat >= AbortErrLev) RETURN

               call WriteVTK(m_FAST%Lin%LinTimes(iLinTime)+tprime, p_FAST, y_FAST, MeshMapData, ED, BD, AD, IfW, OpFM, HD, SD, ExtPtfm, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD)

            end do


         end do ! iLinTime
      end do   ! iMode

   end select

END SUBROUTINE FAST_RestoreForVTKModeShape_T

END MODULE FAST_Subs
!----------------------------------------------------------------------------------------------------------------------------------
