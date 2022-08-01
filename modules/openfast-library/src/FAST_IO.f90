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