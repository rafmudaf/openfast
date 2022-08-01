MODULE FAST_OpenFOAM

    USE, INTRINSIC :: ISO_C_Binding
    USE FAST_Data

    IMPLICIT  NONE

CONTAINS

subroutine FAST_OpFM_Init(iTurb, TMax, InputFileName_c, TurbID, NumSC2CtrlGlob, NumSC2Ctrl, NumCtrl2SC, InitSCOutputsGlob, InitSCOutputsTurbine, NumActForcePtsBlade, NumActForcePtsTower, TurbPosn, AbortErrLev_c, dt_c, NumBl_c, NumBlElem_c, &
                          OpFM_Input_from_FAST, OpFM_Output_to_FAST, SC_DX_Input_from_FAST, SC_DX_Output_to_FAST, ErrStat_c, ErrMsg_c) BIND (C, NAME='FAST_OpFM_Init')
   IMPLICIT NONE
#ifndef IMPLICIT_DLLEXPORT
!DEC$ ATTRIBUTES DLLEXPORT :: FAST_OpFM_Init
!GCC$ ATTRIBUTES DLLEXPORT :: FAST_OpFM_Init
#endif
   INTEGER(C_INT),         INTENT(IN   ) :: iTurb            ! Turbine number 
   REAL(C_DOUBLE),         INTENT(IN   ) :: TMax      
   CHARACTER(KIND=C_CHAR), INTENT(IN   ) :: InputFileName_c(IntfStrLen)      
   INTEGER(C_INT),         INTENT(IN   ) :: TurbID           ! Need not be same as iTurb
   INTEGER(C_INT),         INTENT(IN   ) :: NumSC2CtrlGlob   ! Supercontroller global outputs = controller global inputs   
   INTEGER(C_INT),         INTENT(IN   ) :: NumSC2Ctrl       ! Supercontroller outputs = controller inputs
   INTEGER(C_INT),         INTENT(IN   ) :: NumCtrl2SC       ! controller outputs = Supercontroller inputs
   REAL(C_FLOAT),          INTENT(IN   ) :: InitScOutputsGlob (*) ! Initial Supercontroller global outputs = controller inputs
   REAL(C_FLOAT),          INTENT(IN   ) :: InitScOutputsTurbine (*) ! Initial Supercontroller turbine specific outputs = controller inputs
   INTEGER(C_INT),         INTENT(IN   ) :: NumActForcePtsBlade ! number of actuator line force points in blade
   INTEGER(C_INT),         INTENT(IN   ) :: NumActForcePtsTower ! number of actuator line force points in tower
   REAL(C_FLOAT),          INTENT(IN   ) :: TurbPosn(3)      
   INTEGER(C_INT),         INTENT(  OUT) :: AbortErrLev_c      
   REAL(C_DOUBLE),         INTENT(  OUT) :: dt_c      
   INTEGER(C_INT),         INTENT(  OUT) :: NumBl_c      
   INTEGER(C_INT),         INTENT(  OUT) :: NumBlElem_c      
   TYPE(OpFM_InputType_C), INTENT(INOUT) :: OpFM_Input_from_FAST  !INTENT(INOUT) instead of INTENT(OUT) to avoid gcc compiler warnings about variable tracking sizes
   TYPE(OpFM_OutputType_C),INTENT(INOUT) :: OpFM_Output_to_FAST   !INTENT(INOUT) instead of INTENT(OUT) to avoid gcc compiler warnings about variable tracking sizes
   TYPE(SC_DX_InputType_C),   INTENT(INOUT) :: SC_DX_Input_from_FAST
   TYPE(SC_DX_OutputType_C),  INTENT(INOUT) :: SC_DX_Output_to_FAST
   INTEGER(C_INT),         INTENT(  OUT) :: ErrStat_c      
   CHARACTER(KIND=C_CHAR), INTENT(  OUT) :: ErrMsg_c(IntfStrLen) 
      
   ! local
   CHARACTER(IntfStrLen)                 :: InputFileName   
   INTEGER(C_INT)                        :: i    
   TYPE(FAST_ExternInitType)             :: ExternInitData
   
      ! transfer the character array from C to a Fortran string:   
   InputFileName = TRANSFER( InputFileName_c, InputFileName )
   I = INDEX(InputFileName,C_NULL_CHAR) - 1            ! if this has a c null character at the end...
   IF ( I > 0 ) InputFileName = InputFileName(1:I)     ! remove it
   
      ! initialize variables:   
   n_t_global = 0   
   ErrStat = ErrID_None
   ErrMsg = ""
   
   NumBl_c       = 0    ! initialize here in case of error
   NumBlElem_c   = 0    ! initialize here in case of error
   
   ExternInitData%TMax = TMax
   ExternInitData%TurbineID = TurbID
   ExternInitData%TurbinePos = TurbPosn
   ExternInitData%SensorType = SensorType_None
   ExternInitData%NumCtrl2SC = NumCtrl2SC
   ExternInitData%NumSC2CtrlGlob = NumSC2CtrlGlob
   
   if ( NumSC2CtrlGlob > 0 ) then
      CALL AllocAry( ExternInitData%fromSCGlob, NumSC2CtrlGlob, 'ExternInitData%fromSCGlob', ErrStat, ErrMsg)
         IF (FAILED()) RETURN

      do i=1,NumSC2CtrlGlob
         ExternInitData%fromSCGlob(i) = InitScOutputsGlob(i)
      end do
   end if
   
   ExternInitData%NumSC2Ctrl = NumSC2Ctrl
   if ( NumSC2Ctrl > 0 ) then
      CALL AllocAry( ExternInitData%fromSC, NumSC2Ctrl, 'ExternInitData%fromSC', ErrStat, ErrMsg)
         IF (FAILED()) RETURN

      do i=1,NumSC2Ctrl
         ExternInitData%fromSC(i) = InitScOutputsTurbine(i)
      end do
   end if
   
   ExternInitData%NumActForcePtsBlade = NumActForcePtsBlade
   ExternInitData%NumActForcePtsTower = NumActForcePtsTower

   CALL FAST_InitializeAll_T( t_initial, iTurb, Turbine(iTurb), ErrStat, ErrMsg, InputFileName, ExternInitData )

      ! set values for return to OpenFOAM
   AbortErrLev_c = AbortErrLev   
   dt_c          = Turbine(iTurb)%p_FAST%dt
   ErrStat_c     = ErrStat
   ErrMsg        = TRIM(ErrMsg)//C_NULL_CHAR
   ErrMsg_c      = TRANSFER( ErrMsg//C_NULL_CHAR, ErrMsg_c )
      
   IF ( ErrStat >= AbortErrLev ) THEN
      CALL WrScr( "Error in FAST_OpFM_Init:FAST_InitializeAll_T" // TRIM(ErrMsg) )
      RETURN
   END IF
   
   call SetOpenFOAM_pointers(iTurb, OpFM_Input_from_FAST, OpFM_Output_to_FAST, SC_DX_Input_from_FAST, SC_DX_Output_to_FAST)
                        
   ! 7-Sep-2015: Sang wants these integers for the OpenFOAM mapping, which is tied to the AeroDyn nodes. FAST doesn't restrict the number of nodes on each 
   ! blade mesh to be the same, so if this DOES ever change, we'll need to make OpenFOAM less tied to the AeroDyn mapping.
   IF (Turbine(iTurb)%p_FAST%CompAero == MODULE_AD14) THEN   
      NumBl_c     = SIZE(Turbine(iTurb)%AD14%Input(1)%InputMarkers)
      NumBlElem_c = Turbine(iTurb)%AD14%Input(1)%InputMarkers(1)%Nnodes
   ELSEIF (Turbine(iTurb)%p_FAST%CompAero == MODULE_AD) THEN
      IF (ALLOCATED(Turbine(iTurb)%AD%Input(1)%rotors)) THEN
         IF (ALLOCATED(Turbine(iTurb)%AD%Input(1)%rotors(1)%BladeMotion)) THEN
            NumBl_c     = SIZE(Turbine(iTurb)%AD%Input(1)%rotors(1)%BladeMotion)
         END IF
      END IF
      IF (NumBl_c > 0) THEN
         NumBlElem_c = Turbine(iTurb)%AD%Input(1)%rotors(1)%BladeMotion(1)%Nnodes
      END IF
   END IF
   
contains
   LOGICAL FUNCTION FAILED()
   
      FAILED = ErrStat >= AbortErrLev
      
      IF (ErrStat > 0) THEN
         CALL WrScr( "Error in FAST_OpFM_Init:FAST_InitializeAll_T" // TRIM(ErrMsg) )
         
         IF ( FAILED ) THEN
               
            AbortErrLev_c = AbortErrLev
            ErrStat_c     = ErrStat
            ErrMsg        = TRIM(ErrMsg)//C_NULL_CHAR
            ErrMsg_c      = TRANSFER( ErrMsg//C_NULL_CHAR, ErrMsg_c )
   
            !IF (ALLOCATED(Turbine)) DEALLOCATE(Turbine)
            ! bjj: if there is an error, the driver should call FAST_DeallocateTurbines() instead of putting this deallocate statement here
         END IF
      END IF
      
      
   END FUNCTION FAILED
end subroutine
! ==================================================================================================================================
subroutine FAST_OpFM_Solution0(iTurb, ErrStat_c, ErrMsg_c) BIND (C, NAME='FAST_OpFM_Solution0')
   IMPLICIT NONE
#ifndef IMPLICIT_DLLEXPORT
!DEC$ ATTRIBUTES DLLEXPORT :: FAST_OpFM_Solution0
!GCC$ ATTRIBUTES DLLEXPORT :: FAST_OpFM_Solution0
#endif
   INTEGER(C_INT),         INTENT(IN   ) :: iTurb            ! Turbine number 
   INTEGER(C_INT),         INTENT(  OUT) :: ErrStat_c      
   CHARACTER(KIND=C_CHAR), INTENT(  OUT) :: ErrMsg_c(IntfStrLen) 

   call FAST_Solution0_T(Turbine(iTurb), ErrStat, ErrMsg ) 

!   if(Turbine(iTurb)%SC_DX%p%useSC) then
!      CALL SC_SetInputs(Turbine(iTurb)%p_FAST, Turbine(iTurb)%SrvD%y, Turbine(iTurb)%SC_DX, ErrStat, ErrMsg)
!   end if
   
      ! set values for return to OpenFOAM
   ErrStat_c     = ErrStat
   ErrMsg        = TRIM(ErrMsg)//C_NULL_CHAR
   ErrMsg_c      = TRANSFER( ErrMsg//C_NULL_CHAR, ErrMsg_c )
   
                        
end subroutine FAST_OpFM_Solution0
! ==================================================================================================================================
subroutine FAST_OpFM_Restart(iTurb, CheckpointRootName_c, AbortErrLev_c, dt_c, numblades_c, numElementsPerBlade_c, n_t_global_c, &
                      OpFM_Input_from_FAST, OpFM_Output_to_FAST, SC_DX_Input_from_FAST, SC_DX_Output_to_FAST, ErrStat_c, ErrMsg_c) BIND (C, NAME='FAST_OpFM_Restart')
   IMPLICIT NONE
#ifndef IMPLICIT_DLLEXPORT
!DEC$ ATTRIBUTES DLLEXPORT :: FAST_OpFM_Restart
!GCC$ ATTRIBUTES DLLEXPORT :: FAST_OpFM_Restart
#endif
   INTEGER(C_INT),         INTENT(IN   ) :: iTurb            ! Turbine number 
   CHARACTER(KIND=C_CHAR), INTENT(IN   ) :: CheckpointRootName_c(IntfStrLen)      
   INTEGER(C_INT),         INTENT(  OUT) :: AbortErrLev_c      
   INTEGER(C_INT),         INTENT(  OUT) :: numblades_c
   INTEGER(C_INT),         INTENT(  OUT) :: numElementsPerBlade_c
   REAL(C_DOUBLE),         INTENT(  OUT) :: dt_c      
   INTEGER(C_INT),         INTENT(  OUT) :: n_t_global_c      
   TYPE(OpFM_InputType_C), INTENT(INOUT) :: OpFM_Input_from_FAST  !INTENT(INOUT) instead of INTENT(OUT) to avoid gcc compiler warnings about variable tracking sizes
   TYPE(OpFM_OutputType_C),INTENT(INOUT) :: OpFM_Output_to_FAST   !INTENT(INOUT) instead of INTENT(OUT) to avoid gcc compiler warnings about variable tracking sizes
   TYPE(SC_DX_InputType_C),   INTENT(INOUT) :: SC_DX_Input_from_FAST
   TYPE(SC_DX_OutputType_C),  INTENT(INOUT) :: SC_DX_Output_to_FAST
   INTEGER(C_INT),         INTENT(  OUT) :: ErrStat_c      
   CHARACTER(KIND=C_CHAR), INTENT(  OUT) :: ErrMsg_c(IntfStrLen) 
   
   ! local variables
   INTEGER(C_INT)                        :: NumOuts_c      
   CHARACTER(IntfStrLen)                 :: CheckpointRootName   
   INTEGER(IntKi)                        :: I
   INTEGER(IntKi)                        :: Unit
   REAL(DbKi)                            :: t_initial_out
   INTEGER(IntKi)                        :: NumTurbines_out
   CHARACTER(*),           PARAMETER     :: RoutineName = 'FAST_Restart' 
             
   CALL NWTC_Init()
      ! transfer the character array from C to a Fortran string:   
   CheckpointRootName = TRANSFER( CheckpointRootName_c, CheckpointRootName )
   I = INDEX(CheckpointRootName,C_NULL_CHAR) - 1                 ! if this has a c null character at the end...
   IF ( I > 0 ) CheckpointRootName = CheckpointRootName(1:I)     ! remove it
   
   Unit = -1
   CALL FAST_RestoreFromCheckpoint_T(t_initial_out, n_t_global, NumTurbines_out, Turbine(iTurb), CheckpointRootName, ErrStat, ErrMsg, Unit )
   
      ! check that these are valid:
      IF (t_initial_out /= t_initial) CALL SetErrStat(ErrID_Fatal, "invalid value of t_initial.", ErrStat, ErrMsg, RoutineName )
      IF (NumTurbines_out /= 1) CALL SetErrStat(ErrID_Fatal, "invalid value of NumTurbines.", ErrStat, ErrMsg, RoutineName )
   
       ! transfer Fortran variables to C: 
   n_t_global_c  = n_t_global
   AbortErrLev_c = AbortErrLev   
   NumOuts_c     = min(MAXOUTPUTS, SUM( Turbine(iTurb)%y_FAST%numOuts )) ! includes time
   if (allocated(Turbine(iTurb)%ad%p%rotors)) then ! this might not be allocated if we had an error earlier
      numBlades_c   = Turbine(iTurb)%ad%p%rotors(1)%numblades
      numElementsPerBlade_c = Turbine(iTurb)%ad%p%rotors(1)%numblnds ! I'm not sure if FASTv8 can handle different number of blade nodes for each blade.
   else
      numBlades_c = 0
      numElementsPerBlade_c = 0
   end if
   
   dt_c          = Turbine(iTurb)%p_FAST%dt      
      
   ErrStat_c     = ErrStat
   ErrMsg        = TRIM(ErrMsg)//C_NULL_CHAR
   ErrMsg_c      = TRANSFER( ErrMsg//C_NULL_CHAR, ErrMsg_c )

#ifdef CONSOLE_FILE   
   if (ErrStat /= ErrID_None) call wrscr1(trim(ErrMsg))
#endif   

   if (ErrStat >= AbortErrLev) return
   
   call SetOpenFOAM_pointers(iTurb, OpFM_Input_from_FAST, OpFM_Output_to_FAST, SC_DX_Input_from_FAST, SC_DX_Output_to_FAST)

end subroutine FAST_OpFM_Restart
!==================================================================================================================================
subroutine SetOpenFOAM_pointers(iTurb, OpFM_Input_from_FAST, OpFM_Output_to_FAST, SC_DX_Input_from_FAST, SC_DX_Output_to_FAST)

   IMPLICIT NONE
   INTEGER(C_INT),            INTENT(IN   ) :: iTurb            ! Turbine number 
   TYPE(OpFM_InputType_C),    INTENT(INOUT) :: OpFM_Input_from_FAST
   TYPE(OpFM_OutputType_C),   INTENT(INOUT) :: OpFM_Output_to_FAST
   TYPE(SC_DX_InputType_C),   INTENT(INOUT) :: SC_DX_Input_from_FAST
   TYPE(SC_DX_OutputType_C),  INTENT(INOUT) :: SC_DX_Output_to_FAST

   OpFM_Input_from_FAST%pxVel_Len = Turbine(iTurb)%OpFM%u%c_obj%pxVel_Len; OpFM_Input_from_FAST%pxVel = Turbine(iTurb)%OpFM%u%c_obj%pxVel
   OpFM_Input_from_FAST%pyVel_Len = Turbine(iTurb)%OpFM%u%c_obj%pyVel_Len; OpFM_Input_from_FAST%pyVel = Turbine(iTurb)%OpFM%u%c_obj%pyVel
   OpFM_Input_from_FAST%pzVel_Len = Turbine(iTurb)%OpFM%u%c_obj%pzVel_Len; OpFM_Input_from_FAST%pzVel = Turbine(iTurb)%OpFM%u%c_obj%pzVel
   OpFM_Input_from_FAST%pxForce_Len = Turbine(iTurb)%OpFM%u%c_obj%pxForce_Len; OpFM_Input_from_FAST%pxForce = Turbine(iTurb)%OpFM%u%c_obj%pxForce
   OpFM_Input_from_FAST%pyForce_Len = Turbine(iTurb)%OpFM%u%c_obj%pyForce_Len; OpFM_Input_from_FAST%pyForce = Turbine(iTurb)%OpFM%u%c_obj%pyForce
   OpFM_Input_from_FAST%pzForce_Len = Turbine(iTurb)%OpFM%u%c_obj%pzForce_Len; OpFM_Input_from_FAST%pzForce = Turbine(iTurb)%OpFM%u%c_obj%pzForce
   OpFM_Input_from_FAST%xdotForce_Len = Turbine(iTurb)%OpFM%u%c_obj%xdotForce_Len; OpFM_Input_from_FAST%xdotForce = Turbine(iTurb)%OpFM%u%c_obj%xdotForce
   OpFM_Input_from_FAST%ydotForce_Len = Turbine(iTurb)%OpFM%u%c_obj%ydotForce_Len; OpFM_Input_from_FAST%ydotForce = Turbine(iTurb)%OpFM%u%c_obj%ydotForce
   OpFM_Input_from_FAST%zdotForce_Len = Turbine(iTurb)%OpFM%u%c_obj%zdotForce_Len; OpFM_Input_from_FAST%zdotForce = Turbine(iTurb)%OpFM%u%c_obj%zdotForce
   OpFM_Input_from_FAST%pOrientation_Len = Turbine(iTurb)%OpFM%u%c_obj%pOrientation_Len; OpFM_Input_from_FAST%pOrientation = Turbine(iTurb)%OpFM%u%c_obj%pOrientation
   OpFM_Input_from_FAST%fx_Len = Turbine(iTurb)%OpFM%u%c_obj%fx_Len; OpFM_Input_from_FAST%fx = Turbine(iTurb)%OpFM%u%c_obj%fx
   OpFM_Input_from_FAST%fy_Len = Turbine(iTurb)%OpFM%u%c_obj%fy_Len; OpFM_Input_from_FAST%fy = Turbine(iTurb)%OpFM%u%c_obj%fy
   OpFM_Input_from_FAST%fz_Len = Turbine(iTurb)%OpFM%u%c_obj%fz_Len; OpFM_Input_from_FAST%fz = Turbine(iTurb)%OpFM%u%c_obj%fz
   OpFM_Input_from_FAST%momentx_Len = Turbine(iTurb)%OpFM%u%c_obj%momentx_Len; OpFM_Input_from_FAST%momentx = Turbine(iTurb)%OpFM%u%c_obj%momentx
   OpFM_Input_from_FAST%momenty_Len = Turbine(iTurb)%OpFM%u%c_obj%momenty_Len; OpFM_Input_from_FAST%momenty = Turbine(iTurb)%OpFM%u%c_obj%momenty
   OpFM_Input_from_FAST%momentz_Len = Turbine(iTurb)%OpFM%u%c_obj%momentz_Len; OpFM_Input_from_FAST%momentz = Turbine(iTurb)%OpFM%u%c_obj%momentz
   OpFM_Input_from_FAST%forceNodesChord_Len = Turbine(iTurb)%OpFM%u%c_obj%forceNodesChord_Len; OpFM_Input_from_FAST%forceNodesChord = Turbine(iTurb)%OpFM%u%c_obj%forceNodesChord

   if (Turbine(iTurb)%p_FAST%UseSC) then
      SC_DX_Input_from_FAST%toSC_Len = Turbine(iTurb)%SC_DX%u%c_obj%toSC_Len
      SC_DX_Input_from_FAST%toSC     = Turbine(iTurb)%SC_DX%u%c_obj%toSC
   end if

   OpFM_Output_to_FAST%u_Len   = Turbine(iTurb)%OpFM%y%c_obj%u_Len;  OpFM_Output_to_FAST%u = Turbine(iTurb)%OpFM%y%c_obj%u
   OpFM_Output_to_FAST%v_Len   = Turbine(iTurb)%OpFM%y%c_obj%v_Len;  OpFM_Output_to_FAST%v = Turbine(iTurb)%OpFM%y%c_obj%v
   OpFM_Output_to_FAST%w_Len   = Turbine(iTurb)%OpFM%y%c_obj%w_Len;  OpFM_Output_to_FAST%w = Turbine(iTurb)%OpFM%y%c_obj%w

   if (Turbine(iTurb)%p_FAST%UseSC) then
      SC_DX_Output_to_FAST%fromSC_Len = Turbine(iTurb)%SC_DX%y%c_obj%fromSC_Len
      SC_DX_Output_to_FAST%fromSC     = Turbine(iTurb)%SC_DX%y%c_obj%fromSC
   end if

end subroutine SetOpenFOAM_pointers
!==================================================================================================================================
subroutine FAST_OpFM_Step(iTurb, ErrStat_c, ErrMsg_c) BIND (C, NAME='FAST_OpFM_Step')
   IMPLICIT NONE
#ifndef IMPLICIT_DLLEXPORT
!DEC$ ATTRIBUTES DLLEXPORT :: FAST_OpFM_Step
!GCC$ ATTRIBUTES DLLEXPORT :: FAST_OpFM_Step
#endif
   INTEGER(C_INT),         INTENT(IN   ) :: iTurb            ! Turbine number 
   INTEGER(C_INT),         INTENT(  OUT) :: ErrStat_c      
   CHARACTER(KIND=C_CHAR), INTENT(  OUT) :: ErrMsg_c(IntfStrLen)      
                    
   
   IF ( n_t_global > Turbine(iTurb)%p_FAST%n_TMax_m1 ) THEN !finish 
      
      ! we can't continue because we might over-step some arrays that are allocated to the size of the simulation
      
      if (iTurb .eq. (NumTurbines-1) ) then
         IF (n_t_global == Turbine(iTurb)%p_FAST%n_TMax_m1 + 1) THEN  ! we call update an extra time in Simulink, which we can ignore until the time shift with outputs is solved
            n_t_global = n_t_global + 1
            ErrStat_c = ErrID_None
            ErrMsg = C_NULL_CHAR
            ErrMsg_c = TRANSFER( ErrMsg//C_NULL_CHAR, ErrMsg_c )
         ELSE     
            ErrStat_c = ErrID_Info
            ErrMsg = "Simulation completed."//C_NULL_CHAR
            ErrMsg_c = TRANSFER( ErrMsg//C_NULL_CHAR, ErrMsg_c )
         END IF
      end if
      
   ELSE

      CALL FAST_Solution_T( t_initial, n_t_global, Turbine(iTurb), ErrStat, ErrMsg )                  
      if (iTurb .eq. (NumTurbines-1) ) then
         n_t_global = n_t_global + 1
      end if
            
      ErrStat_c = ErrStat
      ErrMsg = TRIM(ErrMsg)//C_NULL_CHAR
      ErrMsg_c  = TRANSFER( ErrMsg//C_NULL_CHAR, ErrMsg_c )
   END IF
   
      
end subroutine FAST_OpFM_Step 

end module
