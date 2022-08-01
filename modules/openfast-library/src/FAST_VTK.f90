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
! Unless required by applicable law or agreed to in viting, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!**********************************************************************************************************************************
MODULE FAST_VTK

   USE FAST_ModTypes

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------------------------------------------------------------
!> This function builds the path for the vtk directory based on the output file root
FUNCTION get_vtkdir_path( out_file_root )
   CHARACTER(1024) :: get_vtkdir_path
   CHARACTER(*), INTENT(IN) :: out_file_root
   INTEGER(IntKi) :: last_separator_index
   
   ! get the directory of the primary input file (i.e. the case directory); Windows can have either forward or backward slashes (compare with GetPath())
   
   last_separator_index =      index(out_file_root, '/', back=.true.)
   last_separator_index = max( index(out_file_root, '\', back=.true.), last_separator_index )
   
   if (last_separator_index==0) then
      get_vtkdir_path = '.'//PathSep//'vtk'
   else
      get_vtkdir_path = trim(out_file_root(1 : last_separator_index) // 'vtk')
   end if
END FUNCTION
!----------------------------------------------------------------------------------------------------------------------------------
!> This function builds the path for the vtk root file name based on the output file root
FUNCTION get_vtkroot_path( out_file_root )
   CHARACTER(1024) :: get_vtkroot_path
   CHARACTER(*), INTENT(IN) :: out_file_root
   INTEGER(IntKi) :: last_separator_index
   INTEGER(IntKi) :: path_length

   last_separator_index =      index(out_file_root, '/', back=.true.)
   last_separator_index = max( index(out_file_root, '\', back=.true.), last_separator_index )

   get_vtkroot_path = trim( get_vtkdir_path(out_file_root) ) // PathSep &
                      // out_file_root( last_separator_index + 1 :)
END FUNCTION
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine writes Input Mesh information to a binary file (for debugging). It both opens and closes the file.
SUBROUTINE WriteInputMeshesToFile(u_ED, u_AD, u_SD, u_HD, u_MAP, u_BD, FileName, ErrStat, ErrMsg)
   TYPE(ED_InputType),        INTENT(IN)  :: u_ED           !< ElastoDyn inputs
   TYPE(AD_InputType),        INTENT(IN)  :: u_AD           !< AeroDyn inputs
   TYPE(SD_InputType),        INTENT(IN)  :: u_SD           !< SubDyn inputs
   TYPE(HydroDyn_InputType),  INTENT(IN)  :: u_HD           !< HydroDyn inputs
   TYPE(MAP_InputType),       INTENT(IN)  :: u_MAP          !< MAP inputs
   TYPE(BD_InputType),        INTENT(IN)  :: u_BD(:)        !< BeamDyn inputs
   CHARACTER(*),              INTENT(IN)  :: FileName       !< Name of file to write this information to
   INTEGER(IntKi)                         :: ErrStat        !< Error status of the operation
   CHARACTER(*)                           :: ErrMsg         !< Error message if ErrStat /= ErrID_None

   INTEGER(IntKi)           :: unOut
   INTEGER(IntKi)           :: K_local
   INTEGER(B4Ki), PARAMETER :: File_ID = 3
   INTEGER(B4Ki)            :: NumBl

      ! Open the binary output file:
   unOut=-1
   CALL GetNewUnit( unOut, ErrStat, ErrMsg )
   CALL OpenBOutFile ( unOut, TRIM(FileName), ErrStat, ErrMsg )
      IF (ErrStat /= ErrID_None) RETURN

   ! note that I'm not doing anything with the errors here, so it won't tell
   ! you there was a problem writing the data unless it was the last call.

      ! Add a file identification number (in case we ever have to change this):
   WRITE( unOut, IOSTAT=ErrStat )   File_ID

      ! Add how many blade meshes there are:
   NumBl =  SIZE(u_ED%BladePtLoads,1)   ! Note that NumBl is B4Ki
   WRITE( unOut, IOSTAT=ErrStat )   NumBl

      ! Add all of the input meshes:
   DO K_local = 1,NumBl
      CALL MeshWrBin( unOut, u_ED%BladePtLoads(K_local), ErrStat, ErrMsg )
   END DO
   CALL MeshWrBin( unOut, u_ED%TowerPtLoads,            ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_ED%PlatformPtMesh,          ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_SD%TPMesh,                  ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_SD%LMesh,                   ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_HD%Morison%Mesh,      ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_HD%WAMITMesh,                    ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_MAP%PtFairDisplacement,     ErrStat, ErrMsg )
      ! Add how many BD blade meshes there are:
   NumBl =  SIZE(u_BD,1)   ! Note that NumBl is B4Ki
   WRITE( unOut, IOSTAT=ErrStat )   NumBl

   DO K_local = 1,NumBl
      CALL MeshWrBin( unOut, u_BD(K_local)%RootMotion, ErrStat, ErrMsg )
      CALL MeshWrBin( unOut, u_BD(K_local)%DistrLoad, ErrStat, ErrMsg )
   END DO

      ! Add how many AD blade meshes there are:
   NumBl =  SIZE(u_AD%rotors(1)%BladeMotion,1)   ! Note that NumBl is B4Ki 
   WRITE( unOut, IOSTAT=ErrStat )   NumBl

   DO K_local = 1,NumBl
      CALL MeshWrBin( unOut, u_AD%rotors(1)%BladeMotion(k_local), ErrStat, ErrMsg )
   END DO    
      
      ! Close the file
   CLOSE(unOut)

END SUBROUTINE WriteInputMeshesToFile
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine writes motion mesh data to a binary file (for rudimentary visualization and debugging). If unOut < 0, a new file
!! will be opened for writing (FileName). It is up to the caller of this routine to close the file.
SUBROUTINE WriteMotionMeshesToFile(time, y_ED, u_SD, y_SD, u_HD, u_MAP, y_BD, u_BD, UnOut, ErrStat, ErrMsg, FileName)
   REAL(DbKi),                 INTENT(IN)    :: time           !< current simulation time
   TYPE(ED_OutputType),        INTENT(IN)    :: y_ED           !< ElastoDyn outputs
   TYPE(SD_InputType),         INTENT(IN)    :: u_SD           !< SubDyn inputs
   TYPE(SD_OutputType),        INTENT(IN)    :: y_SD           !< SubDyn outputs
   TYPE(HydroDyn_InputType),   INTENT(IN)    :: u_HD           !< HydroDyn inputs
   TYPE(MAP_InputType),        INTENT(IN)    :: u_MAP          !< MAP inputs
   TYPE(BD_OutputType),        INTENT(IN)    :: y_BD(:)        !< BeamDyn outputs
   TYPE(BD_InputType),         INTENT(IN)    :: u_BD(:)        !< BeamDyn inputs
   INTEGER(IntKi) ,            INTENT(INOUT) :: unOut          !< Unit number to write where this info should be written. If unOut < 0, a new file will be opened and the opened unit number will be returned.
   CHARACTER(*),               INTENT(IN)    :: FileName       !< If unOut < 0, FileName will be opened for writing this mesh information.

   INTEGER(IntKi), INTENT(OUT)               :: ErrStat        !< Error status of the operation
   CHARACTER(*)  , INTENT(OUT)               :: ErrMsg         !< Error message if ErrStat /= ErrID_None


   REAL(R8Ki)               :: t

   INTEGER(IntKi)           :: K_local
   INTEGER(B4Ki), PARAMETER :: File_ID = 101
   INTEGER(B4Ki)            :: NumBl

   t = time  ! convert to 8-bytes if necessary (DbKi might not be R8Ki)

   ! note that I'm not doing anything with the errors here, so it won't tell
   ! you there was a problem writing the data unless it was the last call.


      ! Open the binary output file and write a header:
   if (unOut<0) then
      CALL GetNewUnit( unOut, ErrStat, ErrMsg )

      CALL OpenBOutFile ( unOut, TRIM(FileName), ErrStat, ErrMsg )
         IF (ErrStat /= ErrID_None) RETURN

         ! Add a file identification number (in case we ever have to change this):
      WRITE( unOut, IOSTAT=ErrStat )   File_ID

         ! Add how many blade meshes there are:
      NumBl =  SIZE(y_ED%BladeLn2Mesh,1)   ! Note that NumBl is B4Ki
      WRITE( unOut, IOSTAT=ErrStat )   NumBl
      NumBl =  SIZE(y_BD,1)   ! Note that NumBl is B4Ki
      WRITE( unOut, IOSTAT=ErrStat )   NumBl
   end if

   WRITE( unOut, IOSTAT=ErrStat ) t

      ! Add all of the meshes with motions:
   DO K_local = 1,SIZE(y_ED%BladeLn2Mesh,1)
      CALL MeshWrBin( unOut, y_ED%BladeLn2Mesh(K_local), ErrStat, ErrMsg )
   END DO
   CALL MeshWrBin( unOut, y_ED%TowerLn2Mesh,            ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, y_ED%PlatformPtMesh,          ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_SD%TPMesh,                  ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, y_SD%y2Mesh,                  ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, y_SD%y3Mesh,                  ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_HD%Morison%Mesh,      ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_HD%WAMITMesh,                    ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_MAP%PtFairDisplacement,     ErrStat, ErrMsg )
   DO K_local = 1,SIZE(y_BD,1)
      CALL MeshWrBin( unOut, u_BD(K_local)%RootMotion, ErrStat, ErrMsg )
      CALL MeshWrBin( unOut, y_BD(K_local)%BldMotion,  ErrStat, ErrMsg )
   END DO

   !
   !   ! Close the file
   !CLOSE(unOut)
   !
END SUBROUTINE WriteMotionMeshesToFile
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE WriteVTK(t_global, p_FAST, y_FAST, MeshMapData, ED, BD, AD, IfW, OpFM, HD, SD, ExtPtfm, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD)
   REAL(DbKi),               INTENT(IN   ) :: t_global            !< Current global time
   TYPE(FAST_ParameterType), INTENT(IN   ) :: p_FAST              !< Parameters for the glue code
   TYPE(FAST_OutputFileType),INTENT(INOUT) :: y_FAST              !< Output variables for the glue code (only because we're updating VTK_LastWaveIndx)
   TYPE(FAST_ModuleMapType), INTENT(IN   ) :: MeshMapData         !< Data for mapping between modules

   TYPE(ElastoDyn_Data),     INTENT(IN   ) :: ED                  !< ElastoDyn data
   TYPE(BeamDyn_Data),       INTENT(IN   ) :: BD                  !< BeamDyn data
   TYPE(ServoDyn_Data),      INTENT(IN   ) :: SrvD                !< ServoDyn data
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


   INTEGER(IntKi)                          :: ErrStat2
   CHARACTER(ErrMsgLen)                    :: ErrMSg2
   CHARACTER(*), PARAMETER                 :: RoutineName = 'WriteVTK'


      IF ( p_FAST%VTK_Type == VTK_Surf ) THEN
         CALL WrVTK_Surfaces(t_global, p_FAST, y_FAST, MeshMapData, ED, BD, AD, IfW, OpFM, HD, SD, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD)
      ELSE IF ( p_FAST%VTK_Type == VTK_Basic ) THEN
         CALL WrVTK_BasicMeshes(p_FAST, y_FAST, MeshMapData, ED, BD, AD, IfW, OpFM, HD, SD, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD)
      ELSE IF ( p_FAST%VTK_Type == VTK_All ) THEN
         CALL WrVTK_AllMeshes(p_FAST, y_FAST, MeshMapData, ED, BD, AD, IfW, OpFM, HD, SD, ExtPtfm, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD)
      ELSE IF (p_FAST%VTK_Type==VTK_Old) THEN
         CALL WriteInputMeshesToFile( ED%Input(1), AD%Input(1), SD%Input(1), HD%Input(1), MAPp%Input(1), BD%Input(1,:), TRIM(p_FAST%OutFileRoot)//'.InputMeshes.bin', ErrStat2, ErrMsg2)
         CALL WriteMotionMeshesToFile(t_global, ED%y, SD%Input(1), SD%y, HD%Input(1), MAPp%Input(1), BD%y, BD%Input(1,:), y_FAST%UnGra, ErrStat2, ErrMsg2, TRIM(p_FAST%OutFileRoot)//'.gra')
   !unOut = -1
   !CALL MeshWrBin ( unOut, AD%y%BladeLoad(2), ErrStat2, ErrMsg2, 'AD_2_ED_loads.bin');  IF (ErrStat2 /= ErrID_None) CALL WrScr(TRIM(ErrMsg2))
   !CALL MeshWrBin ( unOut, ED%Input(1)%BladePtLoads(2),ErrStat2, ErrMsg2, 'AD_2_ED_loads.bin');  IF (ErrStat2 /= ErrID_None) CALL WrScr(TRIM(ErrMsg2))
   !CALL MeshMapWrBin( unOut, AD%y%BladeLoad(2), ED%Input(1)%BladePtLoads(2), MeshMapData%AD_L_2_BDED_B(2), ErrStat2, ErrMsg2, 'AD_2_ED_loads.bin' );  IF (ErrStat2 /= ErrID_None) CALL WrScr(TRIM(ErrMsg2))
   !close( unOut )
      END IF

     y_FAST%VTK_count = y_FAST%VTK_count + 1

END SUBROUTINE WriteVTK
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine writes all the committed meshes to VTK-formatted files. It doesn't bother with returning an error code.
SUBROUTINE WrVTK_AllMeshes(p_FAST, y_FAST, MeshMapData, ED, BD, AD, IfW, OpFM, HD, SD, ExtPtfm, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD)
   use FVW_IO, only: WrVTK_FVW

   TYPE(FAST_ParameterType), INTENT(IN   ) :: p_FAST              !< Parameters for the glue code
   TYPE(FAST_OutputFileType),INTENT(IN   ) :: y_FAST              !< Output variables for the glue code
   TYPE(FAST_ModuleMapType), INTENT(IN   ) :: MeshMapData         !< Data for mapping between modules

   TYPE(ElastoDyn_Data),     INTENT(IN   ) :: ED                  !< ElastoDyn data
   TYPE(BeamDyn_Data),       INTENT(IN   ) :: BD                  !< BeamDyn data
   TYPE(ServoDyn_Data),      INTENT(IN   ) :: SrvD                !< ServoDyn data
   TYPE(AeroDyn_Data),       INTENT(IN   ) :: AD                  !< AeroDyn data
   TYPE(InflowWind_Data),    INTENT(IN   ) :: IfW                 !< InflowWind data
   TYPE(OpenFOAM_Data),      INTENT(IN   ) :: OpFM                !< OpenFOAM data
   TYPE(HydroDyn_Data),      INTENT(IN   ) :: HD                  !< HydroDyn data
   TYPE(SubDyn_Data),        INTENT(IN   ) :: SD                  !< SubDyn data
   TYPE(ExtPtfm_Data),       INTENT(IN   ) :: ExtPtfm             !< ExtPtfm data
   TYPE(MAP_Data),           INTENT(IN   ) :: MAPp                !< MAP data
   TYPE(FEAMooring_Data),    INTENT(IN   ) :: FEAM                !< FEAMooring data
   TYPE(MoorDyn_Data),       INTENT(IN   ) :: MD                  !< MoorDyn data
   TYPE(OrcaFlex_Data),      INTENT(IN   ) :: Orca                !< OrcaFlex interface data
   TYPE(IceFloe_Data),       INTENT(IN   ) :: IceF                !< IceFloe data
   TYPE(IceDyn_Data),        INTENT(IN   ) :: IceD                !< All the IceDyn data used in time-step loop


!   logical                                 :: outputFields        ! flag to determine if we want to output the HD mesh fields
   INTEGER(IntKi)                          :: NumBl, k
   INTEGER(IntKi)                          :: j                   ! counter for StC instance at location

   INTEGER(IntKi)                          :: ErrStat2
   CHARACTER(ErrMsgLen)                    :: ErrMSg2
   CHARACTER(*), PARAMETER                 :: RoutineName = 'WrVTK_AllMeshes'



   NumBl = 0
   if (allocated(ED%y%BladeRootMotion)) then
      NumBl = SIZE(ED%y%BladeRootMotion)
   end if



! I'm first going to just put all of the meshes that get mapped together, then decide if we're going to print/plot them all

!  ElastoDyn
   if (allocated(ED%Input)) then

         !  ElastoDyn outputs (motions)
      DO K=1,NumBl
         !%BladeLn2Mesh(K) used only when not BD (see below)
         call MeshWrVTK(p_FAST%TurbinePos, ED%y%BladeRootMotion(K), trim(p_FAST%VTK_OutFileRoot)//'.ED_BladeRootMotion'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
      END DO

      call MeshWrVTK(p_FAST%TurbinePos, ED%y%TowerLn2Mesh, trim(p_FAST%VTK_OutFileRoot)//'.ED_TowerLn2Mesh_motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )

! these will get output with their sibling input meshes
      !call MeshWrVTK(p_FAST%TurbinePos, ED%y%HubPtMotion, trim(p_FAST%VTK_OutFileRoot)//'.ED_HubPtMotion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
      !call MeshWrVTK(p_FAST%TurbinePos, ED%y%NacelleMotion, trim(p_FAST%VTK_OutFileRoot)//'.ED_NacelleMotion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
      !call MeshWrVTK(p_FAST%TurbinePos, ED%y%PlatformPtMesh, trim(p_FAST%VTK_OutFileRoot)//'.ED_PlatformPtMesh_motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )

         !  ElastoDyn inputs (loads)
      ! %BladePtLoads used only when not BD (see below)
      call MeshWrVTK(p_FAST%TurbinePos, ED%Input(1)%TowerPtLoads, trim(p_FAST%VTK_OutFileRoot)//'.ED_TowerPtLoads', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, ED%y%TowerLn2Mesh )
      call MeshWrVTK(p_FAST%TurbinePos, ED%Input(1)%HubPtLoad, trim(p_FAST%VTK_OutFileRoot)//'.ED_Hub', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, ED%y%HubPtMotion )
      call MeshWrVTK(p_FAST%TurbinePos, ED%Input(1)%NacelleLoads, trim(p_FAST%VTK_OutFileRoot)//'.ED_Nacelle' ,y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, ED%y%NacelleMotion )
      call MeshWrVTK(p_FAST%TurbinePos, ED%Input(1)%PlatformPtMesh, trim(p_FAST%VTK_OutFileRoot)//'.ED_PlatformPtMesh', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, ED%y%PlatformPtMesh )
   end if


!  BeamDyn
   IF ( p_FAST%CompElast == Module_BD .and. allocated(BD%Input) .and. allocated(BD%y)) THEN

      do K=1,NumBl
            ! BeamDyn inputs
         !call MeshWrVTK(p_FAST%TurbinePos, BD%Input(1,k)%RootMotion, trim(p_FAST%VTK_OutFileRoot)//'.BD_RootMotion'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
         call MeshWrVTK(p_FAST%TurbinePos, BD%Input(1,k)%HubMotion, trim(p_FAST%VTK_OutFileRoot)//'.BD_HubMotion'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
      end do
      if (allocated(MeshMapData%y_BD_BldMotion_4Loads)) then
         do K=1,NumBl
            call MeshWrVTK(p_FAST%TurbinePos, BD%Input(1,k)%DistrLoad, trim(p_FAST%VTK_OutFileRoot)//'.BD_DistrLoad'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, MeshMapData%y_BD_BldMotion_4Loads(k) )
            ! skipping PointLoad
         end do
      elseif (p_FAST%BD_OutputSibling) then
         do K=1,NumBl
            call MeshWrVTK(p_FAST%TurbinePos, BD%Input(1,k)%DistrLoad, trim(p_FAST%VTK_OutFileRoot)//'.BD_Blade'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, BD%y(k)%BldMotion )
            ! skipping PointLoad
         end do
      end if

      do K=1,NumBl
            ! BeamDyn outputs
         call MeshWrVTK(p_FAST%TurbinePos, BD%y(k)%ReactionForce, trim(p_FAST%VTK_OutFileRoot)//'.BD_ReactionForce_RootMotion'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, BD%Input(1,k)%RootMotion )
      end do

      if (.not. p_FAST%BD_OutputSibling) then !otherwise this mesh has been put with the DistrLoad mesh
         do K=1,NumBl
               ! BeamDyn outputs
            call MeshWrVTK(p_FAST%TurbinePos, BD%y(k)%BldMotion, trim(p_FAST%VTK_OutFileRoot)//'.BD_BldMotion'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
         end do
      end if


   ELSE if (p_FAST%CompElast == Module_ED .and. allocated(ED%Input)) then
      ! ElastoDyn
      DO K=1,NumBl
         call MeshWrVTK(p_FAST%TurbinePos, ED%y%BladeLn2Mesh(K), trim(p_FAST%VTK_OutFileRoot)//'.ED_BladeLn2Mesh_motion'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
         call MeshWrVTK(p_FAST%TurbinePos, ED%Input(1)%BladePtLoads(K), trim(p_FAST%VTK_OutFileRoot)//'.ED_BladePtLoads'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, ED%y%BladeLn2Mesh(K) )
      END DO
   END IF

!  ServoDyn
   if (allocated(SrvD%Input)) then
      IF ( ALLOCATED(SrvD%Input(1)%NStCMotionMesh) ) THEN
         do j=1,size(SrvD%Input(1)%NStCMotionMesh)
            IF ( SrvD%Input(1)%NStCMotionMesh(j)%Committed ) THEN
               call MeshWrVTK(p_FAST%TurbinePos, SrvD%y%NStCLoadMesh(j), trim(p_FAST%VTK_OutFileRoot)//'.SrvD_NStC'//trim(num2lstr(j)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, SrvD%Input(1)%NStCMotionMesh(j) )
            ENDIF
         enddo
      ENDIF
      IF ( ALLOCATED(SrvD%Input(1)%TStCMotionMesh) ) THEN
         do j=1,size(SrvD%Input(1)%TStCMotionMesh)
            IF ( SrvD%Input(1)%TStCMotionMesh(j)%Committed ) THEN
               call MeshWrVTK(p_FAST%TurbinePos, SrvD%y%TStCLoadMesh(j), trim(p_FAST%VTK_OutFileRoot)//'.SrvD_TStC'//trim(num2lstr(j)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, SrvD%Input(1)%TStCMotionMesh(j) )
            ENDIF
         enddo
     ENDIF
     IF ( ALLOCATED(SrvD%Input(1)%BStCMotionMesh) ) THEN
        do j=1,size(SrvD%Input(1)%BStCMotionMesh,2)
           DO K=1,size(SrvD%Input(1)%BStCMotionMesh,1)
              call MeshWrVTK(p_FAST%TurbinePos, SrvD%y%BStCLoadMesh(k,j), trim(p_FAST%VTK_OutFileRoot)//'.SrvD_BStC'//trim(num2lstr(j))//'B'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, SrvD%Input(1)%BStCMotionMesh(k,j) )
           ENDDO
         enddo
      ENDIF
      IF ( ALLOCATED(SrvD%Input(1)%SStCMotionMesh) ) THEN
         do j=1,size(SrvD%Input(1)%SStCMotionMesh)
            IF ( SrvD%Input(1)%SStCMotionMesh(j)%Committed ) THEN
               call MeshWrVTK(p_FAST%TurbinePos, SrvD%y%SStCLoadMesh(j), trim(p_FAST%VTK_OutFileRoot)//'.SrvD_SStC'//trim(num2lstr(j)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, SrvD%Input(1)%SStCMotionMesh(j) )
            ENDIF
         enddo
     ENDIF
   end if
   
      
!  AeroDyn   
   IF ( p_FAST%CompAero == Module_AD .and. allocated(AD%Input)) THEN 
               
      if (allocated(AD%Input(1)%rotors(1)%BladeRootMotion)) then      
      
         DO K=1,NumBl   
            call MeshWrVTK(p_FAST%TurbinePos, AD%Input(1)%rotors(1)%BladeRootMotion(K), trim(p_FAST%VTK_OutFileRoot)//'.AD_BladeRootMotion'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
            !call MeshWrVTK(p_FAST%TurbinePos, AD%Input(1)%BladeMotion(K), trim(p_FAST%VTK_OutFileRoot)//'.AD_BladeMotion'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
         END DO

         call MeshWrVTK(p_FAST%TurbinePos, AD%Input(1)%rotors(1)%HubMotion, trim(p_FAST%VTK_OutFileRoot)//'.AD_HubMotion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
         !call MeshWrVTK(p_FAST%TurbinePos, AD%Input(1)%TowerMotion, trim(p_FAST%VTK_OutFileRoot)//'.AD_TowerMotion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
               
         DO K=1,NumBl   
            call MeshWrVTK(p_FAST%TurbinePos, AD%y%rotors(1)%BladeLoad(K), trim(p_FAST%VTK_OutFileRoot)//'.AD_Blade'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, AD%Input(1)%rotors(1)%BladeMotion(k) )
         END DO            
         call MeshWrVTK(p_FAST%TurbinePos, AD%y%rotors(1)%TowerLoad, trim(p_FAST%VTK_OutFileRoot)//'.AD_Tower', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, AD%Input(1)%rotors(1)%TowerMotion )
         
      end if

         ! FVW submodule of AD15
      if (allocated(AD%m%FVW_u)) then
         if (allocated(AD%m%FVW_u(1)%WingsMesh)) then
            DO K=1,NumBl
               call MeshWrVTK(p_FAST%TurbinePos, AD%m%FVW_u(1)%WingsMesh(k), trim(p_FAST%VTK_OutFileRoot)//'.FVW_WingsMesh'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, AD%Input(1)%rotors(1)%BladeMotion(k) )
               !call MeshWrVTK(p_FAST%TurbinePos, AD%Input(1)%BladeMotion(K), trim(p_FAST%OutFileRoot)//'.AD_BladeMotion'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2 )
            END DO
            ! Free wake
            call WrVTK_FVW(AD%p%FVW, AD%x(1)%FVW, AD%z(1)%FVW, AD%m%FVW, trim(p_FAST%VTK_OutFileRoot)//'.FVW', y_FAST%VTK_count, p_FAST%VTK_tWidth, bladeFrame=.FALSE.)  ! bladeFrame==.FALSE. to output in global coords
         end if
      end if
   END IF
   
! HydroDyn            
   IF ( p_FAST%CompHydro == Module_HD .and. allocated(HD%Input)) THEN     
      !TODO: Fix for Visualizaton GJH 4/23/20
      !call MeshWrVTK(p_FAST%TurbinePos, HD%Input(1)%Mesh, trim(p_FAST%VTK_OutFileRoot)//'.HD_Mesh_motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2 )     
      call MeshWrVTK(p_FAST%TurbinePos, HD%Input(1)%Morison%Mesh, trim(p_FAST%VTK_OutFileRoot)//'.HD_Morison_Motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
      
      if (HD%y%WamitMesh%Committed) then
!         if (p_FAST%CompSub == Module_NONE) then
!TODO         call MeshWrVTK(p_FAST%TurbinePos, HD%y%WamitMesh, trim(p_FAST%VTK_OutFileRoot)//'.HD_Mesh', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, HD%Input(1)%WAMITMesh )
!            outputFields = .false.
!         else
            call MeshWrVTK(p_FAST%TurbinePos, HD%y%WamitMesh, trim(p_FAST%VTK_OutFileRoot)//'.HD_Mesh', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, HD%Input(1)%WAMITMesh )
!            outputFields = p_FAST%VTK_fields
!         end if
      endif
      if (HD%y%Morison%Mesh%Committed) then
         call MeshWrVTK(p_FAST%TurbinePos, HD%y%Morison%Mesh, trim(p_FAST%VTK_OutFileRoot)//'.HD_Morison', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, HD%Input(1)%Morison%Mesh )
      endif
   END IF

! SubDyn
   IF ( p_FAST%CompSub == Module_SD .and. allocated(SD%Input)) THEN
      !call MeshWrVTK(p_FAST%TurbinePos, SD%Input(1)%TPMesh, trim(p_FAST%VTK_OutFileRoot)//'.SD_TPMesh_motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
      call MeshWrVTK(p_FAST%TurbinePos, SD%Input(1)%LMesh, trim(p_FAST%VTK_OutFileRoot)//'.SD_LMesh_y2Mesh', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, SD%y%y2Mesh )

      call MeshWrVTK(p_FAST%TurbinePos, SD%y%y1Mesh, trim(p_FAST%VTK_OutFileRoot)//'.SD_y1Mesh_TPMesh', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, SD%Input(1)%TPMesh )
      !call MeshWrVTK(p_FAST%TurbinePos, SD%y%y2Mesh, trim(p_FAST%VTK_OutFileRoot)//'.SD_y2Mesh_motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
      !call MeshWrVTK(p_FAST%TurbinePos, SD%y%y3Mesh, trim(p_FAST%VTK_OutFileRoot)//'.SD_y3Mesh_motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
   ELSE IF ( p_FAST%CompSub == Module_ExtPtfm .and. allocated(ExtPtfm%Input)) THEN
      call MeshWrVTK(p_FAST%TurbinePos, ExtPtfm%y%PtfmMesh, trim(p_FAST%VTK_OutFileRoot)//'.ExtPtfm', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, ExtPtfm%Input(1)%PtfmMesh )
   END IF

! MAP
   IF ( p_FAST%CompMooring == Module_MAP ) THEN
      if (allocated(MAPp%Input)) then
         call MeshWrVTK(p_FAST%TurbinePos, MAPp%y%PtFairleadLoad, trim(p_FAST%VTK_OutFileRoot)//'.MAP_PtFairlead', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, MAPp%Input(1)%PtFairDisplacement )
         !call MeshWrVTK(p_FAST%TurbinePos, MAPp%Input(1)%PtFairDisplacement, trim(p_FAST%VTK_OutFileRoot)//'.MAP_PtFair_motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
      end if

! MoorDyn
   ELSEIF ( p_FAST%CompMooring == Module_MD ) THEN
      if (allocated(MD%Input)) then
         call MeshWrVTK(p_FAST%TurbinePos, MD%y%PtFairleadLoad, trim(p_FAST%VTK_OutFileRoot)//'.MD_PtFairlead', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, MD%Input(1)%PtFairleadDisplacement )
         !call MeshWrVTK(p_FAST%TurbinePos, MD%Input(1)%PtFairleadDisplacement, trim(p_FAST%VTK_OutFileRoot)//'.MD_PtFair_motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
      end if

! FEAMooring
   ELSEIF ( p_FAST%CompMooring == Module_FEAM ) THEN
      if (allocated(FEAM%Input)) then
         call MeshWrVTK(p_FAST%TurbinePos, FEAM%y%PtFairleadLoad, trim(p_FAST%VTK_OutFileRoot)//'.FEAM_PtFairlead', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, FEAM%Input(1)%PtFairleadDisplacement )
         !call MeshWrVTK(p_FAST%TurbinePos, FEAM%Input(1)%PtFairleadDisplacement, trim(p_FAST%VTK_OutFileRoot)//'.FEAM_PtFair_motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
      end if

! Orca
   ELSEIF ( p_FAST%CompMooring == Module_Orca ) THEN
      if (allocated(Orca%Input)) then
         call MeshWrVTK(p_FAST%TurbinePos, Orca%y%PtfmMesh, trim(p_FAST%VTK_OutFileRoot)//'.Orca_PtfmMesh', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, Orca%Input(1)%PtfmMesh )
         !call MeshWrVTK(p_FAST%TurbinePos, Orca%Input(1)%PtfmMesh, trim(p_FAST%VTK_OutFileRoot)//'.Orca_PtfmMesh_motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
      end if
   END IF


! IceFloe
   IF ( p_FAST%CompIce == Module_IceF ) THEN
      if (allocated(IceF%Input)) then
         call MeshWrVTK(p_FAST%TurbinePos, IceF%y%iceMesh, trim(p_FAST%VTK_OutFileRoot)//'.IceF_iceMesh', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, IceF%Input(1)%iceMesh )
         !call MeshWrVTK(p_FAST%TurbinePos, IceF%Input(1)%iceMesh, trim(p_FAST%VTK_OutFileRoot)//'.IceF_iceMesh_motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
      end if

! IceDyn
   ELSEIF ( p_FAST%CompIce == Module_IceD ) THEN
      if (allocated(IceD%Input)) then

         DO k = 1,p_FAST%numIceLegs
            call MeshWrVTK(p_FAST%TurbinePos, IceD%y(k)%PointMesh, trim(p_FAST%VTK_OutFileRoot)//'.IceD_PointMesh'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, IceD%Input(1,k)%PointMesh )
            !call MeshWrVTK(p_FAST%TurbinePos, IceD%Input(1,k)%PointMesh, trim(p_FAST%VTK_OutFileRoot)//'.IceD_PointMesh_motion'//trim(num2lstr(k)), y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
         END DO
      end if

   END IF


END SUBROUTINE WrVTK_AllMeshes
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine writes a minimal subset of meshes (enough to visualize the turbine) to VTK-formatted files. It doesn't bother with
!! returning an error code.
SUBROUTINE WrVTK_BasicMeshes(p_FAST, y_FAST, MeshMapData, ED, BD, AD, IfW, OpFM, HD, SD, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD)

   TYPE(FAST_ParameterType), INTENT(IN   ) :: p_FAST              !< Parameters for the glue code
   TYPE(FAST_OutputFileType),INTENT(IN   ) :: y_FAST              !< Output variables for the glue code
   TYPE(FAST_ModuleMapType), INTENT(IN   ) :: MeshMapData         !< Data for mapping between modules

   TYPE(ElastoDyn_Data),     INTENT(IN   ) :: ED                  !< ElastoDyn data
   TYPE(BeamDyn_Data),       INTENT(IN   ) :: BD                  !< BeamDyn data
   TYPE(ServoDyn_Data),      INTENT(IN   ) :: SrvD                !< ServoDyn data
   TYPE(AeroDyn_Data),       INTENT(IN   ) :: AD                  !< AeroDyn data
   TYPE(InflowWind_Data),    INTENT(IN   ) :: IfW                 !< InflowWind data
   TYPE(OpenFOAM_Data),      INTENT(IN   ) :: OpFM                !< OpenFOAM data
   TYPE(HydroDyn_Data),      INTENT(IN   ) :: HD                  !< HydroDyn data
   TYPE(SubDyn_Data),        INTENT(IN   ) :: SD                  !< SubDyn data
   TYPE(MAP_Data),           INTENT(IN   ) :: MAPp                !< MAP data
   TYPE(FEAMooring_Data),    INTENT(IN   ) :: FEAM                !< FEAMooring data
   TYPE(MoorDyn_Data),       INTENT(IN   ) :: MD                  !< MoorDyn data
   TYPE(OrcaFlex_Data),      INTENT(IN   ) :: Orca                !< OrcaFlex interface data
   TYPE(IceFloe_Data),       INTENT(IN   ) :: IceF                !< IceFloe data
   TYPE(IceDyn_Data),        INTENT(IN   ) :: IceD                !< All the IceDyn data used in time-step loop

   logical                                 :: OutputFields
   INTEGER(IntKi)                          :: NumBl, k
   INTEGER(IntKi)                          :: ErrStat2
   CHARACTER(ErrMsgLen)                    :: ErrMSg2
   CHARACTER(*), PARAMETER                 :: RoutineName = 'WrVTK_BasicMeshes'


   NumBl = 0
   if (allocated(ED%y%BladeRootMotion)) then
      NumBl = SIZE(ED%y%BladeRootMotion)
   end if


! Blades
   IF ( p_FAST%CompAero == Module_AD ) THEN  ! These meshes may have airfoil data associated with nodes...
      DO K=1,NumBl   
         call MeshWrVTK(p_FAST%TurbinePos, AD%Input(1)%rotors(1)%BladeMotion(K), trim(p_FAST%VTK_OutFileRoot)//'.AD_Blade'//trim(num2lstr(k)), &
                        y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, Sib=AD%y%rotors(1)%BladeLoad(K) )
      END DO                  
   ELSE IF ( p_FAST%CompElast == Module_BD ) THEN
      DO K=1,NumBl
         call MeshWrVTK(p_FAST%TurbinePos, BD%y(k)%BldMotion, trim(p_FAST%VTK_OutFileRoot)//'.BD_BldMotion'//trim(num2lstr(k)), &
                        y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
      END DO
   ELSE IF ( p_FAST%CompElast == Module_ED ) THEN
      DO K=1,NumBl
         call MeshWrVTK(p_FAST%TurbinePos, ED%y%BladeLn2Mesh(K), trim(p_FAST%VTK_OutFileRoot)//'.ED_BladeLn2Mesh_motion'//trim(num2lstr(k)), &
                        y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
      END DO
   END IF

! Nacelle
   call MeshWrVTK(p_FAST%TurbinePos, ED%y%NacelleMotion, trim(p_FAST%VTK_OutFileRoot)//'.ED_Nacelle', y_FAST%VTK_count, &
                  p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, Sib=ED%Input(1)%NacelleLoads )

! Hub
   call MeshWrVTK(p_FAST%TurbinePos, ED%y%HubPtMotion, trim(p_FAST%VTK_OutFileRoot)//'.ED_Hub', y_FAST%VTK_count, &
                  p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, Sib=ED%Input(1)%HubPtLoad )
! Tower motions
   call MeshWrVTK(p_FAST%TurbinePos, ED%y%TowerLn2Mesh, trim(p_FAST%VTK_OutFileRoot)//'.ED_TowerLn2Mesh_motion', &
                  y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )



! Substructure
!   call MeshWrVTK(p_FAST%TurbinePos, ED%y%PlatformPtMesh, trim(p_FAST%VTK_OutFileRoot)//'.ED_PlatformPtMesh_motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
!   IF ( p_FAST%CompSub == Module_SD ) THEN
!     call MeshWrVTK(p_FAST%TurbinePos, SD%Input(1)%TPMesh, trim(p_FAST%VTK_OutFileRoot)//'.SD_TPMesh_motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
!      call MeshWrVTK(p_FAST%TurbinePos, SD%y%y2Mesh, trim(p_FAST%VTK_OutFileRoot)//'.SD_y2Mesh_motion', y_FAST%VTK_count, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
!      call MeshWrVTK(p_FAST%TurbinePos, SD%y%y3Mesh, trim(p_FAST%VTK_OutFileRoot)//'.SD_y3Mesh_motion', y_FAST%VTK_count, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
!   END IF

   IF ( p_FAST%CompHydro == Module_HD ) THEN

      if (p_FAST%CompSub == Module_NONE) then
         call MeshWrVTK(p_FAST%TurbinePos, HD%y%WAMITMesh, trim(p_FAST%VTK_OutFileRoot)//'.HD_AllHdroOrigin', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, HD%Input(1)%WAMITMesh )
         outputFields = .false.
      else
         OutputFields = p_FAST%VTK_fields
      end if
     !TODO: Fix for Visualization GJH 4/23/20 
     call MeshWrVTK(p_FAST%TurbinePos, HD%Input(1)%Morison%Mesh, trim(p_FAST%VTK_OutFileRoot)//'.HD_Morison', &
                    y_FAST%VTK_count, OutputFields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, Sib=HD%y%Morison%Mesh )
   END IF


! Mooring Lines?
!   IF ( p_FAST%CompMooring == Module_MAP ) THEN
!      call MeshWrVTK(p_FAST%TurbinePos, MAPp%Input(1)%PtFairDisplacement, trim(p_FAST%VTK_OutFileRoot)//'.MAP_PtFair_motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
!   ELSEIF ( p_FAST%CompMooring == Module_MD ) THEN
!      call MeshWrVTK(p_FAST%TurbinePos, MD%Input(1)%PtFairleadDisplacement, trim(p_FAST%VTK_OutFileRoot)//'.MD_PtFair_motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
!   ELSEIF ( p_FAST%CompMooring == Module_FEAM ) THEN
!      call MeshWrVTK(p_FAST%TurbinePos, FEAM%Input(1)%PtFairleadDisplacement, trim(p_FAST%VTK_OutFileRoot)//'FEAM_PtFair_motion', y_FAST%VTK_count, p_FAST%VTK_fields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth )
!   END IF


END SUBROUTINE WrVTK_BasicMeshes
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine writes a minimal subset of meshes with surfaces to VTK-formatted files. It doesn't bother with
!! returning an error code.
SUBROUTINE WrVTK_Surfaces(t_global, p_FAST, y_FAST, MeshMapData, ED, BD, AD, IfW, OpFM, HD, SD, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD)
   use FVW_IO, only: WrVTK_FVW

   REAL(DbKi),               INTENT(IN   ) :: t_global            !< Current global time
   TYPE(FAST_ParameterType), INTENT(IN   ) :: p_FAST              !< Parameters for the glue code
   TYPE(FAST_OutputFileType),INTENT(INOUT) :: y_FAST              !< Output variables for the glue code (only because we're updating VTK_LastWaveIndx)
   TYPE(FAST_ModuleMapType), INTENT(IN   ) :: MeshMapData         !< Data for mapping between modules

   TYPE(ElastoDyn_Data),     INTENT(IN   ) :: ED                  !< ElastoDyn data
   TYPE(BeamDyn_Data),       INTENT(IN   ) :: BD                  !< BeamDyn data
   TYPE(ServoDyn_Data),      INTENT(IN   ) :: SrvD                !< ServoDyn data
   TYPE(AeroDyn_Data),       INTENT(IN   ) :: AD                  !< AeroDyn data
   TYPE(InflowWind_Data),    INTENT(IN   ) :: IfW                 !< InflowWind data
   TYPE(OpenFOAM_Data),      INTENT(IN   ) :: OpFM                !< OpenFOAM data
   TYPE(HydroDyn_Data),      INTENT(IN   ) :: HD                  !< HydroDyn data
   TYPE(SubDyn_Data),        INTENT(IN   ) :: SD                  !< SubDyn data
   TYPE(MAP_Data),           INTENT(IN   ) :: MAPp                !< MAP data
   TYPE(FEAMooring_Data),    INTENT(IN   ) :: FEAM                !< FEAMooring data
   TYPE(MoorDyn_Data),       INTENT(IN   ) :: MD                  !< MoorDyn data
   TYPE(OrcaFlex_Data),      INTENT(IN   ) :: Orca                !< OrcaFlex interface data
   TYPE(IceFloe_Data),       INTENT(IN   ) :: IceF                !< IceFloe data
   TYPE(IceDyn_Data),        INTENT(IN   ) :: IceD                !< All the IceDyn data used in time-step loop


   logical, parameter                      :: OutputFields = .FALSE. ! due to confusion about what fields mean on a surface, we are going to just output the basic meshes if people ask for fields
   INTEGER(IntKi)                          :: NumBl, k
   INTEGER(IntKi)                          :: ErrStat2
   CHARACTER(ErrMsgLen)                    :: ErrMSg2
   CHARACTER(*), PARAMETER                 :: RoutineName = 'WrVTK_Surfaces'

   NumBl = 0
   if (allocated(ED%y%BladeRootMotion)) then
      NumBl = SIZE(ED%y%BladeRootMotion)
   end if

! Ground (written at initialization)

! Wave elevation
   if ( allocated( p_FAST%VTK_Surface%WaveElev ) ) call WrVTK_WaveElev( t_global, p_FAST, y_FAST, HD)

! Nacelle
   call MeshWrVTK_PointSurface (p_FAST%TurbinePos, ED%y%NacelleMotion, trim(p_FAST%VTK_OutFileRoot)//'.NacelleSurface', &
                                y_FAST%VTK_count, OutputFields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth , verts = p_FAST%VTK_Surface%NacelleBox, Sib=ED%Input(1)%NacelleLoads )


! Hub
   call MeshWrVTK_PointSurface (p_FAST%TurbinePos, ED%y%HubPtMotion, trim(p_FAST%VTK_OutFileRoot)//'.HubSurface', &
                                y_FAST%VTK_count, OutputFields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth , &
                                NumSegments=p_FAST%VTK_Surface%NumSectors, radius=p_FAST%VTK_Surface%HubRad, Sib=ED%Input(1)%HubPtLoad )

! Tower motions
   call MeshWrVTK_Ln2Surface (p_FAST%TurbinePos, ED%y%TowerLn2Mesh, trim(p_FAST%VTK_OutFileRoot)//'.TowerSurface', &
                              y_FAST%VTK_count, OutputFields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, p_FAST%VTK_Surface%NumSectors, p_FAST%VTK_Surface%TowerRad )

! Blades
   IF ( p_FAST%CompAero == Module_AD ) THEN  ! These meshes may have airfoil data associated with nodes...
      DO K=1,NumBl
         call MeshWrVTK_Ln2Surface (p_FAST%TurbinePos, AD%Input(1)%rotors(1)%BladeMotion(K), trim(p_FAST%VTK_OutFileRoot)//'.Blade'//trim(num2lstr(k))//'Surface', &
                                    y_FAST%VTK_count, OutputFields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth , verts=p_FAST%VTK_Surface%BladeShape(K)%AirfoilCoords &
                                    ,Sib=AD%y%rotors(1)%BladeLoad(k) )
      END DO                  
   ELSE IF ( p_FAST%CompElast == Module_BD ) THEN
      DO K=1,NumBl
         call MeshWrVTK_Ln2Surface (p_FAST%TurbinePos, BD%y(k)%BldMotion, trim(p_FAST%VTK_OutFileRoot)//'.Blade'//trim(num2lstr(k))//'Surface', &
                                    y_FAST%VTK_count, OutputFields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth , verts=p_FAST%VTK_Surface%BladeShape(K)%AirfoilCoords )
      END DO
   ELSE IF ( p_FAST%CompElast == Module_ED ) THEN
      DO K=1,NumBl
         call MeshWrVTK_Ln2Surface (p_FAST%TurbinePos, ED%y%BladeLn2Mesh(K), trim(p_FAST%VTK_OutFileRoot)//'.Blade'//trim(num2lstr(k))//'Surface', &
                                    y_FAST%VTK_count, OutputFields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth , verts=p_FAST%VTK_Surface%BladeShape(K)%AirfoilCoords )
      END DO
   END IF

! Free wake
   if (allocated(AD%m%FVW_u)) then
      if (allocated(AD%m%FVW_u(1)%WingsMesh)) then
         call WrVTK_FVW(AD%p%FVW, AD%x(1)%FVW, AD%z(1)%FVW, AD%m%FVW, trim(p_FAST%VTK_OutFileRoot)//'.FVW', y_FAST%VTK_count, p_FAST%VTK_tWidth, bladeFrame=.FALSE.)  ! bladeFrame==.FALSE. to output in global coords
      end if
   end if


! Platform
! call MeshWrVTK_PointSurface (p_FAST%TurbinePos, ED%y%PlatformPtMesh, trim(p_FAST%VTK_OutFileRoot)//'.PlatformSurface', y_FAST%VTK_count, OutputFields, ErrStat2, ErrMsg2, Radius = p_FAST%VTK_Surface%GroundRad )


! Substructure
!   call MeshWrVTK(p_FAST%TurbinePos, ED%y%PlatformPtMesh, trim(p_FAST%VTK_OutFileRoot)//'.ED_PlatformPtMesh_motion', y_FAST%VTK_count, OutputFields, ErrStat2, ErrMsg2 )
!   IF ( p_FAST%CompSub == Module_SD ) THEN
!     call MeshWrVTK(p_FAST%TurbinePos, SD%Input(1)%TPMesh, trim(p_FAST%VTK_OutFileRoot)//'.SD_TPMesh_motion', y_FAST%VTK_count, OutputFields, ErrStat2, ErrMsg2 )     
!      call MeshWrVTK(p_FAST%TurbinePos, SD%y%y2Mesh, trim(p_FAST%VTK_OutFileRoot)//'.SD_y2Mesh_motion', y_FAST%VTK_count, OutputFields, ErrStat2, ErrMsg2 )        
!      call MeshWrVTK(p_FAST%TurbinePos, SD%y%y3Mesh, trim(p_FAST%VTK_OutFileRoot)//'.SD_y3Mesh_motion', y_FAST%VTK_count, OutputFields, ErrStat2, ErrMsg2 )        
!   END IF 
!TODO: Fix below section for new Morison GJH 4/23/20
   !   
   !IF ( HD%Input(1)%Morison%Mesh%Committed ) THEN 
   !   !if ( p_FAST%CompSub == Module_NONE ) then ! floating
   !   !   OutputFields = .false.
   !   !else
   !   !   OutputFields = p_FAST%VTK_fields
   !   !end if
   !      
   !   call MeshWrVTK_Ln2Surface (p_FAST%TurbinePos, HD%Input(1)%Morison%Mesh, trim(p_FAST%VTK_OutFileRoot)//'.MorisonSurface', &
   !                              y_FAST%VTK_count, OutputFields, ErrStat2, ErrMsg2, p_FAST%VTK_tWidth, p_FAST%VTK_Surface%NumSectors, &
   !                              p_FAST%VTK_Surface%MorisonRad, Sib=HD%y%Morison%Mesh )
   !END IF
   
   
! Mooring Lines?            
!   IF ( p_FAST%CompMooring == Module_MAP ) THEN
!      call MeshWrVTK(p_FAST%TurbinePos, MAPp%Input(1)%PtFairDisplacement, trim(p_FAST%VTK_OutFileRoot)//'.MAP_PtFair_motion', y_FAST%VTK_count, OutputFields, ErrStat2, ErrMsg2 )
!   ELSEIF ( p_FAST%CompMooring == Module_MD ) THEN
!      call MeshWrVTK(p_FAST%TurbinePos, MD%Input(1)%PtFairleadDisplacement, trim(p_FAST%VTK_OutFileRoot)//'.MD_PtFair_motion', y_FAST%VTK_count, OutputFields, ErrStat2, ErrMsg2 )
!   ELSEIF ( p_FAST%CompMooring == Module_FEAM ) THEN
!      call MeshWrVTK(p_FAST%TurbinePos, FEAM%Input(1)%PtFairleadDisplacement, trim(p_FAST%VTK_OutFileRoot)//'FEAM_PtFair_motion', y_FAST%VTK_count, OutputFields, ErrStat2, ErrMsg2   )
!   END IF


   if (p_FAST%VTK_fields) then
      call WrVTK_BasicMeshes(p_FAST, y_FAST, MeshMapData, ED, BD, AD, IfW, OpFM, HD, SD, SrvD, MAPp, FEAM, MD, Orca, IceF, IceD)
   end if


END SUBROUTINE WrVTK_Surfaces
!----------------------------------------------------------------------------------------------------------------------------------
!> This subroutine writes the wave elevation data for a given time step
SUBROUTINE WrVTK_WaveElev(t_global, p_FAST, y_FAST, HD)

   REAL(DbKi),               INTENT(IN   ) :: t_global            !< Current global time
   TYPE(FAST_ParameterType), INTENT(IN   ) :: p_FAST              !< Parameters for the glue code
   TYPE(FAST_OutputFileType),INTENT(INOUT) :: y_FAST              !< Output variables for the glue code

   TYPE(HydroDyn_Data),      INTENT(IN   ) :: HD                  !< HydroDyn data

   ! local variables
   INTEGER(IntKi)                        :: Un                    ! fortran unit number
   INTEGER(IntKi)                        :: n, iy, ix             ! loop counters
   REAL(SiKi)                            :: t
   CHARACTER(1024)                       :: FileName
   INTEGER(IntKi)                        :: NumberOfPoints
   INTEGER(IntKi), parameter             :: NumberOfLines = 0
   INTEGER(IntKi)                        :: NumberOfPolys
   CHARACTER(1024)                       :: Tstr
   INTEGER(IntKi)                        :: ErrStat2
   CHARACTER(ErrMsgLen)                  :: ErrMsg2
   CHARACTER(*),PARAMETER                :: RoutineName = 'WrVTK_WaveElev'


   NumberOfPoints = size(p_FAST%VTK_surface%WaveElevXY,2)
      ! I'm going to make triangles for now. we should probably just make this a structured file at some point
   NumberOfPolys  = ( p_FAST%VTK_surface%NWaveElevPts(1) - 1 ) * &
                    ( p_FAST%VTK_surface%NWaveElevPts(2) - 1 ) * 2

   !.................................................................
   ! write the data that potentially changes each time step:
   !.................................................................
   ! construct the string for the zero-padded VTK write-out step
   write(Tstr, '(i' // trim(Num2LStr(p_FAST%VTK_tWidth)) //'.'// trim(Num2LStr(p_FAST%VTK_tWidth)) // ')') y_FAST%VTK_count

   ! PolyData (.vtp) - Serial vtkPolyData (unstructured) file
   FileName = TRIM(p_FAST%VTK_OutFileRoot)//'.WaveSurface.'//TRIM(Tstr)//'.vtp'

   call WrVTK_header( FileName, NumberOfPoints, NumberOfLines, NumberOfPolys, Un, ErrStat2, ErrMsg2 )
      if (ErrStat2 >= AbortErrLev) return

! points (nodes, augmented with NumSegments):
      WRITE(Un,'(A)')         '      <Points>'
      WRITE(Un,'(A)')         '        <DataArray type="Float32" NumberOfComponents="3" format="ascii">'

      ! I'm not going to interpolate in time; I'm just going to get the index of the closest wave time value
      t = REAL(t_global,SiKi)
      call GetWaveElevIndx( t, HD%p%WaveTime, y_FAST%VTK_LastWaveIndx )

      n = 1
      do ix=1,p_FAST%VTK_surface%NWaveElevPts(1)
         do iy=1,p_FAST%VTK_surface%NWaveElevPts(2)
            WRITE(Un,VTK_AryFmt) p_FAST%VTK_surface%WaveElevXY(:,n), p_FAST%VTK_surface%WaveElev(y_FAST%VTK_LastWaveIndx,n)
            n = n+1
         end do
      end do

      WRITE(Un,'(A)')         '        </DataArray>'
      WRITE(Un,'(A)')         '      </Points>'


      WRITE(Un,'(A)')         '      <Polys>'
      WRITE(Un,'(A)')         '        <DataArray type="Int32" Name="connectivity" format="ascii">'

      do ix=1,p_FAST%VTK_surface%NWaveElevPts(1)-1
         do iy=1,p_FAST%VTK_surface%NWaveElevPts(2)-1
            n = p_FAST%VTK_surface%NWaveElevPts(1)*(ix-1)+iy - 1 ! points start at 0

            WRITE(Un,'(3(i7))') n,   n+1,                                    n+p_FAST%VTK_surface%NWaveElevPts(2)
            WRITE(Un,'(3(i7))') n+1, n+1+p_FAST%VTK_surface%NWaveElevPts(2), n+p_FAST%VTK_surface%NWaveElevPts(2)

         end do
      end do
      WRITE(Un,'(A)')         '        </DataArray>'

      WRITE(Un,'(A)')         '        <DataArray type="Int32" Name="offsets" format="ascii">'
      do n=1,NumberOfPolys
         WRITE(Un,'(i7)') 3*n
      end do
      WRITE(Un,'(A)')         '        </DataArray>'
      WRITE(Un,'(A)')         '      </Polys>'

      call WrVTK_footer( Un )

END SUBROUTINE WrVTK_WaveElev
!----------------------------------------------------------------------------------------------------------------------------------
!> This function returns the index, Ind, of the XAry closest to XValIn, where XAry is assumed to be periodic. It starts
!! searching at the value of Ind from a previous step.
SUBROUTINE GetWaveElevIndx( XValIn, XAry, Ind )

      ! Argument declarations.

   INTEGER, INTENT(INOUT)       :: Ind                ! Initial and final index into the arrays.

   REAL(SiKi), INTENT(IN)       :: XAry    (:)        !< Array of X values to be interpolated.
   REAL(SiKi), INTENT(IN)       :: XValIn             !< X value to be found


   INTEGER                      :: AryLen             ! Length of the arrays.
   REAL(SiKi)                   :: XVal               !< X to be found (wrapped/periodic)


   AryLen = size(XAry)

      ! Wrap XValIn into the range XAry(1) to XAry(AryLen)
   XVal = MOD(XValIn, XAry(AryLen))



        ! Let's check the limits first.

   IF ( XVal <= XAry(1) )  THEN
      Ind = 1
      RETURN
   ELSE IF ( XVal >= XAry(AryLen) )  THEN
      Ind = AryLen
      RETURN
   ELSE
      ! Set the Ind to the first index if we are at the beginning of XAry
      IF ( XVal <= XAry(2) )  THEN
         Ind = 1
      END IF
   END IF


     ! Let's interpolate!

   Ind = MAX( MIN( Ind, AryLen-1 ), 1 )

   DO

      IF ( XVal < XAry(Ind) )  THEN

         Ind = Ind - 1

      ELSE IF ( XVal >= XAry(Ind+1) )  THEN

         Ind = Ind + 1

      ELSE

         ! XAry(Ind) <= XVal < XAry(Ind+1)
         ! this would make it the "closest" node, but I'm not going to worry about that for visualization purposes
         !if ( XVal > (XAry(Ind+1) + XAry(Ind))/2.0_SiKi ) Ind = Ind + 1

         RETURN

      END IF

   END DO

   RETURN
END SUBROUTINE GetWaveElevIndx
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ReadModeShapeMatlabFile(p_FAST, ErrStat, ErrMsg)
   TYPE(FAST_ParameterType), INTENT(INOUT) :: p_FAST              !< Parameters for the glue code
   INTEGER(IntKi),           INTENT(  OUT) :: ErrStat             !< Error status of the operation
   CHARACTER(*),             INTENT(  OUT) :: ErrMsg              !< Error message if ErrStat /= ErrID_None

   ! local variables
   INTEGER(IntKi)                          :: ErrStat2
   CHARACTER(ErrMsgLen)                    :: ErrMsg2
   CHARACTER(*), PARAMETER                 :: RoutineName = 'ReadModeShapeMatlabFile'

   INTEGER(4)                              :: FileType
   INTEGER(4)                              :: nModes
   INTEGER(4)                              :: nStates
   INTEGER(4)                              :: NLinTimes
   INTEGER(IntKi)                          :: iMode
   INTEGER(IntKi)                          :: UnIn

   ErrStat = ErrID_None
   ErrMsg  = ""

      !  Open data file.
   CALL GetNewUnit( UnIn, ErrStat2, ErrMsg2 )

   CALL OpenBInpFile ( UnIn, trim(p_FAST%VTK_modes%MatlabFileName), ErrStat2, ErrMsg2 )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      IF (ErrStat >= AbortErrLev) RETURN

      ! Process the requested data records of this file.

   CALL WrScr ( NewLine//' =======================================================' )
   CALL WrScr ( ' Reading in data from file "'//TRIM( p_FAST%VTK_modes%MatlabFileName )//'".'//NewLine )


      ! Read some of the header information.

   READ (UnIn, IOSTAT=ErrStat2)  FileType    ! placeholder for future file format changes
   IF ( ErrStat2 /= 0 )  THEN
      CALL SetErrStat ( ErrID_Fatal, 'Fatal error reading FileType from file "'//TRIM( p_FAST%VTK_modes%MatlabFileName )//'".', ErrStat, ErrMsg, RoutineName )
      RETURN
   ENDIF

   READ (UnIn, IOSTAT=ErrStat2)  nModes    ! number of modes in the file
   IF ( ErrStat2 /= 0 )  THEN
      CALL SetErrStat ( ErrID_Fatal, 'Fatal error reading nModes from file "'//TRIM( p_FAST%VTK_modes%MatlabFileName )//'".', ErrStat, ErrMsg, RoutineName )
      RETURN
   ENDIF

   READ (UnIn, IOSTAT=ErrStat2)  nStates    ! number of states in the file
   IF ( ErrStat2 /= 0 )  THEN
      CALL SetErrStat ( ErrID_Fatal, 'Fatal error reading nStates from file "'//TRIM( p_FAST%VTK_modes%MatlabFileName )//'".', ErrStat, ErrMsg, RoutineName )
      RETURN
   ENDIF

   READ (UnIn, IOSTAT=ErrStat2)  NLinTimes    ! number of linearization times / azimuths in the file
   IF ( ErrStat2 /= 0 )  THEN
      CALL SetErrStat ( ErrID_Fatal, 'Fatal error reading NLinTimes from file "'//TRIM( p_FAST%VTK_modes%MatlabFileName )//'".', ErrStat, ErrMsg, RoutineName )
      RETURN
   ENDIF

   ALLOCATE( p_FAST%VTK_Modes%NaturalFreq_Hz(nModes), &
             p_FAST%VTK_Modes%DampingRatio(  nModes), &
             p_FAST%VTK_Modes%DampedFreq_Hz( nModes),   STAT=ErrStat2 )
      IF ( ErrStat2 /= 0 )  THEN
         CALL SetErrStat ( ErrID_Fatal, 'Error allocating arrays to read from file.', ErrStat, ErrMsg, RoutineName )
         RETURN
      ENDIF


   READ(UnIn, IOSTAT=ErrStat2) p_FAST%VTK_Modes%NaturalFreq_Hz ! read entire array
   IF ( ErrStat2 /= 0 )  THEN
      CALL SetErrStat ( ErrID_Fatal, 'Fatal error reading NaturalFreq_Hz array from file "'//TRIM( p_FAST%VTK_modes%MatlabFileName )//'".', ErrStat, ErrMsg, RoutineName )
      RETURN
   ENDIF

   READ(UnIn, IOSTAT=ErrStat2) p_FAST%VTK_Modes%DampingRatio ! read entire array
   IF ( ErrStat2 /= 0 )  THEN
      CALL SetErrStat ( ErrID_Fatal, 'Fatal error reading DampingRatio array from file "'//TRIM( p_FAST%VTK_modes%MatlabFileName )//'".', ErrStat, ErrMsg, RoutineName )
      RETURN
   ENDIF

   READ(UnIn, IOSTAT=ErrStat2) p_FAST%VTK_Modes%DampedFreq_Hz ! read entire array
   IF ( ErrStat2 /= 0 )  THEN
      CALL SetErrStat ( ErrID_Fatal, 'Fatal error reading DampedFreq_Hz array from file "'//TRIM( p_FAST%VTK_modes%MatlabFileName )//'".', ErrStat, ErrMsg, RoutineName )
      RETURN
   ENDIF

   if (nModes < p_FAST%VTK_Modes%VTKLinModes) CALL SetErrStat(ErrID_Severe,'Number of modes requested exceeds the number of modes in the linearization analysis file "'//TRIM( p_FAST%VTK_modes%MatlabFileName )//'".', ErrStat, ErrMsg, RoutineName)
   if (NLinTimes /= p_FAST%NLinTimes) CALL SetErrStat(ErrID_Severe,'Number of times linearization was performed is not the same as the number of linearization times in the linearization analysis file "'//TRIM( p_FAST%VTK_modes%MatlabFileName )//'".', ErrStat, ErrMsg, RoutineName)


      !Let's read only the number of modes we need to use
   nModes = min( nModes, p_FAST%VTK_Modes%VTKLinModes )

   ALLOCATE( p_FAST%VTK_Modes%x_eig_magnitude(nStates, NLinTimes, nModes), &
             p_FAST%VTK_Modes%x_eig_phase(    nStates, NLinTimes, nModes), STAT=ErrStat2 )
      IF ( ErrStat2 /= 0 )  THEN
         CALL SetErrStat ( ErrID_Fatal, 'Error allocating arrays to read from file.', ErrStat, ErrMsg, RoutineName )
         RETURN
      ENDIF

    do iMode = 1,nModes

      READ(UnIn, IOSTAT=ErrStat2) p_FAST%VTK_Modes%x_eig_magnitude(:,:,iMode) ! read data for one mode
      IF ( ErrStat2 /= 0 )  THEN
         CALL SetErrStat ( ErrID_Fatal, 'Fatal error reading x_eig_magnitude from file "'//TRIM( p_FAST%VTK_modes%MatlabFileName )//'".', ErrStat, ErrMsg, RoutineName )
         RETURN
      ENDIF

      READ(UnIn, IOSTAT=ErrStat2) p_FAST%VTK_Modes%x_eig_phase(:,:,iMode) ! read data for one mode
      IF ( ErrStat2 /= 0 )  THEN
         CALL SetErrStat ( ErrID_Fatal, 'Fatal error reading x_eig_phase from file "'//TRIM( p_FAST%VTK_modes%MatlabFileName )//'".', ErrStat, ErrMsg, RoutineName )
         RETURN
      ENDIF

    end do

END SUBROUTINE ReadModeShapeMatlabFile
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ReadModeShapeFile(p_FAST, InputFile, ErrStat, ErrMsg, checkpointOnly)
   TYPE(FAST_ParameterType),     INTENT(INOUT) :: p_FAST          !< Parameters for the glue code
   CHARACTER(*),                 INTENT(IN   ) :: InputFile       !< Name of the text input file to read
   INTEGER(IntKi),               INTENT(  OUT) :: ErrStat         !< Error status of the operation
   CHARACTER(*),                 INTENT(  OUT) :: ErrMsg          !< Error message if ErrStat /= ErrID_None
   LOGICAL,      OPTIONAL,       INTENT(IN   ) :: checkpointOnly  !< Whether to return after reading checkpoint file name

   ! local variables
   INTEGER(IntKi)                          :: ErrStat2
   CHARACTER(ErrMsgLen)                    :: ErrMsg2
   CHARACTER(*), PARAMETER                 :: RoutineName = 'ReadModeShapeFile'

   CHARACTER(1024)                         :: PriPath            ! Path name of the primary file
   INTEGER(IntKi)                          :: i
   INTEGER(IntKi)                          :: UnIn
   INTEGER(IntKi)                          :: UnEc
   LOGICAL                                 :: VTKLinTimes1

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc = -1

   CALL GetPath( InputFile, PriPath )    ! Input files will be relative to the path where the primary input file is located.

      !  Open data file.
   CALL GetNewUnit( UnIn, ErrStat2, ErrMsg2 )

   CALL OpenFInpFile ( UnIn, InputFile, ErrStat2, ErrMsg2 )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      IF (ErrStat >= AbortErrLev) RETURN


   CALL ReadCom( UnIn, InputFile, 'File header: (line 1)', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

   CALL ReadCom( UnIn, InputFile, 'File header: (line 2)', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

   !----------- FILE NAMES ----------------------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: File Names', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

   CALL ReadVar( UnIn, InputFile, p_FAST%VTK_modes%CheckpointRoot, 'CheckpointRoot', 'Name of the checkpoint file written by FAST when linearization data was produced', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

   IF ( PathIsRelative( p_FAST%VTK_modes%CheckpointRoot ) ) p_FAST%VTK_modes%CheckpointRoot = TRIM(PriPath)//TRIM(p_FAST%VTK_modes%CheckpointRoot)

   if (present(checkpointOnly)) then
      if (checkpointOnly) then
         call cleanup()
         return
      end if
   end if


   CALL ReadVar( UnIn, InputFile, p_FAST%VTK_modes%MatlabFileName, 'MatlabFileName', 'Name of the file with eigenvectors written by Matlab', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL Cleanup()
         RETURN
      END IF
   IF ( PathIsRelative( p_FAST%VTK_modes%MatlabFileName ) ) p_FAST%VTK_modes%MatlabFileName = TRIM(PriPath)//TRIM(p_FAST%VTK_modes%MatlabFileName)

   !----------- VISUALIZATION OPTIONS ------------------------------------------

   CALL ReadCom( UnIn, InputFile, 'Section Header: Visualization Options', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

   CALL ReadVar( UnIn, InputFile, p_FAST%VTK_modes%VTKLinModes, 'VTKLinModes', 'Number of modes to visualize', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


   if (p_FAST%VTK_modes%VTKLinModes <= 0) CALL SetErrStat( ErrID_Fatal, "VTKLinModes must be a positive number.", ErrStat, ErrMsg, RoutineName )

   if (ErrStat >= AbortErrLev) then
      CALL Cleanup()
      RETURN
   end if


   call AllocAry( p_FAST%VTK_modes%VTKModes, p_FAST%VTK_modes%VTKLinModes, 'VTKModes', ErrStat2, ErrMsg2)
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      if ( ErrStat >= AbortErrLev ) then
         call Cleanup()
         return
      end if

   p_FAST%VTK_modes%VTKModes = -1

   CALL ReadAry( UnIn, InputFile, p_FAST%VTK_modes%VTKModes, p_FAST%VTK_modes%VTKLinModes, 'VTKModes', 'List of modes to visualize', ErrStat2, ErrMsg2, UnEc )
   ! note that we don't check the ErrStat here; if the user entered fewer than p_FAST%VTK_modes%VTKLinModes values, we will use the
   ! last entry to fill in remaining values.
   !Check 1st value, we need at least one good value from user or throw error
   IF (p_FAST%VTK_modes%VTKModes(1) < 0 ) THEN
      call SetErrStat( ErrID_Fatal, "VTKModes must contain positive numbers.", ErrStat, ErrMsg, RoutineName )
         CALL CleanUp()
         RETURN
   ELSE
      DO i = 2, p_FAST%VTK_modes%VTKLinModes
         IF ( p_FAST%VTK_modes%VTKModes(i) < 0 ) THEN
            p_FAST%VTK_modes%VTKModes(i)=p_FAST%VTK_modes%VTKModes(i-1) + 1
         ENDIF
      ENDDO
   ENDIF


   CALL ReadVar( UnIn, InputFile, p_FAST%VTK_modes%VTKLinScale, 'VTKLinScale', 'Mode shape visualization scaling factor', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

   CALL ReadVar( UnIn, InputFile, p_FAST%VTK_modes%VTKLinTim, 'VTKLinTim', 'Switch to make one animation for all LinTimes together (1) or separate animations for each LinTimes(2)', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

   CALL ReadVar( UnIn, InputFile, VTKLinTimes1, 'VTKLinTimes1', 'If VTKLinTim=2, visualize modes at LinTimes(1) only?', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


   CALL ReadVar( UnIn, InputFile, p_FAST%VTK_modes%VTKLinPhase, 'VTKLinPhase', 'Phase when making one animation for all LinTimes together (used only when VTKLinTim=1)', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

! overwrite these based on inputs:

      if (p_FAST%VTK_modes%VTKLinTim == 2) then
         p_FAST%VTK_modes%VTKLinPhase = 0      ! "Phase when making one animation for all LinTimes together (used only when VTKLinTim=1)" -

         if (VTKLinTimes1) then
            p_FAST%VTK_modes%VTKNLinTimes = 1
         else
            p_FAST%VTK_modes%VTKNLinTimes = p_FAST%NLinTimes
         end if
      else
         p_FAST%VTK_modes%VTKNLinTimes = p_FAST%NLinTimes
      end if

contains
   SUBROUTINE Cleanup()
      IF (UnIn > 0) CLOSE(UnIn)
   END SUBROUTINE Cleanup

END SUBROUTINE ReadModeShapeFile

END MODULE