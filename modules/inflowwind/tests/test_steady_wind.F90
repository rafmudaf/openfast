module test_steady_wind

    use pFUnit_mod
    use test_tools
    use InflowWind_Subs
    use InflowWind_Types

    implicit none

contains

@test
subroutine test_steady_wind_single_height()

    TYPE(FileInfoType)              :: InFileInfo
    TYPE(InflowWind_InputFile)      :: InputFileData
    CHARACTER(1024)                 :: PriPath 
    INTEGER(IntKi)                  :: TmpErrStat
    CHARACTER(ErrMsgLen)            :: TmpErrMsg

    PriPath = ""

    CALL InitFileInfo(steady_one_height, InFileInfo)
    CALL InflowWind_ParseInputFileInfo(InputFileData , InFileInfo, PriPath, TmpErrStat, TmpErrMsg)

    @assertEqual(InputFileData%WindType, 1)
    @assertEqual(InputFileData%NWindVel, 1)
    @assertEqual(InputFileData%WindVziList(1), 90)

end subroutine test_steady_wind_single_height


@test
subroutine test_steady_wind_mult_heights()

    TYPE(FileInfoType)              :: InFileInfo
    TYPE(InflowWind_InputFile)      :: InputFileData 
    CHARACTER(1024)                 :: PriPath
    INTEGER(IntKi)                  :: TmpErrStat
    CHARACTER(ErrMsgLen)            :: TmpErrMsg

    PriPath = ""

    CALL InitFileInfo(steady_multiple_heights, InFileInfo)
    CALL InflowWind_ParseInputFileInfo(InputFileData , InFileInfo, PriPath, TmpErrStat, TmpErrMsg)

    @assertEqual(InputFileData%WindType, 1)
    @assertEqual(InputFileData%NWindVel, 2)
    @assertEqual(InputFileData%WindVziList(1), 80)
    @assertEqual(InputFileData%WindVziList(2), 100)

end subroutine test_steady_wind_mult_heights

end module
