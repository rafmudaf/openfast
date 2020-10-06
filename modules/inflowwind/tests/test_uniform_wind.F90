module test_uniform_wind

    use pFUnit_mod
    use test_tools
    use InflowWind_Subs
    use InflowWind_Types

    implicit none

contains

    @test
    subroutine test_uniform_wind_input()

        TYPE(FileInfoType)              :: InFileInfo
        TYPE(InflowWind_InputFile)      :: InputFileData
        CHARACTER(1024)                 :: PriPath 
        INTEGER(IntKi)                  :: TmpErrStat
        CHARACTER(ErrMsgLen)            :: TmpErrMsg

        CHARACTER(16)                   :: expected

        expected = "Wind/08ms.wnd"
        PriPath = ""

        InFileInfo = getInputFileData()
        CALL InflowWind_ParseInputFileInfo(InputFileData , InFileInfo, PriPath, TmpErrStat, TmpErrMsg)

        @assertEqual(TmpErrStat, 0)
        @assertEqual(InputFileData%Uniform_FileName, trim(expected))
        @assertEqual(InputFileData%Uniform_RefHt, 90)
        @assertEqual(InputFileData%Uniform_RefLength, 125.88)

    end subroutine

end module
