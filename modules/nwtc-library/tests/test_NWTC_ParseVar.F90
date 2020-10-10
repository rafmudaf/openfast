module test_NWTC_ParseVar

    use pFUnit_mod
    use test_tools

    implicit none

contains

    ! PASSING CASES

    @test
    subroutine test_ParseLoVar()

        type(FileInfoType)              :: InFileInfo
        INTEGER(IntKi)                  :: LineNo
        INTEGER(IntKi)                  :: TmpErrStat
        CHARACTER(ErrMsgLen)            :: TmpErrMsg

        LOGICAL                         :: res1
        LOGICAL                         :: res2

        InFileInfo = getTestInputLogicals()

        LineNo = 1
        CALL ParseLoVar(InFileInfo, LineNo, "Echo", res1, TmpErrStat, TmpErrMsg)
        @assertEqual(TmpErrStat, 0)
        @assertEqual(res1, .TRUE.)

        LineNo = 2
        CALL ParseLoVar(InFileInfo, LineNo, "Echo", res2, TmpErrStat, TmpErrMsg)
        @assertEqual(TmpErrStat, 0)
        @assertEqual(res2, .FALSE.)

    end subroutine

end module