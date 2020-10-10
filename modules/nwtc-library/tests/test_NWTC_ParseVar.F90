module test_NWTC_ParseVar

    use pFUnit_mod
    use test_tools

    implicit none

contains

    @test
    subroutine test_ParseLoVar()

        type(FileInfoType)              :: InFileInfo
        INTEGER(IntKi)                  :: LineNo
        INTEGER(IntKi)                  :: TmpErrStat
        CHARACTER(ErrMsgLen)            :: TmpErrMsg

        LOGICAL                         :: res1, res2

        InFileInfo = getTestInputLogicals()

        ! PASSING CASES
        LineNo = 1
        CALL ParseLoVar(InFileInfo, LineNo, "Echo", res1, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(.TRUE., res1)

        LineNo = 2
        CALL ParseLoVar(InFileInfo, LineNo, "Echo", res2, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(.FALSE., res2)

    end subroutine

    @test
    subroutine test_ParseChVar()

        type(FileInfoType)              :: InFileInfo
        INTEGER(IntKi)                  :: LineNo
        INTEGER(IntKi)                  :: TmpErrStat
        CHARACTER(ErrMsgLen)            :: TmpErrMsg

        CHARACTER(32)                   :: res1, res2, res3, res4, res5

        InFileInfo = getTestInputCharacters()

        ! PASSING CASES
        LineNo = 1
        CALL ParseChVar(InFileInfo, LineNo, "UniformFileName", res1, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual("Wind/08ms.wnd", res1)

        LineNo = 2
        CALL ParseChVar(InFileInfo, LineNo, "UniformFileName", res2, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual("Wind/08ms.wnd", res2)

        ! FAILING CASES
        LineNo = 3
        CALL ParseChVar(InFileInfo, LineNo, "UniformFileName", res3, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

        LineNo = 4
        CALL ParseChVar(InFileInfo, LineNo, "UniformFileName", res4, TmpErrStat, TmpErrMsg)
        print *, TmpErrMsg
        @assertEqual(4, TmpErrStat)

        LineNo = 5
        CALL ParseChVar(InFileInfo, LineNo, "UniformFileName", res5, TmpErrStat, TmpErrMsg)
        print *, TmpErrMsg
        @assertEqual(4, TmpErrStat)

    end subroutine

end module