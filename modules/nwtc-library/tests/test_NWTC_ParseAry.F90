module test_NWTC_ParseAry

    use pFUnit_mod
    use test_tools

    implicit none

contains

    @test
    subroutine test_ParseInAry()

        type(FileInfoType)              :: InFileInfo
        INTEGER(IntKi)                  :: LineNo
        INTEGER(IntKi)                  :: TmpErrStat
        CHARACTER(ErrMsgLen)            :: TmpErrMsg

        INTEGER(IntKi)                  :: Exp1 (1)
        INTEGER(IntKi)                  :: Exp2 (2)
        INTEGER(IntKi)                  :: Exp3 (2)
        INTEGER(IntKi)                  :: Exp4 (6)
        INTEGER(IntKi)                  :: Exp5 (20)

        InFileInfo = getTestInputInArrays()

        ! PASSING CASES
        LineNo = 1
        CALL ParseInAry(InFileInfo, LineNo, "WindVziList", Exp1, 1, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(90, Exp1(1))

        LineNo = 2
        CALL ParseInAry(InFileInfo, LineNo, "WindVziList", Exp2, 2, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(80, Exp2(1))
        @assertEqual(90, Exp2(2))

        LineNo = 3
        CALL ParseInAry(InFileInfo, LineNo, "WindVziList", Exp3, 2, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(80, Exp3(1))
        @assertEqual(90, Exp3(2))

        LineNo = 4
        CALL ParseInAry(InFileInfo, LineNo, "WindVziList", Exp4, 6, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(1, Exp4(1))
        @assertEqual(6, Exp4(6))

        LineNo = 5
        CALL ParseInAry(InFileInfo, LineNo, "WindVziList", Exp5, 20, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(1, Exp5(1))
        @assertEqual(20, Exp5(20))

    end subroutine

    @test
    subroutine test_ParseLoAry()

        type(FileInfoType)              :: InFileInfo
        INTEGER(IntKi)                  :: LineNo
        INTEGER(IntKi)                  :: TmpErrStat
        CHARACTER(ErrMsgLen)            :: TmpErrMsg

        LOGICAL                         :: Exp1 (1)
        LOGICAL                         :: Exp2 (2)
        LOGICAL                         :: Exp3 (2)

        InFileInfo = getTestInputLoArrays()

        ! PASSING CASES
        LineNo = 1
        CALL ParseLoAry(InFileInfo, LineNo, "WindVziList", Exp1, 1, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(.TRUE., Exp1(1))

        LineNo = 2
        CALL ParseLoAry(InFileInfo, LineNo, "WindVziList", Exp2, 2, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(.TRUE., Exp2(1))
        @assertEqual(.FALSE., Exp2(2))

        LineNo = 3
        CALL ParseLoAry(InFileInfo, LineNo, "WindVziList", Exp3, 2, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(.TRUE., Exp3(1))
        @assertEqual(.FALSE., Exp3(2))

    end subroutine

end module
