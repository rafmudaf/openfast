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
        @assertEqual(4, TmpErrStat)

        LineNo = 5
        CALL ParseChVar(InFileInfo, LineNo, "UniformFileName", res5, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

    end subroutine

    @test
    subroutine test_ParseInVar()

        type(FileInfoType)              :: InFileInfo
        INTEGER(IntKi)                  :: LineNo
        INTEGER(IntKi)                  :: TmpErrStat
        CHARACTER(ErrMsgLen)            :: TmpErrMsg

        INTEGER(IntKi)                  :: res1, res2, res3, res4, res5, res6, res7

        InFileInfo = getTestInputIntegers()

        ! PASSING CASES
        LineNo = 1
        CALL ParseInVar(InFileInfo, LineNo, "Windtype", res1, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(1, res1)

        LineNo = 2
        CALL ParseInVar(InFileInfo, LineNo, "Windtype", res2, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(1000000000, res2)

        LineNo = 3
        CALL ParseInVar(InFileInfo, LineNo, "Windtype", res3, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(-1, res3)

        LineNo = 4
        CALL ParseInVar(InFileInfo, LineNo, "Windtype", res4, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(-1000000000, res4)

        ! FAILING CASES
        LineNo = 5
        CALL ParseInVar(InFileInfo, LineNo, "Windtype", res5, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

        LineNo = 6
        CALL ParseInVar(InFileInfo, LineNo, "Windtype", res6, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

        LineNo = 7
        CALL ParseInVar(InFileInfo, LineNo, "Windtype", res7, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

    end subroutine

    @test
    subroutine test_ParseSiVar()

        type(FileInfoType)              :: InFileInfo
        INTEGER(IntKi)                  :: LineNo
        INTEGER(IntKi)                  :: TmpErrStat
        CHARACTER(ErrMsgLen)            :: TmpErrMsg

        REAL(ReKi)                      :: res1, res2, res3, res4, res5

        InFileInfo = getTestInputFloats()

        ! PASSING CASES
        LineNo = 1
        CALL ParseSiVar(InFileInfo, LineNo, "HWindSpeed", res1, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(8.0, res1)

        LineNo = 2
        CALL ParseSiVar(InFileInfo, LineNo, "HWindSpeed", res2, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(-8.0, res2)

        LineNo = 3
        CALL ParseSiVar(InFileInfo, LineNo, "HWindSpeed", res3, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(8.0, res3)

        LineNo = 4
        CALL ParseSiVar(InFileInfo, LineNo, "HWindSpeed", res4, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(0.0, res4)

        LineNo = 5
        CALL ParseSiVar(InFileInfo, LineNo, "HWindSpeed", res5, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(0.0, res5)

    end subroutine

    ! @test
    ! subroutine test_ParseDbVar()

    !     type(FileInfoType)              :: InFileInfo
    !     INTEGER(IntKi)                  :: LineNo
    !     INTEGER(IntKi)                  :: TmpErrStat
    !     CHARACTER(ErrMsgLen)            :: TmpErrMsg

    !     REAL(DbKi)                      :: res1, res2, res3, res4, res5

    !     InFileInfo = getTestInputFloats()

        ! PASSING CASES
        ! LineNo = 1
        ! CALL ParseDbVar(InFileInfo, LineNo, "HWindSpeed", res1, TmpErrStat, TmpErrMsg)
        ! @assertEqual(0, TmpErrStat)
        ! @assertEqual(8.0, res1)

        ! LineNo = 2
        ! CALL ParseDbVar(InFileInfo, LineNo, "HWindSpeed", res2, TmpErrStat, TmpErrMsg)
        ! @assertEqual(0, TmpErrStat)
        ! @assertEqual(-8.0, res2)

        ! LineNo = 3
        ! CALL ParseDbVar(InFileInfo, LineNo, "HWindSpeed", res3, TmpErrStat, TmpErrMsg)
        ! @assertEqual(0, TmpErrStat)
        ! @assertEqual(8.0, res3)

        ! LineNo = 4
        ! CALL ParseDbVar(InFileInfo, LineNo, "HWindSpeed", res4, TmpErrStat, TmpErrMsg)
        ! @assertEqual(0, TmpErrStat)
        ! @assertEqual(0.0, res4)

        ! LineNo = 5
        ! CALL ParseDbVar(InFileInfo, LineNo, "HWindSpeed", res5, TmpErrStat, TmpErrMsg)
        ! @assertEqual(0, TmpErrStat)
        ! @assertEqual(0.0, res5)

    ! end subroutine

    @test
    subroutine test_interface()

        type(FileInfoType)              :: InLogicals
        type(FileInfoType)              :: InIntegers
        type(FileInfoType)              :: InReals
        type(FileInfoType)              :: InCharacters

        INTEGER(IntKi)                  :: LineNo
        INTEGER(IntKi)                  :: TmpErrStat
        CHARACTER(ErrMsgLen)            :: TmpErrMsg

        LOGICAL                         :: logical_var
        INTEGER(IntKi)                  :: integer_var
        REAL(ReKi)                      :: real_var
        CHARACTER(64)                   :: char_var


        InLogicals = getTestInputLogicals()
        InIntegers = getTestInputIntegers()
        InReals = getTestInputFloats()
        InCharacters = getTestInputCharacters()

        ! PASSING CASES
        ! These tests have compatible output variable types
        LineNo = 1
        CALL ParseVar(InLogicals, LineNo, "Echo", logical_var, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(.TRUE., logical_var)

        LineNo = 1
        CALL ParseVar(InLogicals, LineNo, "Echo", char_var, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual("true", char_var)

        LineNo = 1
        CALL ParseVar(InIntegers, LineNo, "Windtype", integer_var, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(1, integer_var)

        LineNo = 1
        CALL ParseVar(InIntegers, LineNo, "Windtype", real_var, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(1.0, real_var)

        LineNo = 1
        CALL ParseVar(InIntegers, LineNo, "Windtype", char_var, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual("1", char_var)

        LineNo = 1
        CALL ParseVar(InReals, LineNo, "HWindSpeed", real_var, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(8.0, real_var)

        LineNo = 1
        CALL ParseVar(InReals, LineNo, "HWindSpeed", char_var, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual("8.0", char_var)

        LineNo = 1
        CALL ParseVar(InCharacters, LineNo, "UniformFileName", char_var, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual("Wind/08ms.wnd", char_var)

        ! FAILING CASES
        ! These tests have incompatible output variable types
        LineNo = 1
        CALL ParseVar(InLogicals, LineNo, "Echo", real_var, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

        LineNo = 1
        CALL ParseVar(InLogicals, LineNo, "Echo", integer_var, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

        LineNo = 1
        CALL ParseVar(InCharacters, LineNo, "UniformFileName", real_var, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

        LineNo = 1
        CALL ParseVar(InCharacters, LineNo, "UniformFileName", integer_var, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

        LineNo = 1
        CALL ParseVar(InCharacters, LineNo, "UniformFileName", logical_var, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

        LineNo = 1
        CALL ParseVar(InIntegers, LineNo, "Windtype", logical_var, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

        LineNo = 1
        CALL ParseVar(InReals, LineNo, "HWindSpeed", logical_var, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

        LineNo = 1
        CALL ParseVar(InReals, LineNo, "HWindSpeed", integer_var, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

    end subroutine

end module