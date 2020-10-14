module test_NWTC_ChkParseData

    use pFUnit_mod
    use NWTC_IO

    implicit none

contains

    ! PASSING CASES
    @test
    subroutine test_nameindx_passing()

        character(64)                   :: Words (3)
        character(64)                   :: FileName

        INTEGER(IntKi)                  :: LineNo
        INTEGER(IntKi)                  :: NameIndx
        INTEGER(IntKi)                  :: TmpErrStat
        CHARACTER(ErrMsgLen)            :: TmpErrMsg

        FileName = "NullFile"
        LineNo = 1

        ! Variable name found in index location 1
        Words(1) = "Target"
        Words(2) = "Other"
        Words(3) = "Other"
        CALL ChkParseData(Words, "Target", FileName, LineNo, NameIndx, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(1, NameIndx)

        ! Variable name found in index location 2
        Words(1) = "Other"
        Words(2) = "Target"
        Words(3) = "Other"
        CALL ChkParseData(Words, "Target", FileName, LineNo, NameIndx, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(2, NameIndx)

        ! Variable name is not case sensitive
        Words(1) = "Other"
        Words(2) = "target"
        Words(3) = "Other"
        CALL ChkParseData(Words, "Target", FileName, LineNo, NameIndx, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(2, NameIndx)

        ! Variable name is same as variable
        Words(1) = "Target"
        Words(2) = "Target"
        Words(3) = "Other"
        CALL ChkParseData(Words, "Target", FileName, LineNo, NameIndx, TmpErrStat, TmpErrMsg)
        @assertEqual(0, TmpErrStat)
        @assertEqual(1, NameIndx)

    end subroutine

    ! FAILING CASES
    @test
    subroutine test_nameindx_failing()

        character(64)                   :: Words (3)
        character(64)                   :: FileName

        INTEGER(IntKi)                  :: LineNo
        INTEGER(IntKi)                  :: NameIndx
        INTEGER(IntKi)                  :: TmpErrStat
        CHARACTER(ErrMsgLen)            :: TmpErrMsg

        FileName = "NullFile"
        LineNo = 1

        ! Variable name found in index location 3+
        Words(1) = "Other"
        Words(2) = "Other"
        Words(3) = "Target"
        CALL ChkParseData(Words, "Target", FileName, LineNo, NameIndx, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

        ! Variable name not found
        Words(1) = "Other"
        Words(2) = "Other"
        Words(3) = "Other"
        CALL ChkParseData(Words, "Target", FileName, LineNo, NameIndx, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

        ! Partial match is not sufficient
        Words(1) = "Target Variable Name"
        Words(2) = "Other"
        Words(3) = "Other"
        CALL ChkParseData(Words, "Target", FileName, LineNo, NameIndx, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

        ! Not enough words
        Words(1) = "Target"
        Words(2) = ""
        Words(3) = ""
        CALL ChkParseData(Words, "Target", FileName, LineNo, NameIndx, TmpErrStat, TmpErrMsg)
        @assertEqual(4, TmpErrStat)

    end subroutine

end module
