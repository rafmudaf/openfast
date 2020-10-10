module test_NWTC_GetWords

    use pFUnit_mod
    use NWTC_IO

    implicit none


contains

    ! PASSING CASES

    @test
    subroutine test_whitespace_seperator()

        character(64)           :: case
        character(64)           :: Words   (4)

        case = "word otherword wordwithnumber1 wordwithsymbol&"
        CALL GetWords(case, Words, 4)

        @assertEqual(size(Words), 4)
        @assertEqual(Words(1), "word")
        @assertEqual(Words(2), "otherword")
        @assertEqual(Words(3), "wordwithnumber1")
        @assertEqual(Words(4), "wordwithsymbol&")

    end subroutine

    @test
    subroutine test_comma_seperator()

        character(64)           :: case
        character(64)           :: Words   (4)

        case = "word,otherword,wordwithnumber1,wordwithsymbol&"
        CALL GetWords(case, Words, 4)

        @assertEqual(size(Words), 4)
        @assertEqual(Words(1), "word")
        @assertEqual(Words(2), "otherword")
        @assertEqual(Words(3), "wordwithnumber1")
        @assertEqual(Words(4), "wordwithsymbol&")

    end subroutine

    @test
    subroutine test_semicolon_seperator()

        character(64)           :: case
        character(64)           :: Words   (4)

        case = "word;otherword;wordwithnumber1;wordwithsymbol&"
        CALL GetWords(case, Words, 4)

        @assertEqual(size(Words), 4)
        @assertEqual(Words(1), "word")
        @assertEqual(Words(2), "otherword")
        @assertEqual(Words(3), "wordwithnumber1")
        @assertEqual(Words(4), "wordwithsymbol&")

    end subroutine

    @test
    subroutine test_singlequote_seperator()

        character(64)           :: case
        character(64)           :: Words   (4)

        case = "word'otherword'wordwithnumber1'wordwithsymbol&"
        CALL GetWords(case, Words, 4)

        @assertEqual(size(Words), 4)
        @assertEqual(Words(1), "word")
        @assertEqual(Words(2), "otherword")
        @assertEqual(Words(3), "wordwithnumber1")
        @assertEqual(Words(4), "wordwithsymbol&")

    end subroutine

    @test
    subroutine test_doublequote_seperator()

        character(64)           :: case
        character(64)           :: Words   (4)

        case = 'word"otherword"wordwithnumber1"wordwithsymbol&'
        CALL GetWords(case, Words, 4)

        @assertEqual(size(Words), 4)
        @assertEqual(Words(1), "word")
        @assertEqual(Words(2), "otherword")
        @assertEqual(Words(3), "wordwithnumber1")
        @assertEqual(Words(4), "wordwithsymbol&")

    end subroutine

    @test
    subroutine test_mixed_seperator()

        character(64)           :: case
        character(64)           :: Words   (5)

        case = "word otherword,wordwithnumber1;wordwithsymbol&'lastword"
        CALL GetWords(case, Words, 5)

        @assertEqual(size(Words), 5)
        @assertEqual(Words(1), "word")
        @assertEqual(Words(2), "otherword")
        @assertEqual(Words(3), "wordwithnumber1")
        @assertEqual(Words(4), "wordwithsymbol&")
        @assertEqual(Words(5), "lastword")

    end subroutine

end module
