@test
subroutine test_BD_CheckRotMat()
    use pFUnit_mod
    use BeamDyn_Subs
    implicit none

    integer,    parameter    :: n = 1e3
    real(BDKi), parameter    :: pi1 = 4.0_BDKi * atan(1.0_BDKi)
    
    REAL(BDKi) :: inmat(3, 3), outmat(3, 3)
    ! REAL(BDKi) :: outvec(3), testvec(3)
    integer    :: i, j
    REAL(BDKi) :: normvec(3, n)
    REAL(BDKi) :: angle(n)
    REAL(BDKi) :: ct, st, omct


    INTEGER(IntKi)             :: ErrStat ! Temporary Error status
    CHARACTER(ErrMsgLen)       :: ErrMsg  ! Temporary Error message

    call init_random_seed
    call random_number(normvec)

    do i = 1, n
        normvec(:, i) = normvec(:, i)/norm2(normvec(:, i))
        angle(i) = -pi1 + (real(i, BDKi)/real(n, BDKi)) * 2.0_BDKi * pi1
    end do
    
    do i = 1, n
        do j = 1, n
            ct = cos(angle(j))
            st = sin(angle(j))
            omct = 1.0_BDKi - ct
            inmat = reshape( (/ ct + normvec(1, i)**2 * omct,                              normvec(1, i) * normvec(2, i) * omct - normvec(3, i) * st, normvec(1, i) * normvec(3, i) * omct + normvec(2, i) * st, &
                                normvec(1, i) * normvec(2, i) * omct + normvec(3, i) * st, ct + normvec(2, i)**2 * omct,                              normvec(2, i) * normvec(3, i) * omct - normvec(1, i) * st, &
                                normvec(1, i) * normvec(3, i) * omct - normvec(2, i) * st, normvec(2, i) * normvec(3, i) * omct + normvec(1, i) * st, ct + normvec(3, i)**2 * omct /), &
                                shape(inmat), order=(/2, 1/) )
            inmat = inmat + 1e-14
            call BD_CheckRotMat(inmat, outmat, ErrStat, ErrMsg)
            @assertEqual(inmat, outmat, 1e-13)
        end do
    end do

    contains

        subroutine init_random_seed()
            integer :: i, n, clock
            integer, dimension(:), allocatable :: seed

            call random_seed(size = n)
            allocate(seed(n))

            call system_clock(count = clock)

            seed = clock + 37 * (/ (i - 1, i = 1, n) /)
            call random_seed(put = seed)

            deallocate(seed)

        end subroutine

end subroutine test_BD_CheckRotMat
