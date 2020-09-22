module driver

    implicit none

contains

    subroutine run_cc(&
            occ_a, &
            occ_b, &
            orbs, &
            froz, &
            actocc, &
            actunocc, &
            shift, &
            itol, &
            erepul, &
            eref, &
            ecor, &
            ccpq_energy, &
            diis_space, &
            restart, &
            maxiter, &
            calc_type, &
            onebody, &
            twobody, &
            label)

        use const, only: p
        use solver, only: solve_ccsd, solve_ccsdt, solve_lccsd
        use hbar, only: compute_hbar

        implicit none

        ! Molecular system
        integer, intent(in) :: froz, occ_a, occ_b,orbs
        integer, intent(in) :: actocc, actunocc

        real(p), intent(in out) :: eref
        real(p), intent(in) :: erepul


        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: onebody, twobody

        ! CC variables
        integer :: t2_size
        real(p) :: ecor
        real(p), intent(inout) :: ccpq_energy(4)

        real(p), allocatable :: t2(:)

        ! Calculation parameters
        integer, intent(in) :: calc_type
        integer, intent(in) :: maxiter
        integer, intent(in) :: itol
        integer, intent(in) :: diis_space
        real(p), intent(in) :: shift
        logical, intent(in) :: restart

        ! File management
        ! [TODO] make a module with all units
        integer, parameter :: hbar_unit = 327

        select case(calc_type)

        case(1)
            ! Solve CCSD
            call solve_ccsd(&
                occ_a, &
                occ_b, &
                orbs,froz, &
                shift, &
                itol, &
                erepul, &
                eref, &
                ecor, &
                t2_size, &
                t2, &
                diis_space, &
                restart, &
                maxiter, &
                onebody, &
                twobody, &
                label)

        case(2)
            ! Solve CCSD
            call solve_ccsd(&
                occ_a, &
                occ_b, &
                orbs,froz, &
                shift, &
                itol, &
                erepul, &
                eref, &
                ecor, &
                t2_size, &
                t2, &
                diis_space, &
                restart, &
                maxiter, &
                onebody, &
                twobody, &
                label)

            ! Start HBar
            open(hbar_unit, file='hbar.bin', form='unformatted')
            call compute_hbar(&
                froz, &
                occ_a, &
                occ_b, &
                orbs, &
                t2_size, &
                t2, &
                hbar_unit, &
                onebody, &
                twobody)

            ! Start L-CCSD
            call solve_lccsd(&
                froz, &
                occ_a, &
                occ_b, &
                orbs, &
                hbar_unit, &
                onebody, &
                twobody, &
                t2_size, &
                t2, &
                ecor, &
                itol, &
                maxiter, &
                diis_space)

            ! Start moments correction
            call correct_cc(&
                froz, &
                occ_a, &
                occ_b, &
                orbs, &
                actocc, &
                actunocc, &
                t2_size, &
                t2, &
                hbar_unit, &
                onebody, &
                twobody, &
                ccpq_energy)

            close(hbar_unit,status='delete')
            deallocate(t2)

        case(3)

            ! Solve CCSDt
            call solve_ccsdt(&
                occ_a, &
                occ_b, &
                orbs, &
                froz, &
                actocc, &
                actunocc, &
                shift, &
                itol, &
                erepul, &
                eref, &
                ecor, &
                t2_size, &
                t2, &
                diis_space, &
                restart, &
                maxiter, &
                onebody, &
                twobody, &
                label)

        case(4)

            ! Solve CCSDt
            call solve_ccsdt(&
                occ_a, &
                occ_b, &
                orbs, &
                froz, &
                actocc, &
                actunocc, &
                shift, &
                itol, &
                erepul, &
                eref, &
                ecor, &
                t2_size, &
                t2, &
                diis_space, &
                restart, &
                maxiter, &
                onebody, &
                twobody, &
                label)

            ! Start HBar
            open(hbar_unit, file='hbar.bin', form='unformatted')
            call compute_hbar(&
                froz, &
                occ_a, &
                occ_b, &
                orbs, &
                t2_size, &
                t2, &
                hbar_unit, &
                onebody, &
                twobody)

            ! Start L-CCSD
            call solve_lccsd(&
                froz, &
                occ_a, &
                occ_b, &
                orbs, &
                hbar_unit, &
                onebody, &
                twobody, &
                t2_size, &
                t2, &
                ecor, &
                itol, &
                maxiter, &
                diis_space)

            ! Start moments correction
            call correct_cc(&
                froz, &
                occ_a, &
                occ_b, &
                orbs, &
                actocc, &
                actunocc, &
                t2_size, &
                t2, &
                hbar_unit, &
                onebody, &
                twobody, &
                ccpq_energy)

            close(hbar_unit,status='delete')
            deallocate(t2)

        end select
        ! clean up
        close(250,status='delete') ! PA
        close(251,status='delete') ! PB
        close(252,status='delete') ! PC

    end subroutine run_cc

end module driver
