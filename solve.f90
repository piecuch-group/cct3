subroutine solve_cc(occ_a,occ_b,orbs,froz, actocc, actunocc, &
        shift, itol, &
        erepul, eref, ecor, ccpq_energy, &
        diis_space, restart, maxiter, &
        onebody, twobody, label)

    implicit none

    interface
        subroutine solve_ccsdt(occ_a,occ_b,orbs,froz, actocc, actunocc, &
                shift, itol, &
                erepul, eref, ecor, t2_size, t2,  &
                diis_space, restart, maxiter, &
                onebody, twobody, label)

            integer, intent(in) :: froz, occ_a,occ_b,orbs
            integer, intent(in) :: actocc, actunocc

            real(kind=8), intent(in) :: shift
            integer, intent(in) :: itol

            real(kind=8), intent(inout) :: erepul, eref, ecor

            integer, intent(inout) :: t2_size
            real(kind=8), allocatable, intent(inout) :: t2(:)

            integer, intent(in) :: diis_space, maxiter
            logical, intent(in) :: restart
            character(len=*), intent(in) :: onebody, twobody, label

        end subroutine solve_ccsdt
    end interface


    ! Molecular system
    integer, intent(in) :: froz, occ_a,occ_b,orbs
    integer, intent(in) :: actocc, actunocc

    real(kind=8), intent(inout) :: eref
    real(kind=8), intent(inout) :: erepul

    character(len=*), intent(in) :: label
    character(len=*), intent(in) :: onebody, twobody


    ! CC variables
    integer :: t2_size
    real(kind=8) :: ecor
    real(kind=8), intent(inout) :: ccpq_energy(4)

    real(kind=8), allocatable :: t2(:)

    ! Calculation parameters
    integer, intent(in) :: maxiter
    integer, intent(in) :: itol
    integer, intent(in) :: diis_space
    real(kind=8), intent(in) :: shift
    logical, intent(in) :: restart

    ! File management
    ! [TODO] make a module with all units
    integer, parameter :: hbar_unit = 327


    ! Solve CCSDt
    call solve_ccsdt(occ_a,occ_b,orbs,froz, actocc, actunocc, &
        shift, itol, &
        erepul, eref, ecor, t2_size, t2, &
        diis_space, restart, maxiter, &
        onebody, twobody, label)

    ! Start HBar
    open(hbar_unit, file='hbar.bin', form='unformatted')
    call hbar(froz, occ_a, occ_b, orbs, t2_size, t2, &
        hbar_unit, onebody, twobody)

    ! Start L-CCSD
    call solve_lcc(froz, occ_a, occ_b, orbs, &
        hbar_unit, onebody, twobody, t2_size, t2, &
        ecor, itol, maxiter, diis_space)

    ! Start moments correction
    call correct_cc(froz, occ_a, occ_b, orbs, actocc, actunocc,  &
        t2_size, t2, &
        hbar_unit, onebody, twobody, ccpq_energy)

    close(hbar_unit)
    deallocate(t2)

end subroutine solve_cc
