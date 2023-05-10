subroutine cc(&
        froz, &
        socc, &
        docc, &
        orbs, &
        actocc_in, &
        actunocc_in, &
        etol, &
        maxiter, &
        keep_amps, &
        ifrhf, &
        diis_space, &
        calc_type, &
        onebody, &
        twobody, &
        erepul, &
        eref_psi4, &
        eprim, &
        esec &
    )

    ! Wrapper between PSI4 and the CC(t;3) program. It handles molecular data,
    ! including one- and two-body integrals.

    ! In:
    !    froz: number of frozen occupied orbitals in the molecular system.
    !    socc: singly occupied orbitals in the molecular system.
    !    docc: doubly occupied orbitals in the molecular system.
    !    actocc_in: number of active occupied orbitals, used specified.
    !    actunocc_in: number of active unoccupied orbitals, user specified.
    !    etol: energy convergence criterium. If the energy falls below 10^-etol,
    !       the calculation converges.
    !    maxiter: maximum number of Jacobi iterations permitted before aborting
    !       the calculation.
    !    keep_amps: whether cluster amplitudes should be kept after the calculation
    !       are over or not.
    !    ifrhf: whether the SCF computations were done using restricted Hartree--Fock
    !       (closed-shell) or not.
    !       are over or not.
    !    diis_space: size of the DIIS steps used to accelerate the convergence of
    !       the CC Jacobi iterations.
    !    erepul: core energy, which consists of the electron-nucleus repulsion.
    !    onebody: one-body moleculars integrals coming from SCF calculations in PSI4.
    !    twobody: two-body moleculars integrals coming from SCF calculations in PSI4.
    !    eref_psi4: reference energy from SCF calculations in PSI4.

    use, intrinsic :: iso_fortran_env, only: error_unit
    use :: iso_c_binding, only: c_char, c_null_char

    use const, only: p

    use integrals, only: write_integrals, write_integrals_dbg
    use driver, only: run_cc

    implicit none

    ! Molecular system from PSI4
    integer, intent(in) :: froz
    integer, intent(in) :: socc
    integer, intent(in) :: docc
    integer, intent(in) :: orbs
    integer, intent(in) :: actocc_in, actunocc_in

    ! Integrals
    real(p), intent(in) :: onebody(orbs, orbs)
    real(p), intent(in) :: twobody(orbs, orbs, orbs, orbs)

    real(p), intent(in) :: erepul, eref_psi4
    real(p), intent(out) :: eprim, esec
    real(p) :: eref
    real(p) :: ccpq_energy(4)

    integer :: occ_a, occ_b

    ! Calculation parameters
    integer, intent(in) :: calc_type
    integer, intent(in) :: etol
    integer, intent(in) :: maxiter
    logical, intent(in) :: keep_amps
    logical, intent(in) :: ifrhf
    integer, intent(in) :: diis_space


    real(p) :: shift
    character(len=100) :: label

    logical :: restart
    integer :: itol
    integer :: actocc, actunocc

    ! CC variables
    real(p) :: ecor

    call print_header()

    ! [TODO] change this
    label = ''
    restart = .false.

    ! [TODO] these are the variables needed
    occ_a = socc + docc
    occ_b = docc

    itol = etol

    shift = 0.0d0

    ! [TODO] the C arrays could be deallocated
    call write_integrals(onebody, twobody, orbs)

    ! Set active space
    actocc = max(occ_b-actocc_in, froz)
    actunocc = min(occ_a+actunocc_in, orbs)

    ! Zero active space if CCSD or CR-CC(2,3)
    if (calc_type == 1 .or. calc_type == 2) then
        actocc = occ_b
        actunocc = orbs
    endif

    ! Open binary file
    call run_cc(occ_a, occ_b, orbs, froz, actocc, actunocc, &
        shift, itol, &
        erepul, eref, ecor, ccpq_energy, &
        diis_space, restart, maxiter, calc_type, &
        'onebody.inp', 'twobody.inp', label)


    call print_summary(erepul + eref, ecor, ccpq_energy, calc_type)

    if (calc_type == 2 .or. calc_type == 4) then
        ! If the method-of-moments (MM) correction was calculated
        ! return both CCSDt/CCSD and CC(t;3)/CR-CC(2,3) correlation energies
        eprim = ecor + ccpq_energy(4)
        esec = ecor
    else
        ! If no MM, return the same energy twice
        eprim = ecor
        esec = ecor
    endif

end subroutine cc
