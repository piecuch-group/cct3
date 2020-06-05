module solver

    use const, only: p

    implicit none

contains

    subroutine solve_ccsd(occ_a,occ_b,orbs,froz, &
            shift, itol, &
            erepul, eref, ecor, t2_size, t2,  &
            diis_space, restart, maxiter, &
            onebody, twobody, label)

        ! Wrapper between PSI4 and the CC(t;3) program. It handles molecular data,
        ! including one- and two-body integrals.

        ! [TODO] these section should shrink by defining proper derived types instead

        ! In:
        !    occ_a: number of occupied alpha orbitals in the molecular system.
        !    occ_b: number of occupied beta orbitals in the molecular system.
        !    actocc: number of active occupied orbitals, used specified.
        !    actunocc: number of active unoccupied orbitals, user specified.
        !    shift: shift energy used to help Jacobi convergence.
        !    itol: energy convergence criterium. If the energy falls below 10^-itol,
        !       the calculation converges.
        !    erepul: core energy, which consists of the electron-nucleus repulsion.
        !    eref: reference energy from SCF calculations.
        !    diis_space: size of the DIIS steps used to accelerate the convergence of
        !       the CC Jacobi iterations.
        !    restart: whether the calculation should be restarted from a previous
        !       checkpoint.
        !    maxiter: maximum number of Jacobi iterations permitted before aborting
        !       the calculation.
        !    onebody: one-body moleculars integrals coming from SCF calculations in PSI4.
        !    twobody: two-body moleculars integrals coming from SCF calculations in PSI4.
        !    label: calculation label to be printed on the output, for identification
        !       purposes.

        ! In/Out:
        !    ecor: correlation energy from CC(t;3) calculations.
        !    t2_size: size of the T_1 + T_2 vector.
        !    t2: the T_1 + T_2 vector.

        use integrals, only: load_integrals
        use diis
        use utils

        implicit none

        ! Molecular system
        integer, intent(in) :: froz, occ_a,occ_b,orbs

        real(p), intent(inout) :: eref
        real(p), intent(in) :: erepul

        character(len=*), intent(in) :: onebody, twobody

        ! CC variables
        real(p), allocatable, intent(inout) :: t2(:)
        integer, intent(inout) :: t2_size

        real(p), intent(inout) :: ecor
        real(p), intent(in) :: shift
        real(p) :: ecor_new, energy_diff

        real(p) :: e1a,e1b,e2a,e2b,e2c,e1a1a,e1b1b,e1a1b
        real(p) :: res

        integer, allocatable :: t_pos(:)
        integer :: t_order

        real(p),allocatable::t(:)
        integer :: t_size

        real(p) :: conv(3) = 1
        ! Calculation parameters
        integer, intent(in) :: maxiter
        integer :: iter
        integer, intent(in) :: itol
        integer, intent(in) :: diis_space
        logical, intent(in) :: restart

        character(len=*), intent(in) :: label

        ! File management
        integer :: ios
        integer, parameter :: t_trunc = 3, hbar_unit = 327
        integer, parameter :: t_unit = 326
        integer, parameter :: t_vecs_unit = 180

        ! Timing
        real(p) :: prev_time

        ! Integral sorting
        real(p), allocatable :: FAHH(:,:)
        real(p), allocatable :: FAHP(:,:)
        real(p), allocatable :: FAPP(:,:)
        real(p), allocatable :: FBHH(:,:)
        real(p), allocatable :: FBHP(:,:)
        real(p), allocatable :: FBPP(:,:)
        real(p), allocatable :: VAHHHH(:,:,:,:)
        real(p), allocatable :: VAHHHP(:,:,:,:)
        real(p), allocatable :: VAHHPP(:,:,:,:)
        real(p), allocatable :: VAHPHP(:,:,:,:)
        real(p), allocatable :: VAHPPP(:,:,:,:)
        real(p), allocatable :: VBHHHH(:,:,:,:)
        real(p), allocatable :: VBHHHP(:,:,:,:)
        real(p), allocatable :: VBHHPH(:,:,:,:)
        real(p), allocatable :: VBHHPP(:,:,:,:)
        real(p), allocatable :: VBHPHP(:,:,:,:)
        real(p), allocatable :: VBHPPH(:,:,:,:)
        real(p), allocatable :: VBPHPH(:,:,:,:)
        real(p), allocatable :: VBHPPP(:,:,:,:)
        real(p), allocatable :: VBPHPP(:,:,:,:)
        real(p), allocatable :: VCHHHH(:,:,:,:)
        real(p), allocatable :: VCHHHP(:,:,:,:)
        real(p), allocatable :: VCHHPP(:,:,:,:)
        real(p), allocatable :: VCHPHP(:,:,:,:)
        real(p), allocatable :: VCHPPP(:,:,:,:)

        real(p), allocatable :: VAAPPP(:,:,:,:)
        real(p), allocatable :: VBAPPP(:,:,:,:)
        real(p), allocatable :: VBPAPP(:,:,:,:)
        real(p), allocatable :: VCAPPP(:,:,:,:)


        ! Compute the size of the spin-integrated T_3 vectors
        call count_t_spaces(froz, occ_a, occ_b, orbs, 0, 0, &
            t_trunc, t_order, t_pos, t_size)

        ! Sort integrals by spin and hole/particle type
        call load_integrals(froz, occ_a, occ_b, orbs, 1, 1,&
            eref, &
            FAHH,FAHP,FAPP,FBHH,FBHP,FBPP, &
            VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP, &
            VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH, &
            VBPHPH,VBHPPP,VBPHPP, &
            VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP, &
            VAAPPP, VBAPPP, VBPAPP, VCAPPP, &
            onebody, twobody)

        ! Print calculations parameters to the output file
        call print_calc_params(froz, occ_a, occ_b, orbs, 1, 1, &
            shift, itol, &
            eref, erepul, &
            diis_space, restart, maxiter, &
            label)

        ! CCSD tsizse
        t_size = t_pos(6)-1

        ! Open temporary files to store T vector amplitudes
        open(t_unit,file='t_vec.bin',form='unformatted')
        open(t_vecs_unit, file='t_vecs.bin', form='unformatted', recl=t_size*8, access='direct')

        allocate(t(t_size))
        t = 0.0_p

        call print_io('')
        call print_date('  Starting CCSD calculation on')
        call print_io('')

        rewind(t_unit)

        ! Whether restarting from a previous checkpoint or not
        if (restart) read(t_unit, iostat=ios) t

        ! Initialize variables
        e1a = 0.0_p
        e1b = 0.0_p
        e2a = 0.0_p
        e2b = 0.0_p
        e2c = 0.0_p
        e1a1a = 0.0_p
        e1b1b = 0.0_p
        e1a1b = 0.0_p
        ecor = 0.0_p
        ecor_new = 0.0_p

        ! Output formatting
        call print_iter_head()

        ! Start main CC loop
        !call cpu_time(prev_time)
        prev_time = get_wall_time()
        do iter = 1, maxiter

            ! Update CC amplitudes using the Jacobi method
            call update_ccsd(froz, occ_a, occ_b, orbs, &
                t_order, t_pos, t_size, t, shift, &
                FAHH,FAHP,FAPP,FBHH,FBHP,FBPP, &
                VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP, &
                VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH, &
                VBPHPH,VBHPPP,VBPHPP, &
                VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP)

            ! Calculate the correlation energy
            call calculate_energy(occ_a,occ_b,orbs,froz, &
                FAHP,FBHP,VAHHPP,VBHHPP,VCHHPP, &
                t_order, t_pos, t(1:t_pos(6) - 1), &
                E1A,E1B,E2A,E2B,E2C,E1A1A,E1A1B,E1B1B)

            ecor_new = e1a+e1b+e2a+e2b+e2c+e1a1a+e1a1b+e1b1b
            energy_diff = ecor-ecor_new

            ! Abort calculation if the cluster amplitudes are diverging
            if (isnan(energy_diff)) call abort_cc('Calculation diverged')

            ! Convergence information. Currently using a moving average. This can change
            conv(1) = conv(2)
            conv(2) = conv(3)
            conv(3) = energy_diff

            ecor = ecor_new

            ! Write T vector on a file
            rewind(t_unit)
            write(t_unit) t

            ! Write T vecs
            call write_t_vecs(t_vecs_unit, iter, diis_space, t)

            ! Calculate residuum
            res = residuum(iter, diis_space, t_size, t_vecs_unit)

            ! Do DIIS
            if (mod(iter, diis_space + 1) == 0) then
                call print_io('      DIIS cycle')
                call calc_diis(t_vecs_unit, diis_space, t_size, t)
            endif


            ! Print iterations information
            call print_iteration(iter, ecor_new, energy_diff, res, prev_time)
            prev_time = get_wall_time()
            !call cpu_time(prev_time)

            ! Check for convergence
            if (dabs(conv(1)) < 1.0d1**(-itol) .and. dabs(conv(2)) <1.0d1**(-itol) .and. dabs(conv(3)) < 1.0d1**(-itol)) exit

            ! Abort if out of iterations
            if (iter == maxiter) call abort_cc('Calculation failed to converge.')

        enddo


        close(t_vecs_unit, status='delete')
        close(t_unit)

        call print_io('')
        call print_date('  CCSD calculation finished on:')

        ! Pass T_1 + T_2 for left and correction
        t2_size = t_pos(6) - 1

        allocate(t2(t2_size))

        t2 = 0.0d0
        t2(1:t2_size) = t(1:t2_size)

        !! DEBUG stuff
        !open(886,file='f2_new.moe',form='unformatted')
        !write(886) maxiter, ecor, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
        !    0.0d0, 0.0d0, 0.0d0, t2_size
        !allocate(t2_tmp(t2_size))
        !t2_tmp(1:t_pos(3)-1) = t(1:t_pos(3)-1)
        !t2_tmp(t_pos(3):t_pos(4)-1) = -t(t_pos(3):t_pos(4)-1)
        !t2_tmp(t_pos(4):t_pos(5)-1) = -t(t_pos(5):t_pos(6)-1)
        !t2_tmp(t_pos(5):t_pos(6)-1) = -t(t_pos(4):t_pos(5)-1)
        !write(886) t2_tmp
        !close(886)

        !call calculate_energy(occ_a,occ_b,orbs,froz, &
        !    FAHP,FBHP,VAHHPP,VBHHPP,VCHHPP, &
        !    t_order, t_pos, t(1:t_pos(6) - 1), &
        !    E1A,E1B,E2A,E2B,E2C,E1A1A,E1A1B,E1B1B)

        !write(io, *) 'E1A=', E1A
        !call print_io(io)
        !write(io, *) 'E1B=', E1B
        !call print_io(io)
        !write(io, *) 'E2A=', E2A
        !call print_io(io)
        !write(io, *) 'E2B=', E2B
        !call print_io(io)
        !write(io, *) 'E2C=', E2C
        !call print_io(io)
        !write(io, *) 'E1A1A=', E1A1A
        !call print_io(io)
        !write(io, *) 'E1A1B=', E1A1B
        !call print_io(io)
        !write(io, *) 'E1B1B=', E1B1B
        !call print_io(io)

        !deallocate(t2_tmp)


        deallocate(t)

    end subroutine solve_ccsd

    subroutine solve_ccsdt(occ_a,occ_b,orbs,froz, actocc, actunocc, &
            shift, itol, &
            erepul, eref, ecor, t2_size, t2,  &
            diis_space, restart, maxiter, &
            onebody, twobody, label)

        ! Wrapper between PSI4 and the CC(t;3) program. It handles molecular data,
        ! including one- and two-body integrals.

        ! [TODO] these section should shrink by defining proper derived types instead

        ! In:
        !    occ_a: number of occupied alpha orbitals in the molecular system.
        !    occ_b: number of occupied beta orbitals in the molecular system.
        !    actocc: number of active occupied orbitals, used specified.
        !    actunocc: number of active unoccupied orbitals, user specified.
        !    shift: shift energy used to help Jacobi convergence.
        !    itol: energy convergence criterium. If the energy falls below 10^-itol,
        !       the calculation converges.
        !    erepul: core energy, which consists of the electron-nucleus repulsion.
        !    eref: reference energy from SCF calculations.
        !    diis_space: size of the DIIS steps used to accelerate the convergence of
        !       the CC Jacobi iterations.
        !    restart: whether the calculation should be restarted from a previous
        !       checkpoint.
        !    maxiter: maximum number of Jacobi iterations permitted before aborting
        !       the calculation.
        !    onebody: one-body moleculars integrals coming from SCF calculations in PSI4.
        !    twobody: two-body moleculars integrals coming from SCF calculations in PSI4.
        !    label: calculation label to be printed on the output, for identification
        !       purposes.

        ! In/Out:
        !    ecor: correlation energy from CC(t;3) calculations.
        !    t2_size: size of the T_1 + T_2 vector.
        !    t2: the T_1 + T_2 vector.

        use integrals, only: load_integrals
        use diis
        use utils

        implicit none

        ! Molecular system
        integer, intent(in) :: froz, occ_a,occ_b,orbs
        integer, intent(in) :: actocc, actunocc

        real(p), intent(inout) :: eref
        real(p), intent(in) :: erepul

        character(len=*), intent(in) :: onebody, twobody

        ! CC variables
        real(p), allocatable, intent(inout) :: t2(:)
        integer, intent(inout) :: t2_size

        real(p), intent(inout) :: ecor
        real(p), intent(in) :: shift
        real(p) :: ecor_new, energy_diff

        real(p) :: e1a,e1b,e2a,e2b,e2c,e1a1a,e1b1b,e1a1b
        real(p) :: res

        integer, allocatable :: t_pos(:)
        integer :: t_order

        real(p),allocatable::t(:)
        integer :: t_size

        real(p) :: conv(3) = 1
        ! Calculation parameters
        integer, intent(in) :: maxiter
        integer :: iter
        integer, intent(in) :: itol
        integer, intent(in) :: diis_space
        logical, intent(in) :: restart

        character(len=*), intent(in) :: label

        ! File management
        integer :: ios
        integer, parameter :: t_trunc = 3, hbar_unit = 327
        integer, parameter :: t_unit = 326
        integer, parameter :: t_vecs_unit = 180

        ! Timing
        real(p) :: prev_time


        ! Integral sorting
        real(p), allocatable :: FAHH(:,:)
        real(p), allocatable :: FAHP(:,:)
        real(p), allocatable :: FAPP(:,:)
        real(p), allocatable :: FBHH(:,:)
        real(p), allocatable :: FBHP(:,:)
        real(p), allocatable :: FBPP(:,:)
        real(p), allocatable :: VAHHHH(:,:,:,:)
        real(p), allocatable :: VAHHHP(:,:,:,:)
        real(p), allocatable :: VAHHPP(:,:,:,:)
        real(p), allocatable :: VAHPHP(:,:,:,:)
        real(p), allocatable :: VAHPPP(:,:,:,:)
        real(p), allocatable :: VBHHHH(:,:,:,:)
        real(p), allocatable :: VBHHHP(:,:,:,:)
        real(p), allocatable :: VBHHPH(:,:,:,:)
        real(p), allocatable :: VBHHPP(:,:,:,:)
        real(p), allocatable :: VBHPHP(:,:,:,:)
        real(p), allocatable :: VBHPPH(:,:,:,:)
        real(p), allocatable :: VBPHPH(:,:,:,:)
        real(p), allocatable :: VBHPPP(:,:,:,:)
        real(p), allocatable :: VBPHPP(:,:,:,:)
        real(p), allocatable :: VCHHHH(:,:,:,:)
        real(p), allocatable :: VCHHHP(:,:,:,:)
        real(p), allocatable :: VCHHPP(:,:,:,:)
        real(p), allocatable :: VCHPHP(:,:,:,:)
        real(p), allocatable :: VCHPPP(:,:,:,:)

        real(p), allocatable :: VAAPPP(:,:,:,:)
        real(p), allocatable :: VBAPPP(:,:,:,:)
        real(p), allocatable :: VBPAPP(:,:,:,:)
        real(p), allocatable :: VCAPPP(:,:,:,:)


        ! Compute the size of the spin-integrated T_3 vectors
        call count_t_spaces(froz, occ_a, occ_b, orbs, actocc, actunocc, &
            t_trunc, t_order, t_pos, t_size)

        ! Sort integrals by spin and hole/particle type
        call load_integrals(froz, occ_a, occ_b, orbs, actocc, actunocc,&
            eref, &
            FAHH,FAHP,FAPP,FBHH,FBHP,FBPP, &
            VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP, &
            VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH, &
            VBPHPH,VBHPPP,VBPHPP, &
            VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP, &
            VAAPPP, VBAPPP, VBPAPP, VCAPPP, &
            onebody, twobody)

        ! Print calculations parameters to the output file
        call print_calc_params(froz, occ_a, occ_b, orbs, actocc, actunocc, &
            shift, itol, &
            eref, erepul, &
            diis_space, restart, maxiter, &
            label)

        ! Open temporary files to store T vector amplitudes
        open(t_unit,file='t_vec.bin',form='unformatted')
        open(t_vecs_unit, file='t_vecs.bin', form='unformatted', recl=t_size*8, access='direct')

        allocate(t(t_size))
        t=0.0d0

        call print_io('')
        call print_date('  Starting CCSDt calculation on')
        call print_io('')

        rewind(t_unit)

        ! Whether restarting from a previous checkpoint or not
        if (restart) read(t_unit, iostat=ios) t

        ! Initialize variables
        e1a=0.0d0
        e1b=0.0d0
        e2a=0.0d0
        e2b=0.0d0
        e2c=0.0d0
        e1a1a=0.0d0
        e1b1b=0.0d0
        e1a1b=0.0d0
        ecor=0.0d0
        ecor_new=0.0d0

        ! Output formatting
        call print_iter_head()

        ! Start main CC loop
        !call cpu_time(prev_time)
        prev_time = get_wall_time()
        do iter = 1, maxiter


            ! Update CC amplitudes using the Jacobi method
            call update_cc(froz, occ_a, occ_b, orbs, actocc, actunocc, &
                t_order, t_pos, t_size, t, shift, &
                FAHH,FAHP,FAPP,FBHH,FBHP,FBPP, &
                VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP, &
                VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH, &
                VBPHPH,VBHPPP,VBPHPP, &
                VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP, &
                VAAPPP, VBAPPP, VBPAPP, VCAPPP)

            ! Calculate the correlation energy
            call calculate_energy(occ_a,occ_b,orbs,froz, &
                FAHP,FBHP,VAHHPP,VBHHPP,VCHHPP, &
                t_order, t_pos, t(1:t_pos(6) - 1), &
                E1A,E1B,E2A,E2B,E2C,E1A1A,E1A1B,E1B1B)

            ecor_new = e1a+e1b+e2a+e2b+e2c+e1a1a+e1a1b+e1b1b
            energy_diff = ecor-ecor_new

            ! Abort calculation if the cluster amplitudes are diverging
            if (isnan(energy_diff)) call abort_cc('Calculation diverged')

            ! Convergence information. Currently using a moving average. This can change
            conv(1) = conv(2)
            conv(2) = conv(3)
            conv(3) = energy_diff

            ecor = ecor_new

            ! Write T vector on a file
            rewind(t_unit)
            write(t_unit) t

            ! Write T vecs
            call write_t_vecs(t_vecs_unit, iter, diis_space, t)

            ! Calculate residuum
            res = residuum(iter, diis_space, t_size, t_vecs_unit)

            ! Do DIIS
            if (mod(iter, diis_space + 1) == 0) then
                call print_io('      DIIS cycle')
                call calc_diis(t_vecs_unit, diis_space, t_size, t)
            endif


            ! Print iterations information
            call print_iteration(iter, ecor_new, energy_diff, res, prev_time)
            prev_time = get_wall_time()
            !call cpu_time(prev_time)

            ! Check for convergence
            if (dabs(conv(1)) < 1.0d1**(-itol) .and. dabs(conv(2)) <1.0d1**(-itol) .and. dabs(conv(3)) < 1.0d1**(-itol)) exit

            ! Abort if out of iterations
            if (iter == maxiter) call abort_cc('Calculation failed to converge.')

        enddo


        close(t_vecs_unit, status='delete')
        close(t_unit)

        call print_io('')
        call print_date('  CCSDt calculation finished on:')

        ! Pass T_1 + T_2 for left and correction
        t2_size = t_pos(6) - 1

        allocate(t2(t2_size))

        t2 = t(1:t2_size)

        deallocate(t)

    end subroutine solve_ccsdt

    subroutine solve_lccsd(n0, n1, n2, n3, &
            ihbar, onebody, twobody, t_size, t0, &
            ecor_cc, itol, maxiter, diis_space)

        use integrals, only: load_old_integrals
        use utils, only: get_wall_time
        use diis

        implicit none

        integer, intent(in) :: n0, n1, n2 ,n3
        integer, intent(in) :: maxiter, itol
        integer, intent(in) :: ihbar, t_size
        integer, intent(in) :: diis_space
        real(p), intent(in) :: ecor_cc
        character(len=*), intent(in) :: onebody, twobody
        real(p), intent(in out) :: t0(t_size)

        integer, parameter :: l_unit = 482, l_vecs_unit = 433
        integer :: k1, k2, k3, k4,  kkk
        integer :: k1a, k1b, k2a, k2b, k2c, l2b, l2c
        integer :: iter

        real(p) :: prev_time
        real(p) :: ecor, ecor_new, energy_diff
        real(p), allocatable :: FockR(:,:)
        real(p), allocatable :: FockB(:,:)
        real(p), allocatable :: IntR(:,:,:,:)
        real(p), allocatable :: IntB(:,:,:,:)
        real(p), allocatable :: IntM(:,:,:,:)
        real(p), allocatable :: t(:), l_vec(:)

        real(p), allocatable :: t_tmp(:)

        real(p),allocatable::H1A(:,:)
        real(p),allocatable::H1B(:,:)
        real(p),allocatable::H2A(:,:,:,:)
        real(p),allocatable::H2B(:,:,:,:)
        real(p),allocatable::H2C(:,:,:,:)

        real(p) :: res, shift

        shift = 0.0d0

        call load_old_integrals(n0, n1, n2, n3, &
            FockR, FockB, IntR, IntM, IntB, &
            onebody, twobody)
        ! [DEBUG]
        !allocate(FockR(N3,N3),FockB(N3,N3))
        !allocate(IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3))
        !allocate(IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3))
        !allocate(IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3))
        !open(855, file='f2.inf', form='unformatted')
        !read(855)
        !read(855) ((FockR(i,j),i=1,N3),j=1,N3)
        !read(855) ((FockB(i,j),i=1,N3),j=1,N3)
        !read(855) ((((IntR(i,j,k,l),i=N0+1,N3),j=N0+1,N3),k=N0+1,N3),l=N0+1,N3)
        !read(855) ((((IntM(i,j,k,l),i=N0+1,N3),j=N0+1,N3),k=N0+1,N3),l=N0+1,N3)
        !read(855) ((((IntB(i,j,k,l),i=N0+1,N3),j=N0+1,N3),k=N0+1,N3),l=N0+1,N3)
        !close(855)

        !close(ihbar)
        !open(iHBar, file='f2.HBar', form='unformatted')
        allocate(H1A(N0+1:N3,N0+1:N3),H1B(N0+1:N3,N0+1:N3))
        allocate(H2A(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3))
        allocate(H2B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3))
        allocate(H2C(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3))
        H1A=0.0d0
        H1B=0.0d0
        H2A=0.0d0
        H2B=0.0d0
        H2C=0.0d0
        rewind(iHBar)
        !read(iHBar) ((H1A(i,j),i=N0+1,N3),j=N0+1,N3)
        !read(iHBar) ((H1B(i,j),i=N0+1,N3),j=N0+1,N3)
        !read(iHBar) ((((H2A(i,j,k,l),i=N0+1,N3),j=N0+1,N3),k=N0+1,N3),l=N0+1,N3)
        !read(iHBar) ((((H2B(i,j,k,l),i=N0+1,N3),j=N0+1,N3),k=N0+1,N3),l=N0+1,N3)
        !read(iHBar) ((((H2C(i,j,k,l),i=N0+1,N3),j=N0+1,N3),k=N0+1,N3),l=N0+1,N3)
        read(iHBar) H1A(N0+1:N3, N0+1:N3)
        read(iHBar) H1B(N0+1:N3, N0+1:N3)
        read(iHBar) H2A(N0+1:N3, N0+1:N3, N0+1:N3, N0+1:N3)
        read(iHBar) H2B(N0+1:N3, N0+1:N3, N0+1:N3, N0+1:N3)
        read(iHBar) H2C(N0+1:N3, N0+1:N3, N0+1:N3, N0+1:N3)

        H2A(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1) = IntR(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1)
        H2B(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1) = IntM(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1)
        H2C(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2) = IntB(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2)

        !do i=N0+1,N1
        !    do j=N0+1,N1
        !        do a=N1+1,N3
        !            do b=N1+1,N3
        !                H2A(b,a,j,i)=IntR(b,a,j,i)
        !            enddo
        !        enddo
        !    enddo
        !enddo
        !do i=N0+1,N1
        !    do j=N0+1,N2
        !        do a=N1+1,N3
        !            do b=N2+1,N3
        !                H2B(b,a,j,i)=IntM(b,a,j,i)
        !            enddo
        !        enddo
        !    enddo
        !enddo
        !do i=N0+1,N2
        !    do j=N0+1,N2
        !        do a=N2+1,N3
        !            do b=N2+1,N3
        !                H2C(b,a,j,i)=IntB(b,a,j,i)
        !            enddo
        !        enddo
        !    enddo
        !enddo

        K1=N1-N0
        K3=N3-N1 !OCC
        K2=N2-N0
        K4=N3-N2 !VIR
        KKK=0
        K1A =KKK+1
        KKK=KKK+K1*K3
        K1B =KKK+1
        KKK=KKK+K2*K4
        K2A =KKK+1
        KKK=KKK+K1*K1*K3*K3
        K2B =KKK+1
        KKK=KKK+K1*K2*K3*K4
        K2C =KKK+1
        KKK=KKK+K2*K2*K4*K4
        L2C=K2B
        L2B=L2C+K2*K2*K4*K4

        !write(dbg_out, *) kkk, t_size
        !call print_io(dbg_out)
        !call exit()

        allocate(t_tmp(kkk))
        allocate(t(KKK))
        t = 0.0d0
        t(:) = t0(:)
        !t_tmp = 0.0d0
        !t(1:k2a-1) = t0(1:k2a-1)
        !t(k2a:kkk) = -t0(k2a:kkk)
        !t = -t0(k2a:kkk)
        !open(855, file='f2.moe', form='unformatted')
        !read(855)
        !read(855) t_tmp(1:kkk)
        !close(855)

        !do i=K2A,K2B-1
        !    t(i)=-t_tmp(i)
        !enddo
        !do i=K2B,K2C-1
        !    t(i)=-t_tmp(L2B-K2B+i)
        !enddo
        !do i=K2C,KKK
        !    t(i)=-t_tmp(L2C-K2C+i)
        !enddo

        !allocate(t(KKK))
        !t = 0.0d0
        !!t(1:k2a-1) = t0(1:k2a-1)
        !!t(k2a:kkk) = -t0(k2a:kkk)
        !!t = -t0(k2a:kkk)
        !open(855, file='f2.moe', form='unformatted')
        !read(855)
        !read(855) t0(1:kkk)
        !close(855)

        !do i=K2A,K2B-1
        !    t(i)=-t0(i)
        !enddo
        !do i=K2B,K2C-1
        !    t(i)=-t0(L2B-K2B+i)
        !enddo
        !do i=K2C,KKK
        !    t(i)=-t0(L2C-K2C+i)
        !enddo

        !h1a = 0.0d0
        !h1b = 0.0d0
        !h2a = 0.0d0
        !h2b = 0.0d0
        !h2c = 0.0d0

        !t = 0.0d0


        open(l_unit, file='l_vec.bin', form='unformatted')
        open(l_vecs_unit, file='l_vecs.bin', form='unformatted', recl=kkk*8, access='direct')
        write(l_unit) t

        call print_io('')
        call print_date('  Starting L-CCSD calculation on')
        call print_io('')
        call print_iter_head()
        !call cpu_time(prev_time)
        prev_time = get_wall_time()

        do iter=1, maxiter

            call update_lcc(N0,N1,N2,N3,l_unit,ECor_cc,res,ecor_new,shift, &
                K1,K2,K3,K4,K1A,K1B,K2A,K2B,K2C,KKK, &
                FockR,FockB,IntR,IntB,IntM,H1A,H1B,H2A,H2B,H2C,t)

            energy_diff = ecor - ecor_new
            ecor = ecor_new

            call print_iteration(iter, ecor_cc + ecor, energy_diff, res, prev_time)
            !call cpu_time(prev_time)
            prev_time = get_wall_time()

            ! Write T vecs
            allocate(l_vec(kkk))
            rewind(l_unit)
            read(l_unit) l_vec
            call write_t_vecs(l_vecs_unit, iter, diis_space, l_vec)
            deallocate(l_vec)

            ! Do DIIS
            if (mod(iter, diis_space + 1) == 0) then
                call print_io('      DIIS cycle')
                allocate(l_vec(kkk))
                rewind(l_unit)
                read(l_unit) l_vec

                call calc_diis(l_vecs_unit, diis_space, kkk, l_vec)

                rewind(l_unit)
                write(l_unit) l_vec
                deallocate(l_vec)
            endif

            if (dabs(ecor) < 1.0d1**(-itol)) exit

            if (iter == maxiter) call abort_cc('Calculation failed to converge.')

        enddo

        call print_io('')
        call print_date('  L-CCSD calculation finished on:')
        call print_io('')

        close(l_vecs_unit, status='delete')
        close(l_unit)

    end subroutine solve_lccsd

end module solver
