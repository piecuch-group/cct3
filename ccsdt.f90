subroutine solve_ccsdt(occ_a,occ_b,orbs,froz, actocc, actunocc, &
        shift, itol, &
        erepul, eref, ecor, t2_size, t2,  &
        diis_space, restart, maxiter, &
        onebody, twobody, label)

    use integrals
    use diis
    use utils

    implicit none

    ! Molecular system
    integer, intent(in) :: froz, occ_a,occ_b,orbs
    integer, intent(in) :: actocc, actunocc

    real(kind=8), intent(inout) :: eref
    real(kind=8), intent(inout) :: erepul

    character(len=*), intent(in) :: onebody, twobody

    ! CC variables
    real(kind=8), allocatable, intent(inout) :: t2(:)
    integer, intent(inout) :: t2_size

    real(kind=8), intent(inout) :: ecor
    real(kind=8), intent(in) :: shift
    real(kind=8) :: ecor_new, energy_diff

    real(kind=8) :: e1a,e1b,e2a,e2b,e2c,e1a1a,e1b1b,e1a1b
    real(kind=8) :: res

    integer, allocatable :: t_pos(:)
    integer :: t_order

    real(kind=8),allocatable::t(:)
    integer :: t_size

    real(kind=8) :: conv(3) = 1
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
    real(kind=8) :: prev_time


    ! Integral sorting
    real(kind=8), allocatable :: FAHH(:,:)
    real(kind=8), allocatable :: FAHP(:,:)
    real(kind=8), allocatable :: FAPP(:,:)
    real(kind=8), allocatable :: FBHH(:,:)
    real(kind=8), allocatable :: FBHP(:,:)
    real(kind=8), allocatable :: FBPP(:,:)
    real(kind=8), allocatable :: VAHHHH(:,:,:,:)
    real(kind=8), allocatable :: VAHHHP(:,:,:,:)
    real(kind=8), allocatable :: VAHHPP(:,:,:,:)
    real(kind=8), allocatable :: VAHPHP(:,:,:,:)
    real(kind=8), allocatable :: VAHPPP(:,:,:,:)
    real(kind=8), allocatable :: VBHHHH(:,:,:,:)
    real(kind=8), allocatable :: VBHHHP(:,:,:,:)
    real(kind=8), allocatable :: VBHHPH(:,:,:,:)
    real(kind=8), allocatable :: VBHHPP(:,:,:,:)
    real(kind=8), allocatable :: VBHPHP(:,:,:,:)
    real(kind=8), allocatable :: VBHPPH(:,:,:,:)
    real(kind=8), allocatable :: VBPHPH(:,:,:,:)
    real(kind=8), allocatable :: VBHPPP(:,:,:,:)
    real(kind=8), allocatable :: VBPHPP(:,:,:,:)
    real(kind=8), allocatable :: VCHHHH(:,:,:,:)
    real(kind=8), allocatable :: VCHHHP(:,:,:,:)
    real(kind=8), allocatable :: VCHHPP(:,:,:,:)
    real(kind=8), allocatable :: VCHPHP(:,:,:,:)
    real(kind=8), allocatable :: VCHPPP(:,:,:,:)

    real(kind=8), allocatable :: VAAPPP(:,:,:,:)
    real(kind=8), allocatable :: VBAPPP(:,:,:,:)
    real(kind=8), allocatable :: VBPAPP(:,:,:,:)
    real(kind=8), allocatable :: VCAPPP(:,:,:,:)


    call count_t_spaces(froz, occ_a, occ_b, orbs, actocc, actunocc, &
        t_trunc, t_order, t_pos, t_size)

    call load_integrals(froz, occ_a, occ_b, orbs, actocc, actunocc,&
        erepul, eref, &
        FAHH,FAHP,FAPP,FBHH,FBHP,FBPP, &
        VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP, &
        VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH, &
        VBPHPH,VBHPPP,VBPHPP, &
        VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP, &
        VAAPPP, VBAPPP, VBPAPP, VCAPPP, &
        onebody, twobody)

    call print_calc_params(froz, occ_a, occ_b, orbs, actocc, actunocc, &
        shift, itol, &
        eref, erepul, &
        diis_space, restart, maxiter, &
        label)

    open(t_unit,file='t_vec.bin',form='unformatted')
    open(t_vecs_unit, file='t_vecs.bin', form='unformatted', recl=t_size*8, access='direct')

    allocate(t(t_size))
    t=0.0d0

    call print_io('')
    call print_date('  Starting CCSDt calculation on')
    call print_io('')

    rewind(t_unit)

    if (restart) read(t_unit, iostat=ios) t

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

    call print_iter_head()

    ! Start main CC loop
    call cpu_time(prev_time)
    do iter = 1, maxiter


        call update_cc(froz, occ_a, occ_b, orbs, actocc, actunocc, &
            t_order, t_pos, t_size, t, shift, &
            FAHH,FAHP,FAPP,FBHH,FBHP,FBPP, &
            VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP, &
            VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH, &
            VBPHPH,VBHPPP,VBPHPP, &
            VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP, &
            VAAPPP, VBAPPP, VBPAPP, VCAPPP)

        call calculate_energy(occ_a,occ_b,orbs,froz, &
            FAHP,FBHP,VAHHPP,VBHHPP,VCHHPP, &
            t_order, t_pos, t(1:t_pos(6) - 1), &
            E1A,E1B,E2A,E2B,E2C,E1A1A,E1A1B,E1B1B)

        ecor_new = e1a+e1b+e2a+e2b+e2c+e1a1a+e1a1b+e1b1b
        energy_diff = ecor-ecor_new

        if (isnan(energy_diff)) call abort_cc('Calculation diverged')

        ! Convergence log
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
        call cpu_time(prev_time)

        ! Check for convergence
        if (dabs(conv(1)) < 1.0d1**(-itol) .and. dabs(conv(2)) <1.0d1**(-itol) .and. dabs(conv(3)) < 1.0d1**(-itol)) exit

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
