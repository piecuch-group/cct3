subroutine solve_lcc(n0, n1, n2, n3, &
        ihbar, onebody, twobody, t_size, t0, &
        ecor_cc, itol, maxiter, diis_space)

    use integrals, only: load_old_integrals
    use diis

    implicit none

    integer, intent(in) :: n0, n1, n2 ,n3
    integer, intent(in) :: maxiter, itol
    integer, intent(in) :: ihbar, t_size
    integer, intent(in) :: diis_space
    real(kind=8), intent(in) :: ecor_cc
    character(len=*), intent(in) :: onebody, twobody
    real(kind=8), intent(in) :: t0(t_size)

    integer, parameter :: l_unit = 482, l_vecs_unit = 433
    integer :: k1, k2, k3, k4,  kkk
    integer :: k1a, k1b, k2a, k2b, k2c, l2b, l2c
    integer :: i, j, k, l
    integer :: a, b, c, d
    integer :: iter

    real(kind=8) :: prev_time
    real(kind=8) :: ecor, ecor_new, energy_diff
    real(kind=8), allocatable :: FockR(:,:)
    real(kind=8), allocatable :: FockB(:,:)
    real(kind=8), allocatable :: IntR(:,:,:,:)
    real(kind=8), allocatable :: IntB(:,:,:,:)
    real(kind=8), allocatable :: IntM(:,:,:,:)
    real(kind=8), allocatable :: t(:), l_vec(:)

    real(kind=8),allocatable::H1A(:,:)
    real(kind=8),allocatable::H1B(:,:)
    real(kind=8),allocatable::H2A(:,:,:,:)
    real(kind=8),allocatable::H2B(:,:,:,:)
    real(kind=8),allocatable::H2C(:,:,:,:)

    real(kind=8) :: res, shift

    shift = 0.0d0

    call load_old_integrals(n1, n2, n3, &
        FockR, FockB, IntR, IntM, IntB, &
        onebody, twobody)

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
    read(iHBar) ((H1A(i,j),i=N0+1,N3),j=N0+1,N3)
    read(iHBar) ((H1B(i,j),i=N0+1,N3),j=N0+1,N3)
    read(iHBar) ((((H2A(i,j,k,l),i=N0+1,N3),j=N0+1,N3),k=N0+1,N3),l=N0+1,N3)
    read(iHBar) ((((H2B(i,j,k,l),i=N0+1,N3),j=N0+1,N3),k=N0+1,N3),l=N0+1,N3)
    read(iHBar) ((((H2C(i,j,k,l),i=N0+1,N3),j=N0+1,N3),k=N0+1,N3),l=N0+1,N3)
    do i=N0+1,N1
        do j=N0+1,N1
            do a=N1+1,N3
                do b=N1+1,N3
                    H2A(b,a,j,i)=IntR(b,a,j,i)
                enddo
            enddo
        enddo
    enddo
    do i=N0+1,N1
        do j=N0+1,N2
            do a=N1+1,N3
                do b=N2+1,N3
                    H2B(b,a,j,i)=IntM(b,a,j,i)
                enddo
            enddo
        enddo
    enddo
    do i=N0+1,N2
        do j=N0+1,N2
            do a=N2+1,N3
                do b=N2+1,N3
                    H2C(b,a,j,i)=IntB(b,a,j,i)
                enddo
            enddo
        enddo
    enddo

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

    allocate(t(KKK))
    t=t0(1:kkk)
    !do i=K2A,K2B-1
    !    t(i)=-t0(i)
    !enddo
    !do i=K2B,K2C-1
    !    t(i)=-t0(L2B-K2B+i)
    !enddo
    !do i=K2C,KKK
    !    t(i)=-t0(L2C-K2C+i)
    !enddo


    open(l_unit, file='l_vec.bin', form='unformatted')
    open(l_vecs_unit, file='l_vecs.bin', form='unformatted', recl=kkk*8, access='direct')
    write(l_unit) t

    call print_io('')
    call print_date('  Starting L-CC calculation on')
    call print_io('')
    call print_iter_head()
    call cpu_time(prev_time)
    do iter=1, maxiter

        call update_lcc(N0,N1,N2,N3,l_unit,ECor_cc,res,ecor_new,shift, &
            K1,K2,K3,K4,K1A,K1B,K2A,K2B,K2C,KKK, &
            FockR,FockB,IntR,IntB,IntM,H1A,H1B,H2A,H2B,H2C,t)

        energy_diff = ecor - ecor_new
        ecor = ecor_new

        call print_iteration(iter, ecor + ecor_cc, energy_diff, res, prev_time)
        call cpu_time(prev_time)

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
    call print_date('  L-CC calculation finished on:')
    call print_io('')

    close(l_vecs_unit, status='delete')
    close(l_unit)
end subroutine solve_lcc
