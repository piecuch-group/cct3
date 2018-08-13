subroutine correct_cc(n0, n1, n2, n3, m1, m2, t_size, t0, &
        ihbar, onebody, twobody, ccpq_energy)

    use integrals, only: load_old_integrals

    implicit none

    integer, intent(in) :: n0, n1, n2, n3
    integer, intent(in) :: m1, m2
    integer, intent(in) :: t_size
    integer, intent(in) :: ihbar
    real(kind=8), intent(inout) :: ccpq_energy(4)
    !integer :: ihbar
    character(len=*), intent(in) :: onebody, twobody

    integer :: a,b,c,d,e,f
    integer :: i, j, k
    integer :: nroot, iroot
    integer :: k1a, k1b, k2a, k2b, k2c, k3a, k3b, k3c, k3d, kkk
    integer :: l2b, l2c
    integer :: k1, k2, k3, k4

    integer, parameter :: il = 441

    real(kind=8) :: res,PP,shift,LHM,R0,ECC
    real(kind=8) :: DA,DB,DC,DD
    real(kind=8) :: t0(t_size)

    real(kind=8), allocatable :: FockR(:,:)
    real(kind=8), allocatable :: FockB(:,:)
    real(kind=8), allocatable :: IntR(:,:,:,:)
    real(kind=8), allocatable :: IntB(:,:,:,:)
    real(kind=8), allocatable :: IntM(:,:,:,:)

    real(kind=8), allocatable :: H1A(:,:)
    real(kind=8), allocatable :: H1B(:,:)
    real(kind=8), allocatable :: H2A(:,:,:,:)
    real(kind=8), allocatable :: H2B(:,:,:,:)
    real(kind=8), allocatable :: H2C(:,:,:,:)

    real(kind=8) :: E23A
    real(kind=8) :: E23B
    real(kind=8) :: E23C
    real(kind=8) :: E23D
    real(kind=8), allocatable :: l(:),ECor(:),r(:), t(:), t_test(:)
    real(kind=8), allocatable :: LH3(:,:,:,:,:,:)
    real(kind=8), allocatable :: MM3(:,:,:,:,:,:)
    real(kind=8), allocatable :: D3A1(:,:,:)
    real(kind=8), allocatable :: D3A2(:,:,:)
    real(kind=8), allocatable :: D3B1(:,:,:)
    real(kind=8), allocatable :: D3B2(:,:,:)
    real(kind=8), allocatable :: D3C1(:,:,:)
    real(kind=8), allocatable :: D3C2(:,:,:)
    real(kind=8), allocatable :: D3D1(:,:,:)
    real(kind=8), allocatable :: D3D2(:,:,:)

    call load_old_integrals(n1, n2, n3, &
        FockR, FockB, IntR, IntM, IntB, &
        onebody, twobody)

    iroot = 0
    pp = 0.0d0
    r0 = 1.0d0

    !ihbar = 535
    !open(ihbar,file='h8_hbar.bin',form='unformatted')

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
    read(ihbar) ((H1A(i,a),i=N0+1,N3),a=N0+1,N3)
    read(ihbar) ((H1B(i,a),i=N0+1,N3),a=N0+1,N3)
    read(ihbar) ((((H2A(i,j,a,b),i=N0+1,N3),j=N0+1,N3),a=N0+1,N3),b=N0+1,N3)
    read(ihbar) ((((H2B(i,j,a,b),i=N0+1,N3),j=N0+1,N3),a=N0+1,N3),b=N0+1,N3)
    read(ihbar) ((((H2C(i,j,a,b),i=N0+1,N3),j=N0+1,N3),a=N0+1,N3),b=N0+1,N3)

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
    !allocate(t_test(kkk))

    !open(534, file='h8_t.bin', form='unformatted')
    !read(534) test_1, test_2
    !read(534) t_test
    !t = t_test(1:kkk)
    !close(534)

    e23a = 0.0d0
    e23b = 0.0d0
    e23c = 0.0d0
    e23d = 0.0d0
    allocate(D3A1(N1+1:N3,N0+1:N1,N0+1:N1))
    allocate(D3A2(N1+1:N3,N1+1:N3,N0+1:N1))
    allocate(D3B1(N1+1:N3,N0+1:N2,N0+1:N1))
    allocate(D3B2(N2+1:N3,N1+1:N3,N0+1:N1))
    allocate(D3C1(N2+1:N3,N0+1:N2,N0+1:N1))
    allocate(D3C2(N2+1:N3,N1+1:N3,N0+1:N2))
    allocate(D3D1(N2+1:N3,N0+1:N2,N0+1:N2))
    allocate(D3D2(N2+1:N3,N2+1:N3,N0+1:N2))
    D3A1=0.0d0
    D3A2=0.0d0
    D3B1=0.0d0
    D3B2=0.0d0
    D3C1=0.0d0
    D3C2=0.0d0
    D3D1=0.0d0
    D3D2=0.0d0

    call Cal_D3(N0,N1,N2,N3, &
        H2A,H2B,H2C,t(K2A),t(K2B),t(K2C), &
        D3A1,D3A2,D3B1,D3B2,D3C1,D3C2,D3D1,D3D2)

    open(il, file='l_vec.bin', form='unformatted')
    !open(il, file='oldl.bin', form='unformatted')

    allocate(l(KKK))
    l=0.0d0
    rewind(il)
    read(iL) l

    allocate(LH3(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
    LH3=0.0d0
    call L3A_update(N0,N1,N2,N3,LH3, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        H1A,H1B,H2A,H2B,H2C, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
        l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))

    allocate(MM3(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
    MM3=0.0d0
    if(dabs(R0).gt.1.0d-12)then
        call t3A_update(N0,N1,N2,N3,MM3, &
            K1,K2,K3,K4, &
            FockR,FockB,IntR,IntB,IntM, &
            t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
        MM3=R0*MM3
    endif

    !call print_RA(1111,N0,N1,N2,N3,K3B-K3A,MM3,1.0d-3)

    ! Loop that has to be changed to fit the active space
    do i=N0+1,N1-2
        do j=i+1,N1-1
            do k=j+1,N1
                do a=N1+1,N3-2
                    do b=a+1,N3-1
                        do c=b+1,N3
                            if (k > m1 .and. a <= m2) cycle
                            DA=PP+FockR(i,i)+FocKR(j,j)+FockR(k,k) &
                                -FockR(a,a)-FockR(b,b)-FockR(c,c)
                            DB=PP+H1A(i,i)+H1A(j,j)+H1A(k,k) &
                                -H1A(a,a)-H1A(b,b)-H1A(c,c)
                            DC=DB+H2A(a,i,a,i)+H2A(b,i,b,i)+H2A(c,i,c,i) &
                                +H2A(a,j,a,j)+H2A(b,j,b,j)+H2A(c,j,c,j) &
                                +H2A(a,k,a,k)+H2A(b,k,b,k)+H2A(c,k,c,k) &
                                -H2A(j,i,j,i)-H2A(k,i,k,i)-H2A(k,j,k,j) &
                                -H2A(b,a,b,a)-H2A(c,a,c,a)-H2A(c,b,c,b)
                            DD=DC+D3A1(a,j,i)+D3A1(a,k,i)+D3A1(a,k,j) &
                                +D3A1(b,j,i)+D3A1(b,k,i)+D3A1(b,k,j) &
                                +D3A1(c,j,i)+D3A1(c,k,i)+D3A1(c,k,j) &
                                -D3A2(b,a,i)-D3A2(b,a,j)-D3A2(b,a,k) &
                                -D3A2(c,a,i)-D3A2(c,a,j)-D3A2(c,a,k) &
                                -D3A2(c,b,i)-D3A2(c,b,j)-D3A2(c,b,k)
                            LHM=LH3(c,b,a,k,j,i)*MM3(c,b,a,k,j,i)
                            E23A = E23A +LHM/DA!/36.0d0
                            E23B = E23B +LHM/DB!/36.0d0
                            E23C = E23C +LHM/DC!/36.0d0
                            E23D = E23D +LHM/DD!/36.0d0
                        enddo
                    enddo
                enddo
            enddo
        enddo
    enddo
    deallocate(LH3,MM3)

    allocate(LH3(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
    LH3=0.0d0
    call L3B_update(N0,N1,N2,N3,LH3, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        H1A,H1B,H2A,H2B,H2C, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
        l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))

    allocate(MM3(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
    MM3=0.0d0
    if(dabs(R0).gt.1.0d-12)then
        call t3B_update(N0,N1,N2,N3,MM3, &
            K1,K2,K3,K4, &
            FockR,FockB,IntR,IntB,IntM, &
            t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
        MM3=R0*MM3
    endif
    !call print_RB(2222,N0,N1,N2,N3,K3C-K3B,MM3,1.0d-3)
    do i=N0+1,N1-1
        do j=i+1,N1
            do k=N0+1,N2
                do a=N1+1,N3-1
                    do b=a+1,N3
                        do c=N2+1,N3
                            if ((j > m1 .or. k > m1) .and. (a <= m2 .or. c <= m2)) cycle
                            DA=PP+FockR(i,i)+FocKR(j,j)+FockB(k,k) &
                                -FockR(a,a)-FockR(b,b)-FockB(c,c)
                            DB=PP+H1A(i,i)+H1A(j,j)+H1B(k,k) &
                                -H1A(a,a)-H1A(b,b)-H1B(c,c)
                            DC=DB+H2A(a,i,a,i)+H2A(b,i,b,i)+H2B(c,i,c,i) &
                                +H2A(a,j,a,j)+H2A(b,j,b,j)+H2B(c,j,c,j) &
                                +H2B(k,a,k,a)+H2B(k,b,k,b)+H2C(c,k,c,k) &
                                -H2A(j,i,j,i)-H2B(k,i,k,i)-H2B(k,j,k,j) &
                                -H2A(b,a,b,a)-H2B(c,a,c,a)-H2B(c,b,c,b)
                            DD=DC+D3A1(a,j,i)+D3B1(a,k,i)+D3B1(a,k,j) &
                                +D3A1(b,j,i)+D3B1(b,k,i)+D3B1(b,k,j) &
                                +D3C1(c,k,i)+D3C1(c,k,j) &
                                -D3A2(b,a,i)-D3A2(b,a,j) &
                                -D3B2(c,a,i)-D3B2(c,a,j)-D3C2(c,a,k) &
                                -D3B2(c,b,i)-D3B2(c,b,j)-D3C2(c,b,k)
                            LHM=LH3(c,b,a,k,j,i)*MM3(c,b,a,k,j,i)
                            E23A = E23A+LHM/DA!/4.0d0
                            E23B = E23B+LHM/DB!/4.0d0
                            E23C = E23C+LHM/DC!/4.0d0
                            E23D = E23D+LHM/DD!/4.0d0
                        enddo
                    enddo
                enddo
            enddo
        enddo
    enddo
    deallocate(LH3,MM3)

    allocate(LH3(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
    LH3=0.0d0
    call L3C_update(N0,N1,N2,N3,LH3, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        H1A,H1B,H2A,H2B,H2C, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
        l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))

    allocate(MM3(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
    MM3=0.0d0
    if(dabs(R0).gt.1.0d-12)then
        call t3C_update(N0,N1,N2,N3,MM3, &
            K1,K2,K3,K4, &
            FockR,FockB,IntR,IntB,IntM, &
            t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
        MM3=R0*MM3
    endif

    !call print_RC(3333,N0,N1,N2,N3,K3D-K3C,MM3,1.0d-3)

    do i=N0+1,N1
        do j=N0+1,N2-1
            do k=j+1,N2
                do a=N1+1,N3
                    do b=N2+1,N3-1
                        do c=b+1,N3
                            if ((i > m1 .or. k > m1) .and. (a <= m2 .or. b <= m2)) cycle
                            DA=PP+FockR(i,i)+FocKB(j,j)+FockB(k,k) &
                                -FockR(a,a)-FockB(b,b)-FockB(c,c)
                            DB=PP+H1A(i,i)+H1B(j,j)+H1B(k,k) &
                                -H1A(a,a)-H1B(b,b)-H1B(c,c)
                            DC=DB+H2A(a,i,a,i)+H2B(b,i,b,i)+H2B(c,i,c,i) &
                                +H2B(j,a,j,a)+H2C(b,j,b,j)+H2C(c,j,c,j) &
                                +H2B(k,a,k,a)+H2C(b,k,b,k)+H2C(c,k,c,k) &
                                -H2B(j,i,j,i)-H2B(k,i,k,i)-H2C(k,j,k,j) &
                                -H2B(b,a,b,a)-H2B(c,a,c,a)-H2C(c,b,c,b)
                            DD=DC+D3B1(a,j,i)+D3B1(a,k,i) &
                                +D3C1(b,j,i)+D3C1(b,k,i)+D3D1(b,k,j) &
                                +D3C1(c,j,i)+D3C1(c,k,i)+D3D1(c,k,j) &
                                -D3B2(b,a,i)-D3C2(b,a,j)-D3C2(b,a,k) &
                                -D3B2(c,a,i)-D3C2(c,a,j)-D3C2(c,a,k) &
                                -D3D2(c,b,j)-D3D2(c,b,k)
                            LHM=LH3(c,b,a,k,j,i)*MM3(c,b,a,k,j,i)
                            E23A = E23A+LHM/DA!/4.0d0
                            E23B = E23B+LHM/DB!/4.0d0
                            E23C = E23C+LHM/DC!/4.0d0
                            E23D = E23D+LHM/DD!/4.0d0
                        enddo
                    enddo
                enddo
            enddo
        enddo
    enddo
    deallocate(LH3,MM3)

    allocate(LH3(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
    LH3=0.0d0
    call L3D_update(N0,N1,N2,N3,LH3, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        H1A,H1B,H2A,H2B,H2C, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C), &
        l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))

    allocate(MM3(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
    MM3=0.0d0
    if(dabs(R0).gt.1.0d-12)then
        call t3D_update(N0,N1,N2,N3,MM3, &
            K1,K2,K3,K4, &
            FockR,FockB,IntR,IntB,IntM, &
            t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
        MM3=R0*MM3
    endif

    !call print_RD(4444,N0,N1,N2,N3,KKK-K3D+1,MM3,1.0d-3)

    do i=N0+1,N2-2
        do j=i+1,N2-1
            do k=j+1,N2
                do a=N2+1,N3-2
                    do b=a+1,N3-1
                        do c=b+1,N3
                            if (k > m1 .and. a <= m2) cycle
                            DA=PP+FockB(i,i)+FocKB(j,j)+FockB(k,k) &
                                -FockB(a,a)-FockB(b,b)-FockB(c,c)
                            DB=PP+H1B(i,i)+H1B(j,j)+H1B(k,k) &
                                -H1B(a,a)-H1B(b,b)-H1B(c,c)
                            DC=DB+H2C(a,i,a,i)+H2C(b,i,b,i)+H2C(c,i,c,i) &
                                +H2C(a,j,a,j)+H2C(b,j,b,j)+H2C(c,j,c,j) &
                                +H2C(a,k,a,k)+H2C(b,k,b,k)+H2C(c,k,c,k) &
                                -H2C(j,i,j,i)-H2C(k,i,k,i)-H2C(k,j,k,j) &
                                -H2C(b,a,b,a)-H2C(c,a,c,a)-H2C(c,b,c,b)
                            DD=DC+D3D1(a,j,i)+D3D1(a,k,i)+D3D1(a,k,j) &
                                +D3D1(b,j,i)+D3D1(b,k,i)+D3D1(b,k,j) &
                                +D3D1(c,j,i)+D3D1(c,k,i)+D3D1(c,k,j) &
                                -D3D2(b,a,i)-D3D2(b,a,j)-D3D2(b,a,k) &
                                -D3D2(c,a,i)-D3D2(c,a,j)-D3D2(c,a,k) &
                                -D3D2(c,b,i)-D3D2(c,b,j)-D3D2(c,b,k)
                            LHM=LH3(c,b,a,k,j,i)*MM3(c,b,a,k,j,i)
                            E23A =E23A+LHM/DA!/36.0d0
                            E23B =E23B+LHM/DB!/36.0d0
                            E23C =E23C+LHM/DC!/36.0d0
                            E23D =E23D+LHM/DD!/36.0d0
                        enddo
                    enddo
                enddo
            enddo
        enddo
    enddo
    deallocate(LH3,MM3)

    ccpq_energy = 0.0d0
    ccpq_energy(1) = e23a
    ccpq_energy(2) = e23b
    ccpq_energy(3) = e23c
    ccpq_energy(4) = e23d

    close(il)
end subroutine correct_cc
