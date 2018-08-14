subroutine hbar(N0,N1,N2,N3, t_size, t0, &
        ihbar, onebody, twobody)

    use integrals, only: load_old_integrals
    implicit none

    integer, intent(in) :: n0, n1, n2 ,n3
    integer, intent(in) :: ihbar, t_size
    real(kind=8), intent(in) :: t0(t_size)
    character(len=*), intent(in) :: onebody, twobody

    integer :: k1, k2, k3, k4, kkk
    integer :: k1a, k1b, k2a, k2b, k2c, l2b, l2c
    integer :: i, j, k, l
    integer :: a, b, c, d

    integer, parameter :: io = 6
    real(kind=8), allocatable :: FockR(:,:)
    real(kind=8), allocatable :: FockB(:,:)
    real(kind=8), allocatable :: IntR(:,:,:,:)
    real(kind=8), allocatable :: IntB(:,:,:,:)
    real(kind=8), allocatable :: IntM(:,:,:,:)
    real(kind=8), allocatable :: t(:)

    real(kind=8),allocatable::H1A(:,:)
    real(kind=8),allocatable::H1B(:,:)
    real(kind=8),allocatable::H2A(:,:,:,:)
    real(kind=8),allocatable::H2B(:,:,:,:)
    real(kind=8),allocatable::H2C(:,:,:,:)

    real(kind=8),allocatable::V0A2A(:,:)
    real(kind=8),allocatable::V0A2C(:,:)
    real(kind=8),allocatable::V0A4A(:,:,:,:)
    real(kind=8),allocatable::V0A4C(:,:,:,:)
    real(kind=8),allocatable::V0A4E(:,:,:,:)
    real(kind=8),allocatable::V1A1A(:,:)
    real(kind=8),allocatable::V1A3A(:,:,:,:)
    real(kind=8),allocatable::V1A3C(:,:,:,:)
    real(kind=8),allocatable::V1B1B(:,:)
    real(kind=8),allocatable::V1B3B(:,:,:,:)
    real(kind=8),allocatable::V1B3D(:,:,:,:)
    real(kind=8),allocatable::V2A0A(:,:)
    real(kind=8),allocatable::V2A2A(:,:,:,:)
    real(kind=8),allocatable::V2A2C(:,:,:,:)
    real(kind=8),allocatable::V2B2B(:,:,:,:)
    real(kind=8),allocatable::V2C0A(:,:)
    real(kind=8),allocatable::V2C2A(:,:,:,:)
    real(kind=8),allocatable::V2C2C(:,:,:,:)
    real(kind=8),allocatable::V3A1A(:,:,:,:)
    real(kind=8),allocatable::V3B1B(:,:,:,:)
    real(kind=8),allocatable::V3C1A(:,:,:,:)
    real(kind=8),allocatable::V3D1B(:,:,:,:)
    real(kind=8),allocatable::V4A0A(:,:,:,:)
    real(kind=8),allocatable::V4C0A(:,:,:,:)
    real(kind=8),allocatable::V4E0A(:,:,:,:)

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
    L2C =K2B
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

    call load_old_integrals(n1, n2, n3, &
        FockR, FockB, IntR, IntM, IntB, &
        onebody, twobody)

    call print_io('')
    call print_date('  Similarity transformed Hamiltonian generation started on')

    allocate(H1A(N0+1:N3,N0+1:N3))
    H1A=0.0d0

    allocate(V0A2A(N0+1:N1,N0+1:N1))
    V0A2A=0.0d0

    call HBar0A2A(N0,N1,N2,N3,V0A2A, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do j=N0+1,N1
            H1A(j,i)=V0A2A(j,i)+FockR(j,i)
        enddo
    enddo
    deallocate(V0A2A)

    allocate(V1A1A(N1+1:N3,N0+1:N1))
    V1A1A=0.0d0

    call HBar1A1A(N0,N1,N2,N3,V1A1A, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do a=N1+1,N3
            H1A(a,i)=V1A1A(a,i)+FockR(a,i)
        enddo
    enddo
    deallocate(V1A1A)

    allocate(V2A0A(N1+1:N3,N1+1:N3))
    V2A0A=0.0d0

    call HBar2A0A(N0,N1,N2,N3,V2A0A, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do a=N1+1,N3
        do b=N1+1,N3
            H1A(b,a)=V2A0A(b,a)+FockR(b,a)
        enddo
    enddo
    deallocate(V2A0A)

    write(iHBar)H1A
    deallocate(H1A)

    allocate(H1B(N0+1:N3,N0+1:N3))
    H1B=0.0d0

    allocate(V0A2C(N0+1:N2,N0+1:N2))
    V0A2C=0.0d0

    call HBar0A2C(N0,N1,N2,N3,V0A2C, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N2
        do j=N0+1,N2
            H1B(j,i)=V0A2C(j,i)+FockB(j,i)
        enddo
    enddo
    deallocate(V0A2C)

    allocate(V1B1B(N2+1:N3,N0+1:N2))
    V1B1B=0.0d0

    call HBar1B1B(N0,N1,N2,N3,V1B1B, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N2
        do a=N2+1,N3
            H1B(a,i)=V1B1B(a,i)+FockB(a,i)
        enddo
    enddo
    deallocate(V1B1B)

    allocate(V2C0A(N2+1:N3,N2+1:N3))
    V2C0A=0.0d0

    call HBar2C0A(N0,N1,N2,N3,V2C0A, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do a=N2+1,N3
        do b=N2+1,N3
            H1B(b,a)=V2C0A(b,a)+FockB(b,a)
        enddo
    enddo
    deallocate(V2C0A)

    write(iHBar)H1B
    deallocate(H1B)
    call print_io('    One-body matrix done')

    allocate(H2A(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3))
    H2A=0.0d0

    allocate(V0A4A(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
    V0A4A=0.0d0

    call HBar0A4A(N0,N1,N2,N3,V0A4A, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do j=N0+1,N1
            do k=N0+1,N1
                do l=N0+1,N1
                    H2A(j,i,l,k)=V0A4A(l,k,j,i)+IntR(j,i,l,k)
                enddo
            enddo
        enddo
    enddo
    deallocate(V0A4A)

    allocate(V1A3A(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
    V1A3A=0.0d0

    call HBar1A3A0(N0,N1,N2,N3,V1A3A, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do j=N0+1,N1
            do k=N0+1,N1
                do a=N1+1,N3
                    H2A(a,i,k,j)=V1A3A(a,k,j,i)+IntR(a,i,k,j)
                    H2A(i,a,k,j)=-H2A(a,i,k,j)
                enddo
            enddo
        enddo
    enddo
    deallocate(V1A3A)

    allocate(V1A3A(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
    V1A3A=0.0d0

    call HBar1A3A(N0,N1,N2,N3,V1A3A, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do j=N0+1,N1
            do k=N0+1,N1
                do a=N1+1,N3
                    H2A(k,j,i,a)=V1A3A(a,k,j,i)+IntR(k,j,i,a)
                    H2A(k,j,a,i)=-H2A(k,j,i,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V1A3A)

    do i=N0+1,N1
        do j=N0+1,N1
            do a=N1+1,N3
                do b=N1+1,N3
                    H2A(b,a,j,i)=IntR(b,a,j,i)
                enddo
            enddo
        enddo
    enddo

    allocate(V2A2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
    V2A2A=0.0d0

    call HBar2A2A(N0,N1,N2,N3,V2A2A, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do j=N0+1,N1
            do a=N1+1,N3
                do b=N1+1,N3
                    H2A(b,i,j,a)=V2A2A(b,a,j,i)+IntR(b,i,j,a)
                    H2A(b,i,a,j)=-H2A(b,i,j,a)
                    H2A(i,b,j,a)=-H2A(b,i,j,a)
                    H2A(i,b,a,j)=H2A(b,i,j,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V2A2A)

    allocate(V3A1A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
    V3A1A=0.0d0

    call HBar3A1A0(N0,N1,N2,N3,V3A1A, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do a=N1+1,N3
            do b=N1+1,N3
                do c=N1+1,N3
                    H2A(c,b,i,a)=V3A1A(c,b,a,i)+IntR(c,b,i,a)
                    H2A(c,b,a,i)=-H2A(c,b,i,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V3A1A)

    allocate(V3A1A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
    V3A1A=0.0d0

    call HBar3A1A(N0,N1,N2,N3,V3A1A, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do a=N1+1,N3
            do b=N1+1,N3
                do c=N1+1,N3
                    H2A(b,i,c,a)=V3A1A(c,b,a,i)+IntR(b,i,c,a)
                    H2A(i,b,c,a)=-H2A(b,i,c,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V3A1A)

    allocate(V4A0A(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
    V4A0A=0.0d0

    call HBar4A0A(N0,N1,N2,N3,V4A0A, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do a=N1+1,N3
        do b=N1+1,N3
            do c=N1+1,N3
                do d=N1+1,N3
                    H2A(d,c,b,a)=V4A0A(d,c,b,a)+IntR(d,c,b,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V4A0A)

    write(iHBar)H2A
    deallocate(H2A)

    allocate(H2B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3))
    H2B=0.0d0

    allocate(V0A4C(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
    V0A4C=0.0d0

    call HBar0A4C(N0,N1,N2,N3,V0A4C, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do j=N0+1,N1
            do k=N0+1,N2
                do l=N0+1,N2
                    H2B(k,i,l,j)=V0A4C(l,k,j,i)+IntM(k,i,l,j)
                enddo
            enddo
        enddo
    enddo
    deallocate(V0A4C)

    allocate(V1A3C(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
    V1A3C=0.0d0

    call HBar1A3C0(N0,N1,N2,N3,V1A3C, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do j=N0+1,N2
            do k=N0+1,N2
                do a=N1+1,N3
                    H2B(j,a,k,i)=V1A3C(a,k,j,i)+IntM(j,a,k,i)
                enddo
            enddo
        enddo
    enddo
    deallocate(V1A3C)

    allocate(V1A3C(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
    V1A3C=0.0d0

    call HBar1A3C(N0,N1,N2,N3,V1A3C, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do j=N0+1,N2
            do k=N0+1,N2
                do a=N1+1,N3
                    H2B(j,i,k,a)=V1A3C(a,k,j,i)+IntM(j,i,k,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V1A3C)

    allocate(V1B3B(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
    V1B3B=0.0d0

    call HBar1B3B0(N0,N1,N2,N3,V1B3B, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do j=N0+1,N1
            do k=N0+1,N2
                do a=N2+1,N3
                    H2B(a,i,k,j)=V1B3B(a,k,j,i)+IntM(a,i,k,j)
                enddo
            enddo
        enddo
    enddo
    deallocate(V1B3B)

    allocate(V1B3B(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
    V1B3B=0.0d0

    call HBar1B3B(N0,N1,N2,N3,V1B3B, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do j=N0+1,N1
            do k=N0+1,N2
                do a=N2+1,N3
                    H2B(k,i,a,j)=V1B3B(a,k,j,i)+IntM(k,i,a,j)
                enddo
            enddo
        enddo
    enddo
    deallocate(V1B3B)

    allocate(V2A2C(N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N2))
    V2A2C=0.0d0

    call HBar2A2C(N0,N1,N2,N3,V2A2C, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N2
        do j=N0+1,N2
            do a=N1+1,N3
                do b=N1+1,N3
                    H2B(j,b,i,a)=V2A2C(b,a,j,i)+IntM(j,b,i,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V2A2C)

    do i=N0+1,N1
        do j=N0+1,N2
            do a=N1+1,N3
                do b=N2+1,N3
                    H2B(b,a,j,i)=IntM(b,a,j,i)
                enddo
            enddo
        enddo
    enddo

    allocate(V2B2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
    V2B2B=0.0d0

    call HBar2B2B0(N0,N1,N2,N3,V2B2B, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do j=N0+1,N2
            do a=N1+1,N3
                do b=N2+1,N3
                    H2B(b,i,j,a)=V2B2B(b,a,j,i)+IntM(b,i,j,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V2B2B)

    allocate(V2B2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
    V2B2B=0.0d0

    call HBar2B2B(N0,N1,N2,N3,V2B2B, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do j=N0+1,N2
            do a=N1+1,N3
                do b=N2+1,N3
                    H2B(j,a,b,i)=V2B2B(b,a,j,i)+IntM(j,a,b,i)
                enddo
            enddo
        enddo
    enddo
    deallocate(V2B2B)

    allocate(V2C2A(N2+1:N3,N2+1:N3,N0+1:N1,N0+1:N1))
    V2C2A=0.0d0

    call HBar2C2A(N0,N1,N2,N3,V2C2A, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do j=N0+1,N1
            do a=N2+1,N3
                do b=N2+1,N3
                    H2B(a,i,b,j)=V2C2A(b,a,j,i)+IntM(a,i,b,j)
                enddo
            enddo
        enddo
    enddo
    deallocate(V2C2A)

    allocate(V3B1B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
    V3B1B=0.0d0

    call HBar3B1B0(N0,N1,N2,N3,V3B1B, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N2
        do a=N1+1,N3
            do b=N1+1,N3
                do c=N2+1,N3
                    H2B(c,b,i,a)=V3B1B(c,b,a,i)+IntM(c,b,i,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V3B1B)

    allocate(V3B1B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
    V3B1B=0.0d0

    call HBar3B1B(N0,N1,N2,N3,V3B1B, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N2
        do a=N1+1,N3
            do b=N1+1,N3
                do c=N2+1,N3
                    H2B(i,b,c,a)=V3B1B(c,b,a,i)+IntM(i,b,c,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V3B1B)

    allocate(V3C1A(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
    V3C1A=0.0d0

    call HBar3C1A0(N0,N1,N2,N3,V3C1A, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do a=N1+1,N3
            do b=N2+1,N3
                do c=N2+1,N3
                    H2B(c,a,b,i)=V3C1A(c,b,a,i)+IntM(c,a,b,i)
                enddo
            enddo
        enddo
    enddo
    deallocate(V3C1A)

    allocate(V3C1A(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
    V3C1A=0.0d0

    call HBar3C1A(N0,N1,N2,N3,V3C1A, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N1
        do a=N1+1,N3
            do b=N2+1,N3
                do c=N2+1,N3
                    H2B(b,i,c,a)=V3C1A(c,b,a,i)+IntM(b,i,c,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V3C1A)

    allocate(V4C0A(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3))
    V4C0A=0.0d0
    call HBar4C0A(N0,N1,N2,N3,V4C0A, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do a=N1+1,N3
        do b=N1+1,N3
            do c=N2+1,N3
                do d=N2+1,N3
                    H2B(d,b,c,a)=V4C0A(d,c,b,a)+IntM(d,b,c,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V4C0A)

    write(iHBar)H2B
    deallocate(H2B)

    allocate(H2C(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3))
    H2C=0.0d0

    allocate(V0A4E(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
    V0A4E=0.0d0

    call HBar0A4E(N0,N1,N2,N3,V0A4E, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N2
        do j=N0+1,N2
            do k=N0+1,N2
                do l=N0+1,N2
                    H2C(j,i,l,k)=V0A4E(l,k,j,i)+IntB(j,i,l,k)
                enddo
            enddo
        enddo
    enddo
    deallocate(V0A4E)

    allocate(V1B3D(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
    V1B3D=0.0d0

    call HBar1B3D0(N0,N1,N2,N3,V1B3D, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N2
        do j=N0+1,N2
            do k=N0+1,N2
                do a=N2+1,N3
                    H2C(a,i,k,j)=V1B3D(a,k,j,i)+IntB(a,i,k,j)
                    H2C(i,a,k,j)=-H2C(a,i,k,j)
                enddo
            enddo
        enddo
    enddo
    deallocate(V1B3D)

    allocate(V1B3D(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
    V1B3D=0.0d0

    call HBar1B3D(N0,N1,N2,N3,V1B3D, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N2
        do j=N0+1,N2
            do k=N0+1,N2
                do a=N2+1,N3
                    H2C(k,j,i,a)=V1B3D(a,k,j,i)+IntB(k,j,i,a)
                    H2C(k,j,a,i)=-H2C(k,j,i,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V1B3D)

    do i=N0+1,N2
        do j=N0+1,N2
            do a=N2+1,N3
                do b=N2+1,N3
                    H2C(b,a,j,i)=IntB(b,a,j,i)
                enddo
            enddo
        enddo
    enddo

    allocate(V2C2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
    V2C2C=0.0d0

    call HBar2C2C(N0,N1,N2,N3,V2C2C, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N2
        do j=N0+1,N2
            do a=N2+1,N3
                do b=N2+1,N3
                    H2C(b,i,j,a)=V2C2C(b,a,j,i)+IntB(b,i,j,a)
                    H2C(b,i,a,j)=-H2C(b,i,j,a)
                    H2C(i,b,j,a)=-H2C(b,i,j,a)
                    H2C(i,b,a,j)=H2C(b,i,j,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V2C2C)

    allocate(V3D1B(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
    V3D1B=0.0d0

    call HBar3D1B0(N0,N1,N2,N3,V3D1B, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N2
        do a=N2+1,N3
            do b=N2+1,N3
                do c=N2+1,N3
                    H2C(c,b,i,a)=V3D1B(c,b,a,i)+IntB(c,b,i,a)
                    H2C(c,b,a,i)=-H2C(c,b,i,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V3D1B)

    allocate(V3D1B(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
    V3D1B=0.0d0

    call HBar3D1B(N0,N1,N2,N3,V3D1B, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do i=N0+1,N2
        do a=N2+1,N3
            do b=N2+1,N3
                do c=N2+1,N3
                    H2C(b,i,c,a)=V3D1B(c,b,a,i)+IntB(b,i,c,a)
                    H2C(i,b,c,a)=-H2C(b,i,c,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V3D1B)

    allocate(V4E0A(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
    V4E0A=0.0d0

    call HBar4E0A(N0,N1,N2,N3,V4E0A, &
        K1,K2,K3,K4, &
        FockR,FockB,IntR,IntB,IntM, &
        t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
    do a=N2+1,N3
        do b=N2+1,N3
            do c=N2+1,N3
                do d=N2+1,N3
                    H2C(d,c,b,a)=V4E0A(d,c,b,a)+IntB(d,c,b,a)
                enddo
            enddo
        enddo
    enddo
    deallocate(V4E0A)

    write(iHBar)H2C
    deallocate(H2C)
    call print_io('    Two-body matrix done')
    call print_io('')

end subroutine hbar
