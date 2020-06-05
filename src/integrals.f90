module integrals

    use const, only: p

    implicit none

contains

    subroutine load_integrals(n0, n1, n2, n3, m1, m2, &
            eref, &
            FAHH,FAHP,FAPP,FBHH,FBHP,FBPP, &
            VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP, &
            VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH, &
            VBPHPH,VBHPPP,VBPHPP, &
            VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP, &
            VAAPPP, VBAPPP, VBPAPP, VCAPPP, &
            onebody, twobody)

        integer, intent(in) :: n0, n1, n2, n3, m1, m2
        real(p), intent(inout) :: eref
        character(len=*), intent(in) :: onebody, twobody


        integer :: k3, k4

        real(p), allocatable, intent(inout) :: FAHH(:,:)
        real(p), allocatable, intent(inout) :: FAHP(:,:)
        real(p), allocatable, intent(inout) :: FAPP(:,:)
        real(p), allocatable, intent(inout) :: FBHH(:,:)
        real(p), allocatable, intent(inout) :: FBHP(:,:)
        real(p), allocatable, intent(inout) :: FBPP(:,:)
        real(p), allocatable, intent(inout) :: VAHHHH(:,:,:,:)
        real(p), allocatable, intent(inout) :: VAHHHP(:,:,:,:)
        real(p), allocatable, intent(inout) :: VAHHPP(:,:,:,:)
        real(p), allocatable, intent(inout) :: VAHPHP(:,:,:,:)
        real(p), allocatable, intent(inout) :: VAHPPP(:,:,:,:)
        real(p), allocatable, intent(inout) :: VBHHHH(:,:,:,:)
        real(p), allocatable, intent(inout) :: VBHHHP(:,:,:,:)
        real(p), allocatable, intent(inout) :: VBHHPH(:,:,:,:)
        real(p), allocatable, intent(inout) :: VBHHPP(:,:,:,:)
        real(p), allocatable, intent(inout) :: VBHPHP(:,:,:,:)
        real(p), allocatable, intent(inout) :: VBHPPH(:,:,:,:)
        real(p), allocatable, intent(inout) :: VBPHPH(:,:,:,:)
        real(p), allocatable, intent(inout) :: VBHPPP(:,:,:,:)
        real(p), allocatable, intent(inout) :: VBPHPP(:,:,:,:)
        real(p), allocatable, intent(inout) :: VCHHHH(:,:,:,:)
        real(p), allocatable, intent(inout) :: VCHHHP(:,:,:,:)
        real(p), allocatable, intent(inout) :: VCHHPP(:,:,:,:)
        real(p), allocatable, intent(inout) :: VCHPHP(:,:,:,:)
        real(p), allocatable, intent(inout) :: VCHPPP(:,:,:,:)

        ! Active-space arrays
        real(p), allocatable :: VAAPPP(:,:,:,:)
        real(p), allocatable :: VBAPPP(:,:,:,:)
        real(p), allocatable :: VBPAPP(:,:,:,:)
        real(p), allocatable :: VCAPPP(:,:,:,:)

        ! Aux matrices
        real(p), allocatable :: VAPPP(:,:,:)
        real(p), allocatable :: VBPPP(:,:,:)
        real(p), allocatable :: VCPPP(:,:,:)

        ! Local variables
        real(p), allocatable :: core(:,:)
        real(p), allocatable :: fock_alpha(:,:)
        real(p), allocatable :: fock_beta(:,:)
        real(p), allocatable :: intm(:,:,:,:)

        ! [TODO] implement this
        integer, parameter :: nfrv = 0

        integer, parameter :: int_unit = 150

        integer :: i, j, k, l, b, c, d

        !logical :: pppp_in_disk = .true.

        integer, parameter :: iPA = 250, iPB = 251, iPC = 252

        allocate(FAHH(N0+1:N1,N0+1:N1))
        allocate(FAHP(N1+1:N3,N0+1:N1))
        allocate(FAPP(N1+1:N3,N1+1:N3))
        allocate(FBHH(N0+1:N2,N0+1:N2))
        allocate(FBHP(N2+1:N3,N0+1:N2))
        allocate(FBPP(N2+1:N3,N2+1:N3))
        allocate(VAHHHH(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
        allocate(VAHHHP(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
        allocate(VAHHPP(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
        allocate(VAHPHP(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
        allocate(VAHPPP(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
        allocate(VBHHHH(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
        allocate(VBHHHP(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
        allocate(VBHHPH(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
        allocate(VBHHPP(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
        allocate(VBHPHP(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
        allocate(VBHPPH(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
        allocate(VBPHPH(N0+1:N2,N1+1:N3,N0+1:N2,N1+1:N3))
        allocate(VBHPPP(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
        allocate(VBPHPP(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
        allocate(VCHHHH(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
        allocate(VCHHHP(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
        allocate(VCHHPP(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
        allocate(VCHPHP(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
        allocate(VCHPPP(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))

        ! Active-space
        allocate(VAAPPP(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:M2))
        allocate(VAPPP(N1+1:N3,N1+1:N3,N1+1:N3))

        allocate(VBAPPP(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:M2))
        allocate(VBPAPP(N2+1:N3,N1+1:N3,N2+1:M2,N1+1:N3))
        allocate(VBPPP(N2+1:N3,N1+1:N3,N2+1:N3))

        allocate(VCAPPP(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:M2))
        allocate(VCPPP(N2+1:N3,N2+1:N3,N2+1:N3))


        ! Open onebody integrals
        allocate (core(n3,n3))
        core = 0.0_p
        open(int_unit,file=trim(adjustl(onebody)),form='unformatted')
        read(int_unit) core
        close(int_unit)

        allocate(IntM(N3,N3,N3,N3))
        IntM = 0.0_p

        ! Open twobody integrals
        open(int_unit,file=trim(adjustl(twobody)), form='unformatted')
        read(int_unit) intm
        close(int_unit)

        allocate(fock_alpha(N3,N3),fock_beta(N3,N3))
        fock_alpha = Core
        fock_beta = Core

        call calculate_fock(N1,N2,N3,IntM,fock_alpha,fock_beta,ERef)

        ! Clean integrals
        do i=1,N3
            do j=1,N3
                if (dabs(fock_alpha(i,j)).lt.1.0d-10)fock_alpha(i,j)=0.0d0
                if (dabs(fock_beta(i,j)).lt.1.0d-10)fock_beta(i,j)=0.0d0
            enddo
        enddo
        do i=1,N3
            do j=1,N3
                do k=1,N3
                    do l=1,N3
                        if (dabs(IntM(i,j,k,l)).lt.1.0d0-10)IntM(i,j,k,l)=0.0d0
                    enddo
                enddo
            enddo
        enddo


        do j=N0+1,N1
            do i=N0+1,N1
                FAHH(i,j) = fock_alpha(i,j)
            enddo
        enddo

        do j=N0+1,N1
            do i=N1+1,N3-NFrv
                FAHP(i,j) = fock_alpha(i,j)
            enddo
        enddo

        do j=N1+1,N3-NFrv
            do i=N1+1,N3-NFrv
                FAPP(i,j) = fock_alpha(i,j)
            enddo
        enddo

        do j=N0+1,N2
            do i=N0+1,N2
                FBHH(i,j) = fock_beta(i,j)
            enddo
        enddo

        do j=N0+1,N2
            do i=N2+1,N3-NFrv
                FBHP(i,j) = fock_beta(i,j)
            enddo
        enddo

        do j=N2+1,N3-NFrv
            do i=N2+1,N3-NFrv
                FBPP(i,j) = fock_beta(i,j)
            enddo
        enddo

        do l=N0+1,N1
            do k=N0+1,N1
                do j=N0+1,N1
                    do i=N0+1,N1
                        VAHHHH(i,j,k,l) = intm(i,j,k,l)-intm(i,j,l,k)
                    enddo
                enddo
            enddo
        enddo

        do l=N0+1,N1
            do k=N0+1,N1
                do j=N0+1,N1
                    do i=N1+1,N3-NFrv
                        VAHHHP(i,j,k,l) = intm(i,j,k,l)-intm(i,j,l,k)
                    enddo
                enddo
            enddo
        enddo

        do l=N0+1,N1
            do k=N0+1,N1
                do j=N1+1,N3-NFrv
                    do i=N1+1,N3-NFrv
                        VAHHPP(i,j,k,l) = intm(i,j,k,l)-intm(i,j,l,k)
                    enddo
                enddo
            enddo
        enddo

        do l=N0+1,N1
            do k=N1+1,N3-NFrv
                do j=N0+1,N1
                    do i=N1+1,N3-NFrv
                        VAHPHP(i,j,k,l) = intm(i,j,k,l)-intm(i,j,l,k)
                    enddo
                enddo
            enddo
        enddo

        do l=N0+1,N1
            do k=N1+1,N3-NFrv
                do j=N1+1,N3-NFrv
                    do i=N1+1,N3-NFrv
                        VAHPPP(i,j,k,l) = intm(i,j,k,l)-intm(i,j,l,k)
                    enddo
                enddo
            enddo
        enddo

        do l=N0+1,N1
            do k=N0+1,N2
                do j=N0+1,N1
                    do i=N0+1,N2
                        VBHHHH(i,j,k,l) = intm(i,j,k,l)
                    enddo
                enddo
            enddo
        enddo

        do l=N0+1,N1
            do k=N0+1,N2
                do j=N0+1,N1
                    do i=N2+1,N3-NFrv
                        VBHHHP(i,j,k,l) = intm(i,j,k,l)
                    enddo
                enddo
            enddo
        enddo

        do l=N0+1,N1
            do k=N0+1,N2
                do j=N1+1,N3-NFrv
                    do i=N0+1,N2
                        VBHHPH(i,j,k,l) = intm(i,j,k,l)
                    enddo
                enddo
            enddo
        enddo

        do l=N0+1,N1
            do k=N0+1,N2
                do j=N1+1,N3-NFrv
                    do i=N2+1,N3-NFrv
                        VBHHPP(i,j,k,l) = intm(i,j,k,l)
                    enddo
                enddo
            enddo
        enddo

        do l=N0+1,N1
            do k=N2+1,N3-NFrv
                do j=N0+1,N1
                    do i=N2+1,N3-NFrv
                        VBHPHP(i,j,k,l) = intm(i,j,k,l)
                    enddo
                enddo
            enddo
        enddo

        do l=N0+1,N1
            do k=N2+1,N3-NFrv
                do j=N1+1,N3-NFrv
                    do i=N0+1,N2
                        VBHPPH(i,j,k,l) = intm(i,j,k,l)
                    enddo
                enddo
            enddo
        enddo

        do l=N1+1,N3-NFrv
            do k=N0+1,N2
                do j=N1+1,N3-NFrv
                    do i=N0+1,N2
                        VBPHPH(i,j,k,l) = intm(i,j,k,l)
                    enddo
                enddo
            enddo
        enddo

        do l=N0+1,N1
            do k=N2+1,N3-NFrv
                do j=N1+1,N3-NFrv
                    do i=N2+1,N3-NFrv
                        VBHPPP(i,j,k,l) = intm(i,j,k,l)
                    enddo
                enddo
            enddo
        enddo

        do l=N1+1,N3-NFrv
            do k=N0+1,N2
                do j=N1+1,N3-NFrv
                    do i=N2+1,N3-NFrv
                        VBPHPP(i,j,k,l) = intm(i,j,k,l)
                    enddo
                enddo
            enddo
        enddo

        do l=N0+1,N2
            do k=N0+1,N2
                do j=N0+1,N2
                    do i=N0+1,N2
                        VCHHHH(i,j,k,l) = intm(i,j,k,l)-intm(i,j,l,k)
                    enddo
                enddo
            enddo
        enddo

        do l=N0+1,N2
            do k=N0+1,N2
                do j=N0+1,N2
                    do i=N2+1,N3-NFrv
                        VCHHHP(i,j,k,l) = intm(i,j,k,l)-intm(i,j,l,k)
                    enddo
                enddo
            enddo
        enddo

        do l=N0+1,N2
            do k=N0+1,N2
                do j=N2+1,N3-NFrv
                    do i=N2+1,N3-NFrv
                        VCHHPP(i,j,k,l) = intm(i,j,k,l)-intm(i,j,l,k)
                    enddo
                enddo
            enddo
        enddo

        do l=N0+1,N2
            do k=N2+1,N3-NFrv
                do j=N0+1,N2
                    do i=N2+1,N3-NFrv
                        VCHPHP(i,j,k,l) = intm(i,j,k,l)-intm(i,j,l,k)
                    enddo
                enddo
            enddo
        enddo

        do l=N0+1,N2
            do k=N2+1,N3-NFrv
                do j=N2+1,N3-NFrv
                    do i=N2+1,N3-NFrv
                        VCHPPP(i,j,k,l) = intm(i,j,k,l)-intm(i,j,l,k)
                    enddo
                enddo
            enddo
        enddo


        !K3=N3-NFrv-N1
        !K3=N3-NFrv-N1
        K3=N3-N1
        K4=N3-N2
        open(iPA,file='PA',form='unformatted',recl=K3*K3*K3*8, access='direct')
        open(iPB,file='PB',form='unformatted',recl=K4*K3*K4*8, access='direct')
        open(iPC,file='PC',form='unformatted',recl=K4*K4*K4*8, access='direct')

        !  VAPPPP
        do l=N1+1,n3
            do b=n1+1,n3
                do c=n1+1,n3
                    do d=n1+1,n3
                        VAPPP(d,c,b) = IntM(d,c,b,l)-IntM(d,c,l,b)
                    enddo
                enddo
            enddo

            if (l <= m2) then
                VAAPPP(:,:,:,l) = VAPPP
            endif

            ! Write whole aa matrix to disk
            write(iPA,rec=l) VAPPP
        enddo
        deallocate(VAPPP)

        !  VBPPPP
        do l=N1+1,N3
            do b=N2+1,n3
                do c=N1+1,N3
                    do d=N2+1,N3
                        VBPPP(d,c,b)= intm(d,c,b,l)
                    enddo
                enddo

                !  VBPAPP
                if (b <= m2) then
                    VBPAPP(:,:,b,l) = VBPPP(:,:,b)
                endif
            enddo

            !  VBAPPP
            if (l <= m2) then
                VBAPPP(:,:,:,l) = VBPPP
            endif

            ! Write whole aa matrix to disk
            write(iPB,rec=l) VBPPP
        enddo

        deallocate(VBPPP)

        !  VCPPPP
        do l=N2+1,N3
            do b=n2+1,n3
                do c=n2+1,n3
                    do d=n2+1,n3
                        VCPPP(d,c,b) = IntM(d,c,b,l) - IntM(d,c,l,b)
                    enddo
                enddo
            enddo

            if (l <= m2) then
                VCAPPP(:,:,:,l) = VCPPP
            endif

            ! Write whole bb matrix to disk
            write(iPC,rec=l) VCPPP
        enddo
        deallocate(VCPPP)

        deallocate(IntM,fock_alpha,fock_beta,Core)

    end subroutine load_integrals

    subroutine load_old_integrals(n0, n1, n2, n3, &
            f_a, f_b, v_aa, v_ab, v_bb, &
            onebody, twobody)

        integer, intent(in) :: n0, n1, n2, n3
        character(len=*), intent(in) :: onebody, twobody

        real(p) :: eref

        ! Local variables
        real(p), allocatable :: core(:,:)
        real(p), allocatable :: f_a(:,:)
        real(p), allocatable :: f_b(:,:)
        real(p), allocatable :: r12(:,:,:,:)
        real(p), allocatable :: v_aa(:,:,:,:)
        real(p), allocatable :: v_ab(:,:,:,:)
        real(p), allocatable :: v_bb(:,:,:,:)

        ! [TODO] implement this
        integer, parameter :: nfrv = 0
        integer, parameter :: int_unit = 150

        integer :: i, j, k, l

        ! Open onebody integrals
        allocate(core(n3,n3))
        core = 0.0_p
        open(int_unit,file=trim(adjustl(onebody)), form='unformatted')
        read(int_unit) core
        close(int_unit)


        allocate(r12(N3,N3,N3,N3))
        allocate(v_aa(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3))
        allocate(v_ab(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3))
        allocate(v_bb(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3))

        r12 = 0.0_p
        v_aa=0.0_p
        v_ab=0.0_p
        v_bb=0.0_p

        ! Open twobody integrals
        open(int_unit, file=trim(adjustl(twobody)), form='unformatted')
        read(int_unit) r12
        close(int_unit)

        allocate(f_a(N3,N3), f_b(N3,N3))
        f_a = core
        f_b = core

        call calculate_fock(n1, n2, n3, r12, f_a, f_b, ERef)

        ! Clean integrals
        do i=1,N3
            do j=1,N3
                if (dabs(f_a(i,j)).lt.1.0d-10) f_a(i,j) = 0.0_p
                if (dabs(f_b(i,j)).lt.1.0d-10) f_b(i,j) = 0.0_p
            enddo
        enddo

        do i=N0+1,N3
            do j=N0+1,N3
                do k=N0+1,N3
                    do l=N0+1,N3
                        if (dabs(r12(l,k,j,i)) .lt. 1.0d-10) r12(l,k,j,i) = 0.0_p
                    enddo
                enddo
            enddo
        enddo

        ! Antisymmetrize twobody integrals
        do i=N0+1,N3
            do j=N0+1,N3
                do k=N0+1,N3
                    do l=N0+1,N3
                        v_ab(l,k,j,i) = r12(l,k,j,i)
                        v_aa(l,k,j,i) = r12(l,k,j,i) - r12(k,l,j,i)
                        v_bb(l,k,j,i) = r12(l,k,j,i) - r12(k,l,j,i)
                    enddo
                enddo
            enddo
        enddo


        deallocate(core)
        deallocate(r12)

    end subroutine load_old_integrals

    subroutine unload_integrals(FAHH,FAHP,FAPP,FBHH,FBHP,FBPP, &
            VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP, &
            VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH, &
            VBPHPH,VBHPPP,VBPHPP, &
            VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP)

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

        real(p), allocatable :: VAPPPP(:,:,:,:)
        real(p), allocatable :: VBPPPP(:,:,:,:)
        real(p), allocatable :: VCPPPP(:,:,:,:)

        deallocate(FAHH)
        deallocate(FAHP)
        deallocate(FAPP)
        deallocate(FBHH)
        deallocate(FBHP)
        deallocate(FBPP)
        deallocate(VAHHHH)
        deallocate(VAHHHP)
        deallocate(VAHHPP)
        deallocate(VAHPHP)
        deallocate(VAHPPP)
        deallocate(VBHHHH)
        deallocate(VBHHHP)
        deallocate(VBHHPH)
        deallocate(VBHHPP)
        deallocate(VBHPHP)
        deallocate(VBHPPH)
        deallocate(VBPHPH)
        deallocate(VBHPPP)
        deallocate(VBPHPP)
        deallocate(VCHHHH)
        deallocate(VCHHHP)
        deallocate(VCHHPP)
        deallocate(VCHPHP)
        deallocate(VCHPPP)

        deallocate(VAPPPP)
        deallocate(VBPPPP)
        deallocate(VCPPPP)

    end subroutine unload_integrals

    subroutine calculate_fock(N1,N2,N3,IntM,f_a,f_b,ERef)

        integer, intent(in) :: n1, n2, n3
        real(p), intent(in) :: IntM(N3,N3,N3,N3)

        real(p), intent(in out) :: f_a(N3,N3)
        real(p), intent(in out) :: f_b(N3,N3)
        real(p), intent(in out) :: eref

        integer :: i, j, k

        ! Calculate reference energy
        ERef=0.0_p
        do i=1,N1
            ERef=ERef+f_a(i,i)
        enddo
        do i=1,N2
            ERef=ERef+f_b(i,i)
        enddo

        do i=1,N1
            do j=1,N2
                ERef=ERef+IntM(i,j,i,j)
            enddo
        enddo

        do i=1,N1
            do j=1,N1
                ERef=ERef+0.5d0*(IntM(i,j,i,j)-IntM(i,j,j,i))
            enddo
        enddo

        do i=1,N2
            do j=1,N2
                ERef=ERef+0.5d0*(IntM(i,j,i,j)-IntM(i,j,j,i))
            enddo
        enddo

        ! Calculate Fock Operators
        do i=1,N3
            do j=1,i
                do k=1,N2
                    f_a(i,j)=f_a(i,j)+IntM(i,k,j,k)
                enddo
                do k=1,N1
                    f_a(i,j)=f_a(i,j)+IntM(i,k,j,k)-IntM(i,k,k,j)
                enddo
                f_a(j,i)=f_a(i,j)

                do k=1,N1
                    f_b(i,j)=f_b(i,j)+IntM(k,i,k,j)
                enddo
                do k=1,N2
                    f_b(i,j)=f_b(i,j)+IntM(i,k,j,k)-IntM(i,k,k,j)
                enddo
                f_b(j,i)=f_b(i,j)
            enddo
        enddo

    end subroutine calculate_fock

    subroutine write_integrals(onebody, twobody, orbs)

        integer, intent(in) :: orbs
        real(p) :: onebody(orbs,orbs)
        real(p) :: twobody(orbs, orbs, orbs, orbs)
        integer, parameter :: int_unit = 150

        open(int_unit, file='onebody.inp', form='unformatted')
        write(int_unit) onebody
        close(int_unit)

        open(int_unit, file='twobody.inp', form='unformatted')
        write(int_unit) twobody
        close(int_unit)

    end subroutine write_integrals

    subroutine write_integrals_dbg(erepul, onebody, twobody, orbs)

        real(p) :: erepul
        integer, intent(in) :: orbs
        real(p) :: onebody(orbs,orbs)
        real(p) :: twobody(orbs, orbs, orbs, orbs)
        integer, parameter :: int_unit = 150

        integer :: i, j, a, b

        open(int_unit, file='onebody.inp.txt')
        do i=1, orbs
            do j=1, i
                write(int_unit, '(e28.12,i8)') onebody(i,j), i
            enddo
        enddo
        close(int_unit)

        open(int_unit, file='twobody.inp.txt')
        do i=1,orbs
            do j=1,orbs
                do a=1,orbs
                    do b=1,orbs
                        if (twobody(i,j,a,b) /= 0.0d0) &
                            write(int_unit, '(4i6,e28.12)') i, a, j, b, twobody(i,j,a,b)
                    enddo
                enddo
            enddo
        enddo
        write(int_unit, '(4i6,f18.10)') 0, 0, 0, 0, erepul
        close(int_unit)

    end subroutine write_integrals_dbg

end module integrals
