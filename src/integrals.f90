module integrals
    implicit none

    contains
        subroutine load_integrals(n0, n1, n2, n3, m1, m2, &
                erepul, eref, &
                FAHH,FAHP,FAPP,FBHH,FBHP,FBPP, &
                VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP, &
                VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH, &
                VBPHPH,VBHPPP,VBPHPP, &
                VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP, &
                VAAPPP, VBAPPP, VBPAPP, VCAPPP, &
                onebody, twobody)

            integer, intent(in) :: n0, n1, n2, n3, m1, m2
            real(kind=8), intent(inout) :: erepul, eref
            character(len=*), intent(in) :: onebody, twobody


            integer :: k3, k4

            real(kind=8), allocatable, intent(inout) :: FAHH(:,:)
            real(kind=8), allocatable, intent(inout) :: FAHP(:,:)
            real(kind=8), allocatable, intent(inout) :: FAPP(:,:)
            real(kind=8), allocatable, intent(inout) :: FBHH(:,:)
            real(kind=8), allocatable, intent(inout) :: FBHP(:,:)
            real(kind=8), allocatable, intent(inout) :: FBPP(:,:)
            real(kind=8), allocatable, intent(inout) :: VAHHHH(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VAHHHP(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VAHHPP(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VAHPHP(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VAHPPP(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VBHHHH(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VBHHHP(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VBHHPH(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VBHHPP(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VBHPHP(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VBHPPH(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VBPHPH(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VBHPPP(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VBPHPP(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VCHHHH(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VCHHHP(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VCHHPP(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VCHPHP(:,:,:,:)
            real(kind=8), allocatable, intent(inout) :: VCHPPP(:,:,:,:)

            ! Active-space arrays
            real(kind=8), allocatable :: VAAPPP(:,:,:,:)
            real(kind=8), allocatable :: VBAPPP(:,:,:,:)
            real(kind=8), allocatable :: VBPAPP(:,:,:,:)
            real(kind=8), allocatable :: VCAPPP(:,:,:,:)

            ! Aux matrices
            real(kind=8), allocatable :: VAPPP(:,:,:)
            real(kind=8), allocatable :: VBPPP(:,:,:)
            real(kind=8), allocatable :: VCPPP(:,:,:)

            ! Local variables
            real(kind=8), allocatable :: core(:,:)
            real(kind=8), allocatable :: fock_alpha(:,:)
            real(kind=8), allocatable :: fock_beta(:,:)
            real(kind=8), allocatable :: intm(:,:,:,:)

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
            core=0.0d0
            open(int_unit,file=trim(adjustl(onebody)),form='unformatted')

            read(int_unit) core

            close(int_unit)

            allocate(IntM(N3,N3,N3,N3))
            IntM=0.0d0

            ! Open twobody integrals
            open(int_unit,file=trim(adjustl(twobody)), form='unformatted')

            read(int_unit) intm

            close(int_unit)

            allocate(fock_alpha(N3,N3),fock_beta(N3,N3))
            fock_alpha=Core
            fock_beta=Core
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

        subroutine load_old_integrals(n1, n2, n3, &
                f_a, f_b, v_aa, v_ab, v_bb, &
                onebody, twobody)

            integer, intent(in) :: n1, n2, n3
            character(len=*), intent(in) :: onebody, twobody

            real(kind=8) :: eref

            ! Local variables
            real(kind=8), allocatable :: core(:,:)
            real(kind=8), allocatable :: f_a(:,:)
            real(kind=8), allocatable :: f_b(:,:)
            real(kind=8), allocatable :: v_aa(:,:,:,:)
            real(kind=8), allocatable :: v_ab(:,:,:,:)
            real(kind=8), allocatable :: v_bb(:,:,:,:)

            ! [TODO] implement this
            integer, parameter :: nfrv = 0
            integer, parameter :: int_unit = 150


            integer :: i, j, k, l

            integer, parameter :: iPA = 250, iPB = 251, iPC = 252


            ! Open onebody integrals
            allocate (core(n3,n3))
            core=0.0d0
            open(int_unit,file=trim(adjustl(onebody)), form='unformatted')

            read(int_unit) core

            close(int_unit)


            allocate(v_ab(N3,N3,N3,N3))
            allocate(v_aa(N3,N3,N3,N3))
            allocate(v_bb(N3,N3,N3,N3))
            v_ab=0.0d0
            v_aa=0.0d0
            v_bb=0.0d0

            ! Open twobody integrals
            open(int_unit,file=trim(adjustl(twobody)), form='unformatted')

            read(int_unit) v_ab

            close(int_unit)

            allocate(f_a(N3,N3),f_b(N3,N3))
            f_a = core
            f_b = core
            call calculate_fock(n1,n2,n3,v_ab,f_a,f_b,ERef)

            ! Clean integrals
            do i=1,N3
                do j=1,N3
                    if (dabs(f_a(i,j)).lt.1.0d-10)f_a(i,j)=0.0d0
                    if (dabs(f_b(i,j)).lt.1.0d-10)f_b(i,j)=0.0d0
                enddo
            enddo
            do i=1,N3
                do j=1,N3
                    do k=1,N3
                        do l=1,N3
                            if (dabs(v_ab(l,k,j,i)).lt.1.0d0-10)v_ab(l,k,j,i)=0.0d0
                            v_aa(l,k,j,i) = v_ab(l,k,j,i) - v_ab(k,l,j,i)
                            v_bb(l,k,j,i) = v_ab(l,k,j,i) - v_ab(k,l,j,i)
                        enddo
                    enddo
                enddo
            enddo


            deallocate(core)

        end subroutine load_old_integrals

        subroutine unload_integrals(FAHH,FAHP,FAPP,FBHH,FBHP,FBPP, &
                VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP, &
                VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH, &
                VBPHPH,VBHPPP,VBPHPP, &
                VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP)

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

            real(kind=8), allocatable :: VAPPPP(:,:,:,:)
            real(kind=8), allocatable :: VBPPPP(:,:,:,:)
            real(kind=8), allocatable :: VCPPPP(:,:,:,:)

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

        subroutine calculate_fock(N1,N2,N3,IntM,fock_alpha,fock_beta,ERef)
            integer, intent(in) :: n1, n2, n3
            real(kind=8), intent(in) :: IntM(N3,N3,N3,N3)
            real(kind=8), intent(inout) :: fock_alpha(N3,N3)
            real(kind=8), intent(inout) :: fock_beta(N3,N3)
            real(kind=8), intent(inout) :: eref

            integer :: i, j, k

            ! Calculate reference energy
            ERef=0.0d0
            do i=1,N1
                ERef=ERef+fock_alpha(i,i)
            enddo
            do i=1,N2
                ERef=ERef+fock_beta(i,i)
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
                        fock_alpha(i,j)=fock_alpha(i,j)+IntM(i,k,j,k)
                    enddo
                    do k=1,N1
                        fock_alpha(i,j)=fock_alpha(i,j)+IntM(i,k,j,k)-IntM(i,k,k,j)
                    enddo
                    fock_alpha(j,i)=fock_alpha(i,j)

                    do k=1,N1
                        fock_beta(i,j)=fock_beta(i,j)+IntM(k,i,k,j)
                    enddo
                    do k=1,N2
                        fock_beta(i,j)=fock_beta(i,j)+IntM(i,k,j,k)-IntM(i,k,k,j)
                    enddo
                    fock_beta(j,i)=fock_beta(i,j)
                enddo
            enddo

        end subroutine calculate_fock

        subroutine write_integrals(onebody, twobody, orbs)
            integer, intent(in) :: orbs
            real(kind=8) :: onebody(orbs,orbs)
            real(kind=8) :: twobody(orbs, orbs, orbs, orbs)
            integer, parameter :: int_unit = 150

            open(int_unit, file='onebody.inp', form='unformatted')
            write(int_unit) onebody
            close(int_unit)

            open(int_unit, file='twobody.inp', form='unformatted')
            write(int_unit) twobody
            close(int_unit)

        end subroutine write_integrals

end module integrals
