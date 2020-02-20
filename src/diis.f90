module diis
    implicit none

    contains

        subroutine calc_diis(t_vecs_unit, diis_space, t_size, t)

            ! Input vars
            integer, intent(in) :: t_vecs_unit
            integer, intent(in) :: diis_space
            integer, intent(in) :: t_size

            real(kind=8), allocatable, intent(inout) :: t(:)

            ! Local vars
            integer, allocatable :: ipiv(:)
            integer :: info
            integer :: indx, indy

            real(kind=8), allocatable :: t_aux_1(:), t_aux_2(:), t_aux_3(:)
            real(kind=8), allocatable :: B(:,:), C(:)

            real(kind=8) :: accum
            real(kind=8) ddot

            ! Allocate
            allocate(B(diis_space+1,diis_space+1))
            B=0.0d0
            allocate(t_aux_1(t_size))
            allocate(t_aux_2(t_size))
            allocate(t_aux_3(t_size))

            do indx = 1, diis_space

                ! Generate difference vector
                read(t_vecs_unit, rec=indx) t_aux_1
                read(t_vecs_unit, rec=indx+1) t_aux_2

                t_aux_3 = t_aux_2 - t_aux_1

                do indy = 1, diis_space

                    ! Generate difference vector
                    read(t_vecs_unit, rec=indy) t_aux_1
                    read(t_vecs_unit, rec=indy+1) t_aux_2

                    t_aux_2 = t_aux_2 - t_aux_1

                    accum = 0.0d0
                    accum = ddot(t_size, t_aux_3, 1, t_aux_2, 1)

                    B(indx,indy) = B(indx,indy) + accum
                enddo
            enddo

            deallocate(t_aux_2, t_aux_3)

            ! Set DIIS matrix boundaries
            b(:,diis_space+1) = -1.0d0
            b(diis_space+1,:) = -1.0d0

            !allocate(L(iDIIS+1),C(iDIIS+1))
            allocate(ipiv(diis_space+1))
            allocate(c(diis_space+1))

            c=0.0d0
            c(diis_space+1) = -1.0d0

            ! Solve system of equations
            call dgesv(diis_space+1, 1, B, diis_space+1, ipiv, c, diis_space+1, info)

            if (info /= 0) call abort_cc('DIIS error.')

            t = 0.0d0
            do indx = 1, diis_space
                read(t_vecs_unit, rec=indx) t_aux_1

                call daxpy(t_size, C(indx), t_aux_1, 1, t, 1)

            enddo

            deallocate(t_aux_1)
            deallocate(ipiv, c, b)

        end subroutine calc_diis

        subroutine write_t_vecs(t_vecs_unit, iter, diis_space, t)

            integer, intent(in) :: t_vecs_unit
            integer, intent(in) :: iter
            integer, intent(in) :: diis_space

            real(kind=8), allocatable, intent(in) :: t(:)

            integer :: indx_rec

            indx_rec = mod(iter, diis_space + 1)
            if (indx_rec == 0) indx_rec = diis_space + 1

            write(t_vecs_unit, rec=indx_rec) t

        end subroutine write_t_vecs


    end module diis
