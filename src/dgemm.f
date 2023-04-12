      subroutine egemm(K1,K2,K3,A,B,C)
      implicit none
      integer K1,K2,K3
      real*8 A(K3,K1),B(K3,K2),C(K2,K1)
      character(len=255) :: tmp_str


      if (k1 * k2 * k3 .eq. 0) then
        C = 0.0d0
      else
        call dgemm('t', 'n', K2, K1, K3, 1.0d0, B, K3, A, K3, 0.0d0,
     &     C, K2)
      endif

      end

      subroutine egemm1(K1,K3,A,B,C)
      implicit none
      integer K1,K3
      real*8 A(K3,K1),B(K3),C(K1)

      if (k1 * k3 .eq. 0) then
        C = 0.0d0
      else
        call dgemv('t', K3, K1, 1.0d0, A, K3, B, 1, 0.0d0, C, 1)
      endif

      end

      subroutine egemm2(K2,K3,A,B,C)
      implicit none
      integer K2,K3
      real*8 A(K3),B(K3,K2),C(K2)

      if (k2 * k3 .eq. 0) then
        C = 0.0d0
      else
        call dgemv('t', K3, K2, 1.0d0, B, K3, A, 1, 0.0d0, C, 1)
      endif

      end

