      subroutine update_lcc(N0,N1,N2,N3,iL,ECor,res,de,shift,
     & K1,K2,K3,K4,K1A,K1B,K2A,K2B,K2C,KKK,
     & FockR,FockB,IntR,IntB,IntM,H1A,H1B,H2A,H2B,H2C,t)
C
      integer a,b,c
      real*8 ECor,de,res,shift
      real*8 FockR(N3,N3)
      real*8 FockB(N3,N3)
      real*8 IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
      real*8 IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
      real*8 IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
      real*8 H1A(N0+1:N3,N0+1:N3)
      real*8 H1B(N0+1:N3,N0+1:N3)
      real*8 H2A(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
      real*8 H2B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
      real*8 H2C(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
      real*8 t(KKK)
      real*8,allocatable::LH(:),l(:)
C
      allocate(LH(KKK),L(KKK))
      LH=0.0d0
      l=0.0d0
      rewind(iL)
      read(iL) L
      iroot = 0
C
      call L_input(N0,N1,N2,N3,iroot,
     & H1A,H1B,H2A,H2B,H2C,l(K1A),l(K1B),l(K2A),l(K2B),l(K2C),
     & LH(K1A),LH(K1B),LH(K2A),LH(K2B),LH(K2C))
C
      call L1A_update(N0,N1,N2,N3,LH(K1A),
     & K1,K2,K3,K4,
     & FockR,FockB,IntR,IntB,IntM,
     & H1A,H1B,H2A,H2B,H2C,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))
C
      call L1B_update(N0,N1,N2,N3,LH(K1B),
     & K1,K2,K3,K4,
     & FockR,FockB,IntR,IntB,IntM,
     & H1A,H1B,H2A,H2B,H2C,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))
C
      call L2A_update(N0,N1,N2,N3,LH(K2A),
     & K1,K2,K3,K4,
     & FockR,FockB,IntR,IntB,IntM,
     & H1A,H1B,H2A,H2B,H2C,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))
C
      call L2B_update(N0,N1,N2,N3,LH(K2B),
     & K1,K2,K3,K4,
     & FockR,FockB,IntR,IntB,IntM,
     & H1A,H1B,H2A,H2B,H2C,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))
C
      call L2C_update(N0,N1,N2,N3,LH(K2C),
     & K1,K2,K3,K4,
     & FockR,FockB,IntR,IntB,IntM,
     & H1A,H1B,H2A,H2B,H2C,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & l(K1A),l(K1B),l(K2A),l(K2B),l(K2C))
!goto 1000
C
      if(iroot.ne.0)LH=LH-ECor*L
      call L_update(N0,N1,N2,N3,shift,H1A,H1B,ECor,
     & l(K1A),l(K1B),l(K2A),l(K2B),l(K2C),
     & LH(K1A),LH(K1B),LH(K2A),LH(K2B),LH(K2C))
C
      de=0.0d0
      do i=1,KKK
        de=de+LH(i)*LH(i)
      enddo
      de=dsqrt(de)
!      write(6,*)'|L*H-L*E|=',res
C
      rewind(iL)
      read(iL)LH
      rewind(iL)
      write(iL)L
      L=L-LH
      res=0.0d0
      do i=1,KKK
        res=res+L(i)*L(i)
      enddo
      res=dsqrt(res)
      deallocate(LH,L)
      end
