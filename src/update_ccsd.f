      subroutine update_ccsd(n0, n1, n2, n3,
     &  t_order, t_pos, t_size, t, shift,
     &  FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     &  VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     &  VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     &  VBPHPH,VBHPPP,VBPHPP,
     &  VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP)
C
        implicit none

       integer n0, n1, n2, n3
       integer k1, k2, k3, k4
       integer k1a, k1b, k2a, k2b, k2c
       integer i
       logical ifrhf
       integer t_order
       integer t_size
       integer t_pos(t_order)

       real(kind=8) shift
       real(kind=8) t(t_size)

       integer, parameter :: iPA = 250, iPB = 251, iPC = 252
C
       real*8 FAHH(N0+1:N1,N0+1:N1)
       real*8 FAHP(N1+1:N3,N0+1:N1)
       real*8 FAPP(N1+1:N3,N1+1:N3)
       real*8 FBHH(N0+1:N2,N0+1:N2)
       real*8 FBHP(N2+1:N3,N0+1:N2)
       real*8 FBPP(N2+1:N3,N2+1:N3)
       real*8 VAHHHH(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 VAHHHP(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 VAHHPP(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 VAHPHP(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1)
       real*8 VAHPPP(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1)
       real*8 VBHHHH(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1)
       real*8 VBHHHP(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1)
       real*8 VBHHPH(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 VBHHPP(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 VBHPHP(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1)
       real*8 VBHPPH(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1)
       real*8 VBPHPH(N0+1:N2,N1+1:N3,N0+1:N2,N1+1:N3)
       real*8 VBHPPP(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1)
       real*8 VBPHPP(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3)
       real*8 VCHHHH(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2)
       real*8 VCHHHP(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
       real*8 VCHHPP(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 VCHPHP(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2)
       real*8 VCHPPP(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2)

C
C allocatable arrays
C
       real*8,allocatable::V1A(:,:)
       real*8,allocatable::V1B(:,:)
       real*8,allocatable::V2A(:,:,:,:)
       real*8,allocatable::V2B(:,:,:,:)
       real*8,allocatable::V2C(:,:,:,:)

       ifrhf = .false.
C
C allocate array
C
      K1=N1-N0;K3=N3-N1 !OCC
      K2=N2-N0;K4=N3-N2 !VIR

       t_size=0

       ! T1 alpha size
       K1A =t_size+1
       t_size=t_size+K1*K3

       ! T1 beta size
       K1B =t_size+1
       t_size=t_size+K2*K4

       ! T2 alpha-alpha size
       K2A =t_size+1
       t_size=t_size+K1*K1*K3*K3

       ! T2 alpha-beta size
       K2B =t_size+1
       t_size=t_size+K1*K2*K3*K4

       ! T2 beta-beta size
       K2C =t_size+1
       t_size=t_size+K2*K2*K4*K4
C
       allocate(V2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       V2B=VBHHPP
C
       call ccsd_t2B_update(N0,N1,N2,N3,V2B,shift,
     & K1,K2,K3,K4,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
C
       call ccsd_t2B_update1(N0,N1,N2,N3,V2B,shift,
     & iPB,K1,K2,K3,K4,
     & FAHH,FAPP,FBHH,FBPP,t(K1A),t(K1B),t(K2B))
       deallocate(V2B)
C
       allocate(V2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       V2A=VAHHPP
C      
       call ccsd_t2A_update(N0,N1,N2,N3,V2A,shift,
     & K1,K2,K3,K4,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
C
       call ccsd_t2A_update1(N0,N1,N3,V2A,shift,
     & iPA,K1,K3,FAHH,FAPP,t(K1A),t(K2A))
       deallocate(V2A)
C
      if(ifRHF)then
       do i=K2A,K2B-1
        t(i+K2C-K2A)=t(i)
       enddo
      else
C
       allocate(V2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       V2C=VCHHPP
C
       call ccsd_t2C_update(N0,N1,N2,N3,V2C,shift,
     & K1,K2,K3,K4,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
C
       call ccsd_t2C_update1(N0,N2,N3,V2C,shift,
     & iPC,K2,K4,FBHH,FBPP,t(K1B),t(K2C))
       deallocate(V2C)
       endif
C
       allocate(V1A(N1+1:N3,N0+1:N1))
       V1A=FAHP
C
       call ccsd_t1A_update(N0,N1,N2,N3,V1A,shift,
     & K1,K2,K3,K4,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
       deallocate(V1A)
C
      if(ifRHF)then
       do i=K1A,K1B-1
        t(i+K1B-K1A)=t(i)
       enddo
      else
       allocate(V1B(N2+1:N3,N0+1:N2))
       V1B=FBHP
C
       call ccsd_t1B_update(N0,N1,N2,N3,V1B,shift,
     & K1,K2,K3,K4,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
       deallocate(V1B)
       endif
C
       end

