      subroutine update_cc(n0, n1, n2, n3, m1, m2,
     &  t_order, t_pos, t_size, t, shift,
     &  FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     &  VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     &  VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     &  VBPHPH,VBHPPP,VBPHPP,
     &  VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     &  VAAPPP, VBAPPP, VBPAPP, VCAPPP)

        implicit none

       integer n0, n1, n2, n3
       integer k1, k2, k3, k4, k5, k6, k7, k8, k9, k0
       integer k1a, k1b, k2a, k2b, k2c
       integer k3a, k3b1, k3b2, k3b3, k3b4
       integer k3c1, k3c2, k3c3, k3c4, k3d
       integer m1, m2
       integer t_order
       integer t_size
       integer t_pos(t_order)
       integer, parameter :: iPA = 250, iPB = 251, iPC = 252
       real(kind=8) shift
       real(kind=8) t(t_size)
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

       real*8 VAAPPP(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:M2)
       real*8 VBAPPP(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:M2)
       real*8 VBPAPP(N2+1:N3,N1+1:N3,N2+1:M2,N1+1:N3)
       real*8 VCAPPP(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:M2)

       real*8,allocatable::HT1A(:,:)
       real*8,allocatable::HT1B(:,:)

       real*8,allocatable::HT2A(:,:,:,:)
       real*8,allocatable::HT2B(:,:,:,:)
       real*8,allocatable::HT2C(:,:,:,:)

       real*8,allocatable::HT3A(:,:,:,:,:,:)

       real*8,allocatable::HT3B1(:,:,:,:,:,:)
       real*8,allocatable::HT3B2(:,:,:,:,:,:)
       real*8,allocatable::HT3B3(:,:,:,:,:,:)
       real*8,allocatable::HT3B4(:,:,:,:,:,:)

       real*8,allocatable::HT3C1(:,:,:,:,:,:)
       real*8,allocatable::HT3C2(:,:,:,:,:,:)
       real*8,allocatable::HT3C3(:,:,:,:,:,:)
       real*8,allocatable::HT3C4(:,:,:,:,:,:)

       real*8,allocatable::HT3D(:,:,:,:,:,:)

       shift = 0.0d0


       ! K1 = # of occ alpha
       K1=N1-N0
       ! K3 = # of unocc alpha
       K3=N3-N1
       ! K2 = # of occ beta
       K2=N2-N0
       ! K4 = # of unocc beta
       K4=N3-N2
       ! K5 = # of non-active occ
       K5=M1-N0
       ! K6 = # of non-active unocc
       K6=N3-M2
       ! K7 = # of active occ alpha
       K7=N1-M1
       ! K8 = # of active occ beta
       K8=N2-M1
       ! K9 = # of active unocc alpha
       K9=M2-N1
       ! K0 = # of active unocc beta
       K0=M2-N2

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

       ! T3 sizes
       K3A =t_size+1
       t_size=t_size+K3*K3*K9*K1*K1*K7  !1**1**
       K3B1=t_size+1
       t_size=t_size+K4*K3*K9*K2*K1*K7  !1**1**
       K3B2=t_size+1
       t_size=t_size+K0*K6*K6*K8*K5*K5  !001001
       K3B3=t_size+1
       t_size=t_size+K0*K6*K6*K2*K1*K7  !1**001
       K3B4=t_size+1
       t_size=t_size+K4*K3*K9*K8*K5*K5  !0011**
       K3C1=t_size+1
       t_size=t_size+K4*K0*K3*K2*K8*K1  !*1**1*
       K3C2=t_size+1
       t_size=t_size+K6*K6*K9*K5*K5*K7  !100100
       K3C3=t_size+1
       t_size=t_size+K6*K6*K9*K2*K8*K1  !*1*100
       K3C4=t_size+1
       t_size=t_size+K4*K0*K3*K5*K5*K7  !100*1*
       K3D =t_size+1
       t_size=t_size+K4*K4*K0*K2*K2*K8  !1**1**

C
         allocate(HT2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
         HT2B=0.0d0
C
         if(K5.ge.1.and.K6.ge.1)then
           call t2B0000_update(N0,N1,N2,N3,HT2B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
         endif
C
         if(K5.ge.1.and.K6.ge.1.and.K0.ge.1)then
           call t2B0001_update(N0,N1,N2,N3,HT2B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
         endif
C
         if(K5.ge.1.and.K6.ge.1.and.K9.ge.1)then
           call t2B0010_update(N0,N1,N2,N3,HT2B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K0.ge.1.and.K9.ge.1)then
            call t2B0011_update(N0,N1,N2,N3,HT2B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K6.ge.1.and.K8.ge.1)then
            call t2B0100_update(N0,N1,N2,N3,HT2B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K6.ge.1.and.K8.ge.1.and.K0.ge.1)then
            call t2B0101_update(N0,N1,N2,N3,HT2B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K6.ge.1.and.K8.ge.1.and.K9.ge.1)then
            call t2B0110_update(N0,N1,N2,N3,HT2B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K8.ge.1.and.K9.ge.1.and.k0.ge.1)then
            call t2B0111_update(N0,N1,N2,N3,HT2B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K6.ge.1.and.K7.ge.1)then
            call t2B1000_update(N0,N1,N2,N3,HT2B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K6.ge.1.and.K7.ge.1.and.K0.ge.1)then
            call t2B1001_update(N0,N1,N2,N3,HT2B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K6.ge.1.and.K7.ge.1.and.K9.ge.1)then
            call t2B1010_update(N0,N1,N2,N3,HT2B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K0.ge.1.and.K7.ge.1.and.K9.ge.1)then
            call t2B1011_update(N0,N1,N2,N3,HT2B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K6.ge.1.and.K7.ge.1.and.K8.ge.1)then
            call t2B1100_update(N0,N1,N2,N3,HT2B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K6.ge.1.and.K7.ge.1.and.K8.ge.1.and.K0.ge.1)then
            call t2B1101_update(N0,N1,N2,N3,HT2B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K6.ge.1.and.K7.ge.1.and.K8.ge.1.and.K9.ge.1)then
            call t2B1110_update(N0,N1,N2,N3,HT2B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K9.ge.1.and.K7.ge.1.and.K8.ge.1.and.K0.ge.1)then
            call t2B1111_update(N0,N1,N2,N3,HT2B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          HT2B=HT2B+VBHHPP
          call t2B_update(N0,N1,N2,N3,HT2B,shift,
     & K1,K2,K3,K4,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
C
          call t2B_update1(N0,N1,N2,N3,HT2B,shift,
     & iPB,K1,K2,K3,K4,
     & FAHH,FAPP,FBHH,FBPP,t(K1A),t(K1B),t(K2B))
          deallocate(HT2B)
C
C
          allocate(HT2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
          HT2A=0.0d0
C
          if(K5.ge.2.and.K6.ge.2)then
            call t2A0000_update(N0,N1,N2,N3,HT2A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.2.and.K6.ge.1.and.K9.ge.1)then
            call t2A0010_update(N0,N1,N2,N3,HT2A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.2.and.K9.ge.2)then
            call t2A0011_update(N0,N1,N2,N3,HT2A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K6.ge.2.and.K7.ge.1)then
            call t2A1000_update(N0,N1,N2,N3,HT2A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K6.ge.1.and.K7.ge.1.and.K9.ge.1)then
            call t2A1010_update(N0,N1,N2,N3,HT2A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K7.ge.1.and.K9.ge.2)then
            call t2A1011_update(N0,N1,N2,N3,HT2A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K7.ge.2.and.K6.ge.2)then
            call t2A1100_update(N0,N1,N2,N3,HT2A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K7.ge.2.and.K6.ge.1.and.K9.ge.1)then
            call t2A1110_update(N0,N1,N2,N3,HT2A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K7.ge.2.and.K9.ge.2)then
            call t2A1111_update(N0,N1,N2,N3,HT2A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          HT2A=HT2A+VAHHPP
          call t2A_update(N0,N1,N2,N3,HT2A,shift,
     & K1,K2,K3,K4,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
C
          call t2A_update1(N0,N1,N3,HT2A,shift,
     & iPA,K1,K3,FAHH,FAPP,t(K1A),t(K2A))
          deallocate(HT2A)
C
          allocate(HT2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
          HT2C=0.0d0
C
          if(K5.ge.2.and.K6.ge.2)then
            call t2C0000_update(N0,N1,N2,N3,HT2C,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.2.and.K6.ge.1.and.K0.ge.1)then
            call t2C0010_update(N0,N1,N2,N3,HT2C,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.2.and.K0.ge.2)then
            call t2C0011_update(N0,N1,N2,N3,HT2C,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K6.ge.2.and.K8.ge.1)then
            call t2C1000_update(N0,N1,N2,N3,HT2C,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K6.ge.1.and.K8.ge.1.and.K0.ge.1)then
            call t2C1010_update(N0,N1,N2,N3,HT2C,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K8.ge.1.and.K0.ge.2)then
            call t2C1011_update(N0,N1,N2,N3,HT2C,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K6.ge.2.and.K8.ge.2)then
            call t2C1100_update(N0,N1,N2,N3,HT2C,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K6.ge.1.and.K8.ge.2.and.K0.ge.1)then
            call t2C1110_update(N0,N1,N2,N3,HT2C,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K8.ge.2.and.K0.ge.2)then
            call t2C1111_update(N0,N1,N2,N3,HT2C,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          HT2C=HT2C+VCHHPP
          call t2C_update(N0,N1,N2,N3,HT2C,shift,
     & K1,K2,K3,K4,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
C
          call t2C_update1(N0,N2,N3,HT2C,shift,
     & iPC,K2,K4,FBHH,FBPP,t(K1B),t(K2C))
          deallocate(HT2C)
C
C
          allocate(HT1A(N1+1:N3,N0+1:N1))
          HT1A=0.0d0
C
          if(K5.ge.1.and.K6.ge.1)then
            call t1A00_update(N0,N1,N2,N3,HT1A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K9.ge.1)then
            call t1A01_update(N0,N1,N2,N3,HT1A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K6.ge.1.and.K7.ge.1)then
            call t1A10_update(N0,N1,N2,N3,HT1A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K9.ge.1.and.K7.ge.1)then
C
            call t1A11_update(N0,N1,N2,N3,HT1A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          HT1A=HT1A+FAHP
          call t1A_update(N0,N1,N2,N3,HT1A,shift,
     & K1,K2,K3,K4,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
          deallocate(HT1A)
C
C
          allocate(HT1B(N2+1:N3,N0+1:N2))
          HT1B=0.0d0
C
          if(K5.ge.1.and.K6.ge.1)then
            call t1B00_update(N0,N1,N2,N3,HT1B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K0.ge.1)then
            call t1B01_update(N0,N1,N2,N3,HT1B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K6.ge.1.and.K8.ge.1)then
            call t1B10_update(N0,N1,N2,N3,HT1B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K0.ge.1.and.K8.ge.1)then
            call t1B11_update(N0,N1,N2,N3,HT1B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          HT1B=HT1B+FBHP
          call t1B_update(N0,N1,N2,N3,HT1B,shift,
     & K1,K2,K3,K4,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C))
          deallocate(HT1B)
C
        allocate(HT3B1(N2+1:N3,N1+1:N3,N1+1:M2,N0+1:N2,N0+1:N1,M1+1:N1))
          HT3B1=0.0d0
C
          if(K5.ge.1.and.K6.ge.1.and.K7.ge.1.and.K9.ge.1)then
            call t3B100100_update(N0,N1,N2,N3,HT3B1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
      if(K5.ge.1.and.K6.ge.1.and.K7.ge.1.and.K9.ge.1.and.K0.ge.1)then
            call t3B100101_update(N0,N1,N2,N3,HT3B1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K6.ge.1.and.K7.ge.1.and.K9.ge.2)then
            call t3B100110_update(N0,N1,N2,N3,HT3B1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
          if(K5.ge.1.and.K7.ge.1.and.K9.ge.2.and.K0.ge.1)then
            call t3B100111_update(N0,N1,N2,N3,HT3B1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
      if(K5.ge.1.and.K6.ge.1.and.K7.ge.1.and.K9.ge.1.and.K8.ge.1)then
            call t3B101100_update(N0,N1,N2,N3,HT3B1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
          endif
C
      if(K5.ge.1.and.K6.ge.1.and.K7.ge.1.and.K9.ge.1.and.K8.ge.1
     & .and.K0.ge.1)then
          call t3B101101_update(N0,N1,N2,N3,HT3B1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.1.and.K6.ge.1.and.K7.ge.1.and.K9.ge.2.and.K8.ge.1)then
          call t3B101110_update(N0,N1,N2,N3,HT3B1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.1.and.K0.ge.1.and.K7.ge.1.and.K9.ge.2.and.K8.ge.1)then
          call t3B101111_update(N0,N1,N2,N3,HT3B1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.1.and.K6.ge.1.and.K7.ge.2.and.K9.ge.1)then
          call t3B110100_update(N0,N1,N2,N3,HT3B1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.1.and.K6.ge.1.and.K7.ge.2.and.K9.ge.1.and.K0.ge.1)then
          call t3B110101_update(N0,N1,N2,N3,HT3B1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.1.and.K6.ge.1.and.K7.ge.2.and.K9.ge.2)then
          call t3B110110_update(N0,N1,N2,N3,HT3B1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.1.and.K7.ge.2.and.K9.ge.2.and.K0.ge.1)then
          call t3B110111_update(N0,N1,N2,N3,HT3B1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K7.ge.2.and.K8.ge.1.and.K9.ge.1.and.K6.ge.1)then
          call t3B111100_update(N0,N1,N2,N3,HT3B1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K7.ge.2.and.K8.ge.1.and.K9.ge.1.and.K6.ge.1.and.K0.ge.1)then
          call t3B111101_update(N0,N1,N2,N3,HT3B1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K7.ge.2.and.K8.ge.1.and.K9.ge.2.and.K6.ge.1)then
          call t3B111110_update(N0,N1,N2,N3,HT3B1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K7.ge.2.and.K8.ge.1.and.K9.ge.2.and.K0.ge.1)then
          call t3B111111_update(N0,N1,N2,N3,HT3B1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        allocate(HT3B4(N2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
        HT3B4=0.0d0
C
        if(K5.ge.2.and.K8.ge.1.and.K9.ge.1.and.K6.ge.1)then
          call t3B001100_update(N0,N1,N2,N3,HT3B4,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.2.and.K8.ge.1.and.K9.ge.1.and.K6.ge.1.and.K0.ge.1)then
          call t3B001101_update(N0,N1,N2,N3,HT3B4,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.2.and.K8.ge.1.and.K9.ge.2.and.K6.ge.1)then
          call t3B001110_update(N0,N1,N2,N3,HT3B4,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.2.and.K8.ge.1.and.K9.ge.2.and.K0.ge.1)then
          call t3B001111_update(N0,N1,N2,N3,HT3B4,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        allocate(HT3C1(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:N2,M1+1:N2,N0+1:N1))
        HT3C1=0.0d0
C
        if(K5.ge.1.and.K8.ge.1.and.K6.ge.1.and.K0.ge.1)then
          call t3C010010_update(N0,N1,N2,N3,HT3C1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.1.and.K8.ge.1.and.K6.ge.1.and.K0.ge.2)then
          call t3C010011_update(N0,N1,N2,N3,HT3C1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.1.and.K8.ge.1.and.K6.ge.1.and.K9.ge.1.and.K0.ge.1)then
          call t3C010110_update(N0,N1,N2,N3,HT3C1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.1.and.K8.ge.1.and.K9.ge.1.and.K0.ge.2)then
          call t3C010111_update(N0,N1,N2,N3,HT3C1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.1.and.K8.ge.2.and.K6.ge.1.and.K0.ge.1)then
          call t3C011010_update(N0,N1,N2,N3,HT3C1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.1.and.K8.ge.2.and.K6.ge.1.and.K0.ge.1)then
          call t3C011011_update(N0,N1,N2,N3,HT3C1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.1.and.K8.ge.2.and.K6.ge.1.and.K9.ge.1.and.K0.ge.1)then
          call t3C011110_update(N0,N1,N2,N3,HT3C1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.1.and.K8.ge.2.and.K9.ge.1.and.K0.ge.2)then
          call t3C011111_update(N0,N1,N2,N3,HT3C1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.1.and.K7.ge.1.and.K8.ge.1.and.K6.ge.1.and.K0.ge.1)then
          call t3C110010_update(N0,N1,N2,N3,HT3C1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.1.and.K7.ge.1.and.K8.ge.1.and.K6.ge.1.and.K0.ge.2)then
          call t3C110011_update(N0,N1,N2,N3,HT3C1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
        endif
C
        if(K5.ge.1.and.K7.ge.1.and.K8.ge.1.and.K6.ge.1.and.K0.ge.1
     & .and.K9.ge.1)then
        call t3C110110_update(N0,N1,N2,N3,HT3C1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K5.ge.1.and.K7.ge.1.and.K8.ge.1.and.K9.ge.1.and.K0.ge.2)then
        call t3C110111_update(N0,N1,N2,N3,HT3C1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K7.ge.1.and.K8.ge.2.and.K6.ge.1.and.K0.ge.1)then
        call t3C111010_update(N0,N1,N2,N3,HT3C1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K7.ge.1.and.K8.ge.2.and.K6.ge.1.and.K0.ge.2)then
        call t3C111011_update(N0,N1,N2,N3,HT3C1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K7.ge.1.and.K8.ge.2.and.K6.ge.1.and.K0.ge.1.and.K9.ge.1)then
        call t3C111110_update(N0,N1,N2,N3,HT3C1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K7.ge.1.and.K8.ge.2.and.K0.ge.2.and.K9.ge.1)then
        call t3C111111_update(N0,N1,N2,N3,HT3C1,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      allocate(HT3C4(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
      HT3C4=0.0d0
C
      if(K7.ge.1.and.K5.ge.2.and.K0.ge.1.and.K6.ge.1)then
        call t3C100010_update(N0,N1,N2,N3,HT3C4,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K7.ge.1.and.K5.ge.2.and.K0.ge.2.and.K6.ge.1)then
        call t3C100011_update(N0,N1,N2,N3,HT3C4,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K7.ge.1.and.K5.ge.2.and.K0.ge.1.and.K6.ge.1.and.K9.ge.1)then
        call t3C100110_update(N0,N1,N2,N3,HT3C4,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K7.ge.1.and.K5.ge.2.and.K0.ge.2.and.K9.ge.1)then
        call t3C100111_update(N0,N1,N2,N3,HT3C4,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      call t3BC_update(N0,N1,N2,N3,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,iPB,
     & FAHH,FAPP,FBHH,FBPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3B1),t(K3B4),t(K3C1),t(K3C4),
     & HT3B1,HT3B4,HT3C1,HT3C4)
      deallocate(HT3B1,HT3B4,HT3C1,HT3C4)
C
      allocate(HT3A(N1+1:N3,N1+1:N3,N1+1:M2,N0+1:N1,N0+1:N1,M1+1:N1))
      HT3A=0.0d0
C
      if(K7.ge.1.and.K5.ge.2.and.K6.ge.2.and.K9.ge.1)then
        call t3A100100_update(N0,N1,N2,N3,HT3A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K7.ge.1.and.K5.ge.2.and.K6.ge.1.and.K9.ge.2)then
        call t3A100110_update(N0,N1,N2,N3,HT3A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K7.ge.1.and.K5.ge.2.and.K9.ge.3)then
        call t3A100111_update(N0,N1,N2,N3,HT3A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K7.ge.2.and.K5.ge.1.and.K6.ge.2.and.K9.ge.1)then
        call t3A110100_update(N0,N1,N2,N3,HT3A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K7.ge.2.and.K5.ge.1.and.K6.ge.1.and.K9.ge.2)then
        call t3A110110_update(N0,N1,N2,N3,HT3A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K7.ge.2.and.K5.ge.1.and.K9.ge.3)then
        call t3A110111_update(N0,N1,N2,N3,HT3A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K7.ge.3.and.K6.ge.2.and.K9.ge.1)then
        call t3A111100_update(N0,N1,N2,N3,HT3A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K7.ge.3.and.K6.ge.1.and.K9.ge.2)then
        call t3A111110_update(N0,N1,N2,N3,HT3A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K7.ge.3.and.K9.ge.3)then
        call t3A111111_update(N0,N1,N2,N3,HT3A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      allocate(HT3B2(N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1))
      HT3B2=0.0d0
C
      if(K5.ge.2.and.K6.ge.2.and.K8.ge.1.and.K0.ge.1)then
        call t3B001001_update(N0,N1,N2,N3,HT3B2,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      allocate(HT3B3(N2+1:M2,M2+1:N3,M2+1:N3,N0+1:N2,N0+1:N1,M1+1:N1))
      HT3B3=0.0d0
C
      if(K5.ge.1.and.K6.ge.2.and.K7.ge.1.and.K0.ge.1)then
        call t3B100001_update(N0,N1,N2,N3,HT3B3,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K5.ge.1.and.K6.ge.2.and.K7.ge.1.and.K8.ge.1.and.K0.ge.1)then
        call t3B101001_update(N0,N1,N2,N3,HT3B3,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K5.ge.1.and.K6.ge.2.and.K7.ge.2.and.K0.ge.1)then
        call t3B110001_update(N0,N1,N2,N3,HT3B3,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K6.ge.2.and.K7.ge.2.and.K8.ge.1.and.K0.ge.1)then
        call t3B111001_update(N0,N1,N2,N3,HT3B3,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      call t3AB_update(N0,N1,N2,N3,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,iPA,
     & FAHH,FAPP,FBHH,FBPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B2),t(K3B3),
     & HT3A,HT3B2,HT3B3)
      deallocate(HT3A,HT3B2,HT3B3)
C
      allocate(HT3C2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
      HT3C2=0.0d0
C
      if(K5.ge.2.and.K6.ge.2.and.K7.ge.1.and.K9.ge.1)then
        call t3C100100_update(N0,N1,N2,N3,HT3C2,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      allocate(HT3C3(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:N2,M1+1:N2,N0+1:N1))
      HT3C3=0.0d0
C
      if(K5.ge.1.and.K6.ge.2.and.K8.ge.1.and.K9.ge.1)then
        call t3C010100_update(N0,N1,N2,N3,HT3C3,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K5.ge.1.and.K6.ge.2.and.K8.ge.2.and.K9.ge.1)then
        call t3C011100_update(N0,N1,N2,N3,HT3C3,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K5.ge.1.and.K6.ge.2.and.K7.ge.1.and.K8.ge.1.and.K9.ge.1)then
        call t3C110100_update(N0,N1,N2,N3,HT3C3,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K6.ge.2.and.K7.ge.1.and.K8.ge.2.and.K9.ge.1)then
        call t3C111100_update(N0,N1,N2,N3,HT3C3,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      allocate(HT3D(N2+1:N3,N2+1:N3,N2+1:M2,N0+1:N2,N0+1:N2,M1+1:N2))
      HT3D=0.0d0
C
      if(K5.ge.2.and.K6.ge.2.and.K8.ge.1.and.K0.ge.1)then
        call t3D100100_update(N0,N1,N2,N3,HT3D,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K5.ge.2.and.K6.ge.1.and.K8.ge.1.and.K0.ge.2)then
        call t3D100110_update(N0,N1,N2,N3,HT3D,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K5.ge.2.and.K8.ge.1.and.K0.ge.3)then
        call t3D100111_update(N0,N1,N2,N3,HT3D,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K5.ge.1.and.K6.ge.2.and.K8.ge.2.and.K0.ge.1)then
        call t3D110100_update(N0,N1,N2,N3,HT3D,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K5.ge.1.and.K6.ge.1.and.K8.ge.2.and.K0.ge.2)then
        call t3D110110_update(N0,N1,N2,N3,HT3D,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K5.ge.1.and.K8.ge.2.and.K0.ge.3)then
        call t3D110111_update(N0,N1,N2,N3,HT3D,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K6.ge.2.and.K8.ge.3.and.K0.ge.1)then
        call t3D111100_update(N0,N1,N2,N3,HT3D,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K6.ge.1.and.K8.ge.3.and.K0.ge.2)then
        call t3D111110_update(N0,N1,N2,N3,HT3D,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      if(K8.ge.3.and.K0.ge.3)then
        call t3D111111_update(N0,N1,N2,N3,HT3D,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3A),t(K3B1),t(K3B2),t(K3B3),t(K3B4),
     & t(K3C1),t(K3C2),t(K3C3),t(K3C4),t(K3D))
      endif
C
      call t3CD_update(N0,N1,N2,N3,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,iPC,
     & FAHH,FAPP,FBHH,FBPP,
     & t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),
     & t(K3C2),t(K3C3),t(K3D),
     & HT3C2,HT3C3,HT3D)
      deallocate(HT3D,HT3C2,HT3C3)
C

      end
