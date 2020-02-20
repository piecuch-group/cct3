       subroutine t1B01_update(N0,N1,N2,N3,HT1B,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t1A,t1B,t2A,t2B,t2C,
     & t3A,t3B1,t3B2,t3B3,t3B4,t3C1,t3C2,t3C3,t3C4,t3D)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 CoeLeft,shift,PP
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
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 t3A(N1+1:N3,N1+1:N3,N1+1:M2,N0+1:N1,N0+1:N1,M1+1:N1)
       real*8 t3B1(N2+1:N3,N1+1:N3,N1+1:M2,N0+1:N2,N0+1:N1,M1+1:N1)
       real*8 t3B2(N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1)
       real*8 t3B3(N2+1:M2,M2+1:N3,M2+1:N3,N0+1:N2,N0+1:N1,M1+1:N1)
       real*8 t3B4(N2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1)
       real*8 t3C1(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:N2,M1+1:N2,N0+1:N1)
       real*8 t3C2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1)
       real*8 t3C3(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:N2,M1+1:N2,N0+1:N1)
       real*8 t3C4(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N1)
       real*8 t3D(N2+1:N3,N2+1:N3,N2+1:M2,N0+1:N2,N0+1:N2,M1+1:N2)
       real*8 HT1B(N2+1:N3,N0+1:N2)
C
       real*8,allocatable::V1B(:,:)
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F1(:,:,:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
       real*8,allocatable::Z1(:,:)
       real*8,allocatable::Z2(:,:)
       real*8,allocatable::Z3(:,:)
       real*8,allocatable::Z4(:,:)
       real*8,allocatable::Z5(:,:)
       real*8,allocatable::Z6(:,:)
       real*8,allocatable::Z7(:,:)
       real*8,allocatable::Z8(:,:)
       real*8,allocatable::Z9(:,:)
       real*8,allocatable::Z10(:,:)
       real*8,allocatable::Z11(:,:)
       real*8,allocatable::Z12(:,:)
       real*8,allocatable::Z13(:,:)
       real*8,allocatable::Z14(:,:)
       real*8,allocatable::Z15(:,:)
       real*8,allocatable::Z16(:,:)
       real*8,allocatable::Z17(:,:)
       real*8,allocatable::Z18(:,:)
       real*8,allocatable::Z19(:,:)
       real*8,allocatable::Z20(:,:)
       real*8,allocatable::Z21(:,:)
       real*8,allocatable::Z22(:,:)
       real*8,allocatable::Z23(:,:)
       real*8,allocatable::Z24(:,:)
C
       allocate(V1B(N2+1:M2,N0+1:M1))
       V1B=0.0d0
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder562314(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,t3B3,F2)
       allocate(Z1(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K6*K6*K7*K5
       call EGEMM2(I2,I3,D1,F2,Z1)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B+0.500*Z1
       deallocate(Z1)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,M2,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder562314(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,t3B3,F2)
       allocate(Z2(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K6*K6*K7*K7
       call EGEMM2(I2,I3,D1,F2,Z2)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B+0.250*Z2
       deallocate(Z2)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder562314(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N2,M2,N0,M1,t3B1,F2)
       allocate(Z3(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K9*K6*K7*K5
       call EGEMM2(I2,I3,D1,F2,Z3)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B+Z3
       deallocate(Z3)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N1,M2,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder562314(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N1,M2,N2,M2,N0,M1,t3B1,F2)
       allocate(Z4(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K9*K6*K7*K7
       call EGEMM2(I2,I3,D1,F2,Z4)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B+0.500*Z4
       deallocate(Z4)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder562314(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N1,M2,N2,M2,N0,M1,t3B1,F2)
       allocate(Z5(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K9*K9*K7*K5
       call EGEMM2(I2,I3,D1,F2,Z5)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B+0.500*Z5
       deallocate(Z5)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder562314(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,N1,M2,N2,M2,N0,M1,t3B1,F2)
       allocate(Z6(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K9*K9*K7*K7
       call EGEMM2(I2,I3,D1,F2,Z6)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B+0.250*Z6
       deallocate(Z6)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,M2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder561324(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,t3C1,F2)
       allocate(Z7(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K6*K6*K5*K8
       call EGEMM2(I2,I3,D1,F2,Z7)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B-Z7
       deallocate(Z7)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder461325(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,t3C4,F2)
       allocate(Z8(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K6*K6*K7*K5
       call EGEMM2(I2,I3,D1,F2,Z8)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B+Z8
       deallocate(Z8)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,M2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder561324(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,t3C1,F2)
       allocate(Z9(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K6*K6*K7*K8
       call EGEMM2(I2,I3,D1,F2,Z9)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B-Z9
       deallocate(Z9)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,M2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder561324(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,M2,N3,N2,M2,N0,M1,t3C1,F2)
       allocate(Z10(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K6*K0*K5*K8
       call EGEMM2(I2,I3,D1,F2,Z10)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B-Z10
       deallocate(Z10)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder461325(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,N2,M2,N0,M1,t3C4,F2)
       allocate(Z11(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K6*K0*K7*K5
       call EGEMM2(I2,I3,D1,F2,Z11)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B+Z11
       deallocate(Z11)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder561324(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,N2,M2,N0,M1,t3C1,F2)
       allocate(Z12(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K6*K0*K7*K8
       call EGEMM2(I2,I3,D1,F2,Z12)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B-Z12
       deallocate(Z12)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,M2,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder561324(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,M2,N2,M2,N0,M1,t3C1,F2)
       allocate(Z13(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K9*K6*K5*K8
       call EGEMM2(I2,I3,D1,F2,Z13)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B-Z13
       deallocate(Z13)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder461325(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N2,M2,N0,M1,t3C4,F2)
       allocate(Z14(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K9*K6*K7*K5
       call EGEMM2(I2,I3,D1,F2,Z14)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B+Z14
       deallocate(Z14)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,M2,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder561324(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,M2,N2,M2,N0,M1,t3C1,F2)
       allocate(Z15(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K9*K6*K7*K8
       call EGEMM2(I2,I3,D1,F2,Z15)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B-Z15
       deallocate(Z15)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder561324(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,M2,N2,M2,N0,M1,t3C1,F2)
       allocate(Z16(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K9*K0*K5*K8
       call EGEMM2(I2,I3,D1,F2,Z16)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B-Z16
       deallocate(Z16)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder461325(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,N2,M2,N1,M2,N2,M2,N0,M1,t3C4,F2)
       allocate(Z17(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K9*K0*K7*K5
       call EGEMM2(I2,I3,D1,F2,Z17)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B+Z17
       deallocate(Z17)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder561324(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,M2,N2,M2,N0,M1,t3C1,F2)
       allocate(Z18(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K9*K0*K7*K8
       call EGEMM2(I2,I3,D1,F2,Z18)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B-Z18
       deallocate(Z18)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,M2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,t3D,F2)
       allocate(Z19(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K6*K6*K8*K5
       call EGEMM2(I2,I3,D1,F2,Z19)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B-0.500*Z19
       deallocate(Z19)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,M2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,t3D,F2)
       allocate(Z20(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K6*K6*K8*K8
       call EGEMM2(I2,I3,D1,F2,Z20)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B+0.250*Z20
       deallocate(Z20)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(Z21(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K0*K6*K8*K5
       call EGEMM2(I2,I3,D1,F2,Z21)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B-Z21
       deallocate(Z21)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(Z22(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K0*K6*K8*K8
       call EGEMM2(I2,I3,D1,F2,Z22)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B+0.500*Z22
       deallocate(Z22)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(Z23(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K0*K0*K8*K5
       call EGEMM2(I2,I3,D1,F2,Z23)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B-0.500*Z23
       deallocate(Z23)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(Z24(N2+1:M2,N0+1:M1))
       I2=K5*K0
       I3=K0*K0*K8*K8
       call EGEMM2(I2,I3,D1,F2,Z24)
       deallocate(D1)
       deallocate(F2)
C
       V1B=V1B+0.250*Z24
       deallocate(Z24)
C
       call sumx1(N2,N3,N0,N2,
     & N2,M2,N0,M1,HT1B,V1B,1.0)
       deallocate(V1B)
C
       end
