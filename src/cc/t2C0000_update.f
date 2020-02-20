       subroutine t2C0000_update(N0,N1,N2,N3,HT2C,shift,
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
       real*8 HT2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
C
       real*8,allocatable::V2C(:,:,:,:)
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F1(:,:,:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::Z1(:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::Z2(:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::Z3(:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::Z4(:,:,:,:)
       real*8,allocatable::Z5(:,:,:,:)
       real*8,allocatable::Z6(:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::Z7(:,:,:,:)
       real*8,allocatable::X6(:,:)
       real*8,allocatable::Z8(:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::Z9(:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::Z10(:,:,:,:)
       real*8,allocatable::Z11(:,:,:,:)
       real*8,allocatable::Z12(:,:,:,:)
       real*8,allocatable::Z13(:,:,:,:)
       real*8,allocatable::Z14(:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::Z22(:,:,:,:)
       real*8,allocatable::Z33(:,:,:,:)
       real*8,allocatable::Z35(:,:,:,:)
       real*8,allocatable::Z37(:,:,:,:)
       real*8,allocatable::Z39(:,:,:,:)
C
       allocate(V2C(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       V2C=0.0d0
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q1(M1+1:N1,N1+1:M2))
       I1=K9*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder631245(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,
     & M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3C2,F2)
       allocate(Z13(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I2=K5*K5*K6*K6
       I3=K9*K7
       call EGEMM2(I2,I3,Q1,F2,Z13)
       deallocate(F2)
C
       V2C=V2C+Z13
       deallocate(Z13)
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q2(M1+1:N2,N2+1:M2))
       I1=K0*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder631245(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3D,F2)
       allocate(Z14(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I2=K5*K5*K6*K6
       I3=K0*K8
       call EGEMM2(I2,I3,Q2,F2,Z14)
       deallocate(F2)
C
       V2C=V2C+Z14
       deallocate(Z14)
       deallocate(Q2)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S15(N0+1:M1,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       X2=0.0d0
       call
     & sum4123(M1,N2,N0,M1,N1,M2,N0,M1,X2,S15, 1.000)
       deallocate(S15)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N0,M1,X2,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder563124(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,M2,N3,M2,N3,N0,M1,t3C3,F2)
       allocate(Z2(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,X2,F2,Z2)
       deallocate(F2)
C
       V2C=V2C-Z2
       call
     & sum1243(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z2, 1.000)
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S17(N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       X3=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,N0,M1,X3,S17, 1.000)
       deallocate(S17)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N0,M1,X3,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder463125(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,t3C2,F2)
       allocate(Z3(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,X3,F2,Z3)
       deallocate(F2)
C
       V2C=V2C+Z3
       call
     & sum1243(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z3,-1.000)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S19(N0+1:M1,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(M1+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       X4=0.0d0
       call
     & sum4123(M1,N2,M1,N1,N1,M2,N0,M1,X4,S19, 1.000)
       deallocate(S19)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N0,M1,X4,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder563124(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,t3C3,F2)
       allocate(Z4(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,X4,F2,Z4)
       deallocate(F2)
C
       V2C=V2C-Z4
       call
     & sum1243(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z4, 1.000)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3C4,F2)
       allocate(S21(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K6
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S21)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X9(N0+1:N2,M2+1:N3,N0+1:M1,N0+1:M1))
       X9=0.0d0
       call
     & sum2341(N0,N2,M2,N3,N0,M1,N0,M1,X9,S21, 1.000)
       deallocate(S21)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,N0,M1,N0,M1,t3C2,F2)
       allocate(S23(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S23)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,N0,M1,N0,M1,X9,S23,-1.000)
       deallocate(S23)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Z22(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5*K5*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,X9,B2,Z22)
       deallocate(B2)
C
       V2C=V2C+Z22
       call
     & sum2134(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z22,-1.000)
       deallocate(Z22)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S25(M2+1:N3,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3))
       X5=0.0d0
       call
     & sum4123(M1,N1,N2,M2,N1,M2,M2,N3,X5,S25,-1.000)
       deallocate(S25)
C
       call sumx2341(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,N2,M2,N1,M2,M2,N3,X5,VBHPPP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,N2,M2,N1,M2,M2,N3,N0,M1,N0,M1,t3C4,F2)
       allocate(Z7(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,X5,F2,Z7)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z7,-1.000)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z7, 1.000)
       deallocate(Z7)
       deallocate(X5)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q3(M1+1:N1,N1+1:M2))
       I1=K9*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(M1+1:N1,N1+1:M2))
       X1=0.0d0
       X1=X1+Q3
       deallocate(Q3)
C
       call sumx21(N1,N3,N0,N1,
     & M1,N1,N1,M2,X1,FAHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder631245(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,
     & M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3C2,F2)
       allocate(Z1(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I2=K5*K5*K6*K6
       I3=K9*K7
       call EGEMM2(I2,I3,X1,F2,Z1)
       deallocate(F2)
C
       V2C=V2C+Z1
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S28(N0+1:M1,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       X7=0.0d0
       call
     & sum4123(N0,M1,M1,N2,N2,M2,N0,M1,X7,S28,-1.000)
       deallocate(S28)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N0,M1,X7,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder463125(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,t3D,F2)
       allocate(Z9(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,X7,F2,Z9)
       deallocate(F2)
C
       V2C=V2C-Z9
       call
     & sum1243(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z9, 1.000)
       deallocate(Z9)
       deallocate(X7)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S30(N0+1:M1,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       X8=0.0d0
       call
     & sum4123(M1,N2,M1,N2,N2,M2,N0,M1,X8,S30,-1.000)
       deallocate(S30)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N0,M1,X8,VCHHHP, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder563124(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,t3D,F2)
       allocate(Z10(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,X8,F2,Z10)
       deallocate(F2)
C
       V2C=V2C+0.500*Z10
       call
     & sum1243(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z10,-0.500)
       deallocate(Z10)
       deallocate(X8)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,N0,M1,t3D,F2)
       allocate(S32(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S32)
       deallocate(D1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder4123(M2,N3,N0,M1,N0,M1,N0,N2,
     & N0,N2,M2,N3,N0,M1,N0,M1,S32,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Z33(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5*K5*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,Z33)
       deallocate(D1)
       deallocate(B2)
C
       V2C=V2C-Z33
       deallocate(Z33)
       deallocate(S32)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S34(M2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder2341(M2,N3,M1,N2,N2,M2,N2,M2,
     & M1,N2,N2,M2,N2,M2,M2,N3,S34,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,N0,M1,N0,M1,t3D,F2)
       allocate(Z35(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z35)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z35, 0.500)
       deallocate(Z35)
       deallocate(S34)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,N0,M1,t3D,F2)
       allocate(S36(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S36)
       deallocate(D1)
       deallocate(F2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder4123(M2,N3,N0,M1,N0,M1,N0,N2,
     & N0,N2,M2,N3,N0,M1,N0,M1,S36,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Z37(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5*K5*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,Z37)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z37,-1.000)
       deallocate(Z37)
       deallocate(S36)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S38(M2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder2341(M2,N3,M1,N2,N2,M2,N2,M2,
     & M1,N2,N2,M2,N2,M2,M2,N3,S38,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,N0,M1,N0,M1,t3D,F2)
       allocate(Z39(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z39)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z39, 0.500)
       deallocate(Z39)
       deallocate(S38)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q4(M1+1:N2,N2+1:M2))
       I1=K0*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(M1+1:N2,N2+1:M2))
       X6=0.0d0
       X6=X6+Q4
       deallocate(Q4)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,N2,M2,X6,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder631245(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3D,F2)
       allocate(Z8(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I2=K5*K5*K6*K6
       I3=K0*K8
       call EGEMM2(I2,I3,X6,F2,Z8)
       deallocate(F2)
C
       V2C=V2C+Z8
       deallocate(Z8)
       deallocate(X6)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,VBHPPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3C4,F2)
       allocate(Z5(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,Z5)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z5,-1.000)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z5, 1.000)
       deallocate(Z5)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,VBHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,N0,M1,N0,M1,t3C2,F2)
       allocate(Z6(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z6)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z6, 1.000)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z6,-1.000)
       deallocate(Z6)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,VCHPPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,N0,M1,t3D,F2)
       allocate(Z11(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z11)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z11, 1.000)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z11,-1.000)
       deallocate(Z11)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,VCHPPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,N0,M1,N0,M1,t3D,F2)
       allocate(Z12(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z12)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z12,-0.500)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,N0,M1,V2C,Z12, 0.500)
       deallocate(Z12)
C
       call sumx2(N2,N3,N2,N3,N0,N2,N0,N2,
     & M2,N3,M2,N3,N0,M1,N0,M1,HT2C,V2C,1.0)
       deallocate(V2C)
C
       end
