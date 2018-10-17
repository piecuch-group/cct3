       subroutine t2B0000_update(N0,N1,N2,N3,HT2B,shift,
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
       real*8 HT2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
C
       real*8,allocatable::V2B(:,:,:,:)
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F1(:,:,:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::S58(:,:,:,:)
       real*8,allocatable::S60(:,:,:,:)
       real*8,allocatable::S62(:,:,:,:)
       real*8,allocatable::S64(:,:,:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::Z1(:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::Z2(:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::Z3(:,:,:,:)
       real*8,allocatable::Z4(:,:,:,:)
       real*8,allocatable::Z5(:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::Z6(:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::Z7(:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::Z8(:,:,:,:)
       real*8,allocatable::Z9(:,:,:,:)
       real*8,allocatable::Z10(:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::Z11(:,:,:,:)
       real*8,allocatable::X8(:,:)
       real*8,allocatable::Z12(:,:,:,:)
       real*8,allocatable::Z13(:,:,:,:)
       real*8,allocatable::Z14(:,:,:,:)
       real*8,allocatable::Z15(:,:,:,:)
       real*8,allocatable::Z16(:,:,:,:)
       real*8,allocatable::Z17(:,:,:,:)
       real*8,allocatable::Z18(:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::Z19(:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::Z20(:,:,:,:)
       real*8,allocatable::Z21(:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::Z22(:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::Z28(:,:,:,:)
       real*8,allocatable::Z30(:,:,:,:)
       real*8,allocatable::Z33(:,:,:,:)
       real*8,allocatable::Z35(:,:,:,:)
       real*8,allocatable::Z37(:,:,:,:)
       real*8,allocatable::Z43(:,:,:,:)
       real*8,allocatable::Z44(:,:,:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::Z52(:,:,:,:)
C
       allocate(V2B(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       V2B=0.0d0
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S23(N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       X2=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,N0,M1,X2,S23,-1.000)
       deallocate(S23)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N0,M1,X2,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder563124(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,t3B1,F2)
       allocate(Z2(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,X2,F2,Z2)
       deallocate(F2)
C
       V2B=V2B+Z2
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S25(N0+1:M1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       X3=0.0d0
       call
     & sum4123(M1,N1,M1,N1,N1,M2,N0,M1,X3,S25,-1.000)
       deallocate(S25)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N0,M1,X3,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder563124(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,t3B1,F2)
       allocate(Z3(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,X3,F2,Z3)
       deallocate(F2)
C
       V2B=V2B+0.500*Z3
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,N0,M1,N0,M1,t3B1,F2)
       allocate(S27(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S27)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X12(N0+1:N1,M2+1:N3,N0+1:M1,N0+1:M1))
       X12=0.0d0
       call
     & sum2341(N0,N1,M2,N3,N0,M1,N0,M1,X12,S27, 1.000)
       deallocate(S27)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S29(M2+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder2341(M2,N3,M1,N1,N1,M2,N1,M2,
     & M1,N1,N1,M2,N1,M2,M2,N3,S29,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,M2,N3,N0,M1,N0,M1,t3B1,F2)
       allocate(Z30(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z30)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z30, 0.500)
       deallocate(Z30)
       deallocate(S29)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
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
       allocate(X1(M1+1:N1,N1+1:M2))
       X1=0.0d0
       X1=X1+Q1
       deallocate(Q1)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S32(N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,N0,M1,N2,M2,
     & M1,N2,N0,M1,N2,M2,N0,M1,S32,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder562134(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,M2,N3,M2,N3,N0,M1,t3C1,F2)
       allocate(Z33(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z33)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z33
       deallocate(Z33)
       deallocate(S32)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S34(N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M1,N1,N2,M2,
     & N0,M1,M1,N1,N2,M2,N0,M1,S34,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder462135(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,t3C4,F2)
       allocate(Z35(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z35)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B+Z35
       deallocate(Z35)
       deallocate(S34)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S36(N0+1:M1,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0*K7*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,M1,N1,N2,M2,
     & M1,N2,M1,N1,N2,M2,N0,M1,S36,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder562134(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,t3C1,F2)
       allocate(Z37(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z37)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z37
       deallocate(Z37)
       deallocate(S36)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder523146(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3C1,F2)
       allocate(S38(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S38)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,N0,M1,X12,S38,-1.000)
       deallocate(S38)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder513246(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,M2,N3,N0,M1,N0,M1,t3C3,F2)
       allocate(S40(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S40)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,N0,M1,X12,S40, 1.000)
       deallocate(S40)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Z28(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5*K5*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,X12,B2,Z28)
       deallocate(B2)
C
       call
     & sum2134(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z28, 1.000)
       deallocate(Z28)
       deallocate(X12)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S42(M2+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder2341(M2,N3,M1,N2,N2,M2,N1,M2,
     & M1,N2,N2,M2,N1,M2,M2,N3,S42,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder523146(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,M2,N3,N0,M1,N0,M1,t3C1,F2)
       allocate(Z43(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z43)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z43,-1.000)
       deallocate(Z43)
       deallocate(S42)
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
       call reorder521346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3C1,F2)
       allocate(Z44(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I2=K5*K5*K6*K6
       I3=K0*K8
       call EGEMM2(I2,I3,Q2,F2,Z44)
       deallocate(F2)
C
       V2B=V2B+Z44
       deallocate(Z44)
       deallocate(Q2)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S45(N0+1:M1,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       X4=0.0d0
       call
     & sum4123(M1,N2,N0,M1,N1,M2,N0,M1,X4,S45, 1.000)
       deallocate(S45)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N0,M1,X4,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder453126(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N1,M2,M2,N3,M2,N3,N0,M1,t3B4,F2)
       allocate(Z6(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,X4,F2,Z6)
       deallocate(F2)
C
       call
     & sum1243(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z6, 1.000)
       deallocate(Z6)
       deallocate(X4)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S47(N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       X5=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,N0,M1,X5,S47, 1.000)
       deallocate(S47)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N0,M1,X5,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder463125(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,t3B1,F2)
       allocate(Z7(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,X5,F2,Z7)
       deallocate(F2)
C
       call
     & sum1243(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z7,-1.000)
       deallocate(Z7)
       deallocate(X5)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S49(N0+1:M1,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(M1+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       X6=0.0d0
       call
     & sum4123(M1,N2,M1,N1,N1,M2,N0,M1,X6,S49, 1.000)
       deallocate(S49)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N0,M1,X6,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder463125(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,t3B1,F2)
       allocate(Z8(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,X6,F2,Z8)
       deallocate(F2)
C
       call
     & sum1243(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z8,-1.000)
       deallocate(Z8)
       deallocate(X6)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder612345(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3B3,F2)
       allocate(S51(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K6
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S51)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X13(N0+1:N2,M2+1:N3,N0+1:M1,N0+1:M1))
       X13=0.0d0
       call
     & sum2341(N0,N2,M2,N3,N0,M1,N0,M1,X13,S51, 1.000)
       deallocate(S51)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,N0,M1,N0,M1,t3B1,F2)
       allocate(S53(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S53)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,N0,M1,N0,M1,X13,S53,-1.000)
       deallocate(S53)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S55(M2+1:N3,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3))
       X7=0.0d0
       call
     & sum4123(M1,N1,N2,M2,N1,M2,M2,N3,X7,S55,-1.000)
       deallocate(S55)
C
       call sumx2341(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,N2,M2,N1,M2,M2,N3,X7,VBHPPP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N2,M2,N1,M2,M2,N3,N0,M1,N0,M1,t3B1,F2)
       allocate(Z11(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,X7,F2,Z11)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z11, 1.000)
       deallocate(Z11)
       deallocate(X7)
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
       X1=X1+Q3
       deallocate(Q3)
C
       call sumx21(N1,N3,N0,N1,
     & M1,N1,N1,M2,X1,FAHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder631245(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3B1,F2)
       allocate(Z1(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I2=K5*K5*K6*K6
       I3=K9*K7
       call EGEMM2(I2,I3,X1,F2,Z1)
       deallocate(F2)
C
       V2B=V2B+Z1
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S58(N0+1:M1,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       X9=0.0d0
       call
     & sum4123(N0,M1,M1,N2,N2,M2,N0,M1,X9,S58,-1.000)
       deallocate(S58)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N0,M1,X9,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder452136(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,t3C1,F2)
       allocate(Z19(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,X9,F2,Z19)
       deallocate(F2)
C
       call
     & sum1243(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z19, 1.000)
       deallocate(Z19)
       deallocate(X9)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S60(N0+1:M1,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       X10=0.0d0
       call
     & sum4123(M1,N2,M1,N2,N2,M2,N0,M1,X10,S60,-1.000)
       deallocate(S60)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N0,M1,X10,VCHHHP, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder452136(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,t3C1,F2)
       allocate(Z20(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,X10,F2,Z20)
       deallocate(F2)
C
       call
     & sum1243(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z20, 0.500)
       deallocate(Z20)
       deallocate(X10)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,N0,M1,t3C1,F2)
       allocate(S62(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S62)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,N0,M1,N0,M1,X13,S62,-1.000)
       deallocate(S62)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Z52(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5*K5*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,X13,B2,Z52)
       deallocate(B2)
C
       V2B=V2B+Z52
       deallocate(Z52)
       deallocate(X13)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S64(M2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S64)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       X11=0.0d0
       call
     & sum4123(M1,N2,N2,M2,N2,M2,M2,N3,X11,S64,-1.000)
       deallocate(S64)
C
       call sumx2341(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,X11,VCHPPP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,M2,N3,N0,M1,N0,M1,t3C1,F2)
       allocate(Z22(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,X11,F2,Z22)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z22, 0.500)
       deallocate(Z22)
       deallocate(X11)
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
       allocate(X8(M1+1:N2,N2+1:M2))
       X8=0.0d0
       X8=X8+Q4
       deallocate(Q4)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,N2,M2,X8,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder521346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3C1,F2)
       allocate(Z12(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I2=K5*K5*K6*K6
       I3=K0*K8
       call EGEMM2(I2,I3,X8,F2,Z12)
       deallocate(F2)
C
       V2B=V2B+Z12
       deallocate(Z12)
       deallocate(X8)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,N0,M1,N0,M1,t3B1,F2)
       allocate(Z4(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z4)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z4, 1.000)
       deallocate(Z4)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,M2,N3,VAHPPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,M2,N3,N0,M1,N0,M1,t3B1,F2)
       allocate(Z5(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z5)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z5, 0.500)
       deallocate(Z5)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,VBHPPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder612345(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3B3,F2)
       allocate(Z9(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,Z9)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z9,-1.000)
       deallocate(Z9)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,VBHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,N0,M1,N0,M1,t3B1,F2)
       allocate(Z10(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z10)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z10, 1.000)
       deallocate(Z10)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder562134(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,M2,N3,M2,N3,N0,M1,t3C1,F2)
       allocate(Z13(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z13)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z13
       deallocate(Z13)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder462135(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,t3C4,F2)
       allocate(Z14(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z14)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B+Z14
       deallocate(Z14)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder562134(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,t3C1,F2)
       allocate(Z15(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z15)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z15
       deallocate(Z15)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,M2,N3,M2,N3,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder523146(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3C1,F2)
       allocate(Z16(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z16)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z16, 1.000)
       deallocate(Z16)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,N1,M2,M2,N3,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder513246(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,M2,N3,N0,M1,N0,M1,t3C3,F2)
       allocate(Z17(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z17)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z17,-1.000)
       deallocate(Z17)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,N1,M2,M2,N3,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder523146(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,M2,N3,N0,M1,N0,M1,t3C1,F2)
       allocate(Z18(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z18)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z18, 1.000)
       deallocate(Z18)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,VCHPPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,N0,M1,t3C1,F2)
       allocate(Z21(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z21)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,N0,M1,V2B,Z21, 1.000)
       deallocate(Z21)
C
       call sumx2(N2,N3,N1,N3,N0,N2,N0,N1,
     & M2,N3,M2,N3,N0,M1,N0,M1,HT2B,V2B,1.0)
       deallocate(V2B)
C
       end
