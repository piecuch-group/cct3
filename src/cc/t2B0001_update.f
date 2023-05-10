       subroutine t2B0001_update(N0,N1,N2,N3,HT2B,shift,
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
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::S57(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::S79(:,:,:,:)
       real*8,allocatable::S81(:,:,:,:)
       real*8,allocatable::S83(:,:,:,:)
       real*8,allocatable::S85(:,:,:,:)
       real*8,allocatable::S87(:,:,:,:)
       real*8,allocatable::S89(:,:,:,:)
       real*8,allocatable::S91(:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S95(:,:,:,:)
       real*8,allocatable::S97(:,:,:,:)
       real*8,allocatable::S99(:,:,:,:)
       real*8,allocatable::S101(:,:,:,:)
       real*8,allocatable::S103(:,:,:,:)
       real*8,allocatable::S105(:,:,:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::Z1(:,:,:,:)
       real*8,allocatable::X2(:,:)
       real*8,allocatable::Z2(:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::Z3(:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::Z4(:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::Z5(:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::Z6(:,:,:,:)
       real*8,allocatable::Z7(:,:,:,:)
       real*8,allocatable::Z8(:,:,:,:)
       real*8,allocatable::Z9(:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::Z10(:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::Z11(:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::Z12(:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::Z13(:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::Z14(:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::Z15(:,:,:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::Z16(:,:,:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::Z17(:,:,:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::Z18(:,:,:,:)
       real*8,allocatable::X16(:,:)
       real*8,allocatable::Z19(:,:,:,:)
       real*8,allocatable::X17(:,:)
       real*8,allocatable::Z20(:,:,:,:)
       real*8,allocatable::Z21(:,:,:,:)
       real*8,allocatable::Z22(:,:,:,:)
       real*8,allocatable::Z23(:,:,:,:)
       real*8,allocatable::Z24(:,:,:,:)
       real*8,allocatable::Z25(:,:,:,:)
       real*8,allocatable::Z26(:,:,:,:)
       real*8,allocatable::Z27(:,:,:,:)
       real*8,allocatable::Z28(:,:,:,:)
       real*8,allocatable::Z29(:,:,:,:)
       real*8,allocatable::Z30(:,:,:,:)
       real*8,allocatable::X18(:,:,:,:)
       real*8,allocatable::Z31(:,:,:,:)
       real*8,allocatable::X19(:,:,:,:)
       real*8,allocatable::Z32(:,:,:,:)
       real*8,allocatable::X20(:,:,:,:)
       real*8,allocatable::Z33(:,:,:,:)
       real*8,allocatable::X21(:,:,:,:)
       real*8,allocatable::Z34(:,:,:,:)
       real*8,allocatable::X22(:,:,:,:)
       real*8,allocatable::Z35(:,:,:,:)
       real*8,allocatable::X23(:,:,:,:)
       real*8,allocatable::Z36(:,:,:,:)
       real*8,allocatable::X24(:,:,:,:)
       real*8,allocatable::Z46(:,:,:,:)
       real*8,allocatable::Z54(:,:,:,:)
       real*8,allocatable::Z56(:,:,:,:)
       real*8,allocatable::Z58(:,:,:,:)
       real*8,allocatable::Z60(:,:,:,:)
       real*8,allocatable::Z62(:,:,:,:)
       real*8,allocatable::Z64(:,:,:,:)
       real*8,allocatable::Z73(:,:,:,:)
       real*8,allocatable::Z74(:,:,:,:)
C
       allocate(V2B(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       V2B=0.0d0
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S37(N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       X3=0.0d0
       call
     & sum4123(N0,M1,M1,N1,M2,N3,N0,M1,X3,S37,-1.000)
       deallocate(S37)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N0,M1,X3,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder562134(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,M2,N3,N0,M1,t3B3,F2)
       allocate(Z3(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,X3,F2,Z3)
       deallocate(F2)
C
       V2B=V2B-Z3
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S39(N0+1:M1,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       X4=0.0d0
       call
     & sum4123(M1,N1,M1,N1,M2,N3,N0,M1,X4,S39,-1.000)
       deallocate(S39)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N0,M1,X4,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder562134(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N2,M2,M2,N3,N0,M1,t3B3,F2)
       allocate(Z4(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K6*K7*K7
       call DMATMAT(I1,I2,I3,X4,F2,Z4)
       deallocate(F2)
C
       V2B=V2B-0.500*Z4
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S41(N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       X5=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,N0,M1,X5,S41,-1.000)
       deallocate(S41)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N0,M1,X5,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder563124(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,M2,N3,N0,M1,t3B1,F2)
       allocate(Z5(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,X5,F2,Z5)
       deallocate(F2)
C
       V2B=V2B+Z5
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S43(N0+1:M1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       X6=0.0d0
       call
     & sum4123(M1,N1,M1,N1,N1,M2,N0,M1,X6,S43,-1.000)
       deallocate(S43)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N0,M1,X6,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder563124(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,N2,M2,M2,N3,N0,M1,t3B1,F2)
       allocate(Z6(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K9*K7*K7
       call DMATMAT(I1,I2,I3,X6,F2,Z6)
       deallocate(F2)
C
       V2B=V2B+0.500*Z6
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder623145(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,N0,M1,t3B3,F2)
       allocate(S45(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S45)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X24(N0+1:N1,N2+1:M2,N0+1:M1,N0+1:M1))
       X24=0.0d0
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X24,S45, 1.000)
       deallocate(S45)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,N0,M1,N0,M1,t3B1,F2)
       allocate(S47(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S47)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X24,S47, 2.000)
       deallocate(S47)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N2,M2,N0,M1,N0,M1,t3B1,F2)
       allocate(S49(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K9*K9*K7
       call DMATMAT(I1,I2,I3,D1,F2,S49)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X24,S49, 1.000)
       deallocate(S49)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q1(M1+1:N1,M2+1:N3))
       I1=K6*K7
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(M1+1:N1,M2+1:N3))
       X1=0.0d0
       X1=X1+Q1
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q2(M1+1:N1,N1+1:M2))
       I1=K9*K7
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(M1+1:N1,N1+1:M2))
       X2=0.0d0
       X2=X2+Q2
       deallocate(Q2)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S53(N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,N0,M1,M2,N3,
     & M1,N2,N0,M1,M2,N3,N0,M1,S53,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(Z54(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z54)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B+Z54
       deallocate(Z54)
       deallocate(S53)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S55(N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M1,N1,M2,N3,
     & N0,M1,M1,N1,M2,N3,N0,M1,S55,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,M2,N3,N0,M1,t3C4,F2)
       allocate(Z56(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z56)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z56
       deallocate(Z56)
       deallocate(S55)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S57(N0+1:M1,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,M1,N1,M2,N3,
     & M1,N2,M1,N1,M2,N3,N0,M1,S57,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(Z58(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z58)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B+Z58
       deallocate(Z58)
       deallocate(S57)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S59(N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,N0,M1,N2,M2,
     & M1,N2,N0,M1,N2,M2,N0,M1,S59,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(Z60(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K0*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z60)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B+Z60
       deallocate(Z60)
       deallocate(S59)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S61(N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0*K7*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M1,N1,N2,M2,
     & N0,M1,M1,N1,N2,M2,N0,M1,S61,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,N2,M2,N2,M2,M2,N3,N0,M1,t3C4,F2)
       allocate(Z62(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K0*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z62)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z62
       deallocate(Z62)
       deallocate(S61)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S63(N0+1:M1,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0*K7*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,M1,N1,N2,M2,
     & M1,N2,M1,N1,N2,M2,N0,M1,S63,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(Z64(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K0*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z64)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B+Z64
       deallocate(Z64)
       deallocate(S63)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder513246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(S65(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S65)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X24,S65, 2.000)
       deallocate(S65)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder513246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(S67(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K6*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S67)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X24,S67, 2.000)
       deallocate(S67)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder513246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(S69(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K9*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S69)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X24,S69, 2.000)
       deallocate(S69)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder513246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(S71(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K9*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S71)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X24,S71, 2.000)
       deallocate(S71)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Z46(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K0
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,X24,B2,Z46)
       deallocate(B2)
C
       call
     & sum2134(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z46, 0.500)
       deallocate(Z46)
       deallocate(X24)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q3(M1+1:N2,M2+1:N3))
       I1=K6*K8
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,N0,M1,t3C1,F2)
       allocate(Z73(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I2=K5*K5*K6*K0
       I3=K6*K8
       call DVECMAT(I2,I3,Q3,F2,Z73)
       deallocate(F2)
C
       V2B=V2B-Z73
       deallocate(Z73)
       deallocate(Q3)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q4(M1+1:N2,N2+1:M2))
       I1=K0*K8
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,M2,N3,N0,M1,N0,M1,t3C1,F2)
       allocate(Z74(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I2=K5*K5*K6*K0
       I3=K0*K8
       call DVECMAT(I2,I3,Q4,F2,Z74)
       deallocate(F2)
C
       V2B=V2B-Z74
       deallocate(Z74)
       deallocate(Q4)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S75(N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       X7=0.0d0
       call
     & sum4123(M1,N2,N0,M1,M2,N3,N0,M1,X7,S75, 1.000)
       deallocate(S75)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N0,M1,X7,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder452136(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,M2,N3,N2,M2,M2,N3,N0,M1,t3B2,F2)
       allocate(Z10(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,X7,F2,Z10)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z10,-1.000)
       deallocate(Z10)
       deallocate(X7)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S77(N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       X8=0.0d0
       call
     & sum4123(N0,M1,M1,N1,M2,N3,N0,M1,X8,S77, 1.000)
       deallocate(S77)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N0,M1,X8,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder462135(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,M2,N3,N0,M1,t3B3,F2)
       allocate(Z11(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,X8,F2,Z11)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z11, 1.000)
       deallocate(Z11)
       deallocate(X8)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S79(N0+1:M1,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(M1+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       X9=0.0d0
       call
     & sum4123(M1,N2,M1,N1,M2,N3,N0,M1,X9,S79, 1.000)
       deallocate(S79)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N0,M1,X9,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder462135(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,M2,N3,N0,M1,t3B3,F2)
       allocate(Z12(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,X9,F2,Z12)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z12, 1.000)
       deallocate(Z12)
       deallocate(X9)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S81(N0+1:M1,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S81)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       X10=0.0d0
       call
     & sum4123(M1,N2,N0,M1,N1,M2,N0,M1,X10,S81, 1.000)
       deallocate(S81)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N0,M1,X10,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder453126(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N1,M2,N2,M2,M2,N3,N0,M1,t3B4,F2)
       allocate(Z13(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K9*K5*K8
       call DMATMAT(I1,I2,I3,X10,F2,Z13)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z13, 1.000)
       deallocate(Z13)
       deallocate(X10)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S83(N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S83)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       X11=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,N0,M1,X11,S83, 1.000)
       deallocate(S83)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N0,M1,X11,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder463125(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,M2,N3,N0,M1,t3B1,F2)
       allocate(Z14(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,X11,F2,Z14)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z14,-1.000)
       deallocate(Z14)
       deallocate(X11)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S85(N0+1:M1,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S85)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(M1+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       X12=0.0d0
       call
     & sum4123(M1,N2,M1,N1,N1,M2,N0,M1,X12,S85, 1.000)
       deallocate(S85)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N0,M1,X12,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder463125(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N1,M2,N2,M2,M2,N3,N0,M1,t3B1,F2)
       allocate(Z15(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K9*K7*K8
       call DMATMAT(I1,I2,I3,X12,F2,Z15)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z15,-1.000)
       deallocate(Z15)
       deallocate(X12)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S87(N2+1:M2,M1+1:N1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S87)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2))
       X13=0.0d0
       call
     & sum4123(M1,N1,N2,M2,M2,N3,N2,M2,X13,S87,-1.000)
       deallocate(S87)
C
       call sumx2341(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,N2,M2,M2,N3,N2,M2,X13,VBHPPP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder612345(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3B3,F2)
       allocate(Z16(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K5*K6
       I3=K6*K0*K7
       call DMATMAT(I1,I2,I3,X13,F2,Z16)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z16,-1.000)
       deallocate(Z16)
       deallocate(X13)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S89(N2+1:M2,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S89)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2))
       X14=0.0d0
       call
     & sum4123(M1,N1,M2,N3,N1,M2,N2,M2,X14,S89,-1.000)
       deallocate(S89)
C
       call sumx2341(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,X14,VBHPPP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,N0,M1,N0,M1,t3B1,F2)
       allocate(Z17(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K5*K6
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,X14,F2,Z17)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z17, 1.000)
       deallocate(Z17)
       deallocate(X14)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S91(N2+1:M2,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2))
       X15=0.0d0
       call
     & sum4123(M1,N1,N2,M2,N1,M2,N2,M2,X15,S91,-1.000)
       deallocate(S91)
C
       call sumx2341(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,N2,M2,N1,M2,N2,M2,X15,VBHPPP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N2,M2,N1,M2,M2,N3,N0,M1,N0,M1,t3B1,F2)
       allocate(Z18(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K5*K6
       I3=K9*K0*K7
       call DMATMAT(I1,I2,I3,X15,F2,Z18)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z18, 1.000)
       deallocate(Z18)
       deallocate(X15)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q5(M1+1:N1,M2+1:N3))
       I1=K6*K7
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1+Q5
       deallocate(Q5)
C
       call sumx21(N1,N3,N0,N1,
     & M1,N1,M2,N3,X1,FAHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder621345(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N2,M2,M2,N3,N0,M1,N0,M1,t3B3,F2)
       allocate(Z1(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I2=K5*K5*K6*K0
       I3=K6*K7
       call DVECMAT(I2,I3,X1,F2,Z1)
       deallocate(F2)
C
       V2B=V2B-Z1
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q6(M1+1:N1,N1+1:M2))
       I1=K9*K7
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2+Q6
       deallocate(Q6)
C
       call sumx21(N1,N3,N0,N1,
     & M1,N1,N1,M2,X2,FAHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder631245(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N2,M2,M2,N3,N0,M1,N0,M1,t3B1,F2)
       allocate(Z2(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I2=K5*K5*K6*K0
       I3=K9*K7
       call DVECMAT(I2,I3,X2,F2,Z2)
       deallocate(F2)
C
       V2B=V2B+Z2
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S95(N0+1:M1,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X18(N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       X18=0.0d0
       call
     & sum4123(N0,M1,M1,N2,M2,N3,N0,M1,X18,S95,-1.000)
       deallocate(S95)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N0,M1,X18,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(Z31(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K6*K8*K5
       call DMATMAT(I1,I2,I3,X18,F2,Z31)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z31,-1.000)
       deallocate(Z31)
       deallocate(X18)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S97(N0+1:M1,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X19(M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       X19=0.0d0
       call
     & sum4123(M1,N2,M1,N2,M2,N3,N0,M1,X19,S97,-1.000)
       deallocate(S97)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N0,M1,X19,VCHHHP, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(Z32(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K6*K8*K8
       call DMATMAT(I1,I2,I3,X19,F2,Z32)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z32,-0.500)
       deallocate(Z32)
       deallocate(X19)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S99(N0+1:M1,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X20(N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       X20=0.0d0
       call
     & sum4123(N0,M1,M1,N2,N2,M2,N0,M1,X20,S99,-1.000)
       deallocate(S99)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N0,M1,X20,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,N2,M2,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(Z33(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K0*K8*K5
       call DMATMAT(I1,I2,I3,X20,F2,Z33)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z33,-1.000)
       deallocate(Z33)
       deallocate(X20)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S101(N0+1:M1,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S101)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X21(M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       X21=0.0d0
       call
     & sum4123(M1,N2,M1,N2,N2,M2,N0,M1,X21,S101,-1.000)
       deallocate(S101)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N0,M1,X21,VCHHHP, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,N2,M2,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(Z34(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K0*K8*K8
       call DMATMAT(I1,I2,I3,X21,F2,Z34)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z34,-0.500)
       deallocate(Z34)
       deallocate(X21)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S103(N2+1:M2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S103)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X22(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       X22=0.0d0
       call
     & sum4123(M1,N2,M2,N3,N2,M2,N2,M2,X22,S103,-1.000)
       deallocate(S103)
C
       call sumx2341(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,X22,VCHPPP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,N0,M1,t3C1,F2)
       allocate(Z35(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K5*K6
       I3=K0*K6*K8
       call DMATMAT(I1,I2,I3,X22,F2,Z35)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z35, 1.000)
       deallocate(Z35)
       deallocate(X22)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S105(N2+1:M2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X23(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       X23=0.0d0
       call
     & sum4123(M1,N2,N2,M2,N2,M2,N2,M2,X23,S105,-1.000)
       deallocate(S105)
C
       call sumx2341(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,X23,VCHPPP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,M2,N3,N0,M1,N0,M1,t3C1,F2)
       allocate(Z36(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K5*K6
       I3=K0*K0*K8
       call DMATMAT(I1,I2,I3,X23,F2,Z36)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z36, 0.500)
       deallocate(Z36)
       deallocate(X23)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q7(M1+1:N2,M2+1:N3))
       I1=K6*K8
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(M1+1:N2,M2+1:N3))
       X16=0.0d0
       X16=X16+Q7
       deallocate(Q7)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,M2,N3,X16,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,N0,M1,t3C1,F2)
       allocate(Z19(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I2=K5*K5*K6*K0
       I3=K6*K8
       call DVECMAT(I2,I3,X16,F2,Z19)
       deallocate(F2)
C
       V2B=V2B-Z19
       deallocate(Z19)
       deallocate(X16)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q8(M1+1:N2,N2+1:M2))
       I1=K0*K8
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(M1+1:N2,N2+1:M2))
       X17=0.0d0
       X17=X17+Q8
       deallocate(Q8)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,N2,M2,X17,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,M2,N3,N0,M1,N0,M1,t3C1,F2)
       allocate(Z20(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I2=K5*K5*K6*K0
       I3=K0*K8
       call DVECMAT(I2,I3,X17,F2,Z20)
       deallocate(F2)
C
       V2B=V2B-Z20
       deallocate(Z20)
       deallocate(X17)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M2,N3,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder623145(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,N0,M1,t3B3,F2)
       allocate(Z7(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K0
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z7)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z7, 0.500)
       deallocate(Z7)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,N0,M1,N0,M1,t3B1,F2)
       allocate(Z8(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K0
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z8)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z8, 1.000)
       deallocate(Z8)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,M2,N3,VAHPPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N2,M2,N0,M1,N0,M1,t3B1,F2)
       allocate(Z9(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K0
       I3=K9*K9*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z9)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z9, 0.500)
       deallocate(Z9)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(Z21(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z21)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B+Z21
       deallocate(Z21)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,M2,N3,N0,M1,t3C4,F2)
       allocate(Z22(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z22)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z22
       deallocate(Z22)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(Z23(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z23)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B+Z23
       deallocate(Z23)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(Z24(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K0*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z24)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B+Z24
       deallocate(Z24)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,N2,M2,N2,M2,M2,N3,N0,M1,t3C4,F2)
       allocate(Z25(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K0*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z25)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z25
       deallocate(Z25)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(Z26(N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K6*K0
       I3=K0*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z26)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B+Z26
       deallocate(Z26)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,M2,N3,M2,N3,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder513246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z27(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K0
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z27)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z27,-1.000)
       deallocate(Z27)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,M2,N3,M2,N3,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder513246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z28(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K0
       I3=K6*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z28)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z28,-1.000)
       deallocate(Z28)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,N1,M2,M2,N3,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder513246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z29(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K0
       I3=K9*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z29)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z29,-1.000)
       deallocate(Z29)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,N1,M2,M2,N3,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder513246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z30(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K0
       I3=K9*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z30)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,N0,M1,N0,M1,V2B,Z30,-1.000)
       deallocate(Z30)
C
       call sumx2(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,M2,M2,N3,N0,M1,N0,M1,HT2B,V2B,1.0)
       deallocate(V2B)
C
       end
