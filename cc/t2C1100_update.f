       subroutine t2C1100_update(N0,N1,N2,N3,HT2C,shift,
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
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::S58(:,:,:,:)
       real*8,allocatable::S60(:,:,:,:)
       real*8,allocatable::S62(:,:,:,:)
       real*8,allocatable::S64(:,:,:,:)
       real*8,allocatable::S66(:,:,:,:)
       real*8,allocatable::S68(:,:,:,:)
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
       real*8,allocatable::Z10(:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::Z11(:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::Z12(:,:,:,:)
       real*8,allocatable::X9(:,:)
       real*8,allocatable::Z13(:,:,:,:)
       real*8,allocatable::X10(:,:)
       real*8,allocatable::Z14(:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::Z15(:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::Z16(:,:,:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::Z17(:,:,:,:)
       real*8,allocatable::Z18(:,:,:,:)
       real*8,allocatable::Z19(:,:,:,:)
       real*8,allocatable::Z20(:,:,:,:)
       real*8,allocatable::Z21(:,:,:,:)
       real*8,allocatable::Z22(:,:,:,:)
       real*8,allocatable::Z23(:,:,:,:)
       real*8,allocatable::Z24(:,:,:,:)
       real*8,allocatable::Z25(:,:,:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::Z35(:,:,:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::Z55(:,:,:,:)
       real*8,allocatable::Z59(:,:,:,:)
       real*8,allocatable::Z61(:,:,:,:)
       real*8,allocatable::X16(:,:,:,:)
       real*8,allocatable::Z63(:,:,:,:)
       real*8,allocatable::Z67(:,:,:,:)
       real*8,allocatable::Z69(:,:,:,:)
C
       allocate(V2C(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       V2C=0.0d0
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q1(N0+1:M1,N1+1:M2))
       I1=K9*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder631245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C3,F2)
       allocate(Z22(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I2=K8*K8*K6*K6
       I3=K9*K5
       call EGEMM2(I2,I3,Q1,F2,Z22)
       deallocate(F2)
C
       V2C=V2C+Z22
       deallocate(Z22)
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q2(M1+1:N1,N1+1:M2))
       I1=K9*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder631245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C3,F2)
       allocate(Z23(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I2=K8*K8*K6*K6
       I3=K9*K7
       call EGEMM2(I2,I3,Q2,F2,Z23)
       deallocate(F2)
C
       V2C=V2C+Z23
       deallocate(Z23)
       deallocate(Q2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q3(N0+1:M1,N2+1:M2))
       I1=K0*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder431256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(Z24(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I2=K8*K8*K6*K6
       I3=K0*K5
       call EGEMM2(I2,I3,Q3,F2,Z24)
       deallocate(F2)
C
       V2C=V2C+Z24
       deallocate(Z24)
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
       call EGEMM1(I1,I3,D1,B2,Q4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder431256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(Z25(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I2=K8*K8*K6*K6
       I3=K0*K8
       call EGEMM2(I2,I3,Q4,F2,Z25)
       deallocate(F2)
C
       V2C=V2C+Z25
       deallocate(Z25)
       deallocate(Q4)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S26(M1+1:N2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N2))
       X3=0.0d0
       call
     & sum4123(N0,M1,N0,M1,N1,M2,M1,N2,X3,S26, 1.000)
       deallocate(S26)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,M1,N2,X3,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder463125(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,M2,N3,M2,N3,M1,N2,t3C3,F2)
       allocate(Z3(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K6*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,X3,F2,Z3)
       deallocate(F2)
C
       V2C=V2C+Z3
       call
     & sum1243(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z3,-1.000)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S28(M1+1:N2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(M1+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       X4=0.0d0
       call
     & sum4123(M1,N2,N0,M1,N1,M2,M1,N2,X4,S28, 1.000)
       deallocate(S28)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,M1,N2,X4,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder463125(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,M2,N3,M2,N3,M1,N2,t3C3,F2)
       allocate(Z4(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K6*K6
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,X4,F2,Z4)
       deallocate(F2)
C
       V2C=V2C+Z4
       call
     & sum1243(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z4,-1.000)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S30(M1+1:N2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N2))
       X5=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,M1,N2,X5,S30, 1.000)
       deallocate(S30)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,M1,N2,X5,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder463125(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,M2,N3,M1,N2,t3C3,F2)
       allocate(Z5(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K6*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,X5,F2,Z5)
       deallocate(F2)
C
       V2C=V2C+Z5
       call
     & sum1243(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z5,-1.000)
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S32(M1+1:N2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       X6=0.0d0
       call
     & sum4123(M1,N2,M1,N1,N1,M2,M1,N2,X6,S32, 1.000)
       deallocate(S32)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,M1,N2,X6,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder463125(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,M2,N3,M2,N3,M1,N2,t3C3,F2)
       allocate(Z6(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K6*K6
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,X6,F2,Z6)
       deallocate(F2)
C
       V2C=V2C+Z6
       call
     & sum1243(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z6,-1.000)
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S34(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S34)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X14(N0+1:N2,M2+1:N3,M1+1:N2,M1+1:N2))
       X14=0.0d0
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X14,S34, 1.000)
       deallocate(S34)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S36(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S36)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X14,S36, 1.000)
       deallocate(S36)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N2,t3C3,F2)
       allocate(S38(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S38)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X14,S38,-1.000)
       deallocate(S38)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N2,t3C3,F2)
       allocate(S40(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S40)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X14,S40,-1.000)
       deallocate(S40)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Z35(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8*K8*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,X14,B2,Z35)
       deallocate(B2)
C
       V2C=V2C+Z35
       call
     & sum2134(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z35,-1.000)
       deallocate(Z35)
       deallocate(X14)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S42(M2+1:N3,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3))
       X7=0.0d0
       call
     & sum4123(N0,M1,N2,M2,N1,M2,M2,N3,X7,S42,-1.000)
       deallocate(S42)
C
       call sumx2341(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,N1,M2,M2,N3,X7,VBHPPP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z11(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,X7,F2,Z11)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z11,-1.000)
       call
     & sum1342(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z11, 1.000)
       deallocate(Z11)
       deallocate(X7)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S44(M2+1:N3,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3))
       X8=0.0d0
       call
     & sum4123(M1,N1,N2,M2,N1,M2,M2,N3,X8,S44,-1.000)
       deallocate(S44)
C
       call sumx2341(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,N2,M2,N1,M2,M2,N3,X8,VBHPPP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z12(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K6
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,X8,F2,Z12)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z12,-1.000)
       call
     & sum1342(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z12, 1.000)
       deallocate(Z12)
       deallocate(X8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q5(N0+1:M1,N1+1:M2))
       I1=K9*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:M1,N1+1:M2))
       X1=0.0d0
       X1=X1+Q5
       deallocate(Q5)
C
       call sumx21(N1,N3,N0,N1,
     & N0,M1,N1,M2,X1,FAHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder631245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C3,F2)
       allocate(Z1(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I2=K8*K8*K6*K6
       I3=K9*K5
       call EGEMM2(I2,I3,X1,F2,Z1)
       deallocate(F2)
C
       V2C=V2C+Z1
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
       call EGEMM1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(M1+1:N1,N1+1:M2))
       X2=0.0d0
       X2=X2+Q6
       deallocate(Q6)
C
       call sumx21(N1,N3,N0,N1,
     & M1,N1,N1,M2,X2,FAHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder631245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C3,F2)
       allocate(Z2(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I2=K8*K8*K6*K6
       I3=K9*K7
       call EGEMM2(I2,I3,X2,F2,Z2)
       deallocate(F2)
C
       V2C=V2C+Z2
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,M1,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S48(M1+1:N2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       X11=0.0d0
       call
     & sum4123(N0,M1,N0,M1,N2,M2,M1,N2,X11,S48,-1.000)
       deallocate(S48)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,N0,M1,N2,M2,M1,N2,X11,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder453126(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,t3D,F2)
       allocate(Z15(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K6*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,X11,F2,Z15)
       deallocate(F2)
C
       V2C=V2C+0.500*Z15
       call
     & sum1243(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z15,-0.500)
       deallocate(Z15)
       deallocate(X11)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S50(M1+1:N2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       X12=0.0d0
       call
     & sum4123(N0,M1,M1,N2,N2,M2,M1,N2,X12,S50,-1.000)
       deallocate(S50)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,M1,N2,X12,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder453126(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,M2,N3,M2,N3,M1,N2,t3D,F2)
       allocate(Z16(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K6*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,X12,F2,Z16)
       deallocate(F2)
C
       V2C=V2C+Z16
       call
     & sum1243(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z16,-1.000)
       deallocate(Z16)
       deallocate(X12)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S52(M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       X13=0.0d0
       call
     & sum4123(M1,N2,M1,N2,N2,M2,M1,N2,X13,S52,-1.000)
       deallocate(S52)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,M1,N2,X13,VCHHHP, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder453126(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,M2,N3,M2,N3,M1,N2,t3D,F2)
       allocate(Z17(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K6*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,X13,F2,Z17)
       deallocate(F2)
C
       V2C=V2C+0.500*Z17
       call
     & sum1243(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z17,-0.500)
       deallocate(Z17)
       deallocate(X13)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder413256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S54(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S54)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X15(N0+1:N2,M2+1:N3,M1+1:N2,M1+1:N2))
       X15=0.0d0
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X15,S54, 1.000)
       deallocate(S54)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder413256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S56(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S56)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X15,S56, 1.000)
       deallocate(S56)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Z55(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8*K8*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,X15,B2,Z55)
       deallocate(B2)
C
       V2C=V2C-Z55
       deallocate(Z55)
       deallocate(X15)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S58(M2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder2341(M2,N3,N0,M1,N2,M2,N2,M2,
     & N0,M1,N2,M2,N2,M2,M2,N3,S58,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder423156(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(Z59(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K6
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z59)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z59, 0.500)
       deallocate(Z59)
       deallocate(S58)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S60(M2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder2341(M2,N3,M1,N2,N2,M2,N2,M2,
     & M1,N2,N2,M2,N2,M2,M2,N3,S60,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder423156(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(Z61(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z61)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z61, 0.500)
       deallocate(Z61)
       deallocate(S60)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder413256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S62(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S62)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X16(N0+1:N2,M2+1:N3,M1+1:N2,M1+1:N2))
       X16=0.0d0
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X16,S62, 1.000)
       deallocate(S62)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder413256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S64(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S64)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X16,S64, 1.000)
       deallocate(S64)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Z63(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8*K8*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,X16,B2,Z63)
       deallocate(B2)
C
       call
     & sum2134(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z63,-1.000)
       deallocate(Z63)
       deallocate(X16)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S66(M2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S66)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder2341(M2,N3,N0,M1,N2,M2,N2,M2,
     & N0,M1,N2,M2,N2,M2,M2,N3,S66,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder423156(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(Z67(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K6
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z67)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z67, 0.500)
       deallocate(Z67)
       deallocate(S66)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S68(M2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S68)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder2341(M2,N3,M1,N2,N2,M2,N2,M2,
     & M1,N2,N2,M2,N2,M2,M2,N3,S68,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder423156(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(Z69(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z69)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z69, 0.500)
       deallocate(Z69)
       deallocate(S68)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q7(N0+1:M1,N2+1:M2))
       I1=K0*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N0+1:M1,N2+1:M2))
       X9=0.0d0
       X9=X9+Q7
       deallocate(Q7)
C
       call sumx21(N2,N3,N0,N2,
     & N0,M1,N2,M2,X9,FBHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder431256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(Z13(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I2=K8*K8*K6*K6
       I3=K0*K5
       call EGEMM2(I2,I3,X9,F2,Z13)
       deallocate(F2)
C
       V2C=V2C+Z13
       deallocate(Z13)
       deallocate(X9)
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
       call EGEMM1(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(M1+1:N2,N2+1:M2))
       X10=0.0d0
       X10=X10+Q8
       deallocate(Q8)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,N2,M2,X10,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder431256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(Z14(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       I2=K8*K8*K6*K6
       I3=K0*K8
       call EGEMM2(I2,I3,X10,F2,Z14)
       deallocate(F2)
C
       V2C=V2C+Z14
       deallocate(Z14)
       deallocate(X10)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,VBHPPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z7(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z7)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z7,-1.000)
       call
     & sum1342(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z7, 1.000)
       deallocate(Z7)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,VBHPPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z8(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K6
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,Z8)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z8,-1.000)
       call
     & sum1342(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z8, 1.000)
       deallocate(Z8)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,VBHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N2,t3C3,F2)
       allocate(Z9(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z9)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z9, 1.000)
       call
     & sum1342(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z9,-1.000)
       deallocate(Z9)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,VBHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N2,t3C3,F2)
       allocate(Z10(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z10)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z10, 1.000)
       call
     & sum1342(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z10,-1.000)
       deallocate(Z10)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,M1,M2,N3,N2,M2,M2,N3,VCHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder413256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(Z18(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K6
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z18)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z18, 1.000)
       call
     & sum1342(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z18,-1.000)
       deallocate(Z18)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,VCHPPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder413256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(Z19(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z19)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z19, 1.000)
       call
     & sum1342(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z19,-1.000)
       deallocate(Z19)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,M1,N2,M2,N2,M2,M2,N3,VCHPPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder423156(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(Z20(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K6
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z20)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z20,-0.500)
       call
     & sum1342(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z20, 0.500)
       deallocate(Z20)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,VCHPPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder423156(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(Z21(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z21)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z21,-0.500)
       call
     & sum1342(M2,N3,M2,N3,M1,N2,M1,N2,V2C,Z21, 0.500)
       deallocate(Z21)
C
       call sumx2(N2,N3,N2,N3,N0,N2,N0,N2,
     & M2,N3,M2,N3,M1,N2,M1,N2,HT2C,V2C,1.0)
       deallocate(V2C)
C
       end
