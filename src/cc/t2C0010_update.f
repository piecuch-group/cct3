       subroutine t2C0010_update(N0,N1,N2,N3,HT2C,shift,
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
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::S73(:,:,:,:)
       real*8,allocatable::S75(:,:,:,:)
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
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::Z7(:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::Z8(:,:,:,:)
       real*8,allocatable::Z9(:,:,:,:)
       real*8,allocatable::Z10(:,:,:,:)
       real*8,allocatable::Z11(:,:,:,:)
       real*8,allocatable::Z12(:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::Z13(:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::Z14(:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::Z15(:,:,:,:)
       real*8,allocatable::X12(:,:)
       real*8,allocatable::Z16(:,:,:,:)
       real*8,allocatable::X13(:,:)
       real*8,allocatable::Z17(:,:,:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::Z18(:,:,:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::Z19(:,:,:,:)
       real*8,allocatable::X16(:,:,:,:)
       real*8,allocatable::Z20(:,:,:,:)
       real*8,allocatable::X17(:,:,:,:)
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
       real*8,allocatable::Z44(:,:,:,:)
       real*8,allocatable::Z74(:,:,:,:)
       real*8,allocatable::Z76(:,:,:,:)
C
       allocate(V2C(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       V2C=0.0d0
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q1(M1+1:N1,M2+1:N3))
       I1=K6*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,N0,M1,t3C4,F2)
       allocate(Z27(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K0*K6
       I3=K6*K7
       call EGEMM2(I2,I3,Q1,F2,Z27)
       deallocate(F2)
C
       V2C=V2C+Z27
       deallocate(Z27)
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
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,N0,M1,N0,M1,t3C4,F2)
       allocate(Z28(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K0*K6
       I3=K9*K7
       call EGEMM2(I2,I3,Q2,F2,Z28)
       deallocate(F2)
C
       V2C=V2C+Z28
       deallocate(Z28)
       deallocate(Q2)
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
       call EGEMM1(I1,I3,D1,B2,Q3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,N0,M1,t3D,F2)
       allocate(Z29(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K0*K6
       I3=K6*K8
       call EGEMM2(I2,I3,Q3,F2,Z29)
       deallocate(F2)
C
       V2C=V2C+Z29
       deallocate(Z29)
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
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder621345(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,N0,M1,t3D,F2)
       allocate(Z30(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K0*K6
       I3=K0*K8
       call EGEMM2(I2,I3,Q4,F2,Z30)
       deallocate(F2)
C
       V2C=V2C-Z30
       deallocate(Z30)
       deallocate(Q4)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S31(N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       X3=0.0d0
       call
     & sum4123(M1,N2,N0,M1,M2,N3,N0,M1,X3,S31, 1.000)
       deallocate(S31)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N0,M1,X3,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,t3C1,F2)
       allocate(Z3(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K0*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,X3,F2,Z3)
       deallocate(F2)
C
       V2C=V2C-Z3
       call
     & sum1243(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z3, 1.000)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S33(N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       X4=0.0d0
       call
     & sum4123(N0,M1,M1,N1,M2,N3,N0,M1,X4,S33, 1.000)
       deallocate(S33)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N0,M1,X4,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,t3C4,F2)
       allocate(Z4(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K0*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,X4,F2,Z4)
       deallocate(F2)
C
       V2C=V2C+Z4
       call
     & sum1243(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z4,-1.000)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S35(N0+1:M1,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(M1+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       X5=0.0d0
       call
     & sum4123(M1,N2,M1,N1,M2,N3,N0,M1,X5,S35, 1.000)
       deallocate(S35)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N0,M1,X5,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,t3C1,F2)
       allocate(Z5(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K0*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,X5,F2,Z5)
       deallocate(F2)
C
       V2C=V2C-Z5
       call
     & sum1243(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z5, 1.000)
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S37(N0+1:M1,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       X6=0.0d0
       call
     & sum4123(M1,N2,N0,M1,N1,M2,N0,M1,X6,S37, 1.000)
       deallocate(S37)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N0,M1,X6,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,M2,N3,N2,M2,N0,M1,t3C1,F2)
       allocate(Z6(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K0*K6
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,X6,F2,Z6)
       deallocate(F2)
C
       V2C=V2C-Z6
       call
     & sum1243(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z6, 1.000)
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S39(N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       X7=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,N0,M1,X7,S39, 1.000)
       deallocate(S39)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N0,M1,X7,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,N2,M2,N0,M1,t3C4,F2)
       allocate(Z7(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K0*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,X7,F2,Z7)
       deallocate(F2)
C
       V2C=V2C+Z7
       call
     & sum1243(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z7,-1.000)
       deallocate(Z7)
       deallocate(X7)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S41(N0+1:M1,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(M1+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       X8=0.0d0
       call
     & sum4123(M1,N2,M1,N1,N1,M2,N0,M1,X8,S41, 1.000)
       deallocate(S41)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N0,M1,X8,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,M2,N3,N2,M2,N0,M1,t3C1,F2)
       allocate(Z8(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K0*K6
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,X8,F2,Z8)
       deallocate(F2)
C
       V2C=V2C-Z8
       call
     & sum1243(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z8, 1.000)
       deallocate(Z8)
       deallocate(X8)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,N0,M1,t3C4,F2)
       allocate(S43(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S43)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X18(N0+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       X18=0.0d0
       call
     & sum2341(N0,N2,N2,M2,N0,M1,N0,M1,X18,S43, 1.000)
       deallocate(S43)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,N2,M2,M2,N3,N2,M2,N0,M1,N0,M1,t3C4,F2)
       allocate(S45(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S45)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,N0,M1,X18,S45, 1.000)
       deallocate(S45)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,N0,M1,N0,M1,t3C4,F2)
       allocate(S47(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S47)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,N0,M1,X18,S47, 1.000)
       deallocate(S47)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,N2,M2,N1,M2,N2,M2,N0,M1,N0,M1,t3C4,F2)
       allocate(S49(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S49)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,N0,M1,X18,S49, 1.000)
       deallocate(S49)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S51(N2+1:M2,M1+1:N1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2))
       X9=0.0d0
       call
     & sum4123(M1,N1,N2,M2,M2,N3,N2,M2,X9,S51,-1.000)
       deallocate(S51)
C
       call sumx2341(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,N2,M2,M2,N3,N2,M2,X9,VBHPPP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3C4,F2)
       allocate(Z13(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K5*K6
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,X9,F2,Z13)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z13, 1.000)
       deallocate(Z13)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S53(N2+1:M2,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2))
       X10=0.0d0
       call
     & sum4123(M1,N1,M2,N3,N1,M2,N2,M2,X10,S53,-1.000)
       deallocate(S53)
C
       call sumx2341(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,X10,VBHPPP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,N0,M1,N0,M1,t3C2,F2)
       allocate(Z14(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,X10,F2,Z14)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z14,-1.000)
       deallocate(Z14)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S55(N2+1:M2,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2))
       X11=0.0d0
       call
     & sum4123(M1,N1,N2,M2,N1,M2,N2,M2,X11,S55,-1.000)
       deallocate(S55)
C
       call sumx2341(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,N2,M2,N1,M2,N2,M2,X11,VBHPPP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,N2,M2,N1,M2,M2,N3,N0,M1,N0,M1,t3C4,F2)
       allocate(Z15(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K5*K6
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,X11,F2,Z15)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z15, 1.000)
       deallocate(Z15)
       deallocate(X11)
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
       call EGEMM1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(M1+1:N1,M2+1:N3))
       X1=0.0d0
       X1=X1+Q5
       deallocate(Q5)
C
       call sumx21(N1,N3,N0,N1,
     & M1,N1,M2,N3,X1,FAHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,N0,M1,t3C4,F2)
       allocate(Z1(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K0*K6
       I3=K6*K7
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
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,N0,M1,N0,M1,t3C4,F2)
       allocate(Z2(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K0*K6
       I3=K9*K7
       call EGEMM2(I2,I3,X2,F2,Z2)
       deallocate(F2)
C
       V2C=V2C+Z2
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S59(N0+1:M1,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       X14=0.0d0
       call
     & sum4123(N0,M1,M1,N2,M2,N3,N0,M1,X14,S59,-1.000)
       deallocate(S59)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N0,M1,X14,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,t3D,F2)
       allocate(Z18(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K0*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,X14,F2,Z18)
       deallocate(F2)
C
       V2C=V2C-Z18
       call
     & sum1243(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z18, 1.000)
       deallocate(Z18)
       deallocate(X14)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S61(N0+1:M1,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       X15=0.0d0
       call
     & sum4123(M1,N2,M1,N2,M2,N3,N0,M1,X15,S61,-1.000)
       deallocate(S61)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N0,M1,X15,VCHHHP, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,t3D,F2)
       allocate(Z19(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K0*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,X15,F2,Z19)
       deallocate(F2)
C
       V2C=V2C+0.500*Z19
       call
     & sum1243(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z19,-0.500)
       deallocate(Z19)
       deallocate(X15)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S63(N0+1:M1,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       X16=0.0d0
       call
     & sum4123(N0,M1,M1,N2,N2,M2,N0,M1,X16,S63,-1.000)
       deallocate(S63)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N0,M1,X16,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder462135(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,t3D,F2)
       allocate(Z20(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K0*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,X16,F2,Z20)
       deallocate(F2)
C
       V2C=V2C+Z20
       call
     & sum1243(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z20,-1.000)
       deallocate(Z20)
       deallocate(X16)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S65(N0+1:M1,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       X17=0.0d0
       call
     & sum4123(M1,N2,M1,N2,N2,M2,N0,M1,X17,S65,-1.000)
       deallocate(S65)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N0,M1,X17,VCHHHP, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder562134(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,t3D,F2)
       allocate(Z21(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K0*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,X17,F2,Z21)
       deallocate(F2)
C
       V2C=V2C-0.500*Z21
       call
     & sum1243(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z21, 0.500)
       deallocate(Z21)
       deallocate(X17)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,N0,M1,t3D,F2)
       allocate(S67(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S67)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,N0,M1,X18,S67,-0.500)
       deallocate(S67)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,N0,M1,t3D,F2)
       allocate(S69(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S69)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,N0,M1,X18,S69,-1.000)
       deallocate(S69)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,N0,M1,t3D,F2)
       allocate(S71(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S71)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,N0,M1,X18,S71,-0.500)
       deallocate(S71)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Z44(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,X18,B2,Z44)
       deallocate(B2)
C
       V2C=V2C-Z44
       deallocate(Z44)
       deallocate(X18)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S73(N2+1:M2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,M1,N2,M2,N3,N2,M2,
     & M1,N2,M2,N3,N2,M2,N2,M2,S73,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,N0,M1,t3D,F2)
       allocate(Z74(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K5*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z74)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z74,-1.000)
       deallocate(Z74)
       deallocate(S73)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S75(N2+1:M2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,M1,N2,N2,M2,N2,M2,
     & M1,N2,N2,M2,N2,M2,N2,M2,S75,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,N0,M1,N0,M1,t3D,F2)
       allocate(Z76(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K5*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z76)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z76, 0.500)
       deallocate(Z76)
       deallocate(S75)
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
       call EGEMM1(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(M1+1:N2,M2+1:N3))
       X12=0.0d0
       X12=X12+Q7
       deallocate(Q7)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,M2,N3,X12,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,N0,M1,t3D,F2)
       allocate(Z16(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K0*K6
       I3=K6*K8
       call EGEMM2(I2,I3,X12,F2,Z16)
       deallocate(F2)
C
       V2C=V2C+Z16
       deallocate(Z16)
       deallocate(X12)
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
       allocate(X13(M1+1:N2,N2+1:M2))
       X13=0.0d0
       X13=X13+Q8
       deallocate(Q8)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,N2,M2,X13,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder621345(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,N0,M1,t3D,F2)
       allocate(Z17(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K0*K6
       I3=K0*K8
       call EGEMM2(I2,I3,X13,F2,Z17)
       deallocate(F2)
C
       V2C=V2C-Z17
       deallocate(Z17)
       deallocate(X13)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M2,N3,VBHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,N0,M1,t3C4,F2)
       allocate(Z9(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z9)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z9, 1.000)
       deallocate(Z9)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,VBHPPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,N2,M2,M2,N3,N2,M2,N0,M1,N0,M1,t3C4,F2)
       allocate(Z10(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K0
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,Z10)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z10, 1.000)
       deallocate(Z10)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,VBHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,N0,M1,N0,M1,t3C4,F2)
       allocate(Z11(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z11)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z11, 1.000)
       deallocate(Z11)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,N2,M2,N1,M2,M2,N3,VBHPPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & M1,N1,N2,M2,N1,M2,N2,M2,N0,M1,N0,M1,t3C4,F2)
       allocate(Z12(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K0
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,Z12)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z12, 1.000)
       deallocate(Z12)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,M2,N3,M2,N3,M2,N3,VCHPPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,N0,M1,t3D,F2)
       allocate(Z22(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z22)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z22,-0.500)
       deallocate(Z22)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,VCHPPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,N0,M1,t3D,F2)
       allocate(Z23(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z23)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z23,-1.000)
       deallocate(Z23)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,VCHPPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,N0,M1,t3D,F2)
       allocate(Z24(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z24)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z24,-0.500)
       deallocate(Z24)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder613245(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,N0,M1,t3D,F2)
       allocate(Z25(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K5*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z25)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z25,-1.000)
       deallocate(Z25)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,N0,M1,N0,M1,t3D,F2)
       allocate(Z26(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K5*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z26)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,N2,M2,N0,M1,N0,M1,V2C,Z26, 0.500)
       deallocate(Z26)
C
       call sumx2(N2,N3,N2,N3,N0,N2,N0,N2,
     & M2,N3,N2,M2,N0,M1,N0,M1,HT2C,V2C,1.0)
       deallocate(V2C)
C
       end
