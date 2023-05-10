       subroutine t2A0011_update(N0,N1,N2,N3,HT2A,shift,
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
       real*8 HT2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
C
       real*8,allocatable::V2A(:,:,:,:)
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F1(:,:,:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::S51(:,:,:,:)
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
       real*8,allocatable::S73(:,:,:,:)
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
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
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::Z9(:,:,:,:)
       real*8,allocatable::Z10(:,:,:,:)
       real*8,allocatable::Z11(:,:,:,:)
       real*8,allocatable::Z12(:,:,:,:)
       real*8,allocatable::X8(:,:)
       real*8,allocatable::Z13(:,:,:,:)
       real*8,allocatable::X9(:,:)
       real*8,allocatable::Z14(:,:,:,:)
       real*8,allocatable::Z15(:,:,:,:)
       real*8,allocatable::Z16(:,:,:,:)
       real*8,allocatable::Z17(:,:,:,:)
       real*8,allocatable::Z18(:,:,:,:)
       real*8,allocatable::Z19(:,:,:,:)
       real*8,allocatable::Z20(:,:,:,:)
       real*8,allocatable::Z21(:,:,:,:)
       real*8,allocatable::Z22(:,:,:,:)
       real*8,allocatable::Z23(:,:,:,:)
       real*8,allocatable::Z24(:,:,:,:)
       real*8,allocatable::Z25(:,:,:,:)
       real*8,allocatable::Z26(:,:,:,:)
       real*8,allocatable::Z27(:,:,:,:)
       real*8,allocatable::Z28(:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::Z38(:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::Z44(:,:,:,:)
       real*8,allocatable::Z48(:,:,:,:)
       real*8,allocatable::Z52(:,:,:,:)
       real*8,allocatable::Z54(:,:,:,:)
       real*8,allocatable::Z56(:,:,:,:)
       real*8,allocatable::Z58(:,:,:,:)
       real*8,allocatable::Z60(:,:,:,:)
       real*8,allocatable::Z62(:,:,:,:)
       real*8,allocatable::Z70(:,:,:,:)
       real*8,allocatable::Z78(:,:,:,:)
       real*8,allocatable::Z79(:,:,:,:)
       real*8,allocatable::Z80(:,:,:,:)
C
       allocate(V2A(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       V2A=0.0d0
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S29(N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       X3=0.0d0
       call
     & sum4123(N0,M1,M1,N1,M2,N3,N0,M1,X3,S29,-1.000)
       deallocate(S29)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N0,M1,X3,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3A,F2)
       allocate(Z3(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K9
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,X3,F2,Z3)
       deallocate(F2)
C
       V2A=V2A-Z3
       call
     & sum1243(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z3, 1.000)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S31(N0+1:M1,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       X4=0.0d0
       call
     & sum4123(M1,N1,M1,N1,M2,N3,N0,M1,X4,S31,-1.000)
       deallocate(S31)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N0,M1,X4,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3A,F2)
       allocate(Z4(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K9
       I3=K6*K7*K7
       call DMATMAT(I1,I2,I3,X4,F2,Z4)
       deallocate(F2)
C
       V2A=V2A+0.500*Z4
       call
     & sum1243(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z4,-0.500)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S33(N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       X5=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,N0,M1,X5,S33,-1.000)
       deallocate(S33)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N0,M1,X5,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,t3A,F2)
       allocate(Z5(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K9
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,X5,F2,Z5)
       deallocate(F2)
C
       V2A=V2A-Z5
       call
     & sum1243(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z5, 1.000)
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S35(N0+1:M1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       X6=0.0d0
       call
     & sum4123(M1,N1,M1,N1,N1,M2,N0,M1,X6,S35,-1.000)
       deallocate(S35)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N0,M1,X6,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,t3A,F2)
       allocate(Z6(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K9
       I3=K9*K7*K7
       call DMATMAT(I1,I2,I3,X6,F2,Z6)
       deallocate(F2)
C
       V2A=V2A+0.500*Z6
       call
     & sum1243(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z6,-0.500)
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(S37(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S37)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X10(N0+1:N1,N1+1:M2,N0+1:M1,N0+1:M1))
       X10=0.0d0
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X10,S37, 1.000)
       deallocate(S37)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(S39(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S39)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X10,S39, 2.000)
       deallocate(S39)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S41(N1+1:M2,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       X7=0.0d0
       call
     & sum4123(M1,N1,N1,M2,N1,M2,N1,M2,X7,S41,-1.000)
       deallocate(S41)
C
       call sumx2341(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,X7,VAHPPP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(Z9(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K9*K9*K7
       call DMATMAT(I1,I2,I3,X7,F2,Z9)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z9,-0.500)
       deallocate(Z9)
       deallocate(X7)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(S43(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S43)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X11(N0+1:N1,N1+1:M2,N0+1:M1,N0+1:M1))
       X11=0.0d0
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X11,S43, 1.000)
       deallocate(S43)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(S45(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S45)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X11,S45, 2.000)
       deallocate(S45)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S47(N1+1:M2,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,M1,N1,N1,M2,N1,M2,
     & M1,N1,N1,M2,N1,M2,N1,M2,S47,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(Z48(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K9*K9*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z48)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z48, 0.500)
       deallocate(Z48)
       deallocate(S47)
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
       allocate(S51(N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,N0,M1,M2,N3,
     & M1,N2,N0,M1,M2,N3,N0,M1,S51,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,t3B4,F2)
       allocate(Z52(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K9
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z52)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z52
       call
     & sum1243(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z52,-1.000)
       deallocate(Z52)
       deallocate(S51)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S53(N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M1,N1,M2,N3,
     & N0,M1,M1,N1,M2,N3,N0,M1,S53,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(Z54(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K9
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z54)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z54
       call
     & sum1243(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z54, 1.000)
       deallocate(Z54)
       deallocate(S53)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S55(N0+1:M1,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,M1,N1,M2,N3,
     & M1,N2,M1,N1,M2,N3,N0,M1,S55,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(Z56(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K9
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z56)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z56
       call
     & sum1243(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z56, 1.000)
       deallocate(Z56)
       deallocate(S55)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S57(N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,N0,M1,N2,M2,
     & M1,N2,N0,M1,N2,M2,N0,M1,S57,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,t3B4,F2)
       allocate(Z58(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K9
       I3=K0*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z58)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z58
       call
     & sum1243(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z58,-1.000)
       deallocate(Z58)
       deallocate(S57)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S59(N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0*K7*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M1,N1,N2,M2,
     & N0,M1,M1,N1,N2,M2,N0,M1,S59,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(Z60(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K9
       I3=K0*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z60)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z60
       call
     & sum1243(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z60, 1.000)
       deallocate(Z60)
       deallocate(S59)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S61(N0+1:M1,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0*K7*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,M1,N1,N2,M2,
     & M1,N2,M1,N1,N2,M2,N0,M1,S61,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(Z62(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K9
       I3=K0*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z62)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z62
       call
     & sum1243(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z62, 1.000)
       deallocate(Z62)
       deallocate(S61)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(S63(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S63)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X10,S63,-2.000)
       deallocate(S63)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(S65(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S65)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X10,S65,-2.000)
       deallocate(S65)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(S67(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S67)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X10,S67,-2.000)
       deallocate(S67)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Z38(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K9
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,X10,B2,Z38)
       deallocate(B2)
C
       V2A=V2A+0.500*Z38
       deallocate(Z38)
       deallocate(X10)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S69(N1+1:M2,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,M1,N2,N2,M2,N1,M2,
     & M1,N2,N2,M2,N1,M2,N1,M2,S69,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z70(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K9*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z70)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z70,-1.000)
       deallocate(Z70)
       deallocate(S69)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(S71(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S71)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X11,S71, 2.000)
       deallocate(S71)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(S73(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S73)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X11,S73, 2.000)
       deallocate(S73)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(S75(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S75)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X11,S75, 2.000)
       deallocate(S75)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Z44(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K9
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,X11,B2,Z44)
       deallocate(B2)
C
       call
     & sum2134(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z44, 0.500)
       deallocate(Z44)
       deallocate(X11)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S77(N1+1:M2,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,M1,N2,N2,M2,N1,M2,
     & M1,N2,N2,M2,N1,M2,N1,M2,S77,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z78(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K9*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z78)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z78, 1.000)
       deallocate(Z78)
       deallocate(S77)
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
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z79(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K9*K9
       I3=K6*K8
       call DVECMAT(I2,I3,Q3,F2,Z79)
       deallocate(F2)
C
       V2A=V2A+Z79
       deallocate(Z79)
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
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z80(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K9*K9
       I3=K0*K8
       call DVECMAT(I2,I3,Q4,F2,Z80)
       deallocate(F2)
C
       V2A=V2A+Z80
       deallocate(Z80)
       deallocate(Q4)
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
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(Z1(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K9*K9
       I3=K6*K7
       call DVECMAT(I2,I3,X1,F2,Z1)
       deallocate(F2)
C
       V2A=V2A+Z1
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
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(Z2(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K9*K9
       I3=K9*K7
       call DVECMAT(I2,I3,X2,F2,Z2)
       deallocate(F2)
C
       V2A=V2A+Z2
       deallocate(Z2)
       deallocate(X2)
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
       allocate(X8(M1+1:N2,M2+1:N3))
       X8=0.0d0
       X8=X8+Q7
       deallocate(Q7)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,M2,N3,X8,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z13(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K9*K9
       I3=K6*K8
       call DVECMAT(I2,I3,X8,F2,Z13)
       deallocate(F2)
C
       V2A=V2A+Z13
       deallocate(Z13)
       deallocate(X8)
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
       allocate(X9(M1+1:N2,N2+1:M2))
       X9=0.0d0
       X9=X9+Q8
       deallocate(Q8)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,N2,M2,X9,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z14(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K9*K9
       I3=K0*K8
       call DVECMAT(I2,I3,X9,F2,Z14)
       deallocate(F2)
C
       V2A=V2A+Z14
       deallocate(Z14)
       deallocate(X9)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(Z7(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z7)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z7,-0.500)
       deallocate(Z7)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(Z8(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z8)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z8,-1.000)
       deallocate(Z8)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(Z10(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z10)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z10, 0.500)
       deallocate(Z10)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(Z11(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z11)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z11, 1.000)
       deallocate(Z11)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,VAHPPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(Z12(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K9*K9*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z12)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z12, 0.500)
       deallocate(Z12)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,t3B4,F2)
       allocate(Z15(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K9
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z15)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z15
       call
     & sum1243(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z15,-1.000)
       deallocate(Z15)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(Z16(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K9
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z16)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z16
       call
     & sum1243(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z16, 1.000)
       deallocate(Z16)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(Z17(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K9
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z17)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z17
       call
     & sum1243(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z17, 1.000)
       deallocate(Z17)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,t3B4,F2)
       allocate(Z18(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K9
       I3=K0*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z18)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z18
       call
     & sum1243(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z18,-1.000)
       deallocate(Z18)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(Z19(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K9
       I3=K0*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z19)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z19
       call
     & sum1243(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z19, 1.000)
       deallocate(Z19)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(Z20(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K9
       I3=K0*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z20)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z20
       call
     & sum1243(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z20, 1.000)
       deallocate(Z20)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z21(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z21)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z21, 1.000)
       deallocate(Z21)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z22(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K6*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z22)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z22, 1.000)
       deallocate(Z22)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z23(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K9*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z23)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z23, 1.000)
       deallocate(Z23)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z24(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K9*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z24)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z24, 1.000)
       deallocate(Z24)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z25(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z25)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z25,-1.000)
       deallocate(Z25)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z26(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K6*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z26)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z26,-1.000)
       deallocate(Z26)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z27(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K9*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z27)
       deallocate(D1)
       deallocate(F2)

       call
     & sum1342(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z27,-1.000)
       deallocate(Z27)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z28(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K9
       I3=K9*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z28)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,N0,M1,V2A,Z28,-1.000)
       deallocate(Z28)
C
       call sumx2(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,M2,N1,M2,N0,M1,N0,M1,HT2A,V2A,1.0)
       deallocate(V2A)
C
       end
