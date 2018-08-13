       subroutine t2A1000_update(N0,N1,N2,N3,HT2A,shift,
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
       real*8,allocatable::S27(:,:,:,:)
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
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
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
       real*8,allocatable::S79(:,:,:,:)
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
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::Z7(:,:,:,:)
       real*8,allocatable::Z8(:,:,:,:)
       real*8,allocatable::Z9(:,:,:,:)
       real*8,allocatable::Z10(:,:,:,:)
       real*8,allocatable::Z11(:,:,:,:)
       real*8,allocatable::X8(:,:)
       real*8,allocatable::Z12(:,:,:,:)
       real*8,allocatable::X9(:,:)
       real*8,allocatable::Z13(:,:,:,:)
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
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::Z38(:,:,:,:)
       real*8,allocatable::Z42(:,:,:,:)
       real*8,allocatable::Z44(:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::Z46(:,:,:,:)
       real*8,allocatable::Z50(:,:,:,:)
       real*8,allocatable::Z52(:,:,:,:)
       real*8,allocatable::Z56(:,:,:,:)
       real*8,allocatable::Z58(:,:,:,:)
       real*8,allocatable::Z60(:,:,:,:)
       real*8,allocatable::Z62(:,:,:,:)
       real*8,allocatable::Z64(:,:,:,:)
       real*8,allocatable::Z66(:,:,:,:)
       real*8,allocatable::Z68(:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::Z70(:,:,:,:)
       real*8,allocatable::Z78(:,:,:,:)
       real*8,allocatable::Z80(:,:,:,:)
       real*8,allocatable::Z81(:,:,:,:)
       real*8,allocatable::Z82(:,:,:,:)
C
       allocate(V2A(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       V2A=0.0d0
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S27(M1+1:N1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       X3=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,M1,N1,X3,S27,-1.000)
       deallocate(S27)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,M1,N1,X3,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder463125(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,t3A,F2)
       allocate(Z3(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K6*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,X3,F2,Z3)
       deallocate(F2)
C
       V2A=V2A-Z3
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S29(M1+1:N1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       X4=0.0d0
       call
     & sum4123(M1,N1,M1,N1,N1,M2,M1,N1,X4,S29,-1.000)
       deallocate(S29)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,M1,N1,X4,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder563124(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,t3A,F2)
       allocate(Z4(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K6*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,X4,F2,Z4)
       deallocate(F2)
C
       V2A=V2A+0.500*Z4
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,M1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S31(N0+1:M1,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:M1,N0+1:M1,N1+1:M2,N0+1:M1))
       X5=0.0d0
       call
     & sum4123(N0,M1,N0,M1,N1,M2,N0,M1,X5,S31,-1.000)
       deallocate(S31)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,N0,M1,N1,M2,N0,M1,X5,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder453126(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,N1,M2,M2,N3,M2,N3,M1,N1,t3A,F2)
       allocate(Z5(M2+1:N3,M2+1:N3,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K6*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,X5,F2,Z5)
       deallocate(F2)
C
       call
     & sum1243(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z5,-0.500)
       deallocate(Z5)
       deallocate(X5)
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
       call EGEMM(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       X6=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,N0,M1,X6,S33,-1.000)
       deallocate(S33)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N0,M1,X6,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder453126(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,M2,N3,M1,N1,t3A,F2)
       allocate(Z6(M2+1:N3,M2+1:N3,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K6*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,X6,F2,Z6)
       deallocate(F2)
C
       call
     & sum1243(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z6,-1.000)
       deallocate(Z6)
       deallocate(X6)
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
       call EGEMM(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       X7=0.0d0
       call
     & sum4123(M1,N1,M1,N1,N1,M2,N0,M1,X7,S35,-1.000)
       deallocate(S35)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N0,M1,X7,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder453126(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,M2,N3,M2,N3,M1,N1,t3A,F2)
       allocate(Z7(M2+1:N3,M2+1:N3,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K6*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,X7,F2,Z7)
       deallocate(F2)
C
       call
     & sum1243(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z7,-0.500)
       deallocate(Z7)
       deallocate(X7)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(S37(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S37)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X10(N0+1:N1,M2+1:N3,N0+1:M1,M1+1:N1))
       X10=0.0d0
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X10,S37, 1.000)
       deallocate(S37)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder513246(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(S39(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S39)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X10,S39,-1.000)
       deallocate(S39)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Z38(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I1=K7*K5*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,X10,B2,Z38)
       deallocate(B2)
C
       V2A=V2A-Z38
       deallocate(Z38)
       deallocate(X10)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S41(M2+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder2341(M2,N3,N0,M1,N1,M2,N1,M2,
     & N0,M1,N1,M2,N1,M2,M2,N3,S41,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder423156(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(Z42(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K6
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z42)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z42, 0.500)
       deallocate(Z42)
       deallocate(S41)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S43(M2+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder2341(M2,N3,M1,N1,N1,M2,N1,M2,
     & M1,N1,N1,M2,N1,M2,M2,N3,S43,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder523146(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(Z44(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z44)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z44,-0.500)
       deallocate(Z44)
       deallocate(S43)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(S45(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S45)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X11(N0+1:N1,M2+1:N3,N0+1:M1,M1+1:N1))
       X11=0.0d0
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X11,S45, 1.000)
       deallocate(S45)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder513246(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(S47(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S47)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X11,S47,-1.000)
       deallocate(S47)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Z46(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I1=K7*K5*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,X11,B2,Z46)
       deallocate(B2)
C
       call
     & sum2134(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z46,-1.000)
       deallocate(Z46)
       deallocate(X11)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S49(M2+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder2341(M2,N3,N0,M1,N1,M2,N1,M2,
     & N0,M1,N1,M2,N1,M2,M2,N3,S49,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder423156(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(Z50(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K6
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z50)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z50, 0.500)
       deallocate(Z50)
       deallocate(S49)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S51(M2+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder2341(M2,N3,M1,N1,N1,M2,N1,M2,
     & M1,N1,N1,M2,N1,M2,M2,N3,S51,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder523146(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(Z52(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z52)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z52,-0.500)
       deallocate(Z52)
       deallocate(S51)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
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
       allocate(X1(N0+1:M1,N1+1:M2))
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
       call EGEMM1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(M1+1:N1,N1+1:M2))
       X2=0.0d0
       X2=X2+Q2
       deallocate(Q2)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S55(M1+1:N1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,N0,M1,N2,M2,
     & M1,N2,N0,M1,N2,M2,M1,N1,S55,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder451236(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N2,M2,M2,N3,M2,N3,N0,M1,t3B2,F2)
       allocate(Z56(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K6*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z56)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z56
       deallocate(Z56)
       deallocate(S55)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S57(M1+1:N1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,M1,N1,N2,M2,
     & N0,M1,M1,N1,N2,M2,M1,N1,S57,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder461235(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,t3B3,F2)
       allocate(Z58(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K6*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z58)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z58
       deallocate(Z58)
       deallocate(S57)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S59(M1+1:N1,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0*K7*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,M1,N1,N2,M2,
     & M1,N2,M1,N1,N2,M2,M1,N1,S59,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder461235(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,t3B3,F2)
       allocate(Z60(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K6*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z60)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z60
       deallocate(Z60)
       deallocate(S59)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S61(N0+1:M1,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,N0,M1,N2,M2,
     & N0,M1,N0,M1,N2,M2,N0,M1,S61,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder451236(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,N2,M2,M2,N3,M2,N3,M1,N1,t3B3,F2)
       allocate(Z62(M2+1:N3,M2+1:N3,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K6*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z62)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z62,-1.000)
       deallocate(Z62)
       deallocate(S61)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S63(N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,N0,M1,N2,M2,
     & M1,N2,N0,M1,N2,M2,N0,M1,S63,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder451236(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,N2,M2,M2,N3,M2,N3,M1,N1,t3B3,F2)
       allocate(Z64(M2+1:N3,M2+1:N3,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K6*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z64)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z64,-1.000)
       deallocate(Z64)
       deallocate(S63)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S65(N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M1,N1,N2,M2,
     & N0,M1,M1,N1,N2,M2,N0,M1,S65,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder451236(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,M2,N3,M1,N1,t3B3,F2)
       allocate(Z66(M2+1:N3,M2+1:N3,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K6*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z66)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z66,-1.000)
       deallocate(Z66)
       deallocate(S65)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S67(N0+1:M1,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0*K7*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,M1,N1,N2,M2,
     & M1,N2,M1,N1,N2,M2,N0,M1,S67,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder451236(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,M2,N3,M1,N1,t3B3,F2)
       allocate(Z68(M2+1:N3,M2+1:N3,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K6*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z68)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z68,-1.000)
       deallocate(Z68)
       deallocate(S67)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(S69(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S69)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X12(N0+1:N1,M2+1:N3,N0+1:M1,M1+1:N1))
       X12=0.0d0
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X12,S69, 1.000)
       deallocate(S69)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(S71(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S71)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X12,S71, 1.000)
       deallocate(S71)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3B1,F2)
       allocate(S73(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S73)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X12,S73,-1.000)
       deallocate(S73)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3B1,F2)
       allocate(S75(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S75)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X12,S75,-1.000)
       deallocate(S75)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Z70(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I1=K7*K5*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,X12,B2,Z70)
       deallocate(B2)
C
       V2A=V2A-Z70
       call
     & sum2134(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z70, 1.000)
       deallocate(Z70)
       deallocate(X12)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,M1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S77(M2+1:N3,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder2341(M2,N3,N0,M1,N2,M2,N1,M2,
     & N0,M1,N2,M2,N1,M2,M2,N3,S77,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3B1,F2)
       allocate(Z78(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z78)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z78, 1.000)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z78,-1.000)
       deallocate(Z78)
       deallocate(S77)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S79(M2+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder2341(M2,N3,M1,N2,N2,M2,N1,M2,
     & M1,N2,N2,M2,N1,M2,M2,N3,S79,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3B1,F2)
       allocate(Z80(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z80)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z80, 1.000)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z80,-1.000)
       deallocate(Z80)
       deallocate(S79)
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
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(Z81(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I2=K7*K5*K6*K6
       I3=K0*K5
       call EGEMM2(I2,I3,Q3,F2,Z81)
       deallocate(F2)
C
       V2A=V2A+Z81
       deallocate(Z81)
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
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(Z82(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I2=K7*K5*K6*K6
       I3=K0*K8
       call EGEMM2(I2,I3,Q4,F2,Z82)
       deallocate(F2)
C
       V2A=V2A+Z82
       deallocate(Z82)
       deallocate(Q4)
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
       X1=X1+Q5
       deallocate(Q5)
C
       call sumx21(N1,N3,N0,N1,
     & N0,M1,N1,M2,X1,FAHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder431256(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(Z1(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I2=K7*K5*K6*K6
       I3=K9*K5
       call EGEMM2(I2,I3,X1,F2,Z1)
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
       call EGEMM1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2+Q6
       deallocate(Q6)
C
       call sumx21(N1,N3,N0,N1,
     & M1,N1,N1,M2,X2,FAHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder531246(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(Z2(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I2=K7*K5*K6*K6
       I3=K9*K7
       call EGEMM2(I2,I3,X2,F2,Z2)
       deallocate(F2)
C
       V2A=V2A-Z2
       deallocate(Z2)
       deallocate(X2)
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
       allocate(X8(N0+1:M1,N2+1:M2))
       X8=0.0d0
       X8=X8+Q7
       deallocate(Q7)
C
       call sumx21(N2,N3,N0,N2,
     & N0,M1,N2,M2,X8,FBHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(Z12(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I2=K7*K5*K6*K6
       I3=K0*K5
       call EGEMM2(I2,I3,X8,F2,Z12)
       deallocate(F2)
C
       V2A=V2A+Z12
       deallocate(Z12)
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
       call EGEMM1(I1,I3,D1,B2,Q8)
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
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(Z13(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I2=K7*K5*K6*K6
       I3=K0*K8
       call EGEMM2(I2,I3,X9,F2,Z13)
       deallocate(F2)
C
       V2A=V2A+Z13
       deallocate(Z13)
       deallocate(X9)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,VAHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(Z8(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z8)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z8, 1.000)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z8,-1.000)
       deallocate(Z8)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder513246(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(Z9(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z9)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z9,-1.000)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z9, 1.000)
       deallocate(Z9)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,M2,N3,VAHPPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder423156(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(Z10(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K6
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z10)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z10,-0.500)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z10, 0.500)
       deallocate(Z10)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,M2,N3,VAHPPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder523146(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(Z11(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z11)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z11, 0.500)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z11,-0.500)
       deallocate(Z11)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder451236(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N2,M2,M2,N3,M2,N3,N0,M1,t3B2,F2)
       allocate(Z14(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K6*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z14)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z14
       deallocate(Z14)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder461235(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,t3B3,F2)
       allocate(Z15(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K6*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z15)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z15
       deallocate(Z15)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder461235(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,t3B3,F2)
       allocate(Z16(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K6*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z16)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z16
       deallocate(Z16)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder451236(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,N2,M2,M2,N3,M2,N3,M1,N1,t3B3,F2)
       allocate(Z17(M2+1:N3,M2+1:N3,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K6*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z17)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z17,-1.000)
       deallocate(Z17)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder451236(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,N2,M2,M2,N3,M2,N3,M1,N1,t3B3,F2)
       allocate(Z18(M2+1:N3,M2+1:N3,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K6*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z18)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z18,-1.000)
       deallocate(Z18)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder451236(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,M2,N3,M1,N1,t3B3,F2)
       allocate(Z19(M2+1:N3,M2+1:N3,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K6*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z19)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z19,-1.000)
       deallocate(Z19)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder451236(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,M2,N3,M1,N1,t3B3,F2)
       allocate(Z20(M2+1:N3,M2+1:N3,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K6*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z20)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z20,-1.000)
       deallocate(Z20)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,N2,M2,M2,N3,M2,N3,VBPHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(Z21(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z21)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z21, 1.000)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z21,-1.000)
       deallocate(Z21)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,M2,N3,M2,N3,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(Z22(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z22)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z22, 1.000)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z22,-1.000)
       deallocate(Z22)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,N1,M2,M2,N3,VBPHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3B1,F2)
       allocate(Z23(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z23)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z23,-1.000)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z23, 1.000)
       deallocate(Z23)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,N1,M2,M2,N3,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3B1,F2)
       allocate(Z24(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z24)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z24,-1.000)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z24, 1.000)
       deallocate(Z24)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,N2,M2,N1,M2,M2,N3,VBPHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3B1,F2)
       allocate(Z25(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z25)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z25,-1.000)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z25, 1.000)
       deallocate(Z25)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,N1,M2,M2,N3,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3B1,F2)
       allocate(Z26(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z26)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z26,-1.000)
       call
     & sum1342(M2,N3,M2,N3,N0,M1,M1,N1,V2A,Z26, 1.000)
       deallocate(Z26)
C
       call sumx2(N1,N3,N1,N3,N0,N1,N0,N1,
     & M2,N3,M2,N3,N0,M1,M1,N1,HT2A,V2A,1.0)
       deallocate(V2A)
C
       end
