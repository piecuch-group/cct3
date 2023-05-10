       subroutine t2A1111_update(N0,N1,N2,N3,HT2A,shift,
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
       real*8,allocatable::S79(:,:,:,:)
       real*8,allocatable::S81(:,:,:,:)
       real*8,allocatable::S83(:,:,:,:)
       real*8,allocatable::S85(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::S91(:,:,:,:)
       real*8,allocatable::S93(:,:,:,:)
       real*8,allocatable::S95(:,:,:,:)
       real*8,allocatable::S97(:,:,:,:)
       real*8,allocatable::S99(:,:,:,:)
       real*8,allocatable::S101(:,:,:,:)
       real*8,allocatable::S103(:,:,:,:)
       real*8,allocatable::S105(:,:,:,:)
       real*8,allocatable::S107(:,:,:,:)
       real*8,allocatable::S109(:,:,:,:)
       real*8,allocatable::S111(:,:,:,:)
       real*8,allocatable::S113(:,:,:,:)
       real*8,allocatable::S115(:,:,:,:)
       real*8,allocatable::S117(:,:,:,:)
       real*8,allocatable::S119(:,:,:,:)
       real*8,allocatable::S121(:,:,:,:)
       real*8,allocatable::S123(:,:,:,:)
       real*8,allocatable::S125(:,:,:,:)
       real*8,allocatable::S127(:,:,:,:)
       real*8,allocatable::S129(:,:,:,:)
       real*8,allocatable::S131(:,:,:,:)
       real*8,allocatable::S133(:,:,:,:)
       real*8,allocatable::S135(:,:,:,:)
       real*8,allocatable::S137(:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::Z1(:,:,:,:)
       real*8,allocatable::X2(:,:)
       real*8,allocatable::Z2(:,:,:,:)
       real*8,allocatable::X3(:,:)
       real*8,allocatable::Z3(:,:,:,:)
       real*8,allocatable::X4(:,:)
       real*8,allocatable::Z4(:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::Z5(:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::Z6(:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::Z7(:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::Z8(:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::Z9(:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::Z10(:,:,:,:)
       real*8,allocatable::Z11(:,:,:,:)
       real*8,allocatable::Z12(:,:,:,:)
       real*8,allocatable::Z13(:,:,:,:)
       real*8,allocatable::Z14(:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::Z15(:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::Z16(:,:,:,:)
       real*8,allocatable::Z17(:,:,:,:)
       real*8,allocatable::Z18(:,:,:,:)
       real*8,allocatable::Z19(:,:,:,:)
       real*8,allocatable::Z20(:,:,:,:)
       real*8,allocatable::Z21(:,:,:,:)
       real*8,allocatable::Z22(:,:,:,:)
       real*8,allocatable::X13(:,:)
       real*8,allocatable::Z23(:,:,:,:)
       real*8,allocatable::X14(:,:)
       real*8,allocatable::Z24(:,:,:,:)
       real*8,allocatable::X15(:,:)
       real*8,allocatable::Z25(:,:,:,:)
       real*8,allocatable::X16(:,:)
       real*8,allocatable::Z26(:,:,:,:)
       real*8,allocatable::Z27(:,:,:,:)
       real*8,allocatable::Z28(:,:,:,:)
       real*8,allocatable::Z29(:,:,:,:)
       real*8,allocatable::Z30(:,:,:,:)
       real*8,allocatable::Z31(:,:,:,:)
       real*8,allocatable::Z32(:,:,:,:)
       real*8,allocatable::Z33(:,:,:,:)
       real*8,allocatable::Z34(:,:,:,:)
       real*8,allocatable::Z35(:,:,:,:)
       real*8,allocatable::Z36(:,:,:,:)
       real*8,allocatable::Z37(:,:,:,:)
       real*8,allocatable::Z38(:,:,:,:)
       real*8,allocatable::Z39(:,:,:,:)
       real*8,allocatable::Z40(:,:,:,:)
       real*8,allocatable::Z41(:,:,:,:)
       real*8,allocatable::Z42(:,:,:,:)
       real*8,allocatable::Z43(:,:,:,:)
       real*8,allocatable::Z44(:,:,:,:)
       real*8,allocatable::Z45(:,:,:,:)
       real*8,allocatable::Z46(:,:,:,:)
       real*8,allocatable::Z47(:,:,:,:)
       real*8,allocatable::Z48(:,:,:,:)
       real*8,allocatable::Z49(:,:,:,:)
       real*8,allocatable::Z50(:,:,:,:)
       real*8,allocatable::X17(:,:,:,:)
       real*8,allocatable::Z64(:,:,:,:)
       real*8,allocatable::X18(:,:,:,:)
       real*8,allocatable::Z76(:,:,:,:)
       real*8,allocatable::Z84(:,:,:,:)
       real*8,allocatable::Z86(:,:,:,:)
       real*8,allocatable::Z92(:,:,:,:)
       real*8,allocatable::Z94(:,:,:,:)
       real*8,allocatable::Z96(:,:,:,:)
       real*8,allocatable::Z98(:,:,:,:)
       real*8,allocatable::Z100(:,:,:,:)
       real*8,allocatable::Z102(:,:,:,:)
       real*8,allocatable::Z104(:,:,:,:)
       real*8,allocatable::Z106(:,:,:,:)
       real*8,allocatable::Z120(:,:,:,:)
       real*8,allocatable::Z122(:,:,:,:)
       real*8,allocatable::Z136(:,:,:,:)
       real*8,allocatable::Z138(:,:,:,:)
       real*8,allocatable::Z139(:,:,:,:)
       real*8,allocatable::Z140(:,:,:,:)
       real*8,allocatable::Z141(:,:,:,:)
       real*8,allocatable::Z142(:,:,:,:)
C
       allocate(V2A(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       V2A=0.0d0
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,M1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S51(M1+1:N1,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5*K5
       I2=K7
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       X5=0.0d0
       call
     & sum4123(N0,M1,N0,M1,M2,N3,M1,N1,X5,S51,-1.000)
       deallocate(S51)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,N0,M1,M2,N3,M1,N1,X5,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(Z5(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K6*K5*K5
       call DMATMAT(I1,I2,I3,X5,F2,Z5)
       deallocate(F2)
C
       V2A=V2A+0.500*Z5
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z5,-0.500)
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S53(M1+1:N1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K7
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       X6=0.0d0
       call
     & sum4123(N0,M1,M1,N1,M2,N3,M1,N1,X6,S53,-1.000)
       deallocate(S53)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,M1,N1,X6,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(Z6(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,X6,F2,Z6)
       deallocate(F2)
C
       V2A=V2A+Z6
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z6,-1.000)
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S55(M1+1:N1,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K7
       I2=K7
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       X7=0.0d0
       call
     & sum4123(M1,N1,M1,N1,M2,N3,M1,N1,X7,S55,-1.000)
       deallocate(S55)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,M1,N1,X7,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(Z7(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K6*K7*K7
       call DMATMAT(I1,I2,I3,X7,F2,Z7)
       deallocate(F2)
C
       V2A=V2A+0.500*Z7
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z7,-0.500)
       deallocate(Z7)
       deallocate(X7)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,M1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S57(M1+1:N1,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5*K5
       I2=K7
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       X8=0.0d0
       call
     & sum4123(N0,M1,N0,M1,N1,M2,M1,N1,X8,S57,-1.000)
       deallocate(S57)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,N0,M1,N1,M2,M1,N1,X8,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,N1,M2,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(Z8(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K9*K5*K5
       call DMATMAT(I1,I2,I3,X8,F2,Z8)
       deallocate(F2)
C
       V2A=V2A+0.500*Z8
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z8,-0.500)
       deallocate(Z8)
       deallocate(X8)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S59(M1+1:N1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K7
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       X9=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,M1,N1,X9,S59,-1.000)
       deallocate(S59)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,M1,N1,X9,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(Z9(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,X9,F2,Z9)
       deallocate(F2)
C
       V2A=V2A+Z9
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z9,-1.000)
       deallocate(Z9)
       deallocate(X9)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S61(M1+1:N1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K7
       I2=K7
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       X10=0.0d0
       call
     & sum4123(M1,N1,M1,N1,N1,M2,M1,N1,X10,S61,-1.000)
       deallocate(S61)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,M1,N1,X10,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(Z10(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K9*K7*K7
       call DMATMAT(I1,I2,I3,X10,F2,Z10)
       deallocate(F2)
C
       V2A=V2A+0.500*Z10
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z10,-0.500)
       deallocate(Z10)
       deallocate(X10)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S63(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S63)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X17(N0+1:N1,N1+1:M2,M1+1:N1,M1+1:N1))
       X17=0.0d0
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X17,S63, 1.000)
       deallocate(S63)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S65(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S65)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X17,S65, 1.000)
       deallocate(S65)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S67(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S67)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X17,S67, 2.000)
       deallocate(S67)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S69(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S69)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X17,S69, 2.000)
       deallocate(S69)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S71(N1+1:M2,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2))
       X11=0.0d0
       call
     & sum4123(N0,M1,N1,M2,N1,M2,N1,M2,X11,S71,-1.000)
       deallocate(S71)
C
       call sumx2341(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,X11,VAHPPP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z15(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K9*K5
       call DMATMAT(I1,I2,I3,X11,F2,Z15)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z15,-0.500)
       deallocate(Z15)
       deallocate(X11)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S73(N1+1:M2,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       X12=0.0d0
       call
     & sum4123(M1,N1,N1,M2,N1,M2,N1,M2,X12,S73,-1.000)
       deallocate(S73)
C
       call sumx2341(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,X12,VAHPPP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z16(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K9*K7
       call DMATMAT(I1,I2,I3,X12,F2,Z16)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z16,-0.500)
       deallocate(Z16)
       deallocate(X12)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S75(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S75)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X18(N0+1:N1,N1+1:M2,M1+1:N1,M1+1:N1))
       X18=0.0d0
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X18,S75, 1.000)
       deallocate(S75)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S77(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S77)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X18,S77, 1.000)
       deallocate(S77)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S79(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S79)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X18,S79, 2.000)
       deallocate(S79)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S81(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S81)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X18,S81, 2.000)
       deallocate(S81)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S83(N1+1:M2,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S83)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,M1,N1,M2,N1,M2,
     & N0,M1,N1,M2,N1,M2,N1,M2,S83,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z84(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K9*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z84)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z84, 0.500)
       deallocate(Z84)
       deallocate(S83)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S85(N1+1:M2,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S85)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,M1,N1,N1,M2,N1,M2,
     & M1,N1,N1,M2,N1,M2,N1,M2,S85,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z86(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K9*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z86)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z86, 0.500)
       deallocate(Z86)
       deallocate(S85)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q1(N0+1:M1,M2+1:N3))
       I1=K6*K5
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:M1,M2+1:N3))
       X1=0.0d0
       X1=X1+Q1
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q2(M1+1:N1,M2+1:N3))
       I1=K6*K7
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(M1+1:N1,M2+1:N3))
       X2=0.0d0
       X2=X2+Q2
       deallocate(Q2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q3(N0+1:M1,N1+1:M2))
       I1=K9*K5
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:M1,N1+1:M2))
       X3=0.0d0
       X3=X3+Q3
       deallocate(Q3)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q4(M1+1:N1,N1+1:M2))
       I1=K9*K7
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(M1+1:N1,N1+1:M2))
       X4=0.0d0
       X4=X4+Q4
       deallocate(Q4)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S91(M1+1:N1,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5*K5
       I2=K7
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,N0,M1,M2,N3,
     & N0,M1,N0,M1,M2,N3,M1,N1,S91,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z92(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K6*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z92)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z92
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z92,-1.000)
       deallocate(Z92)
       deallocate(S91)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S93(M1+1:N1,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K8
       I2=K7
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S93)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,N0,M1,M2,N3,
     & M1,N2,N0,M1,M2,N3,M1,N1,S93,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z94(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z94)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z94
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z94,-1.000)
       deallocate(Z94)
       deallocate(S93)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S95(M1+1:N1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K7
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,M1,N1,M2,N3,
     & N0,M1,M1,N1,M2,N3,M1,N1,S95,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z96(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z96)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z96
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z96,-1.000)
       deallocate(Z96)
       deallocate(S95)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S97(M1+1:N1,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K8
       I2=K7
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,M1,N1,M2,N3,
     & M1,N2,M1,N1,M2,N3,M1,N1,S97,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z98(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z98)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z98
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z98,-1.000)
       deallocate(Z98)
       deallocate(S97)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S99(M1+1:N1,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5*K5
       I2=K7
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,N0,M1,N2,M2,
     & N0,M1,N0,M1,N2,M2,M1,N1,S99,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z100(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K0*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z100)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z100
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z100,-1.000)
       deallocate(Z100)
       deallocate(S99)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S101(M1+1:N1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K8
       I2=K7
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S101)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,N0,M1,N2,M2,
     & M1,N2,N0,M1,N2,M2,M1,N1,S101,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z102(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K0*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z102)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z102
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z102,-1.000)
       deallocate(Z102)
       deallocate(S101)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S103(M1+1:N1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0*K7*K5
       I2=K7
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S103)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,M1,N1,N2,M2,
     & N0,M1,M1,N1,N2,M2,M1,N1,S103,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z104(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K0*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z104)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z104
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z104,-1.000)
       deallocate(Z104)
       deallocate(S103)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S105(M1+1:N1,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0*K7*K8
       I2=K7
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,M1,N1,N2,M2,
     & M1,N2,M1,N1,N2,M2,M1,N1,S105,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z106(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K0*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z106)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z106
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z106,-1.000)
       deallocate(Z106)
       deallocate(S105)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S107(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S107)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X17,S107,-2.000)
       deallocate(S107)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S109(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S109)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X17,S109,-2.000)
       deallocate(S109)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S111(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S111)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X17,S111,-2.000)
       deallocate(S111)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S113(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S113)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X17,S113,-2.000)
       deallocate(S113)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S115(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S115)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X17,S115,-2.000)
       deallocate(S115)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S117(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S117)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X17,S117,-2.000)
       deallocate(S117)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Z64(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K9
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,X17,B2,Z64)
       deallocate(B2)
C
       V2A=V2A+0.500*Z64
       deallocate(Z64)
       deallocate(X17)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,M1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S119(N1+1:M2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S119)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,M1,N2,M2,N1,M2,
     & N0,M1,N2,M2,N1,M2,N1,M2,S119,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z120(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z120)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z120,-1.000)
       deallocate(Z120)
       deallocate(S119)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S121(N1+1:M2,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S121)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,M1,N2,N2,M2,N1,M2,
     & M1,N2,N2,M2,N1,M2,N1,M2,S121,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z122(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z122)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z122,-1.000)
       deallocate(Z122)
       deallocate(S121)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S123(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S123)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X18,S123, 2.000)
       deallocate(S123)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S125(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S125)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X18,S125, 2.000)
       deallocate(S125)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S127(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S127)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X18,S127, 2.000)
       deallocate(S127)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S129(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S129)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X18,S129, 2.000)
       deallocate(S129)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S131(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S131)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X18,S131, 2.000)
       deallocate(S131)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S133(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S133)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X18,S133, 2.000)
       deallocate(S133)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Z76(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K9
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,X18,B2,Z76)
       deallocate(B2)
C
       call
     & sum2134(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z76, 0.500)
       deallocate(Z76)
       deallocate(X18)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,M1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S135(N1+1:M2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S135)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,M1,N2,M2,N1,M2,
     & N0,M1,N2,M2,N1,M2,N1,M2,S135,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z136(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z136)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z136, 1.000)
       deallocate(Z136)
       deallocate(S135)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S137(N1+1:M2,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S137)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,M1,N2,N2,M2,N1,M2,
     & M1,N2,N2,M2,N1,M2,N1,M2,S137,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z138(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z138)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z138, 1.000)
       deallocate(Z138)
       deallocate(S137)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q5(N0+1:M1,M2+1:N3))
       I1=K6*K5
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z139(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I2=K7*K7*K9*K9
       I3=K6*K5
       call DVECMAT(I2,I3,Q5,F2,Z139)
       deallocate(F2)
C
       V2A=V2A+Z139
       deallocate(Z139)
       deallocate(Q5)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q6(M1+1:N2,M2+1:N3))
       I1=K6*K8
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z140(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I2=K7*K7*K9*K9
       I3=K6*K8
       call DVECMAT(I2,I3,Q6,F2,Z140)
       deallocate(F2)
C
       V2A=V2A+Z140
       deallocate(Z140)
       deallocate(Q6)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q7(N0+1:M1,N2+1:M2))
       I1=K0*K5
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z141(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I2=K7*K7*K9*K9
       I3=K0*K5
       call DVECMAT(I2,I3,Q7,F2,Z141)
       deallocate(F2)
C
       V2A=V2A+Z141
       deallocate(Z141)
       deallocate(Q7)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q8(M1+1:N2,N2+1:M2))
       I1=K0*K8
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z142(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I2=K7*K7*K9*K9
       I3=K0*K8
       call DVECMAT(I2,I3,Q8,F2,Z142)
       deallocate(F2)
C
       V2A=V2A+Z142
       deallocate(Z142)
       deallocate(Q8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q9(N0+1:M1,M2+1:N3))
       I1=K6*K5
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1+Q9
       deallocate(Q9)
C
       call sumx21(N1,N3,N0,N1,
     & N0,M1,M2,N3,X1,FAHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z1(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I2=K7*K7*K9*K9
       I3=K6*K5
       call DVECMAT(I2,I3,X1,F2,Z1)
       deallocate(F2)
C
       V2A=V2A+Z1
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q10(M1+1:N1,M2+1:N3))
       I1=K6*K7
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q10)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2+Q10
       deallocate(Q10)
C
       call sumx21(N1,N3,N0,N1,
     & M1,N1,M2,N3,X2,FAHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z2(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I2=K7*K7*K9*K9
       I3=K6*K7
       call DVECMAT(I2,I3,X2,F2,Z2)
       deallocate(F2)
C
       V2A=V2A+Z2
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q11(N0+1:M1,N1+1:M2))
       I1=K9*K5
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3+Q11
       deallocate(Q11)
C
       call sumx21(N1,N3,N0,N1,
     & N0,M1,N1,M2,X3,FAHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z3(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I2=K7*K7*K9*K9
       I3=K9*K5
       call DVECMAT(I2,I3,X3,F2,Z3)
       deallocate(F2)
C
       V2A=V2A+Z3
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q12(M1+1:N1,N1+1:M2))
       I1=K9*K7
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       X4=X4+Q12
       deallocate(Q12)
C
       call sumx21(N1,N3,N0,N1,
     & M1,N1,N1,M2,X4,FAHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z4(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I2=K7*K7*K9*K9
       I3=K9*K7
       call DVECMAT(I2,I3,X4,F2,Z4)
       deallocate(F2)
C
       V2A=V2A+Z4
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q13(N0+1:M1,M2+1:N3))
       I1=K6*K5
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(N0+1:M1,M2+1:N3))
       X13=0.0d0
       X13=X13+Q13
       deallocate(Q13)
C
       call sumx21(N2,N3,N0,N2,
     & N0,M1,M2,N3,X13,FBHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z23(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I2=K7*K7*K9*K9
       I3=K6*K5
       call DVECMAT(I2,I3,X13,F2,Z23)
       deallocate(F2)
C
       V2A=V2A+Z23
       deallocate(Z23)
       deallocate(X13)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q14(M1+1:N2,M2+1:N3))
       I1=K6*K8
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(M1+1:N2,M2+1:N3))
       X14=0.0d0
       X14=X14+Q14
       deallocate(Q14)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,M2,N3,X14,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z24(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I2=K7*K7*K9*K9
       I3=K6*K8
       call DVECMAT(I2,I3,X14,F2,Z24)
       deallocate(F2)
C
       V2A=V2A+Z24
       deallocate(Z24)
       deallocate(X14)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q15(N0+1:M1,N2+1:M2))
       I1=K0*K5
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(N0+1:M1,N2+1:M2))
       X15=0.0d0
       X15=X15+Q15
       deallocate(Q15)
C
       call sumx21(N2,N3,N0,N2,
     & N0,M1,N2,M2,X15,FBHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z25(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I2=K7*K7*K9*K9
       I3=K0*K5
       call DVECMAT(I2,I3,X15,F2,Z25)
       deallocate(F2)
C
       V2A=V2A+Z25
       deallocate(Z25)
       deallocate(X15)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q16(M1+1:N2,N2+1:M2))
       I1=K0*K8
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(M1+1:N2,N2+1:M2))
       X16=0.0d0
       X16=X16+Q16
       deallocate(Q16)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,N2,M2,X16,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z26(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I2=K7*K7*K9*K9
       I3=K0*K8
       call DVECMAT(I2,I3,X16,F2,Z26)
       deallocate(F2)
C
       V2A=V2A+Z26
       deallocate(Z26)
       deallocate(X16)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,VAHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z11(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z11)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z11,-0.500)
       deallocate(Z11)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z12(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z12)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z12,-0.500)
       deallocate(Z12)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z13(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z13)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z13,-1.000)
       deallocate(Z13)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z14(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z14)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z14,-1.000)
       deallocate(Z14)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,VAHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z17(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z17)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z17, 0.500)
       deallocate(Z17)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z18(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z18)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z18, 0.500)
       deallocate(Z18)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z19(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z19)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z19, 1.000)
       deallocate(Z19)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z20(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z20)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z20, 1.000)
       deallocate(Z20)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,VAHPPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z21(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K9*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z21)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z21, 0.500)
       deallocate(Z21)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,VAHPPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z22(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K9*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z22)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z22, 0.500)
       deallocate(Z22)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z27(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K6*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z27)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z27
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z27,-1.000)
       deallocate(Z27)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z28(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z28)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z28
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z28,-1.000)
       deallocate(Z28)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z29(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z29)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z29
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z29,-1.000)
       deallocate(Z29)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z30(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z30)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z30
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z30,-1.000)
       deallocate(Z30)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z31(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K0*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z31)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z31
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z31,-1.000)
       deallocate(Z31)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z32(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K0*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z32)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z32
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z32,-1.000)
       deallocate(Z32)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z33(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K0*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z33)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z33
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z33,-1.000)
       deallocate(Z33)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z34(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K9*K9
       I3=K0*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z34)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z34
       call
     & sum1243(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z34,-1.000)
       deallocate(Z34)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z35(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z35)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z35, 1.000)
       deallocate(Z35)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z36(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z36)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z36, 1.000)
       deallocate(Z36)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,N2,M2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z37(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K6*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z37)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z37, 1.000)
       deallocate(Z37)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z38(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K6*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z38)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z38, 1.000)
       deallocate(Z38)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z39(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z39)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z39, 1.000)
       deallocate(Z39)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z40(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z40)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z40, 1.000)
       deallocate(Z40)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,N2,M2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z41(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z41)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z41, 1.000)
       deallocate(Z41)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z42(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z42)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z42, 1.000)
       deallocate(Z42)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z43(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z43)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z43,-1.000)
       deallocate(Z43)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z44(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z44)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z44,-1.000)
       deallocate(Z44)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,N2,M2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z45(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K6*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z45)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z45,-1.000)
       deallocate(Z45)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z46(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K6*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z46)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z46,-1.000)
       deallocate(Z46)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z47(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z47)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z47,-1.000)
       deallocate(Z47)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z48(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z48)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z48,-1.000)
       deallocate(Z48)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,N2,M2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z49(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z49)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z49,-1.000)
       deallocate(Z49)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z50(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K9
       I3=K9*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z50)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,M1,N1,M1,N1,V2A,Z50,-1.000)
       deallocate(Z50)
C
       call sumx2(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,M2,N1,M2,M1,N1,M1,N1,HT2A,V2A,1.0)
       deallocate(V2A)
C
       end
