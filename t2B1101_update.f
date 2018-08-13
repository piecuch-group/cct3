       subroutine t2B1101_update(N0,N1,N2,N3,HT2B,shift,
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
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::S89(:,:,:,:)
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
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::S125(:,:,:,:)
       real*8,allocatable::S127(:,:,:,:)
       real*8,allocatable::S129(:,:,:,:)
       real*8,allocatable::S131(:,:,:,:)
       real*8,allocatable::S133(:,:,:,:)
       real*8,allocatable::S135(:,:,:,:)
       real*8,allocatable::S137(:,:,:,:)
       real*8,allocatable::S139(:,:,:,:)
       real*8,allocatable::S141(:,:,:,:)
       real*8,allocatable::S143(:,:,:,:)
       real*8,allocatable::S145(:,:,:,:)
       real*8,allocatable::S147(:,:,:,:)
       real*8,allocatable::S149(:,:,:,:)
       real*8,allocatable::S151(:,:,:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::S157(:,:,:,:)
       real*8,allocatable::S159(:,:,:,:)
       real*8,allocatable::S161(:,:,:,:)
       real*8,allocatable::S163(:,:,:,:)
       real*8,allocatable::S165(:,:,:,:)
       real*8,allocatable::S167(:,:,:,:)
       real*8,allocatable::S169(:,:,:,:)
       real*8,allocatable::S171(:,:,:,:)
       real*8,allocatable::S173(:,:,:,:)
       real*8,allocatable::S175(:,:,:,:)
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
       real*8,allocatable::Z15(:,:,:,:)
       real*8,allocatable::Z16(:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::Z17(:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::Z18(:,:,:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::Z19(:,:,:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::Z20(:,:,:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::Z21(:,:,:,:)
       real*8,allocatable::X16(:,:,:,:)
       real*8,allocatable::Z22(:,:,:,:)
       real*8,allocatable::X17(:,:,:,:)
       real*8,allocatable::Z23(:,:,:,:)
       real*8,allocatable::X18(:,:,:,:)
       real*8,allocatable::Z24(:,:,:,:)
       real*8,allocatable::X19(:,:,:,:)
       real*8,allocatable::Z25(:,:,:,:)
       real*8,allocatable::X20(:,:,:,:)
       real*8,allocatable::Z26(:,:,:,:)
       real*8,allocatable::X21(:,:,:,:)
       real*8,allocatable::Z27(:,:,:,:)
       real*8,allocatable::X22(:,:,:,:)
       real*8,allocatable::Z28(:,:,:,:)
       real*8,allocatable::X23(:,:,:,:)
       real*8,allocatable::Z29(:,:,:,:)
       real*8,allocatable::X24(:,:,:,:)
       real*8,allocatable::Z30(:,:,:,:)
       real*8,allocatable::X25(:,:)
       real*8,allocatable::Z31(:,:,:,:)
       real*8,allocatable::X26(:,:)
       real*8,allocatable::Z32(:,:,:,:)
       real*8,allocatable::X27(:,:)
       real*8,allocatable::Z33(:,:,:,:)
       real*8,allocatable::X28(:,:)
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
       real*8,allocatable::X29(:,:,:,:)
       real*8,allocatable::Z51(:,:,:,:)
       real*8,allocatable::X30(:,:,:,:)
       real*8,allocatable::Z52(:,:,:,:)
       real*8,allocatable::X31(:,:,:,:)
       real*8,allocatable::Z53(:,:,:,:)
       real*8,allocatable::X32(:,:,:,:)
       real*8,allocatable::Z54(:,:,:,:)
       real*8,allocatable::X33(:,:,:,:)
       real*8,allocatable::Z55(:,:,:,:)
       real*8,allocatable::X34(:,:,:,:)
       real*8,allocatable::Z56(:,:,:,:)
       real*8,allocatable::X35(:,:,:,:)
       real*8,allocatable::Z57(:,:,:,:)
       real*8,allocatable::X36(:,:,:,:)
       real*8,allocatable::Z58(:,:,:,:)
       real*8,allocatable::X37(:,:,:,:)
       real*8,allocatable::Z59(:,:,:,:)
       real*8,allocatable::X38(:,:,:,:)
       real*8,allocatable::Z60(:,:,:,:)
       real*8,allocatable::X39(:,:,:,:)
       real*8,allocatable::Z74(:,:,:,:)
       real*8,allocatable::Z90(:,:,:,:)
       real*8,allocatable::Z92(:,:,:,:)
       real*8,allocatable::Z94(:,:,:,:)
       real*8,allocatable::Z96(:,:,:,:)
       real*8,allocatable::Z98(:,:,:,:)
       real*8,allocatable::Z100(:,:,:,:)
       real*8,allocatable::Z102(:,:,:,:)
       real*8,allocatable::Z104(:,:,:,:)
       real*8,allocatable::Z121(:,:,:,:)
       real*8,allocatable::Z122(:,:,:,:)
       real*8,allocatable::Z123(:,:,:,:)
       real*8,allocatable::Z124(:,:,:,:)
C
       allocate(V2B(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       V2B=0.0d0
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,M1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S61(M1+1:N1,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       X5=0.0d0
       call
     & sum4123(N0,M1,N0,M1,M2,N3,M1,N1,X5,S61,-1.000)
       deallocate(S61)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,N0,M1,M2,N3,M1,N1,X5,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder562134(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & N0,M1,N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,t3B2,F2)
       allocate(Z5(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,X5,F2,Z5)
       deallocate(F2)
C
       V2B=V2B-0.500*Z5
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S63(M1+1:N1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       X6=0.0d0
       call
     & sum4123(N0,M1,M1,N1,M2,N3,M1,N1,X6,S63,-1.000)
       deallocate(S63)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,M1,N1,X6,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder562134(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,t3B3,F2)
       allocate(Z6(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,X6,F2,Z6)
       deallocate(F2)
C
       V2B=V2B-Z6
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S65(M1+1:N1,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       X7=0.0d0
       call
     & sum4123(M1,N1,M1,N1,M2,N3,M1,N1,X7,S65,-1.000)
       deallocate(S65)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,M1,N1,X7,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder562134(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,t3B3,F2)
       allocate(Z7(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,X7,F2,Z7)
       deallocate(F2)
C
       V2B=V2B-0.500*Z7
       deallocate(Z7)
       deallocate(X7)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,M1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S67(M1+1:N1,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       X8=0.0d0
       call
     & sum4123(N0,M1,N0,M1,N1,M2,M1,N1,X8,S67,-1.000)
       deallocate(S67)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,N0,M1,N1,M2,M1,N1,X8,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder563124(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N0,M1,N1,M2,N2,M2,M2,N3,M1,N2,t3B4,F2)
       allocate(Z8(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,X8,F2,Z8)
       deallocate(F2)
C
       V2B=V2B+0.500*Z8
       deallocate(Z8)
       deallocate(X8)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S69(M1+1:N1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       X9=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,M1,N1,X9,S69,-1.000)
       deallocate(S69)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,M1,N1,X9,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder563124(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,M2,N3,M1,N2,t3B1,F2)
       allocate(Z9(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,X9,F2,Z9)
       deallocate(F2)
C
       V2B=V2B+Z9
       deallocate(Z9)
       deallocate(X9)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S71(M1+1:N1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       X10=0.0d0
       call
     & sum4123(M1,N1,M1,N1,N1,M2,M1,N1,X10,S71,-1.000)
       deallocate(S71)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,M1,N1,X10,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder563124(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,N2,M2,M2,N3,M1,N2,t3B1,F2)
       allocate(Z10(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,X10,F2,Z10)
       deallocate(F2)
C
       V2B=V2B+0.500*Z10
       deallocate(Z10)
       deallocate(X10)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N1,t3B3,F2)
       allocate(S73(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S73)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X39(N0+1:N1,N2+1:M2,M1+1:N2,M1+1:N1))
       X39=0.0d0
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X39,S73, 1.000)
       deallocate(S73)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N1,t3B3,F2)
       allocate(S75(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S75)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X39,S75, 1.000)
       deallocate(S75)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(S77(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S77)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X39,S77, 2.000)
       deallocate(S77)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(S79(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S79)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X39,S79, 2.000)
       deallocate(S79)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N2,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(S81(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S81)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X39,S81, 1.000)
       deallocate(S81)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N2,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(S83(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S83)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X39,S83, 1.000)
       deallocate(S83)
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
       call EGEMM1(I1,I3,D1,B2,Q1)
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
       call EGEMM1(I1,I3,D1,B2,Q2)
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
       call EGEMM1(I1,I3,D1,B2,Q3)
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
       call EGEMM1(I1,I3,D1,B2,Q4)
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
       allocate(S89(M1+1:N1,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S89)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,N0,M1,M2,N3,
     & N0,M1,N0,M1,M2,N3,M1,N1,S89,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z90(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z90)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z90
       deallocate(Z90)
       deallocate(S89)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S91(M1+1:N1,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,N0,M1,M2,N3,
     & M1,N2,N0,M1,M2,N3,M1,N1,S91,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z92(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z92)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z92
       deallocate(Z92)
       deallocate(S91)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S93(M1+1:N1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S93)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,M1,N1,M2,N3,
     & N0,M1,M1,N1,M2,N3,M1,N1,S93,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z94(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z94)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z94
       deallocate(Z94)
       deallocate(S93)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S95(M1+1:N1,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,M1,N1,M2,N3,
     & M1,N2,M1,N1,M2,N3,M1,N1,S95,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z96(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z96)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z96
       deallocate(Z96)
       deallocate(S95)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S97(M1+1:N1,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,N0,M1,N2,M2,
     & N0,M1,N0,M1,N2,M2,M1,N1,S97,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z98(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z98)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z98
       deallocate(Z98)
       deallocate(S97)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S99(M1+1:N1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,N0,M1,N2,M2,
     & M1,N2,N0,M1,N2,M2,M1,N1,S99,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z100(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z100)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z100
       deallocate(Z100)
       deallocate(S99)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S101(M1+1:N1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S101)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,M1,N1,N2,M2,
     & N0,M1,M1,N1,N2,M2,M1,N1,S101,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z102(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z102)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z102
       deallocate(Z102)
       deallocate(S101)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S103(M1+1:N1,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0*K7*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S103)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,M1,N1,N2,M2,
     & M1,N2,M1,N1,N2,M2,M1,N1,S103,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z104(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z104)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z104
       deallocate(Z104)
       deallocate(S103)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S105(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S105)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X39,S105, 2.000)
       deallocate(S105)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S107(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S107)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X39,S107, 2.000)
       deallocate(S107)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S109(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S109)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X39,S109, 2.000)
       deallocate(S109)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S111(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S111)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X39,S111, 2.000)
       deallocate(S111)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S113(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S113)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X39,S113, 2.000)
       deallocate(S113)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S115(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S115)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X39,S115, 2.000)
       deallocate(S115)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S117(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S117)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X39,S117, 2.000)
       deallocate(S117)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S119(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S119)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X39,S119, 2.000)
       deallocate(S119)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Z74(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K0
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,X39,B2,Z74)
       deallocate(B2)
C
       call
     & sum2134(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z74,-0.500)
       deallocate(Z74)
       deallocate(X39)
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
       call EGEMM1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N1,t3C1,F2)
       allocate(Z121(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I2=K7*K8*K6*K0
       I3=K6*K5
       call EGEMM2(I2,I3,Q5,F2,Z121)
       deallocate(F2)
C
       V2B=V2B+Z121
       deallocate(Z121)
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
       call EGEMM1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,M2,N3,M1,N2,M1,N1,t3C1,F2)
       allocate(Z122(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I2=K7*K8*K6*K0
       I3=K6*K8
       call EGEMM2(I2,I3,Q6,F2,Z122)
       deallocate(F2)
C
       V2B=V2B+Z122
       deallocate(Z122)
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
       call EGEMM1(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,M1,N1,t3C1,F2)
       allocate(Z123(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I2=K7*K8*K6*K0
       I3=K0*K5
       call EGEMM2(I2,I3,Q7,F2,Z123)
       deallocate(F2)
C
       V2B=V2B+Z123
       deallocate(Z123)
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
       call EGEMM1(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,M2,N3,M1,N2,M1,N1,t3C1,F2)
       allocate(Z124(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I2=K7*K8*K6*K0
       I3=K0*K8
       call EGEMM2(I2,I3,Q8,F2,Z124)
       deallocate(F2)
C
       V2B=V2B+Z124
       deallocate(Z124)
       deallocate(Q8)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S125(M1+1:N2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S125)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N2))
       X11=0.0d0
       call
     & sum4123(N0,M1,N0,M1,M2,N3,M1,N2,X11,S125, 1.000)
       deallocate(S125)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,M1,N2,X11,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder452136(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,M2,N3,M1,N1,t3B3,F2)
       allocate(Z17(N2+1:M2,M2+1:N3,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K6*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,X11,F2,Z17)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z17,-1.000)
       deallocate(Z17)
       deallocate(X11)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S127(M1+1:N2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S127)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       X12=0.0d0
       call
     & sum4123(M1,N2,N0,M1,M2,N3,M1,N2,X12,S127, 1.000)
       deallocate(S127)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,M1,N2,X12,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder452136(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,M2,N3,M1,N1,t3B3,F2)
       allocate(Z18(N2+1:M2,M2+1:N3,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K6*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,X12,F2,Z18)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z18,-1.000)
       deallocate(Z18)
       deallocate(X12)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S129(M1+1:N2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S129)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N2))
       X13=0.0d0
       call
     & sum4123(N0,M1,M1,N1,M2,N3,M1,N2,X13,S129, 1.000)
       deallocate(S129)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,M1,N2,X13,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder452136(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,M2,N3,M1,N1,t3B3,F2)
       allocate(Z19(N2+1:M2,M2+1:N3,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K6*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,X13,F2,Z19)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z19,-1.000)
       deallocate(Z19)
       deallocate(X13)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S131(M1+1:N2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S131)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       X14=0.0d0
       call
     & sum4123(M1,N2,M1,N1,M2,N3,M1,N2,X14,S131, 1.000)
       deallocate(S131)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,M1,N2,X14,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder452136(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,M2,N3,M1,N1,t3B3,F2)
       allocate(Z20(N2+1:M2,M2+1:N3,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K6*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,X14,F2,Z20)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z20,-1.000)
       deallocate(Z20)
       deallocate(X14)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S133(M1+1:N2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S133)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N2))
       X15=0.0d0
       call
     & sum4123(N0,M1,N0,M1,N1,M2,M1,N2,X15,S133, 1.000)
       deallocate(S133)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,M1,N2,X15,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder453126(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,N1,M2,N2,M2,M2,N3,M1,N1,t3B1,F2)
       allocate(Z21(N2+1:M2,M2+1:N3,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K6*K0
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,X15,F2,Z21)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z21, 1.000)
       deallocate(Z21)
       deallocate(X15)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S135(M1+1:N2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S135)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(M1+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       X16=0.0d0
       call
     & sum4123(M1,N2,N0,M1,N1,M2,M1,N2,X16,S135, 1.000)
       deallocate(S135)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,M1,N2,X16,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder453126(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,N1,M2,N2,M2,M2,N3,M1,N1,t3B1,F2)
       allocate(Z22(N2+1:M2,M2+1:N3,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K6*K0
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,X16,F2,Z22)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z22, 1.000)
       deallocate(Z22)
       deallocate(X16)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S137(M1+1:N2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S137)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N2))
       X17=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,M1,N2,X17,S137, 1.000)
       deallocate(S137)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,M1,N2,X17,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder453126(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,M2,N3,M1,N1,t3B1,F2)
       allocate(Z23(N2+1:M2,M2+1:N3,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K6*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,X17,F2,Z23)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z23, 1.000)
       deallocate(Z23)
       deallocate(X17)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S139(M1+1:N2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S139)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X18(M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       X18=0.0d0
       call
     & sum4123(M1,N2,M1,N1,N1,M2,M1,N2,X18,S139, 1.000)
       deallocate(S139)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,M1,N2,X18,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder453126(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N1,M2,N2,M2,M2,N3,M1,N1,t3B1,F2)
       allocate(Z24(N2+1:M2,M2+1:N3,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K6*K0
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,X18,F2,Z24)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z24, 1.000)
       deallocate(Z24)
       deallocate(X18)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N2,M2,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S141(N2+1:M2,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S141)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X19(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2))
       X19=0.0d0
       call
     & sum4123(N0,M1,N2,M2,M2,N3,N2,M2,X19,S141,-1.000)
       deallocate(S141)
C
       call sumx2341(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,X19,VBHPPP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder512346(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N1,t3B3,F2)
       allocate(Z25(M2+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K8*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,X19,F2,Z25)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z25, 1.000)
       deallocate(Z25)
       deallocate(X19)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S143(N2+1:M2,M1+1:N1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S143)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X20(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2))
       X20=0.0d0
       call
     & sum4123(M1,N1,N2,M2,M2,N3,N2,M2,X20,S143,-1.000)
       deallocate(S143)
C
       call sumx2341(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,N2,M2,M2,N3,N2,M2,X20,VBHPPP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder512346(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N1,t3B3,F2)
       allocate(Z26(M2+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K8*K6
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,X20,F2,Z26)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z26, 1.000)
       deallocate(Z26)
       deallocate(X20)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S145(N2+1:M2,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S145)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X21(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2))
       X21=0.0d0
       call
     & sum4123(N0,M1,M2,N3,N1,M2,N2,M2,X21,S145,-1.000)
       deallocate(S145)
C
       call sumx2341(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,X21,VBHPPP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder513246(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N1,t3B1,F2)
       allocate(Z27(M2+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K8*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,X21,F2,Z27)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z27,-1.000)
       deallocate(Z27)
       deallocate(X21)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S147(N2+1:M2,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S147)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X22(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2))
       X22=0.0d0
       call
     & sum4123(M1,N1,M2,N3,N1,M2,N2,M2,X22,S147,-1.000)
       deallocate(S147)
C
       call sumx2341(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,X22,VBHPPP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder513246(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N1,t3B1,F2)
       allocate(Z28(M2+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K8*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,X22,F2,Z28)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z28,-1.000)
       deallocate(Z28)
       deallocate(X22)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S149(N2+1:M2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S149)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X23(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2))
       X23=0.0d0
       call
     & sum4123(N0,M1,N2,M2,N1,M2,N2,M2,X23,S149,-1.000)
       deallocate(S149)
C
       call sumx2341(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,X23,VBHPPP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder513246(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,M2,N3,M1,N2,M1,N1,t3B1,F2)
       allocate(Z29(M2+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K8*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,X23,F2,Z29)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z29,-1.000)
       deallocate(Z29)
       deallocate(X23)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S151(N2+1:M2,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S151)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X24(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2))
       X24=0.0d0
       call
     & sum4123(M1,N1,N2,M2,N1,M2,N2,M2,X24,S151,-1.000)
       deallocate(S151)
C
       call sumx2341(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,N2,M2,N1,M2,N2,M2,X24,VBHPPP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder513246(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N2,M2,N1,M2,M2,N3,M1,N2,M1,N1,t3B1,F2)
       allocate(Z30(M2+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K8*K6
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,X24,F2,Z30)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z30,-1.000)
       deallocate(Z30)
       deallocate(X24)
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
       call EGEMM1(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1+Q9
       deallocate(Q9)
C
       call sumx21(N1,N3,N0,N1,
     & N0,M1,M2,N3,X1,FAHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder521346(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N1,t3B3,F2)
       allocate(Z1(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I2=K7*K8*K6*K0
       I3=K6*K5
       call EGEMM2(I2,I3,X1,F2,Z1)
       deallocate(F2)
C
       V2B=V2B+Z1
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
       call EGEMM1(I1,I3,D1,B2,Q10)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2+Q10
       deallocate(Q10)
C
       call sumx21(N1,N3,N0,N1,
     & M1,N1,M2,N3,X2,FAHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder521346(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N1,t3B3,F2)
       allocate(Z2(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I2=K7*K8*K6*K0
       I3=K6*K7
       call EGEMM2(I2,I3,X2,F2,Z2)
       deallocate(F2)
C
       V2B=V2B+Z2
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
       call EGEMM1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3+Q11
       deallocate(Q11)
C
       call sumx21(N1,N3,N0,N1,
     & N0,M1,N1,M2,X3,FAHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder531246(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N1,M2,N2,M2,M2,N3,M1,N2,M1,N1,t3B1,F2)
       allocate(Z3(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I2=K7*K8*K6*K0
       I3=K9*K5
       call EGEMM2(I2,I3,X3,F2,Z3)
       deallocate(F2)
C
       V2B=V2B-Z3
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
       call EGEMM1(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       X4=X4+Q12
       deallocate(Q12)
C
       call sumx21(N1,N3,N0,N1,
     & M1,N1,N1,M2,X4,FAHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder531246(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N2,M2,M2,N3,M1,N2,M1,N1,t3B1,F2)
       allocate(Z4(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I2=K7*K8*K6*K0
       I3=K9*K7
       call EGEMM2(I2,I3,X4,F2,Z4)
       deallocate(F2)
C
       V2B=V2B-Z4
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,M1,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S157(M1+1:N2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S157)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X29(N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N2))
       X29=0.0d0
       call
     & sum4123(N0,M1,N0,M1,M2,N3,M1,N2,X29,S157,-1.000)
       deallocate(S157)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,N0,M1,M2,N3,M1,N2,X29,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,M2,N3,M1,N1,t3C4,F2)
       allocate(Z51(N2+1:M2,M2+1:N3,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K6*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,X29,F2,Z51)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z51,-0.500)
       deallocate(Z51)
       deallocate(X29)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S159(M1+1:N2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S159)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X30(N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       X30=0.0d0
       call
     & sum4123(N0,M1,M1,N2,M2,N3,M1,N2,X30,S159,-1.000)
       deallocate(S159)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,M1,N2,X30,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,M2,N3,N2,M2,M2,N3,M1,N1,t3C1,F2)
       allocate(Z52(N2+1:M2,M2+1:N3,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K6*K0
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,X30,F2,Z52)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z52,-1.000)
       deallocate(Z52)
       deallocate(X30)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S161(M1+1:N2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S161)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X31(M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       X31=0.0d0
       call
     & sum4123(M1,N2,M1,N2,M2,N3,M1,N2,X31,S161,-1.000)
       deallocate(S161)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,M1,N2,X31,VCHHHP, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,M2,N3,N2,M2,M2,N3,M1,N1,t3C1,F2)
       allocate(Z53(N2+1:M2,M2+1:N3,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K6*K0
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,X31,F2,Z53)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z53,-0.500)
       deallocate(Z53)
       deallocate(X31)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,M1,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S163(M1+1:N2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S163)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X32(N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       X32=0.0d0
       call
     & sum4123(N0,M1,N0,M1,N2,M2,M1,N2,X32,S163,-1.000)
       deallocate(S163)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,N0,M1,N2,M2,M1,N2,X32,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,N0,M1,N2,M2,N2,M2,M2,N3,M1,N1,t3C4,F2)
       allocate(Z54(N2+1:M2,M2+1:N3,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K6*K0
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,X32,F2,Z54)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z54,-0.500)
       deallocate(Z54)
       deallocate(X32)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S165(M1+1:N2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S165)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X33(N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       X33=0.0d0
       call
     & sum4123(N0,M1,M1,N2,N2,M2,M1,N2,X33,S165,-1.000)
       deallocate(S165)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,M1,N2,X33,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,N2,M2,N2,M2,M2,N3,M1,N1,t3C1,F2)
       allocate(Z55(N2+1:M2,M2+1:N3,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K6*K0
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,X33,F2,Z55)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z55,-1.000)
       deallocate(Z55)
       deallocate(X33)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S167(M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S167)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X34(M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       X34=0.0d0
       call
     & sum4123(M1,N2,M1,N2,N2,M2,M1,N2,X34,S167,-1.000)
       deallocate(S167)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,M1,N2,X34,VCHHHP, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,N2,M2,N2,M2,M2,N3,M1,N1,t3C1,F2)
       allocate(Z56(N2+1:M2,M2+1:N3,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K6*K0
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,X34,F2,Z56)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z56,-0.500)
       deallocate(Z56)
       deallocate(X34)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,M1,M2,N3,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S169(N2+1:M2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S169)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X35(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2))
       X35=0.0d0
       call
     & sum4123(N0,M1,M2,N3,N2,M2,N2,M2,X35,S169,-1.000)
       deallocate(S169)
C
       call sumx2341(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,X35,VCHPPP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N1,t3C1,F2)
       allocate(Z57(M2+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K8*K6
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,X35,F2,Z57)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z57,-1.000)
       deallocate(Z57)
       deallocate(X35)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S171(N2+1:M2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S171)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X36(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       X36=0.0d0
       call
     & sum4123(M1,N2,M2,N3,N2,M2,N2,M2,X36,S171,-1.000)
       deallocate(S171)
C
       call sumx2341(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,X36,VCHPPP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,M2,N3,M1,N2,M1,N1,t3C1,F2)
       allocate(Z58(M2+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K8*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,X36,F2,Z58)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z58,-1.000)
       deallocate(Z58)
       deallocate(X36)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S173(N2+1:M2,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S173)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X37(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2))
       X37=0.0d0
       call
     & sum4123(N0,M1,N2,M2,N2,M2,N2,M2,X37,S173,-1.000)
       deallocate(S173)
C
       call sumx2341(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,X37,VCHPPP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,M1,N1,t3C1,F2)
       allocate(Z59(M2+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K8*K6
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,X37,F2,Z59)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z59,-0.500)
       deallocate(Z59)
       deallocate(X37)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S175(N2+1:M2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S175)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X38(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       X38=0.0d0
       call
     & sum4123(M1,N2,N2,M2,N2,M2,N2,M2,X38,S175,-1.000)
       deallocate(S175)
C
       call sumx2341(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,X38,VCHPPP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,M2,N3,M1,N2,M1,N1,t3C1,F2)
       allocate(Z60(M2+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K8*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,X38,F2,Z60)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z60,-0.500)
       deallocate(Z60)
       deallocate(X38)
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
       call EGEMM1(I1,I3,D1,B2,Q13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X25(N0+1:M1,M2+1:N3))
       X25=0.0d0
       X25=X25+Q13
       deallocate(Q13)
C
       call sumx21(N2,N3,N0,N2,
     & N0,M1,M2,N3,X25,FBHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N1,t3C1,F2)
       allocate(Z31(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I2=K7*K8*K6*K0
       I3=K6*K5
       call EGEMM2(I2,I3,X25,F2,Z31)
       deallocate(F2)
C
       V2B=V2B+Z31
       deallocate(Z31)
       deallocate(X25)
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
       call EGEMM1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X26(M1+1:N2,M2+1:N3))
       X26=0.0d0
       X26=X26+Q14
       deallocate(Q14)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,M2,N3,X26,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,M2,N3,M1,N2,M1,N1,t3C1,F2)
       allocate(Z32(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I2=K7*K8*K6*K0
       I3=K6*K8
       call EGEMM2(I2,I3,X26,F2,Z32)
       deallocate(F2)
C
       V2B=V2B+Z32
       deallocate(Z32)
       deallocate(X26)
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
       call EGEMM1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X27(N0+1:M1,N2+1:M2))
       X27=0.0d0
       X27=X27+Q15
       deallocate(Q15)
C
       call sumx21(N2,N3,N0,N2,
     & N0,M1,N2,M2,X27,FBHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,M1,N1,t3C1,F2)
       allocate(Z33(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I2=K7*K8*K6*K0
       I3=K0*K5
       call EGEMM2(I2,I3,X27,F2,Z33)
       deallocate(F2)
C
       V2B=V2B+Z33
       deallocate(Z33)
       deallocate(X27)
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
       call EGEMM1(I1,I3,D1,B2,Q16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X28(M1+1:N2,N2+1:M2))
       X28=0.0d0
       X28=X28+Q16
       deallocate(Q16)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,N2,M2,X28,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,M2,N3,M1,N2,M1,N1,t3C1,F2)
       allocate(Z34(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I2=K7*K8*K6*K0
       I3=K0*K8
       call EGEMM2(I2,I3,X28,F2,Z34)
       deallocate(F2)
C
       V2B=V2B+Z34
       deallocate(Z34)
       deallocate(X28)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,M2,N3,VAHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N1,t3B3,F2)
       allocate(Z11(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z11)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z11,-0.500)
       deallocate(Z11)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M2,N3,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N1,t3B3,F2)
       allocate(Z12(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z12)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z12,-0.500)
       deallocate(Z12)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,VAHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(Z13(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z13)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z13,-1.000)
       deallocate(Z13)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(Z14(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z14)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z14,-1.000)
       deallocate(Z14)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,M2,N3,VAHPPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N2,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(Z15(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K0
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z15)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z15,-0.500)
       deallocate(Z15)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,M2,N3,VAHPPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N2,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(Z16(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K0
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z16)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z16,-0.500)
       deallocate(Z16)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z35(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z35)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z35
       deallocate(Z35)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z36(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z36)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z36
       deallocate(Z36)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z37(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z37)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z37
       deallocate(Z37)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z38(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z38)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z38
       deallocate(Z38)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z39(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z39)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z39
       deallocate(Z39)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z40(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z40)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z40
       deallocate(Z40)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z41(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z41)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z41
       deallocate(Z41)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z42(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K6*K0
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z42)
       deallocate(D1)
       deallocate(F2)
C
       V2B=V2B-Z42
       deallocate(Z42)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,M2,N3,M2,N3,VBPHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z43(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z43)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z43, 1.000)
       deallocate(Z43)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,M2,N3,M2,N3,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z44(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z44)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z44, 1.000)
       deallocate(Z44)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,N2,M2,M2,N3,M2,N3,VBPHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z45(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K0
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z45)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z45, 1.000)
       deallocate(Z45)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,M2,N3,M2,N3,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z46(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K0
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z46)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z46, 1.000)
       deallocate(Z46)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,N1,M2,M2,N3,VBPHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z47(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z47)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z47, 1.000)
       deallocate(Z47)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,N1,M2,M2,N3,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z48(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K0
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z48)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z48, 1.000)
       deallocate(Z48)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,N2,M2,N1,M2,M2,N3,VBPHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z49(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K0
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z49)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z49, 1.000)
       deallocate(Z49)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,N1,M2,M2,N3,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z50(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K0
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z50)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,M2,N3,M1,N2,M1,N1,V2B,Z50, 1.000)
       deallocate(Z50)
C
       call sumx2(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,M2,M2,N3,M1,N2,M1,N1,HT2B,V2B,1.0)
       deallocate(V2B)
C
       end
