       subroutine t2A1011_update(N0,N1,N2,N3,HT2A,shift,
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
       real*8,allocatable::S87(:,:,:,:)
       real*8,allocatable::S89(:,:,:,:)
       real*8,allocatable::S91(:,:,:,:)
       real*8,allocatable::S93(:,:,:,:)
       real*8,allocatable::S95(:,:,:,:)
       real*8,allocatable::S97(:,:,:,:)
       real*8,allocatable::S99(:,:,:,:)
       real*8,allocatable::S101(:,:,:,:)
       real*8,allocatable::S103(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
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
       real*8,allocatable::S139(:,:,:,:)
       real*8,allocatable::S141(:,:,:,:)
       real*8,allocatable::S143(:,:,:,:)
       real*8,allocatable::S145(:,:,:,:)
       real*8,allocatable::S147(:,:,:,:)
       real*8,allocatable::S149(:,:,:,:)
       real*8,allocatable::S151(:,:,:,:)
       real*8,allocatable::S153(:,:,:,:)
       real*8,allocatable::S155(:,:,:,:)
       real*8,allocatable::S157(:,:,:,:)
       real*8,allocatable::S159(:,:,:,:)
       real*8,allocatable::S161(:,:,:,:)
       real*8,allocatable::S163(:,:,:,:)
       real*8,allocatable::S165(:,:,:,:)
       real*8,allocatable::S167(:,:,:,:)
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
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::Z11(:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::Z12(:,:,:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::Z13(:,:,:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::Z14(:,:,:,:)
       real*8,allocatable::Z15(:,:,:,:)
       real*8,allocatable::Z16(:,:,:,:)
       real*8,allocatable::Z17(:,:,:,:)
       real*8,allocatable::Z18(:,:,:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::Z19(:,:,:,:)
       real*8,allocatable::X16(:,:,:,:)
       real*8,allocatable::Z20(:,:,:,:)
       real*8,allocatable::Z21(:,:,:,:)
       real*8,allocatable::Z22(:,:,:,:)
       real*8,allocatable::Z23(:,:,:,:)
       real*8,allocatable::Z24(:,:,:,:)
       real*8,allocatable::Z25(:,:,:,:)
       real*8,allocatable::Z26(:,:,:,:)
       real*8,allocatable::X17(:,:)
       real*8,allocatable::Z27(:,:,:,:)
       real*8,allocatable::X18(:,:)
       real*8,allocatable::Z28(:,:,:,:)
       real*8,allocatable::X19(:,:)
       real*8,allocatable::Z29(:,:,:,:)
       real*8,allocatable::X20(:,:)
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
       real*8,allocatable::Z51(:,:,:,:)
       real*8,allocatable::Z52(:,:,:,:)
       real*8,allocatable::Z53(:,:,:,:)
       real*8,allocatable::Z54(:,:,:,:)
       real*8,allocatable::Z55(:,:,:,:)
       real*8,allocatable::Z56(:,:,:,:)
       real*8,allocatable::Z57(:,:,:,:)
       real*8,allocatable::Z58(:,:,:,:)
       real*8,allocatable::Z59(:,:,:,:)
       real*8,allocatable::Z60(:,:,:,:)
       real*8,allocatable::X21(:,:,:,:)
       real*8,allocatable::Z82(:,:,:,:)
       real*8,allocatable::X22(:,:,:,:)
       real*8,allocatable::Z94(:,:,:,:)
       real*8,allocatable::Z102(:,:,:,:)
       real*8,allocatable::Z104(:,:,:,:)
       real*8,allocatable::Z110(:,:,:,:)
       real*8,allocatable::Z112(:,:,:,:)
       real*8,allocatable::Z114(:,:,:,:)
       real*8,allocatable::Z116(:,:,:,:)
       real*8,allocatable::Z118(:,:,:,:)
       real*8,allocatable::Z120(:,:,:,:)
       real*8,allocatable::Z122(:,:,:,:)
       real*8,allocatable::Z124(:,:,:,:)
       real*8,allocatable::Z126(:,:,:,:)
       real*8,allocatable::Z128(:,:,:,:)
       real*8,allocatable::Z130(:,:,:,:)
       real*8,allocatable::Z132(:,:,:,:)
       real*8,allocatable::Z134(:,:,:,:)
       real*8,allocatable::Z136(:,:,:,:)
       real*8,allocatable::Z150(:,:,:,:)
       real*8,allocatable::Z152(:,:,:,:)
       real*8,allocatable::Z166(:,:,:,:)
       real*8,allocatable::Z168(:,:,:,:)
       real*8,allocatable::Z169(:,:,:,:)
       real*8,allocatable::Z170(:,:,:,:)
       real*8,allocatable::Z171(:,:,:,:)
       real*8,allocatable::Z172(:,:,:,:)
C
       allocate(V2A(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       V2A=0.0d0
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S61(M1+1:N1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       X5=0.0d0
       call
     & sum4123(N0,M1,M1,N1,M2,N3,M1,N1,X5,S61,-1.000)
       deallocate(S61)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,M1,N1,X5,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3A,F2)
       allocate(Z5(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K9*K9
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,X5,F2,Z5)
       deallocate(F2)
C
       V2A=V2A-Z5
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S63(M1+1:N1,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       X6=0.0d0
       call
     & sum4123(M1,N1,M1,N1,M2,N3,M1,N1,X6,S63,-1.000)
       deallocate(S63)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,M1,N1,X6,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3A,F2)
       allocate(Z6(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K9*K9
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,X6,F2,Z6)
       deallocate(F2)
C
       V2A=V2A+0.500*Z6
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S65(M1+1:N1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       X7=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,M1,N1,X7,S65,-1.000)
       deallocate(S65)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,M1,N1,X7,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,t3A,F2)
       allocate(Z7(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K9*K9
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,X7,F2,Z7)
       deallocate(F2)
C
       V2A=V2A-Z7
       deallocate(Z7)
       deallocate(X7)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S67(M1+1:N1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       X8=0.0d0
       call
     & sum4123(M1,N1,M1,N1,N1,M2,M1,N1,X8,S67,-1.000)
       deallocate(S67)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,M1,N1,X8,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,t3A,F2)
       allocate(Z8(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K9*K9
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,X8,F2,Z8)
       deallocate(F2)
C
       V2A=V2A+0.500*Z8
       deallocate(Z8)
       deallocate(X8)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,M1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S69(N0+1:M1,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N0+1:M1,N0+1:M1,M2+1:N3,N0+1:M1))
       X9=0.0d0
       call
     & sum4123(N0,M1,N0,M1,M2,N3,N0,M1,X9,S69,-1.000)
       deallocate(S69)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,N0,M1,M2,N3,N0,M1,X9,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(Z9(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,X9,F2,Z9)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z9,-0.500)
       deallocate(Z9)
       deallocate(X9)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S71(N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       X10=0.0d0
       call
     & sum4123(N0,M1,M1,N1,M2,N3,N0,M1,X10,S71,-1.000)
       deallocate(S71)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N0,M1,X10,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(Z10(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,X10,F2,Z10)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z10,-1.000)
       deallocate(Z10)
       deallocate(X10)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S73(N0+1:M1,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       X11=0.0d0
       call
     & sum4123(M1,N1,M1,N1,M2,N3,N0,M1,X11,S73,-1.000)
       deallocate(S73)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N0,M1,X11,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(Z11(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,X11,F2,Z11)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z11,-0.500)
       deallocate(Z11)
       deallocate(X11)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,M1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S75(N0+1:M1,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(N0+1:M1,N0+1:M1,N1+1:M2,N0+1:M1))
       X12=0.0d0
       call
     & sum4123(N0,M1,N0,M1,N1,M2,N0,M1,X12,S75,-1.000)
       deallocate(S75)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,N0,M1,N1,M2,N0,M1,X12,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,N1,M2,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(Z12(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,X12,F2,Z12)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z12,-0.500)
       deallocate(Z12)
       deallocate(X12)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S77(N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       X13=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,N0,M1,X13,S77,-1.000)
       deallocate(S77)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N0,M1,X13,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(Z13(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,X13,F2,Z13)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z13,-1.000)
       deallocate(Z13)
       deallocate(X13)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S79(N0+1:M1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       X14=0.0d0
       call
     & sum4123(M1,N1,M1,N1,N1,M2,N0,M1,X14,S79,-1.000)
       deallocate(S79)
C
       call sumx3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N0,M1,X14,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(Z14(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,X14,F2,Z14)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z14,-0.500)
       deallocate(Z14)
       deallocate(X14)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S81(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S81)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X21(N0+1:N1,N1+1:M2,N0+1:M1,M1+1:N1))
       X21=0.0d0
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X21,S81, 1.000)
       deallocate(S81)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S83(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S83)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X21,S83,-1.000)
       deallocate(S83)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S85(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S85)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X21,S85, 2.000)
       deallocate(S85)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S87(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S87)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X21,S87,-2.000)
       deallocate(S87)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S89(N1+1:M2,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S89)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2))
       X15=0.0d0
       call
     & sum4123(N0,M1,N1,M2,N1,M2,N1,M2,X15,S89,-1.000)
       deallocate(S89)
C
       call sumx2341(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,X15,VAHPPP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z19(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,X15,F2,Z19)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z19,-0.500)
       deallocate(Z19)
       deallocate(X15)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S91(N1+1:M2,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       X16=0.0d0
       call
     & sum4123(M1,N1,N1,M2,N1,M2,N1,M2,X16,S91,-1.000)
       deallocate(S91)
C
       call sumx2341(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,X16,VAHPPP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z20(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,X16,F2,Z20)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z20, 0.500)
       deallocate(Z20)
       deallocate(X16)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S93(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S93)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X22(N0+1:N1,N1+1:M2,N0+1:M1,M1+1:N1))
       X22=0.0d0
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X22,S93, 1.000)
       deallocate(S93)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S95(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S95)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X22,S95,-1.000)
       deallocate(S95)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S97(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S97)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X22,S97, 2.000)
       deallocate(S97)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S99(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S99)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X22,S99,-2.000)
       deallocate(S99)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S101(N1+1:M2,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S101)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,M1,N1,M2,N1,M2,
     & N0,M1,N1,M2,N1,M2,N1,M2,S101,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z102(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z102)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z102, 0.500)
       deallocate(Z102)
       deallocate(S101)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S103(N1+1:M2,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S103)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,M1,N1,N1,M2,N1,M2,
     & M1,N1,N1,M2,N1,M2,N1,M2,S103,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z104(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z104)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z104,-0.500)
       deallocate(Z104)
       deallocate(S103)
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
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S109(M1+1:N1,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S109)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,N0,M1,M2,N3,
     & M1,N2,N0,M1,M2,N3,M1,N1,S109,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,t3B4,F2)
       allocate(Z110(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K9*K9
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z110)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z110
       deallocate(Z110)
       deallocate(S109)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S111(M1+1:N1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S111)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,M1,N1,M2,N3,
     & N0,M1,M1,N1,M2,N3,M1,N1,S111,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(Z112(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K9*K9
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z112)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z112
       deallocate(Z112)
       deallocate(S111)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S113(M1+1:N1,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S113)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,M1,N1,M2,N3,
     & M1,N2,M1,N1,M2,N3,M1,N1,S113,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(Z114(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K9*K9
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z114)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z114
       deallocate(Z114)
       deallocate(S113)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S115(M1+1:N1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S115)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,N0,M1,N2,M2,
     & M1,N2,N0,M1,N2,M2,M1,N1,S115,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,t3B4,F2)
       allocate(Z116(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K9*K9
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z116)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z116
       deallocate(Z116)
       deallocate(S115)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S117(M1+1:N1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S117)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,M1,N1,N2,M2,
     & N0,M1,M1,N1,N2,M2,M1,N1,S117,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(Z118(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K9*K9
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z118)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z118
       deallocate(Z118)
       deallocate(S117)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S119(M1+1:N1,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0*K7*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S119)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,M1,N1,N2,M2,
     & M1,N2,M1,N1,N2,M2,M1,N1,S119,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(Z120(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K9*K9
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z120)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z120
       deallocate(Z120)
       deallocate(S119)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S121(N0+1:M1,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S121)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,N0,M1,M2,N3,
     & N0,M1,N0,M1,M2,N3,N0,M1,S121,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z122(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z122)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z122,-1.000)
       deallocate(Z122)
       deallocate(S121)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S123(N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S123)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,N0,M1,M2,N3,
     & M1,N2,N0,M1,M2,N3,N0,M1,S123,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z124(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z124)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z124,-1.000)
       deallocate(Z124)
       deallocate(S123)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S125(N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S125)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M1,N1,M2,N3,
     & N0,M1,M1,N1,M2,N3,N0,M1,S125,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z126(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z126)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z126,-1.000)
       deallocate(Z126)
       deallocate(S125)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S127(N0+1:M1,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S127)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,M1,N1,M2,N3,
     & M1,N2,M1,N1,M2,N3,N0,M1,S127,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z128(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z128)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z128,-1.000)
       deallocate(Z128)
       deallocate(S127)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S129(N0+1:M1,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S129)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,N0,M1,N2,M2,
     & N0,M1,N0,M1,N2,M2,N0,M1,S129,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z130(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z130)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z130,-1.000)
       deallocate(Z130)
       deallocate(S129)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S131(N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S131)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,N0,M1,N2,M2,
     & M1,N2,N0,M1,N2,M2,N0,M1,S131,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z132(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z132)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z132,-1.000)
       deallocate(Z132)
       deallocate(S131)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S133(N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S133)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M1,N1,N2,M2,
     & N0,M1,M1,N1,N2,M2,N0,M1,S133,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z134(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z134)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z134,-1.000)
       deallocate(Z134)
       deallocate(S133)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S135(N0+1:M1,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0*K7*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S135)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,M1,N1,N2,M2,
     & M1,N2,M1,N1,N2,M2,N0,M1,S135,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z136(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z136)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z136,-1.000)
       deallocate(Z136)
       deallocate(S135)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S137(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S137)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X21,S137,-2.000)
       deallocate(S137)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S139(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S139)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X21,S139,-2.000)
       deallocate(S139)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S141(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S141)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X21,S141,-2.000)
       deallocate(S141)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S143(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S143)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X21,S143,-2.000)
       deallocate(S143)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S145(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S145)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X21,S145,-2.000)
       deallocate(S145)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S147(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S147)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X21,S147,-2.000)
       deallocate(S147)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Z82(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,X21,B2,Z82)
       deallocate(B2)
C
       V2A=V2A+0.500*Z82
       deallocate(Z82)
       deallocate(X21)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,M1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S149(N1+1:M2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S149)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,M1,N2,M2,N1,M2,
     & N0,M1,N2,M2,N1,M2,N1,M2,S149,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z150(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z150)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z150,-1.000)
       deallocate(Z150)
       deallocate(S149)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S151(N1+1:M2,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S151)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,M1,N2,N2,M2,N1,M2,
     & M1,N2,N2,M2,N1,M2,N1,M2,S151,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z152(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z152)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z152,-1.000)
       deallocate(Z152)
       deallocate(S151)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S153(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S153)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X22,S153, 2.000)
       deallocate(S153)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S155(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S155)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X22,S155, 2.000)
       deallocate(S155)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S157(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S157)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X22,S157, 2.000)
       deallocate(S157)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S159(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S159)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X22,S159, 2.000)
       deallocate(S159)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S161(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S161)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X22,S161, 2.000)
       deallocate(S161)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S163(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S163)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X22,S163, 2.000)
       deallocate(S163)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Z94(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,X22,B2,Z94)
       deallocate(B2)
C
       call
     & sum2134(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z94, 0.500)
       deallocate(Z94)
       deallocate(X22)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,M1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S165(N1+1:M2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S165)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,M1,N2,M2,N1,M2,
     & N0,M1,N2,M2,N1,M2,N1,M2,S165,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z166(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z166)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z166, 1.000)
       deallocate(Z166)
       deallocate(S165)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S167(N1+1:M2,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S167)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,M1,N2,N2,M2,N1,M2,
     & M1,N2,N2,M2,N1,M2,N1,M2,S167,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z168(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z168)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z168, 1.000)
       deallocate(Z168)
       deallocate(S167)
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
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z169(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I2=K7*K5*K9*K9
       I3=K6*K5
       call EGEMM2(I2,I3,Q5,F2,Z169)
       deallocate(F2)
C
       V2A=V2A+Z169
       deallocate(Z169)
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
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z170(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I2=K7*K5*K9*K9
       I3=K6*K8
       call EGEMM2(I2,I3,Q6,F2,Z170)
       deallocate(F2)
C
       V2A=V2A+Z170
       deallocate(Z170)
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
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z171(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I2=K7*K5*K9*K9
       I3=K0*K5
       call EGEMM2(I2,I3,Q7,F2,Z171)
       deallocate(F2)
C
       V2A=V2A+Z171
       deallocate(Z171)
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
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z172(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I2=K7*K5*K9*K9
       I3=K0*K8
       call EGEMM2(I2,I3,Q8,F2,Z172)
       deallocate(F2)
C
       V2A=V2A+Z172
       deallocate(Z172)
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
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z1(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I2=K7*K5*K9*K9
       I3=K6*K5
       call EGEMM2(I2,I3,X1,F2,Z1)
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
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z2(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I2=K7*K5*K9*K9
       I3=K6*K7
       call EGEMM2(I2,I3,X2,F2,Z2)
       deallocate(F2)
C
       V2A=V2A-Z2
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
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z3(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I2=K7*K5*K9*K9
       I3=K9*K5
       call EGEMM2(I2,I3,X3,F2,Z3)
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
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z4(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I2=K7*K5*K9*K9
       I3=K9*K7
       call EGEMM2(I2,I3,X4,F2,Z4)
       deallocate(F2)
C
       V2A=V2A-Z4
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
       call EGEMM1(I1,I3,D1,B2,Q13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(N0+1:M1,M2+1:N3))
       X17=0.0d0
       X17=X17+Q13
       deallocate(Q13)
C
       call sumx21(N2,N3,N0,N2,
     & N0,M1,M2,N3,X17,FBHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z27(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I2=K7*K5*K9*K9
       I3=K6*K5
       call EGEMM2(I2,I3,X17,F2,Z27)
       deallocate(F2)
C
       V2A=V2A+Z27
       deallocate(Z27)
       deallocate(X17)
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
       allocate(X18(M1+1:N2,M2+1:N3))
       X18=0.0d0
       X18=X18+Q14
       deallocate(Q14)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,M2,N3,X18,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z28(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I2=K7*K5*K9*K9
       I3=K6*K8
       call EGEMM2(I2,I3,X18,F2,Z28)
       deallocate(F2)
C
       V2A=V2A+Z28
       deallocate(Z28)
       deallocate(X18)
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
       allocate(X19(N0+1:M1,N2+1:M2))
       X19=0.0d0
       X19=X19+Q15
       deallocate(Q15)
C
       call sumx21(N2,N3,N0,N2,
     & N0,M1,N2,M2,X19,FBHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z29(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I2=K7*K5*K9*K9
       I3=K0*K5
       call EGEMM2(I2,I3,X19,F2,Z29)
       deallocate(F2)
C
       V2A=V2A+Z29
       deallocate(Z29)
       deallocate(X19)
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
       allocate(X20(M1+1:N2,N2+1:M2))
       X20=0.0d0
       X20=X20+Q16
       deallocate(Q16)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,N2,M2,X20,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z30(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I2=K7*K5*K9*K9
       I3=K0*K8
       call EGEMM2(I2,I3,X20,F2,Z30)
       deallocate(F2)
C
       V2A=V2A+Z30
       deallocate(Z30)
       deallocate(X20)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,VAHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z15(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z15)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z15,-0.500)
       deallocate(Z15)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z16(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z16)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z16, 0.500)
       deallocate(Z16)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z17(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z17)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z17,-1.000)
       deallocate(Z17)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z18(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z18)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z18, 1.000)
       deallocate(Z18)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,VAHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z21(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z21)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z21, 0.500)
       deallocate(Z21)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z22(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z22)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z22,-0.500)
       deallocate(Z22)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z23(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z23)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z23, 1.000)
       deallocate(Z23)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z24(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z24)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z24,-1.000)
       deallocate(Z24)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,VAHPPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z25(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z25)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z25, 0.500)
       deallocate(Z25)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,VAHPPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z26(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z26)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z26,-0.500)
       deallocate(Z26)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,t3B4,F2)
       allocate(Z31(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K9*K9
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z31)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z31
       deallocate(Z31)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(Z32(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K9*K9
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z32)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z32
       deallocate(Z32)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(Z33(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K9*K9
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z33)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z33
       deallocate(Z33)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,t3B4,F2)
       allocate(Z34(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K9*K9
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z34)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z34
       deallocate(Z34)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(Z35(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K9*K9
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z35)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z35
       deallocate(Z35)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(Z36(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K9*K9
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z36)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z36
       deallocate(Z36)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z37(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z37)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z37,-1.000)
       deallocate(Z37)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z38(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z38)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z38,-1.000)
       deallocate(Z38)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z39(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z39)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z39,-1.000)
       deallocate(Z39)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z40(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z40)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z40,-1.000)
       deallocate(Z40)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z41(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z41)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z41,-1.000)
       deallocate(Z41)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z42(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z42)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z42,-1.000)
       deallocate(Z42)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z43(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z43)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z43,-1.000)
       deallocate(Z43)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(Z44(N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K9*K9
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z44)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1243(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z44,-1.000)
       deallocate(Z44)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z45(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z45)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z45, 1.000)
       deallocate(Z45)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z46(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z46)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z46, 1.000)
       deallocate(Z46)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,N2,M2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z47(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z47)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z47, 1.000)
       deallocate(Z47)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z48(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z48)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z48, 1.000)
       deallocate(Z48)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z49(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z49)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z49, 1.000)
       deallocate(Z49)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z50(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z50)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z50, 1.000)
       deallocate(Z50)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,N2,M2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z51(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z51)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z51, 1.000)
       deallocate(Z51)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z52(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z52)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z52, 1.000)
       deallocate(Z52)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z53(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z53)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z53,-1.000)
       deallocate(Z53)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z54(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z54)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z54,-1.000)
       deallocate(Z54)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,N2,M2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z55(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z55)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z55,-1.000)
       deallocate(Z55)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z56(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z56)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z56,-1.000)
       deallocate(Z56)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z57(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z57)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z57,-1.000)
       deallocate(Z57)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z58(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z58)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z58,-1.000)
       deallocate(Z58)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,M1,N2,M2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z59(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z59)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z59,-1.000)
       deallocate(Z59)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & M1,N2,N2,M2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z60(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z60)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N1,M2,N1,M2,N0,M1,M1,N1,V2A,Z60,-1.000)
       deallocate(Z60)
C
       call sumx2(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,M2,N1,M2,N0,M1,M1,N1,HT2A,V2A,1.0)
       deallocate(V2A)
C
       end
