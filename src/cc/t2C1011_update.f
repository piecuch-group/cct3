       subroutine t2C1011_update(N0,N1,N2,N3,HT2C,shift,
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
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
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
       real*8,allocatable::S87(:,:,:,:)
       real*8,allocatable::S91(:,:,:,:)
       real*8,allocatable::S95(:,:,:,:)
       real*8,allocatable::S97(:,:,:,:)
       real*8,allocatable::S85(:,:,:,:)
       real*8,allocatable::S89(:,:,:,:)
       real*8,allocatable::S93(:,:,:,:)
       real*8,allocatable::S105(:,:,:,:)
       real*8,allocatable::S107(:,:,:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
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
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::Z15(:,:,:,:)
       real*8,allocatable::X16(:,:,:,:)
       real*8,allocatable::Z16(:,:,:,:)
       real*8,allocatable::X17(:,:,:,:)
       real*8,allocatable::Z17(:,:,:,:)
       real*8,allocatable::X18(:,:,:,:)
       real*8,allocatable::Z18(:,:,:,:)
       real*8,allocatable::Z19(:,:,:,:)
       real*8,allocatable::Z20(:,:,:,:)
       real*8,allocatable::Z21(:,:,:,:)
       real*8,allocatable::Z22(:,:,:,:)
       real*8,allocatable::Z23(:,:,:,:)
       real*8,allocatable::Z24(:,:,:,:)
       real*8,allocatable::Z25(:,:,:,:)
       real*8,allocatable::Z26(:,:,:,:)
       real*8,allocatable::X19(:,:)
       real*8,allocatable::Z27(:,:,:,:)
       real*8,allocatable::X20(:,:)
       real*8,allocatable::Z28(:,:,:,:)
       real*8,allocatable::X21(:,:)
       real*8,allocatable::Z29(:,:,:,:)
       real*8,allocatable::X22(:,:)
       real*8,allocatable::Z30(:,:,:,:)
       real*8,allocatable::X23(:,:,:,:)
       real*8,allocatable::Z31(:,:,:,:)
       real*8,allocatable::X24(:,:,:,:)
       real*8,allocatable::Z32(:,:,:,:)
       real*8,allocatable::X25(:,:,:,:)
       real*8,allocatable::Z33(:,:,:,:)
       real*8,allocatable::X26(:,:,:,:)
       real*8,allocatable::Z34(:,:,:,:)
       real*8,allocatable::X27(:,:,:,:)
       real*8,allocatable::Z35(:,:,:,:)
       real*8,allocatable::X28(:,:,:,:)
       real*8,allocatable::Z36(:,:,:,:)
       real*8,allocatable::X29(:,:,:,:)
       real*8,allocatable::Z37(:,:,:,:)
       real*8,allocatable::X30(:,:,:,:)
       real*8,allocatable::Z38(:,:,:,:)
       real*8,allocatable::X31(:,:,:,:)
       real*8,allocatable::Z39(:,:,:,:)
       real*8,allocatable::X32(:,:,:,:)
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
       real*8,allocatable::X33(:,:,:,:)
       real*8,allocatable::Z84(:,:,:,:)
       real*8,allocatable::X34(:,:,:,:)
       real*8,allocatable::Z99(:,:,:,:)
       real*8,allocatable::Z96(:,:,:,:)
       real*8,allocatable::Z98(:,:,:,:)
       real*8,allocatable::Z106(:,:,:,:)
       real*8,allocatable::Z108(:,:,:,:)
       real*8,allocatable::Z142(:,:,:,:)
       real*8,allocatable::Z144(:,:,:,:)
       real*8,allocatable::Z154(:,:,:,:)
       real*8,allocatable::Z156(:,:,:,:)
C
       allocate(V2C(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       V2C=0.0d0
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
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
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z47(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I2=K8*K5*K0*K0
       I3=K6*K5
       call DVECMAT(I2,I3,Q1,F2,Z47)
       deallocate(F2)
C
       V2C=V2C+Z47
       deallocate(Z47)
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
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
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z48(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I2=K8*K5*K0*K0
       I3=K6*K7
       call DVECMAT(I2,I3,Q2,F2,Z48)
       deallocate(F2)
C
       V2C=V2C+Z48
       deallocate(Z48)
       deallocate(Q2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
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
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z49(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I2=K8*K5*K0*K0
       I3=K9*K5
       call DVECMAT(I2,I3,Q3,F2,Z49)
       deallocate(F2)
C
       V2C=V2C+Z49
       deallocate(Z49)
       deallocate(Q3)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
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
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z50(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I2=K8*K5*K0*K0
       I3=K9*K7
       call DVECMAT(I2,I3,Q4,F2,Z50)
       deallocate(F2)
C
       V2C=V2C+Z50
       deallocate(Z50)
       deallocate(Q4)
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
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z51(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I2=K8*K5*K0*K0
       I3=K6*K5
       call DVECMAT(I2,I3,Q5,F2,Z51)
       deallocate(F2)
C
       V2C=V2C+Z51
       deallocate(Z51)
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
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z52(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I2=K8*K5*K0*K0
       I3=K6*K8
       call DVECMAT(I2,I3,Q6,F2,Z52)
       deallocate(F2)
C
       V2C=V2C-Z52
       deallocate(Z52)
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
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z53(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I2=K8*K5*K0*K0
       I3=K0*K5
       call DVECMAT(I2,I3,Q7,F2,Z53)
       deallocate(F2)
C
       V2C=V2C+Z53
       deallocate(Z53)
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
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z54(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I2=K8*K5*K0*K0
       I3=K0*K8
       call DVECMAT(I2,I3,Q8,F2,Z54)
       deallocate(F2)
C
       V2C=V2C-Z54
       deallocate(Z54)
       deallocate(Q8)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S55(M1+1:N2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       X5=0.0d0
       call
     & sum4123(M1,N2,N0,M1,M2,N3,M1,N2,X5,S55, 1.000)
       deallocate(S55)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,M1,N2,X5,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,t3C1,F2)
       allocate(Z5(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K0*K0
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,X5,F2,Z5)
       deallocate(F2)
C
       V2C=V2C-Z5
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S57(M1+1:N2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N2))
       X6=0.0d0
       call
     & sum4123(N0,M1,M1,N1,M2,N3,M1,N2,X6,S57, 1.000)
       deallocate(S57)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,M1,N2,X6,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N2,M2,N0,M1,t3C4,F2)
       allocate(Z6(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K0*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,X6,F2,Z6)
       deallocate(F2)
C
       V2C=V2C+Z6
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S59(M1+1:N2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       X7=0.0d0
       call
     & sum4123(M1,N2,M1,N1,M2,N3,M1,N2,X7,S59, 1.000)
       deallocate(S59)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,M1,N2,X7,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,N2,M2,N0,M1,t3C1,F2)
       allocate(Z7(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K0*K0
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,X7,F2,Z7)
       deallocate(F2)
C
       V2C=V2C-Z7
       deallocate(Z7)
       deallocate(X7)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S61(M1+1:N2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(M1+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       X8=0.0d0
       call
     & sum4123(M1,N2,N0,M1,N1,M2,M1,N2,X8,S61, 1.000)
       deallocate(S61)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,M1,N2,X8,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,M2,N2,M2,N0,M1,t3C1,F2)
       allocate(Z8(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K0*K0
       I3=K9*K5*K8
       call DMATMAT(I1,I2,I3,X8,F2,Z8)
       deallocate(F2)
C
       V2C=V2C-Z8
       deallocate(Z8)
       deallocate(X8)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S63(M1+1:N2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N2))
       X9=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,M1,N2,X9,S63, 1.000)
       deallocate(S63)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,M1,N2,X9,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,N2,M2,N0,M1,t3C4,F2)
       allocate(Z9(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K0*K0
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,X9,F2,Z9)
       deallocate(F2)
C
       V2C=V2C+Z9
       deallocate(Z9)
       deallocate(X9)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S65(M1+1:N2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       X10=0.0d0
       call
     & sum4123(M1,N2,M1,N1,N1,M2,M1,N2,X10,S65, 1.000)
       deallocate(S65)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,M1,N2,X10,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,M2,N2,M2,N0,M1,t3C1,F2)
       allocate(Z10(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K0*K0
       I3=K9*K7*K8
       call DMATMAT(I1,I2,I3,X10,F2,Z10)
       deallocate(F2)
C
       V2C=V2C-Z10
       deallocate(Z10)
       deallocate(X10)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S67(N0+1:M1,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N0+1:M1,N0+1:M1,M2+1:N3,N0+1:M1))
       X11=0.0d0
       call
     & sum4123(N0,M1,N0,M1,M2,N3,N0,M1,X11,S67, 1.000)
       deallocate(S67)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N0,M1,X11,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(Z11(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K0*K0
       I3=K6*K5*K5
       call DMATMAT(I1,I2,I3,X11,F2,Z11)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z11,-1.000)
       deallocate(Z11)
       deallocate(X11)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S69(N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       X12=0.0d0
       call
     & sum4123(M1,N2,N0,M1,M2,N3,N0,M1,X12,S69, 1.000)
       deallocate(S69)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N0,M1,X12,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(Z12(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K0*K0
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,X12,F2,Z12)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z12,-1.000)
       deallocate(Z12)
       deallocate(X12)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S71(N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       X13=0.0d0
       call
     & sum4123(N0,M1,M1,N1,M2,N3,N0,M1,X13,S71, 1.000)
       deallocate(S71)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N0,M1,X13,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(Z13(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K0*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,X13,F2,Z13)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z13,-1.000)
       deallocate(Z13)
       deallocate(X13)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S73(N0+1:M1,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(M1+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       X14=0.0d0
       call
     & sum4123(M1,N2,M1,N1,M2,N3,N0,M1,X14,S73, 1.000)
       deallocate(S73)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N0,M1,X14,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(Z14(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K0*K0
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,X14,F2,Z14)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z14,-1.000)
       deallocate(Z14)
       deallocate(X14)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S75(N0+1:M1,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(N0+1:M1,N0+1:M1,N1+1:M2,N0+1:M1))
       X15=0.0d0
       call
     & sum4123(N0,M1,N0,M1,N1,M2,N0,M1,X15,S75, 1.000)
       deallocate(S75)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,N0,M1,X15,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(Z15(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K0*K0
       I3=K9*K5*K5
       call DMATMAT(I1,I2,I3,X15,F2,Z15)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z15,-1.000)
       deallocate(Z15)
       deallocate(X15)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S77(N0+1:M1,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       X16=0.0d0
       call
     & sum4123(M1,N2,N0,M1,N1,M2,N0,M1,X16,S77, 1.000)
       deallocate(S77)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N0,M1,X16,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(Z16(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K0*K0
       I3=K9*K5*K8
       call DMATMAT(I1,I2,I3,X16,F2,Z16)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z16,-1.000)
       deallocate(Z16)
       deallocate(X16)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S79(N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       X17=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N1,M2,N0,M1,X17,S79, 1.000)
       deallocate(S79)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N0,M1,X17,VBHHPH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(Z17(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K0*K0
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,X17,F2,Z17)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z17,-1.000)
       deallocate(Z17)
       deallocate(X17)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S81(N0+1:M1,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S81)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X18(M1+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       X18=0.0d0
       call
     & sum4123(M1,N2,M1,N1,N1,M2,N0,M1,X18,S81, 1.000)
       deallocate(S81)
C
       call sumx4312(N0,N2,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N0,M1,X18,VBHHPH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(Z18(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K0*K0
       I3=K9*K7*K8
       call DMATMAT(I1,I2,I3,X18,F2,Z18)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z18,-1.000)
       deallocate(Z18)
       deallocate(X18)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S83(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S83)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X33(N0+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       X33=0.0d0
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X33,S83, 1.000)
C
       allocate(X34(N0+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       X34=0.0d0
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X34,S83, 1.000)
       deallocate(S83)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S87(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S87)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X33,S87, 1.000)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X34,S87, 1.000)
       deallocate(S87)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S91(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S91)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X33,S91, 1.000)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X34,S91, 1.000)
       deallocate(S91)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S95(N2+1:M2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,M1,N2,M2,N1,M2,
     & N0,M1,N2,M2,N1,M2,N2,M2,S95,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z96(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z96)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z96,-1.000)
       deallocate(Z96)
       deallocate(S95)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S97(N2+1:M2,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2))
       call reorder2341(N2,M2,M1,N1,N2,M2,N1,M2,
     & M1,N1,N2,M2,N1,M2,N2,M2,S97,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z98(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K9*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z98)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z98,-1.000)
       deallocate(Z98)
       deallocate(S97)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S85(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S85)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X33,S85, 1.000)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X34,S85, 1.000)
       deallocate(S85)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S89(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,S89)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X33,S89, 1.000)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X34,S89, 1.000)
       deallocate(S89)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S93(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S93)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X33,S93, 1.000)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X34,S93, 1.000)
       deallocate(S93)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S105(N2+1:M2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,M1,N2,M2,N1,M2,
     & N0,M1,N2,M2,N1,M2,N2,M2,S105,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z106(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z106)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z106, 1.000)
       deallocate(Z106)
       deallocate(S105)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S107(N2+1:M2,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S107)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2))
       call reorder2341(N2,M2,M1,N1,N2,M2,N1,M2,
     & M1,N1,N2,M2,N1,M2,N2,M2,S107,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z108(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K9*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z108)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z108, 1.000)
       deallocate(Z108)
       deallocate(S107)
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
       allocate(X1(N0+1:M1,M2+1:N3))
       X1=0.0d0
       X1=X1+Q9
       deallocate(Q9)
C
       call sumx21(N1,N3,N0,N1,
     & N0,M1,M2,N3,X1,FAHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z1(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I2=K8*K5*K0*K0
       I3=K6*K5
       call DVECMAT(I2,I3,X1,F2,Z1)
       deallocate(F2)
C
       V2C=V2C+Z1
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
       allocate(X2(M1+1:N1,M2+1:N3))
       X2=0.0d0
       X2=X2+Q10
       deallocate(Q10)
C
       call sumx21(N1,N3,N0,N1,
     & M1,N1,M2,N3,X2,FAHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z2(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I2=K8*K5*K0*K0
       I3=K6*K7
       call DVECMAT(I2,I3,X2,F2,Z2)
       deallocate(F2)
C
       V2C=V2C+Z2
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
       allocate(X3(N0+1:M1,N1+1:M2))
       X3=0.0d0
       X3=X3+Q11
       deallocate(Q11)
C
       call sumx21(N1,N3,N0,N1,
     & N0,M1,N1,M2,X3,FAHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z3(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I2=K8*K5*K0*K0
       I3=K9*K5
       call DVECMAT(I2,I3,X3,F2,Z3)
       deallocate(F2)
C
       V2C=V2C+Z3
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
       allocate(X4(M1+1:N1,N1+1:M2))
       X4=0.0d0
       X4=X4+Q12
       deallocate(Q12)
C
       call sumx21(N1,N3,N0,N1,
     & M1,N1,N1,M2,X4,FAHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z4(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I2=K8*K5*K0*K0
       I3=K9*K7
       call DVECMAT(I2,I3,X4,F2,Z4)
       deallocate(F2)
C
       V2C=V2C+Z4
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S113(M1+1:N2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S113)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X23(N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       X23=0.0d0
       call
     & sum4123(N0,M1,M1,N2,M2,N3,M1,N2,X23,S113,-1.000)
       deallocate(S113)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,M1,N2,X23,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(Z31(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K0*K0
       I3=K6*K8*K5
       call DMATMAT(I1,I2,I3,X23,F2,Z31)
       deallocate(F2)
C
       V2C=V2C-Z31
       deallocate(Z31)
       deallocate(X23)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S115(M1+1:N2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S115)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X24(M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       X24=0.0d0
       call
     & sum4123(M1,N2,M1,N2,M2,N3,M1,N2,X24,S115,-1.000)
       deallocate(S115)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,M1,N2,X24,VCHHHP, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(Z32(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K0*K0
       I3=K6*K8*K8
       call DMATMAT(I1,I2,I3,X24,F2,Z32)
       deallocate(F2)
C
       V2C=V2C+0.500*Z32
       deallocate(Z32)
       deallocate(X24)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S117(M1+1:N2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S117)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X25(N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       X25=0.0d0
       call
     & sum4123(N0,M1,M1,N2,N2,M2,M1,N2,X25,S117,-1.000)
       deallocate(S117)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,M1,N2,X25,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(Z33(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K0*K0
       I3=K0*K8*K5
       call DMATMAT(I1,I2,I3,X25,F2,Z33)
       deallocate(F2)
C
       V2C=V2C-Z33
       deallocate(Z33)
       deallocate(X25)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S119(M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S119)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X26(M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       X26=0.0d0
       call
     & sum4123(M1,N2,M1,N2,N2,M2,M1,N2,X26,S119,-1.000)
       deallocate(S119)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,M1,N2,X26,VCHHHP, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(Z34(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K0*K0
       I3=K0*K8*K8
       call DMATMAT(I1,I2,I3,X26,F2,Z34)
       deallocate(F2)
C
       V2C=V2C+0.500*Z34
       deallocate(Z34)
       deallocate(X26)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,M1,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S121(N0+1:M1,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S121)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X27(N0+1:M1,N0+1:M1,M2+1:N3,N0+1:M1))
       X27=0.0d0
       call
     & sum4123(N0,M1,N0,M1,M2,N3,N0,M1,X27,S121,-1.000)
       deallocate(S121)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,N0,M1,M2,N3,N0,M1,X27,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(Z35(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K0*K0
       I3=K6*K5*K5
       call DMATMAT(I1,I2,I3,X27,F2,Z35)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z35,-0.500)
       deallocate(Z35)
       deallocate(X27)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S123(N0+1:M1,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S123)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X28(N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       X28=0.0d0
       call
     & sum4123(N0,M1,M1,N2,M2,N3,N0,M1,X28,S123,-1.000)
       deallocate(S123)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N0,M1,X28,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(Z36(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K0*K0
       I3=K6*K8*K5
       call DMATMAT(I1,I2,I3,X28,F2,Z36)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z36,-1.000)
       deallocate(Z36)
       deallocate(X28)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S125(N0+1:M1,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S125)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X29(M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       X29=0.0d0
       call
     & sum4123(M1,N2,M1,N2,M2,N3,N0,M1,X29,S125,-1.000)
       deallocate(S125)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N0,M1,X29,VCHHHP, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(Z37(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K0*K0
       I3=K6*K8*K8
       call DMATMAT(I1,I2,I3,X29,F2,Z37)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z37,-0.500)
       deallocate(Z37)
       deallocate(X29)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,M1,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S127(N0+1:M1,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S127)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X30(N0+1:M1,N0+1:M1,N2+1:M2,N0+1:M1))
       X30=0.0d0
       call
     & sum4123(N0,M1,N0,M1,N2,M2,N0,M1,X30,S127,-1.000)
       deallocate(S127)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,N0,M1,N2,M2,N0,M1,X30,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,N2,M2,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(Z38(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K0*K0
       I3=K0*K5*K5
       call DMATMAT(I1,I2,I3,X30,F2,Z38)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z38,-0.500)
       deallocate(Z38)
       deallocate(X30)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S129(N0+1:M1,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S129)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X31(N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       X31=0.0d0
       call
     & sum4123(N0,M1,M1,N2,N2,M2,N0,M1,X31,S129,-1.000)
       deallocate(S129)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N0,M1,X31,VCHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(Z39(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K0*K0
       I3=K0*K8*K5
       call DMATMAT(I1,I2,I3,X31,F2,Z39)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z39,-1.000)
       deallocate(Z39)
       deallocate(X31)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S131(N0+1:M1,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S131)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X32(M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       X32=0.0d0
       call
     & sum4123(M1,N2,M1,N2,N2,M2,N0,M1,X32,S131,-1.000)
       deallocate(S131)
C
       call sumx3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N0,M1,X32,VCHHHP, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(Z40(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K0*K0
       I3=K0*K8*K8
       call DMATMAT(I1,I2,I3,X32,F2,Z40)
       deallocate(F2)
C
       call
     & sum1243(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z40,-0.500)
       deallocate(Z40)
       deallocate(X32)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S133(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S133)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X33,S133,-0.500)
       deallocate(S133)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S135(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S135)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X33,S135, 0.500)
       deallocate(S135)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S137(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S137)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X33,S137,-1.000)
       deallocate(S137)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S139(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S139)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X33,S139, 1.000)
       deallocate(S139)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Z84(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,X33,B2,Z84)
       deallocate(B2)
C
       V2C=V2C-Z84
       deallocate(Z84)
       deallocate(X33)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S141(N2+1:M2,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S141)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,M1,N2,M2,N2,M2,
     & N0,M1,N2,M2,N2,M2,N2,M2,S141,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z142(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K0*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z142)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z142, 0.500)
       deallocate(Z142)
       deallocate(S141)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S143(N2+1:M2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S143)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,M1,N2,N2,M2,N2,M2,
     & M1,N2,N2,M2,N2,M2,N2,M2,S143,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z144(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K0*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z144)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z144,-0.500)
       deallocate(Z144)
       deallocate(S143)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S145(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S145)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X34,S145, 0.500)
       deallocate(S145)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S147(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S147)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X34,S147,-0.500)
       deallocate(S147)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S149(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S149)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X34,S149, 1.000)
       deallocate(S149)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S151(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S151)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X34,S151,-1.000)
       deallocate(S151)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Z99(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,X34,B2,Z99)
       deallocate(B2)
C
       call
     & sum2134(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z99, 1.000)
       deallocate(Z99)
       deallocate(X34)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S153(N2+1:M2,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S153)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,M1,N2,M2,N2,M2,
     & N0,M1,N2,M2,N2,M2,N2,M2,S153,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z154(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K0*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z154)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z154, 0.500)
       deallocate(Z154)
       deallocate(S153)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S155(N2+1:M2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S155)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,M1,N2,N2,M2,N2,M2,
     & M1,N2,N2,M2,N2,M2,N2,M2,S155,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z156(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K0*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z156)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z156,-0.500)
       deallocate(Z156)
       deallocate(S155)
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
       allocate(X19(N0+1:M1,M2+1:N3))
       X19=0.0d0
       X19=X19+Q13
       deallocate(Q13)
C
       call sumx21(N2,N3,N0,N2,
     & N0,M1,M2,N3,X19,FBHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z27(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I2=K8*K5*K0*K0
       I3=K6*K5
       call DVECMAT(I2,I3,X19,F2,Z27)
       deallocate(F2)
C
       V2C=V2C+Z27
       deallocate(Z27)
       deallocate(X19)
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
       allocate(X20(M1+1:N2,M2+1:N3))
       X20=0.0d0
       X20=X20+Q14
       deallocate(Q14)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,M2,N3,X20,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z28(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I2=K8*K5*K0*K0
       I3=K6*K8
       call DVECMAT(I2,I3,X20,F2,Z28)
       deallocate(F2)
C
       V2C=V2C-Z28
       deallocate(Z28)
       deallocate(X20)
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
       allocate(X21(N0+1:M1,N2+1:M2))
       X21=0.0d0
       X21=X21+Q15
       deallocate(Q15)
C
       call sumx21(N2,N3,N0,N2,
     & N0,M1,N2,M2,X21,FBHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z29(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I2=K8*K5*K0*K0
       I3=K0*K5
       call DVECMAT(I2,I3,X21,F2,Z29)
       deallocate(F2)
C
       V2C=V2C+Z29
       deallocate(Z29)
       deallocate(X21)
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
       allocate(X22(M1+1:N2,N2+1:M2))
       X22=0.0d0
       X22=X22+Q16
       deallocate(Q16)
C
       call sumx21(N2,N3,N0,N2,
     & M1,N2,N2,M2,X22,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z30(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       I2=K8*K5*K0*K0
       I3=K0*K8
       call DVECMAT(I2,I3,X22,F2,Z30)
       deallocate(F2)
C
       V2C=V2C-Z30
       deallocate(Z30)
       deallocate(X22)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,VBHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z19(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z19)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z19, 1.000)
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z19,-1.000)
       deallocate(Z19)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,VBHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z20(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z20)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z20, 1.000)
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z20,-1.000)
       deallocate(Z20)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,VBHPPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z21(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K6*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z21)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z21, 1.000)
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z21,-1.000)
       deallocate(Z21)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,N2,M2,M2,N3,N2,M2,VBHPPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z22(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K6*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z22)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z22, 1.000)
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z22,-1.000)
       deallocate(Z22)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,VBHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z23(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z23)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z23, 1.000)
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z23,-1.000)
       deallocate(Z23)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,VBHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z24(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z24)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z24, 1.000)
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z24,-1.000)
       deallocate(Z24)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,VBHPPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z25(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z25)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z25, 1.000)
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z25,-1.000)
       deallocate(Z25)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & M1,N1,N2,M2,N1,M2,N2,M2,VBHPPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z26(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K9*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z26)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z26, 1.000)
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z26,-1.000)
       deallocate(Z26)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,VCHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z41(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z41)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z41,-0.500)
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z41, 0.500)
       deallocate(Z41)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,VCHPPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z42(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z42)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z42, 0.500)
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z42,-0.500)
       deallocate(Z42)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z43(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K0*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z43)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z43,-1.000)
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z43, 1.000)
       deallocate(Z43)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z44(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K0*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z44)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z44, 1.000)
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z44,-1.000)
       deallocate(Z44)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z45(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K0*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z45)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z45,-0.500)
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z45, 0.500)
       deallocate(Z45)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z46(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K0
       I3=K0*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z46)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z46, 0.500)
       call
     & sum1342(N2,M2,N2,M2,N0,M1,M1,N2,V2C,Z46,-0.500)
       deallocate(Z46)
C
       call sumx2(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,M2,N2,M2,N0,M1,M1,N2,HT2C,V2C,1.0)
       deallocate(V2C)
C
       end
