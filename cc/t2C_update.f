       subroutine t2C_update(N0,N1,N2,N3,V2C,shift,
     & K1,K2,K3,K4,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & t1A,t1B,t2A,t2B,t2C)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 shift
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
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
C
       real*8 V2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U3(:,:,:,:)
       real*8,allocatable::X3(:,:)
       real*8,allocatable::U4(:,:,:,:)
       real*8,allocatable::X4(:,:)
       real*8,allocatable::U5(:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::U11(:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U13(:,:,:,:)
       real*8,allocatable::U21(:,:,:,:)
       real*8,allocatable::U45(:,:,:,:)
       real*8,allocatable::U41(:,:,:,:)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N0,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q1(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N2,N0+1:N2))
       X3=0.0d0
       X3=X3+Q1
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,N3,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q2(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N2+1:N3,N2+1:N3))
       X4=0.0d0
       X4=X4+Q2
       deallocate(Q2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder4312(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S10(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X7=0.0d0
       call sum2134(N0,N2,N2,N3,N0,N2,N0,N2,X7,S10, 1.000)
       deallocate(S10)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N0,N2,VCHPHP,D1)
       allocate(S12(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S12)
       deallocate(D1)
C
       allocate(X8(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X8=0.0d0
       call sum3124(N0,N2,N2,N3,N0,N2,N0,N2,X8,S12, 1.000)
       deallocate(S12)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N0,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S14(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S14)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N2,N2,N3,N0,N2,N0,N2,X8,S14, 1.000)
       deallocate(S14)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,N1,N1,N3,N2,N3,VBHPPP,D1)
       allocate(S16(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S16)
       deallocate(D1)
C
       allocate(X2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       X2=0.0d0
       call sum4123(N0,N1,N1,N3,N2,N3,N0,N2,X2,S16, 1.000)
       deallocate(S16)
C
       allocate(Q3(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,FBHP,t1B,Q3)
C
       call sum21(N0,N2,N0,N2,X3,Q3, 1.000)
       deallocate(Q3)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q4(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q4)
       deallocate(B1)
       deallocate(B2)
C
       call sum21(N2,N3,N2,N3,X4,Q4,-1.000)
       deallocate(Q4)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,N0,N2,VCHHHP,D1)
       allocate(S20(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S20)
       deallocate(D1)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder2314(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S20,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(U21(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,U21)
       deallocate(D1)
       deallocate(D2)
C
       V2C=V2C+0.500*U21
       call
     & sum1243(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U21,-0.500)
       deallocate(U21)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder3214(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S20,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S44(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,S44,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U45(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,U45)
       deallocate(D1)
       deallocate(B2)
C
       V2C=V2C+U45
       call
     & sum1243(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U45,-1.000)
       deallocate(U45)
       deallocate(S44)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q5(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3+Q5
       deallocate(Q5)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N2,N3,VCHPPP,D1)
       allocate(S25(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S25)
       deallocate(D1)
C
       allocate(X6(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       X6=0.0d0
       call sum4123(N0,N2,N2,N3,N2,N3,N0,N2,X6,S25,-1.000)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder3241(N0,N2,N0,N2,N2,N3,N2,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,S25,D1)
       allocate(S46(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S46)
       deallocate(D1)
C
       allocate(X1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X1=0.0d0
       call sum3124(N0,N2,N2,N3,N0,N2,N0,N2,X1,S46,-1.000)
       deallocate(S46)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q6(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       X4=X4-Q6
       deallocate(Q6)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S30(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S30)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N1,N1,N3,N2,N3,N0,N2,X2,S30, 0.500)
       deallocate(S30)
C
       call sum4231(N0,N1,N1,N3,N2,N3,N0,N2,X2,VBHPPH, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(U3(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,X2,D2,U3)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U3,-1.000)
       call
     & sum1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U3, 1.000)
       call
     & sum2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U3, 1.000)
       call
     & sum1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U3,-1.000)
       deallocate(U3)
       deallocate(X2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S32(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S32)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N2,N3,N2,N3,N0,N2,X6,S32,-1.000)
C
       call sum2431(N0,N2,N2,N3,N2,N3,N0,N2,X6,VCHPHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U7(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,X6,D2,U7)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U7, 1.000)
       call
     & sum1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U7,-1.000)
       call
     & sum2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U7,-1.000)
       call
     & sum1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U7, 1.000)
       deallocate(U7)
       deallocate(X6)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,S32,D1)
       allocate(S48(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S48)
       deallocate(D1)
C
       call sum4123(N0,N2,N2,N3,N0,N2,N0,N2,X8,S48, 1.000)
       deallocate(S48)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(Q8(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q8)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N2,N3,N2,N3,X4,Q8,-1.000)
       deallocate(Q8)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(Q9(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q9)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N2,N3,N2,N3,X4,Q9, 0.500)
       deallocate(Q9)
C
       allocate(S37(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K4*K4
       call EGEMM(I1,I2,I3,VCHHPP,t2C,S37)
C
       allocate(X5(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       X5=0.0d0
       call sum3412(N0,N2,N0,N2,N0,N2,N0,N2,X5,S37, 0.500)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder4312(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S37,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S56(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N2,N2,N3,N0,N2,N0,N2,X7,S56, 0.500)
       deallocate(S56)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S40(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S40)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S40,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U41(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,U41)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U41,-1.000)
       call
     & sum1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U41, 1.000)
       deallocate(U41)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,S40,D1)
       allocate(S53(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S53)
       deallocate(D1)
C
       call sum4123(N0,N2,N2,N3,N0,N2,N0,N2,X8,S53, 1.000)
       deallocate(S53)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q11(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q11,B1)
       allocate(Q12(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,B1,t1B,Q12)
       deallocate(B1)
C
       call sum21(N0,N2,N0,N2,X3,Q12, 1.000)
       deallocate(Q12)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q13(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,Q11,B2,Q13)
       deallocate(B2)
C
       call sum21(N2,N3,N2,N3,X4,Q13,-1.000)
       deallocate(Q13)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder1243(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N0,N2,N2,N3,VCHPPP,D1)
       allocate(S27(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K2
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,t2C,S27)
       deallocate(D1)
C
       call sum3412(N0,N2,N2,N3,N0,N2,N0,N2,X1,S27, 0.500)
       deallocate(S27)
C
       call sum2134(N0,N2,N2,N3,N0,N2,N0,N2,X1,VCHHHP, 1.000)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,X1,B2,U1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U1,-1.000)
       V2C=V2C+U1
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(Q7(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q7)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N2,N0,N2,X3,Q7, 1.000)
       deallocate(Q7)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder2341(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,N2,N3,VCHHPP,D1)
       allocate(S50(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S50)
       deallocate(D1)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder2431(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S50,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q14(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3+Q14
       deallocate(Q14)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder4231(N0,N2,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N0,N2,N0,N2,S50,D1)
       allocate(S51(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S51)
       deallocate(D1)
C
       call sum3124(N0,N2,N0,N2,N0,N2,N0,N2,X5,S51, 1.000)
C
       call sum3412(N0,N2,N0,N2,N0,N2,N0,N2,X5,VCHHHH, 1.000)
C
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(U6(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K2*K2
       call EGEMM(I1,I2,I3,X5,D2,U6)
       deallocate(D2)
C
       V2C=V2C+0.500*U6
       deallocate(U6)
       deallocate(X5)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder3214(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S51,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S59(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N2,N2,N3,N0,N2,N0,N2,X7,S59, 1.000)
       deallocate(S59)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U11(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,X7,B2,U11)
       deallocate(B2)
C
       V2C=V2C+U11
       deallocate(U11)
       deallocate(X7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(Q10(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q10)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N2,N0,N2,X3,Q10, 0.500)
       deallocate(Q10)
C
       call sum21(N0,N2,N0,N2,X3,FBHH, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U4(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,X3,D2,U4)
       deallocate(D2)
C
       V2C=V2C+U4
       call
     & sum1243(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U4,-1.000)
       deallocate(U4)
       deallocate(X3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q15(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q16(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,Q15,B2,Q16)
       deallocate(B2)
C
       call sum21(N2,N3,N2,N3,X4,Q16,-1.000)
       deallocate(Q16)
C
       X4=X4+FBPP
C
       allocate(U5(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,X4,t2C,U5)
C
       call
     & sum2341(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U5, 1.000)
       call
     & sum1342(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U5,-1.000)
       deallocate(U5)
       deallocate(X4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder4132(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S22(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S22)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N2,N2,N3,N0,N2,N0,N2,X8,S22,-1.000)
       deallocate(S22)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U13(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,X8,B2,U13)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U13,-1.000)
       V2C=V2C+U13
       call
     & sum2143(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U13, 1.000)
       call
     & sum1243(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U13,-1.000)
       deallocate(U13)
       deallocate(X8)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N2,N3,N0,N2,VCHPPP,D1)
       allocate(U2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,U2)
       deallocate(D1)
C
       call
     & sum3124(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U2, 1.000)
       call
     & sum4123(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U2,-1.000)
       deallocate(U2)
C
       end
