       subroutine t2B_update(N0,N1,N2,N3,V2B,shift,
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
       real*8 V2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S90(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::S104(:,:,:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::S102(:,:,:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::S64(:,:,:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::S70(:,:,:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::S74(:,:,:,:)
       real*8,allocatable::S119(:,:,:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::S78(:,:,:,:)
       real*8,allocatable::S115(:,:,:,:)
       real*8,allocatable::S81(:,:,:,:)
       real*8,allocatable::S108(:,:,:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::S88(:,:,:,:)
       real*8,allocatable::S121(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S92(:,:,:,:)
       real*8,allocatable::S94(:,:,:,:)
       real*8,allocatable::S95(:,:,:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::S99(:,:,:,:)
       real*8,allocatable::S106(:,:,:,:)
       real*8,allocatable::Q24(:,:)
       real*8,allocatable::S100(:,:,:,:)
       real*8,allocatable::S125(:,:,:,:)
       real*8,allocatable::S83(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::S111(:,:,:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::S112(:,:,:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q28(:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U3(:,:,:,:)
       real*8,allocatable::U4(:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:)
       real*8,allocatable::X4(:,:)
       real*8,allocatable::U6(:,:,:,:)
       real*8,allocatable::X5(:,:)
       real*8,allocatable::U7(:,:,:,:)
       real*8,allocatable::X6(:,:)
       real*8,allocatable::U8(:,:,:,:)
       real*8,allocatable::X7(:,:)
       real*8,allocatable::U9(:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U10(:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::U11(:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::U12(:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::U13(:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::U14(:,:,:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::U15(:,:,:,:)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       call reorder2431(N0,N2,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,N1,N2,N3,N0,N2,VBHPPH,D1)
       allocate(S16(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4*K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S16)
       deallocate(D1)
C
       allocate(X1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       X1=0.0d0
       call sum4123(N0,N1,N2,N3,N0,N2,N0,N1,X1,S16, 1.000)
       deallocate(S16)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call reorder4312(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,N2,N0,N2,N0,N1,VBHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S18(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       X2=0.0d0
       call sum2134(N0,N2,N1,N3,N0,N2,N0,N1,X2,S18,-1.000)
       deallocate(S18)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call reorder1432(N2,N3,N0,N1,N2,N3,N0,N1,
     & N2,N3,N0,N1,N2,N3,N0,N1,VBHPHP,D1)
       allocate(S20(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S20)
       deallocate(D1)
C
       call sum3124(N0,N1,N2,N3,N0,N2,N0,N1,X1,S20, 1.000)
       deallocate(S20)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       call reorder2341(N0,N2,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,N2,N1,N3,N0,N2,VBPHPH,D1)
       allocate(S22(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S22)
       deallocate(D1)
C
       call sum4123(N0,N2,N1,N3,N0,N2,N0,N1,X2,S22, 1.000)
       deallocate(S22)
C
       allocate(Q1(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,FAHP,t1A,Q1)
C
       allocate(X4(N0+1:N1,N0+1:N1))
       X4=0.0d0
       call sum21(N0,N1,N0,N1,X4,Q1, 1.000)
       deallocate(Q1)
C
       allocate(B1(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q2(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q2)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X5(N1+1:N3,N1+1:N3))
       X5=0.0d0
       call sum21(N1,N3,N1,N3,X5,Q2,-1.000)
       deallocate(Q2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S26(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S26)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N1,N2,N3,N0,N2,N0,N1,X1,S26, 1.000)
       deallocate(S26)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q3(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q3)
       deallocate(D1)
       deallocate(B2)
C
       X4=X4+Q3
       deallocate(Q3)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N1,N3,VAHPPP,D1)
       allocate(S29(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S29)
       deallocate(D1)
C
       allocate(X8(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       X8=0.0d0
       call sum4123(N0,N1,N1,N3,N1,N3,N0,N1,X8,S29, 1.000)
       deallocate(S29)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q4(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q4)
       deallocate(D1)
       deallocate(B2)
C
       X5=X5-Q4
       deallocate(Q4)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N2))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,N1,N0,N2,VBHHPH,D1)
       allocate(S32(N0+1:N1,N0+1:N2,N0+1:N1,N0+1:N2))
       I1=K2*K1*K2
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S32)
       deallocate(D1)
C
       allocate(X9(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       X9=0.0d0
       call sum4123(N0,N2,N0,N1,N0,N2,N0,N1,X9,S32, 1.000)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call reorder3241(N0,N1,N0,N2,N0,N1,N0,N2,
     & N0,N1,N0,N2,N0,N2,N0,N1,S32,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S90(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N2,N1,N3,N0,N2,N0,N1,X2,S90,-1.000)
       deallocate(S90)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N2))
       call reorder3241(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,N1,N0,N2,VBHHPH,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,t2B,D2)
       allocate(S36(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K1*K4
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S36)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N1,N2,N3,N0,N2,N0,N1,X1,S36,-1.000)
       deallocate(S36)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N0,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q5(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:N2,N0+1:N2))
       X6=0.0d0
       X6=X6+Q5
       deallocate(Q5)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       call reorder1243(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N1,N3,N0,N1,N2,N3,VBHPPP,D1)
       allocate(S39(N0+1:N2,N0+1:N1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K1*K2
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,t2B,S39)
       deallocate(D1)
C
       call sum3412(N0,N1,N2,N3,N0,N2,N0,N1,X1,S39, 1.000)
       deallocate(S39)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,N3,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q6(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N2+1:N3,N2+1:N3))
       X7=0.0d0
       X7=X7+Q6
       deallocate(Q6)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,N1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S42(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S42)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N1,N2,N3,N0,N2,N0,N1,X1,S42, 1.000)
       deallocate(S42)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,N2,N2,N3,N1,N3,VBPHPP,D1)
       allocate(S44(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S44)
       deallocate(D1)
C
       allocate(X13(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       X13=0.0d0
       call sum4123(N0,N2,N2,N3,N1,N3,N0,N1,X13,S44, 1.000)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call reorder3241(N0,N1,N0,N2,N2,N3,N1,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,S44,D1)
       allocate(S104(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S104)
       deallocate(D1)
C
       call sum3124(N0,N2,N1,N3,N0,N2,N0,N1,X2,S104, 1.000)
       deallocate(S104)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N0,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S48(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S48)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N2,N1,N3,N0,N2,N0,N1,X2,S48, 1.000)
       deallocate(S48)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,N1,N1,N3,N2,N3,VBHPPP,D1)
       allocate(S50(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S50)
       deallocate(D1)
C
       allocate(X3(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       X3=0.0d0
       call sum4123(N0,N1,N1,N3,N2,N3,N0,N2,X3,S50, 1.000)
       deallocate(S50)
C
       call sum4231(N0,N1,N1,N3,N2,N3,N0,N2,X3,VBHPPH, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U5(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K3*K1
       call EGEMM(I1,I2,I3,X3,D2,U5)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U5, 1.000)
       deallocate(U5)
       deallocate(X3)
C
       allocate(Q7(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,FBHP,t1B,Q7)
C
       call sum21(N0,N2,N0,N2,X6,Q7, 1.000)
       deallocate(Q7)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q8(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q8)
       deallocate(B1)
       deallocate(B2)
C
       call sum21(N2,N3,N2,N3,X7,Q8,-1.000)
       deallocate(Q8)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,N1,N0,N1,VBHHHP,D1)
       allocate(S54(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S54)
       deallocate(D1)
C
       call sum3124(N0,N2,N0,N1,N0,N2,N0,N1,X9,S54, 1.000)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call reorder3214(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N2,N0,N2,N0,N1,S54,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S102(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S102)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N2,N1,N3,N0,N2,N0,N1,X2,S102,-1.000)
       deallocate(S102)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,N1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q9(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       X4=X4+Q9
       deallocate(Q9)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,N0,N2,N1,N3,N1,N3,VBPHPP,D1)
       allocate(S59(N0+1:N2,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S59)
       deallocate(D1)
C
       allocate(X11(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       X11=0.0d0
       call sum4123(N0,N2,N1,N3,N1,N3,N0,N2,X11,S59, 1.000)
       deallocate(S59)
C
       allocate(S61(N0+1:N2,N0+1:N1,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K1*K2
       I3=K3*K4
       call EGEMM(I1,I2,I3,VBPHPP,t2B,S61)
C
       call sum3412(N0,N2,N1,N3,N0,N2,N0,N1,X2,S61, 1.000)
       deallocate(S61)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q10(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q10)
       deallocate(D1)
       deallocate(B2)
C
       X5=X5+Q10
       deallocate(Q10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder4132(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S64(N1+1:N3,N0+1:N1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K1*K3
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S64)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N2,N1,N3,N0,N2,N0,N1,X2,S64,-1.000)
       deallocate(S64)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q11(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       X6=X6+Q11
       deallocate(Q11)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N2,N3,VCHPPP,D1)
       allocate(S67(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S67)
       deallocate(D1)
C
       allocate(X12(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       X12=0.0d0
       call sum4123(N0,N2,N2,N3,N2,N3,N0,N2,X12,S67,-1.000)
       deallocate(S67)
C
       call sum2431(N0,N2,N2,N3,N2,N3,N0,N2,X12,VCHPHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(U14(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K4*K2
       call EGEMM(I1,I2,I3,X12,D2,U14)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U14,-1.000)
       deallocate(U14)
       deallocate(X12)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q12(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       X7=X7-Q12
       deallocate(Q12)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S70(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S70)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N1,N1,N3,N1,N3,N0,N1,X8,S70,-1.000)
       deallocate(S70)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(Q13(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q13)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N1,N0,N1,X4,Q13, 0.500)
       deallocate(Q13)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q14(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q14)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N1,N3,N1,N3,X5,Q14, 0.500)
       deallocate(Q14)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S74(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S74)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N2,N3,N1,N3,N0,N1,X13,S74, 1.000)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N2,N2,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,S74,D1)
       allocate(S119(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S119)
       deallocate(D1)
C
       call sum3124(N0,N2,N1,N3,N0,N2,N0,N1,X2,S119, 1.000)
       deallocate(S119)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(Q16(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q16)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N2,N3,N2,N3,X7,Q16,-1.000)
       deallocate(Q16)
C
       allocate(S78(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K2
       I3=K3*K4
       call EGEMM(I1,I2,I3,VBHHPP,t2B,S78)
C
       call sum3412(N0,N2,N0,N1,N0,N2,N0,N1,X9,S78, 1.000)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call reorder4312(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,N2,N0,N2,N0,N1,S78,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S115(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S115)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N2,N1,N3,N0,N2,N0,N1,X2,S115,-1.000)
       deallocate(S115)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,N2,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S81(N1+1:N3,N0+1:N2,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K2*K3
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S81)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N1,N3,N1,N3,N0,N2,X11,S81,-1.000)
C
       call sum4213(N0,N2,N1,N3,N1,N3,N0,N2,X11,VBPHPH, 1.000)
C
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,t2B,D2)
       allocate(U13(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K4
       I3=K3*K2
       call EGEMM(I1,I2,I3,X11,D2,U13)
       deallocate(D2)
C
       call
     & sum1423(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U13,-1.000)
       deallocate(U13)
       deallocate(X11)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       call reorder4312(N1,N3,N0,N2,N0,N2,N1,N3,
     & N1,N3,N0,N2,N1,N3,N0,N2,S81,D1)
       allocate(S108(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S108)
       deallocate(D1)
C
       call sum4123(N0,N2,N1,N3,N0,N2,N0,N1,X2,S108,-1.000)
       deallocate(S108)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(Q18(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q18)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N1,N0,N1,X4,Q18, 1.000)
       deallocate(Q18)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(Q19(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q19)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N2,N3,N2,N3,X7,Q19,-0.500)
       deallocate(Q19)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(Q20(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q20)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N2,N0,N2,X6,Q20, 0.500)
       deallocate(Q20)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S88(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S88)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N2,N3,N1,N3,N0,N1,X13,S88, 1.000)
C
       call sum1324(N0,N2,N2,N3,N1,N3,N0,N1,X13,VBHPPH, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U15(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,X13,D2,U15)
       deallocate(D2)
C
       call
     & sum1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U15, 1.000)
       deallocate(U15)
       deallocate(X13)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N2,N2,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,S88,D1)
       allocate(S121(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S121)
       deallocate(D1)
C
       call sum3124(N0,N2,N1,N3,N0,N2,N0,N1,X2,S121, 1.000)
       deallocate(S121)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,N1,N2,N3,N2,N3,VBHPPP,D1)
       allocate(S34(N0+1:N1,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4*K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S34)
       deallocate(D1)
C
       allocate(X10(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       X10=0.0d0
       call sum4123(N0,N1,N2,N3,N2,N3,N0,N1,X10,S34, 1.000)
C
       call sum2431(N0,N1,N2,N3,N2,N3,N0,N1,X10,VBHPHP, 1.000)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(U12(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K2*K3
       I3=K4*K1
       call EGEMM(I1,I2,I3,X10,D2,U12)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U12,-1.000)
       deallocate(U12)
       deallocate(X10)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       call reorder3241(N0,N1,N0,N1,N2,N3,N2,N3,
     & N2,N3,N0,N1,N2,N3,N0,N1,S34,D1)
       allocate(S92(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4*K1
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S92)
       deallocate(D1)
C
       call sum3124(N0,N1,N2,N3,N0,N2,N0,N1,X1,S92, 1.000)
       deallocate(S92)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,N1,N1,N3,VAHHPP,D1)
       allocate(S94(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S94)
       deallocate(D1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder2431(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S94,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S95(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S95)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N1,N2,N3,N0,N2,N0,N1,X1,S95, 1.000)
       deallocate(S95)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder2431(N0,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,N1,S94,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q21(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q21)
       deallocate(D1)
       deallocate(B2)
C
       X4=X4+Q21
       deallocate(Q21)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,N1,N2,N3,VBHHPP,D1)
       allocate(S99(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S99)
       deallocate(D1)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call reorder4231(N0,N1,N0,N2,N0,N1,N2,N3,
     & N2,N3,N0,N2,N0,N1,N0,N1,S99,D1)
       allocate(S106(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S106)
       deallocate(D1)
C
       call sum3124(N0,N2,N0,N1,N0,N2,N0,N1,X9,S106, 1.000)
C
       call sum3412(N0,N2,N0,N1,N0,N2,N0,N1,X9,VBHHHH, 1.000)
C
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(U11(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K1*K2
       call EGEMM(I1,I2,I3,X9,D2,U11)
       deallocate(D2)
C
       V2B=V2B+U11
       deallocate(U11)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder2431(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,S99,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q24(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q24)
       deallocate(D1)
       deallocate(B2)
C
       X4=X4+Q24
       deallocate(Q24)
C
       call sum21(N0,N1,N0,N1,X4,FAHH, 1.000)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(U6(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1
       I2=K2*K3*K4
       I3=K1
       call EGEMM(I1,I2,I3,X4,D2,U6)
       deallocate(D2)
C
       V2B=V2B-U6
       deallocate(U6)
       deallocate(X4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder2431(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,N1,S99,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S100(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S100)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N1,N2,N3,N0,N2,N0,N1,X1,S100, 1.000)
       deallocate(S100)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       call reorder3214(N0,N2,N0,N2,N0,N1,N0,N1,
     & N0,N1,N0,N2,N0,N2,N0,N1,S106,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S125(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S125)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N2,N1,N3,N0,N2,N0,N1,X2,S125,-1.000)
       deallocate(S125)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S83(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S83)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N1,N1,N3,N1,N3,N0,N1,X8,S83,-1.000)
       deallocate(S83)
C
       call sum2431(N0,N1,N1,N3,N1,N3,N0,N1,X8,VAHPHP, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(U10(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,X8,D2,U10)
       deallocate(D2)
C
       call
     & sum1324(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U10,-1.000)
       deallocate(U10)
       deallocate(X8)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call reorder3124(N0,N2,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,N2,N1,N3,N0,N1,VBHPPH,D1)
       allocate(S46(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S46)
       deallocate(D1)
C
       call sum3124(N0,N2,N1,N3,N0,N2,N0,N1,X2,S46, 1.000)
       deallocate(S46)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,N1,N1,N3,VBHHPP,D1)
       allocate(S111(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S111)
       deallocate(D1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder3421(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,S111,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q25(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q25)
       deallocate(D1)
       deallocate(B2)
C
       X6=X6+Q25
       deallocate(Q25)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N2))
       call reorder2431(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N1,N3,N0,N1,N0,N2,S111,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,t2B,D2)
       allocate(S112(N2+1:N3,N0+1:N1,N0+1:N1,N0+1:N2))
       I1=K2*K1
       I2=K1*K4
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S112)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N1,N2,N3,N0,N2,N0,N1,X1,S112,-1.000)
       deallocate(S112)
C
       call sum2134(N0,N1,N2,N3,N0,N2,N0,N1,X1,VBHHHP, 1.000)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(U1(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,X1,B2,U1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U1,-1.000)
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(Q17(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q17)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N2,N0,N2,X6,Q17, 1.000)
       deallocate(Q17)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q26(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q27(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,Q26,B2,Q27)
       deallocate(B2)
C
       call sum21(N2,N3,N2,N3,X7,Q27,-1.000)
       deallocate(Q27)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q28(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q28)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q29(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,Q28,B2,Q29)
       deallocate(B2)
C
       call sum21(N1,N3,N1,N3,X5,Q29,-1.000)
       deallocate(Q29)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(Q15(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q15)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N1,N3,N1,N3,X5,Q15,-1.000)
       deallocate(Q15)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q22(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q22)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q23(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,Q22,B2,Q23)
       deallocate(B2)
C
       call sum21(N1,N3,N1,N3,X5,Q23,-1.000)
       deallocate(Q23)
C
       X5=X5+FAPP
C
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2B,D2)
       allocate(U7(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K2*K4
       I3=K3
       call EGEMM(I1,I2,I3,X5,D2,U7)
       deallocate(D2)
C
       call
     & sum1342(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U7, 1.000)
       deallocate(U7)
       deallocate(X5)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q30(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q30)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q32(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,Q30,B2,Q32)
       deallocate(B2)
C
       call sum21(N2,N3,N2,N3,X7,Q32,-1.000)
       deallocate(Q32)
C
       X7=X7+FBPP
C
       allocate(U9(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K2*K3
       I3=K4
       call EGEMM(I1,I2,I3,X7,t2B,U9)
C
       call
     & sum2341(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U9, 1.000)
       deallocate(U9)
       deallocate(X7)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q30,B1)
       allocate(Q31(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,B1,t1B,Q31)
       deallocate(B1)
C
       call sum21(N0,N2,N0,N2,X6,Q31, 1.000)
       deallocate(Q31)
C
       call sum21(N0,N2,N0,N2,X6,FBHH, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(U8(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K3*K4
       I3=K2
       call EGEMM(I1,I2,I3,X6,D2,U8)
       deallocate(D2)
C
       call
     & sum1243(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U8,-1.000)
       deallocate(U8)
       deallocate(X6)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder4132(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,N2,N0,N1,VBHHHP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(S56(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K2*K3
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S56)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N2,N1,N3,N0,N2,N0,N1,X2,S56,-1.000)
       deallocate(S56)
C
       X2=X2+VBHHPH
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U3(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,X2,B2,U3)
       deallocate(B2)
C
       V2B=V2B-U3
       deallocate(U3)
       deallocate(X2)
C
       allocate(D1(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N2,N3,N1,N3,N0,N2,VBPHPP,D1)
       allocate(U2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,U2)
       deallocate(D1)
C
       call
     & sum4123(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U2, 1.000)
       deallocate(U2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N2,N3,N1,N3,N0,N1,VBHPPP,D1)
       allocate(U4(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,U4)
       deallocate(D1)
C
       call
     & sum3124(N2,N3,N1,N3,N0,N2,N0,N1,V2B,U4, 1.000)
       deallocate(U4)
C
       end
