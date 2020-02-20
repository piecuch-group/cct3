       subroutine t1B_update(N0,N1,N2,N3,V1B,shift,
     & K1,K2,K3,K4,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & t1A,t1B,t2A,t2B,t2C)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 CoeLeft,shift
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
       real*8 V1B(N2+1:N3,N0+1:N2)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::U1(:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::U2(:,:)
       real*8,allocatable::X2(:,:)
       real*8,allocatable::U3(:,:)
       real*8,allocatable::U4(:,:)
       real*8,allocatable::X3(:,:)
       real*8,allocatable::U5(:,:)
       real*8,allocatable::U6(:,:)
       real*8,allocatable::U7(:,:)
       real*8,allocatable::X4(:,:)
       real*8,allocatable::U8(:,:)
       real*8,allocatable::U9(:,:)
       real*8,allocatable::U10(:,:)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N0,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q11(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N2,N0+1:N2))
       X1=0.0d0
       X1=X1+Q11
       deallocate(Q11)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,N3,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q13(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N2+1:N3,N2+1:N3))
       X2=0.0d0
       X2=X2+Q13
       deallocate(Q13)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q15(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N1,N1+1:N3))
       X3=0.0d0
       X3=X3+Q15
       deallocate(Q15)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q17(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q17)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:N2,N2+1:N3))
       X4=0.0d0
       X4=X4+Q17
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q17,B1)
       allocate(Q37(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,B1,t1B,Q37)
       deallocate(B1)
C
       call sum21(N0,N2,N0,N2,X1,Q37, 1.000)
       deallocate(Q37)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q21(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q21)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1+Q21
       deallocate(Q21)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q23(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q23)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2-Q23
       deallocate(Q23)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(Q25(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q25)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N2,N3,N2,N3,X2,Q25,-1.000)
       deallocate(Q25)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(Q27(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q27)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N2,N0,N2,X1,Q27, 1.000)
       deallocate(Q27)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q29(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q29)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3+Q29
       deallocate(Q29)
C
       call sum21(N0,N1,N1,N3,X3,FAHP, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(U5(N2+1:N3,N0+1:N2))
       I2=K2*K4
       I3=K3*K1
       call EGEMM2(I2,I3,X3,D2,U5)
       deallocate(D2)
C
       V1B=V1B+U5
       deallocate(U5)
       deallocate(X3)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(Q31(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q31)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N2,N3,N2,N3,X2,Q31, 0.500)
       deallocate(Q31)
C
       X2=X2+FBPP
C
       allocate(U3(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,X2,t1B,U3)
C
       call
     & sum21(N2,N3,N0,N2,V1B,U3, 1.000)
       deallocate(U3)
       deallocate(X2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(Q33(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q33)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N2,N0,N2,X1,Q33, 0.500)
       deallocate(Q33)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q35(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q35)
       deallocate(D1)
       deallocate(B2)
C
       X4=X4+Q35
C
       call sum21(N0,N2,N2,N3,X4,FBHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U8(N2+1:N3,N0+1:N2))
       I2=K2*K4
       I3=K4*K2
       call EGEMM2(I2,I3,X4,D2,U8)
       deallocate(D2)
C
       V1B=V1B+U8
       deallocate(U8)
       deallocate(X4)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q35,B1)
       allocate(Q39(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,B1,t1B,Q39)
       deallocate(B1)
C
       call sum21(N0,N2,N0,N2,X1,Q39, 1.000)
       deallocate(Q39)
C
       allocate(Q19(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,FBHP,t1B,Q19)
C
       call sum21(N0,N2,N0,N2,X1,Q19, 1.000)
       deallocate(Q19)
C
       call sum21(N0,N2,N0,N2,X1,FBHH, 1.000)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U2(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,X1,B2,U2)
       deallocate(B2)
C
       V1B=V1B-U2
       deallocate(U2)
       deallocate(X1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,VBHPPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(U1(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,U1)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B+U1
       deallocate(U1)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U4(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,U4)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-U4
       deallocate(U4)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N0,N2,VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(U6(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,U6)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B-U6
       deallocate(U6)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,N3,N1,N3,N2,N3,VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(U7(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,U7)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,N3,N0,N2,V1B,U7, 1.000)
       deallocate(U7)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(U9(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,U9)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B-0.500*U9
       deallocate(U9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,N3,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U10(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,U10)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,N3,N0,N2,V1B,U10,-0.500)
       deallocate(U10)
C
       do i=N0+1,N2
       do a=N2+1,N3
         CoeLeft=FBPP(a,a)
     &          -FBHH(i,i)
     &          +shift
         t1B(a,i)=t1B(a,i)-V1B(a,i)/CoeLeft
       enddo
       enddo
C
       end
