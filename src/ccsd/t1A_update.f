       subroutine ccsd_t1A_update(N0,N1,N2,N3,V1A,shift,
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
       real*8 V1A(N1+1:N3,N0+1:N1)
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
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::U1(:,:)
       real*8,allocatable::X2(:,:)
       real*8,allocatable::U2(:,:)
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
       allocate(Q11(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,FAHP,t1A,Q11)
C
       allocate(X1(N0+1:N1,N0+1:N1))
       X1=0.0d0
       call sum21(N0,N1,N0,N1,X1,Q11, 1.000)
       deallocate(Q11)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q13(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q13)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1+Q13
       deallocate(Q13)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q15(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N1+1:N3,N1+1:N3))
       X2=0.0d0
       X2=X2-Q15
       deallocate(Q15)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,N1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q17(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q17)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1+Q17
       deallocate(Q17)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q19(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q19)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2+Q19
       deallocate(Q19)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q21(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q21)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N1,N3,N1,N3,X2,Q21, 0.500)
       deallocate(Q21)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(Q23(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q23)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N1,N0,N1,X1,Q23, 0.500)
       deallocate(Q23)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q25(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N1,N1+1:N3))
       X3=0.0d0
       X3=X3+Q25
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q25,B1)
       allocate(Q37(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,B1,t1A,Q37)
       deallocate(B1)
C
       call sum21(N0,N1,N0,N1,X1,Q37, 1.000)
       deallocate(Q37)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(Q29(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q29)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N0,N1,N0,N1,X1,Q29, 1.000)
       deallocate(Q29)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q31(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:N2,N2+1:N3))
       X4=0.0d0
       X4=X4+Q31
       deallocate(Q31)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q33(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q33)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3+Q33
C
       call sum21(N0,N1,N1,N3,X3,FAHP, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U5(N1+1:N3,N0+1:N1))
       I2=K1*K3
       I3=K3*K1
       call EGEMM2(I2,I3,X3,D2,U5)
       deallocate(D2)
C
       V1A=V1A+U5
       deallocate(U5)
       deallocate(X3)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q33,B1)
       allocate(Q39(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,B1,t1A,Q39)
       deallocate(B1)
C
       call sum21(N0,N1,N0,N1,X1,Q39, 1.000)
       deallocate(Q39)
C
       call sum21(N0,N1,N0,N1,X1,FAHH, 1.000)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(U1(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,X1,B2,U1)
       deallocate(B2)
C
       V1A=V1A-U1
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(Q27(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q27)
       deallocate(D1)
       deallocate(D2)
C
       call sum21(N1,N3,N1,N3,X2,Q27,-1.000)
       deallocate(Q27)
C
       X2=X2+FAPP
C
       allocate(U2(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,X2,t1A,U2)
C
       call
     & sum21(N1,N3,N0,N1,V1A,U2, 1.000)
       deallocate(U2)
       deallocate(X2)
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
       deallocate(Q35)
C
       call sum21(N0,N2,N2,N3,X4,FBHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(U8(N1+1:N3,N0+1:N1))
       I2=K1*K3
       I3=K4*K2
       call EGEMM2(I2,I3,X4,D2,U8)
       deallocate(D2)
C
       V1A=V1A+U8
       deallocate(U8)
       deallocate(X4)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(U3(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,U3)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A-U3
       deallocate(U3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,VBHPPH,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U4(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,U4)
       deallocate(D1)
       deallocate(B2)
C
       V1A=V1A+U4
       deallocate(U4)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(U6(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,U6)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-0.500*U6
       deallocate(U6)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,N3,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U7(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,U7)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N1,N3,N0,N1,V1A,U7,-0.500)
       deallocate(U7)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,N1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(U9(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,U9)
       deallocate(D1)
       deallocate(D2)
C
       V1A=V1A-U9
       deallocate(U9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,N3,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(U10(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,U10)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N1,N3,N0,N1,V1A,U10, 1.000)
       deallocate(U10)
C
       do i=N0+1,N1
       do a=N1+1,N3
         CoeLeft=FAPP(a,a)
     &          -FAHH(i,i)
     &          +shift
         t1A(a,i)=t1A(a,i)-V1A(a,i)/CoeLeft
       enddo
       enddo
C
       end
