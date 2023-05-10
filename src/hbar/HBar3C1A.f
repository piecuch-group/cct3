       subroutine HBar3C1A(N0,N1,N2,N3,V3C1A,
     & K1,K2,K3,K4,
     & FockR,FockB,IntR,IntB,IntM,
     & t1A,t1B,t2A,t2B,t2C)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 FockR(N3,N3)
       real*8 FockB(N3,N3)
       real*8 IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
C
       real*8 V3C1A(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:)
       real*8,allocatable::U3(:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U4(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:)
       real*8,allocatable::U23(:,:,:,:)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       call reorder2431(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N2,N3,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S8(N0+1:N1,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4*K1
       I2=K1
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       X2=0.0d0
       call sum4123(N0,N1,N2,N3,N2,N3,N0,N1,X2,S8, 1.000)
       deallocate(S8)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N2,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S10(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K3
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       X3=0.0d0
       call sum3124(N0,N2,N2,N3,N1,N3,N0,N1,X3,S10,-1.000)
       deallocate(S10)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       call reorder2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S12(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K1
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       call sum4123(N0,N2,N2,N3,N1,N3,N0,N1,X3,S12, 1.000)
       deallocate(S12)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N1,N2,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S14(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       X4=0.0d0
       call sum4123(N0,N2,N0,N1,N2,N3,N0,N1,X4,S14, 1.000)
C
       call sumxold3412(N0,N3,N0,N2,N0,N1,N2,N3,N0,N1,X4,IntM, 1.000)
C
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(U6(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K3*K4
       I3=K1*K2
       call DMATMAT(I1,I2,I3,X4,D2,U6)
       deallocate(D2)
C
       call
     & sum1324(N2,N3,N2,N3,N1,N3,N0,N1,V3C1A,U6, 1.000)
       deallocate(U6)
       deallocate(X4)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       call reorder3241(N0,N1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N0,N2,N2,N3,N0,N1,S14,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(S21(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K3
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N2,N2,N3,N1,N3,N0,N1,X3,S21,-1.000)
       deallocate(S21)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q1(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N2,N2+1:N3))
       X1=0.0d0
       X1=X1+Q1
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(S19(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S19)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N2,N3,N1,N3,N0,N1,X3,S19, 1.000)
       deallocate(S19)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N2+1:N3))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N0,N1,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,t2B,D2)
       allocate(S16(N2+1:N3,N0+1:N1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K1*K4
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S16)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N1,N2,N3,N2,N3,N0,N1,X2,S16,-1.000)
       deallocate(S16)
C
       call sumxold3421(N0,N3,N0,N1,N2,N3,N2,N3,N0,N1,X2,IntM, 1.000)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(U2(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K3
       I3=K1
       call DMATMAT(I1,I2,I3,X2,B2,U2)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,N3,N1,N3,N0,N1,V3C1A,U2,-1.000)
       deallocate(U2)
       deallocate(X2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(S24(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S24)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N2,N3,N1,N3,N0,N1,X3,S24, 1.000)
       deallocate(S24)
C
       call sumxold2413(N0,N3,N0,N2,N2,N3,N1,N3,N0,N1,X3,IntM, 1.000)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U4(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K4
       I3=K2
       call DMATMAT(I1,I2,I3,X3,B2,U4)
       deallocate(B2)
C
       V3C1A=V3C1A-U4
       deallocate(U4)
       deallocate(X3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q2(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1+Q2
       deallocate(Q2)
C
       call sumxold21(0,N3,N0,N2,N2,N3,X1,FockB, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(U1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K3*K4
       I3=K2
       call DMATMAT(I1,I2,I3,X1,D2,U1)
       deallocate(D2)
C
       call
     & sum1342(N2,N3,N2,N3,N1,N3,N0,N1,V3C1A,U1,-1.000)
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:N3,N1+1:N3))
       call reorder2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N2,N3,N1,N3,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(U3(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       I1=K3*K4*K4
       I2=K1
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,U3)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,N2,N3,N1,N3,N0,N1,V3C1A,U3, 1.000)
       deallocate(U3)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(U5(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K1*K3
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,U5)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,N3,N1,N3,N0,N1,V3C1A,U5, 1.000)
       deallocate(U5)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:N3))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N2,N3,N1,N3,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,t2B,D2)
       allocate(U7(N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K4
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,U7)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum1423(N2,N3,N2,N3,N1,N3,N0,N1,V3C1A,U7,-1.000)
       deallocate(U7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder3241(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(U23(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K1*K3
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,U23)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,N3,N1,N3,N0,N1,V3C1A,U23,-1.000)
       deallocate(U23)
C
       end
