       subroutine HBar1B3B(N0,N1,N2,N3,V1B3B,
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
       real*8 V1B3B(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::S3(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U8(:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U9(:,:,:,:)
       real*8,allocatable::U10(:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U11(:,:,:,:)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N1,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S3(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X2=0.0d0
       call sum4123(N0,N1,N1,N3,N0,N1,N0,N1,X2,S3, 1.000)
       deallocate(S3)
C
       call sumxold2413(N0,N3,N0,N1,N1,N3,N0,N1,N0,N1,X2,IntR, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(U2(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K3*K1
       call DMATMAT(I1,I2,I3,X2,D2,U2)
       deallocate(D2)
C
       V1B3B=V1B3B+U2
       deallocate(U2)
       deallocate(X2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N1,N3,N0,N1,IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q1(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N1+1:N3,N0+1:N1))
       X1=0.0d0
       X1=X1+Q1
       deallocate(Q1)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call reorder2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N0,N2,N0,N1,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S12(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K1
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       X3=0.0d0
       call sum4123(N0,N2,N0,N2,N0,N1,N0,N1,X3,S12, 1.000)
       deallocate(S12)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       call reorder2134(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S14(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K1
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N2+1:N3,N2+1:N3,N0+1:N1,N0+1:N1))
       X4=0.0d0
       call sum4123(N2,N3,N2,N3,N0,N1,N0,N1,X4,S14, 1.000)
       deallocate(S14)
C
       call sumxold1423(N0,N3,N2,N3,N2,N3,N0,N1,N0,N1,X4,IntM, 1.000)
C
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(U8(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K4
       I2=K2
       I3=K4
       call DMATMAT(I1,I2,I3,X4,B2,U8)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N0,N2,N0,N1,N0,N1,V1B3B,U8, 1.000)
       deallocate(U8)
       deallocate(X4)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       call reorder2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S16(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K1
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       X6=0.0d0
       call sum4123(N0,N2,N2,N3,N0,N1,N0,N1,X6,S16, 1.000)
C
       call sumxold2413(N0,N3,N0,N2,N2,N3,N0,N1,N0,N1,X6,IntM, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U11(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K2*K4
       I3=K4*K2
       call DMATMAT(I1,I2,I3,X6,D2,U11)
       deallocate(D2)
C
       V1B3B=V1B3B+U11
       deallocate(U11)
       deallocate(X6)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call reorder3241(N0,N1,N0,N2,N2,N3,N0,N1,
     & N2,N3,N0,N2,N0,N1,N0,N1,S16,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S25(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N2,N0,N2,N0,N1,N0,N1,X3,S25, 1.000)
       deallocate(S25)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       call reorder1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S20(N0+1:N2,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3*K2
       I2=K2
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       X5=0.0d0
       call sum3124(N0,N2,N1,N3,N0,N2,N0,N1,X5,S20, 1.000)
       deallocate(S20)
C
       call sumxold3214(N0,N3,N0,N2,N1,N3,N0,N2,N0,N1,X5,IntM, 1.000)
C
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,t2B,D2)
       allocate(U9(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K4
       I3=K3*K2
       call DMATMAT(I1,I2,I3,X5,D2,U9)
       deallocate(D2)
C
       call
     & sum1423(N2,N3,N0,N2,N0,N1,N0,N1,V1B3B,U9,-1.000)
       deallocate(U9)
       deallocate(X5)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N0,N2,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(S22(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K2
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S22)
       deallocate(D1)
       deallocate(D2)
C
       call sum2413(N0,N2,N0,N2,N0,N1,N0,N1,X3,S22, 1.000)
       deallocate(S22)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N1,N3,N0,N1,IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q2(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1+Q2
       deallocate(Q2)
C
       call sumxold12(0,N3,N1,N3,N0,N1,X1,FockR, 1.000)
C
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,N3,N0,N2,N0,N1,t2B,D2)
       allocate(U1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K2*K4
       I3=K3
       call DMATMAT(I1,I2,I3,X1,D2,U1)
       deallocate(D2)
C
       call
     & sum1243(N2,N3,N0,N2,N0,N1,N0,N1,V1B3B,U1, 1.000)
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       call reorder1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N0,N1,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S18(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N2,N0,N2,N0,N1,N0,N1,X3,S18, 1.000)
       deallocate(S18)
C
       call sumxold2413(N0,N3,N0,N2,N0,N2,N0,N1,N0,N1,X3,IntM, 1.000)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U7(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K4
       I3=K2
       call DMATMAT(I1,I2,I3,X3,B2,U7)
       deallocate(B2)
C
       V1B3B=V1B3B-U7
       deallocate(U7)
       deallocate(X3)
C
       allocate(D1(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N0,N2,N0,N1,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(U6(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K1
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,U6)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,N0,N2,N0,N1,N0,N1,V1B3B,U6, 1.000)
       deallocate(U6)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1))
       call reorder1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N1,N3,N2,N3,N0,N1,IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,t2B,D2)
       allocate(U10(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K1*K2
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,U10)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N0,N2,N0,N1,N0,N1,V1B3B,U10, 1.000)
       deallocate(U10)
C
       end
