       subroutine HBar2C0A(N0,N1,N2,N3,V2C0A,
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
       real*8 V2C0A(N2+1:N3,N2+1:N3)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::U1(:,:)
       real*8,allocatable::U2(:,:)
       real*8,allocatable::U3(:,:)
       real*8,allocatable::U6(:,:)
       real*8,allocatable::U7(:,:)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q4(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N2,N2+1:N3))
       X1=0.0d0
       X1=X1+Q4
       deallocate(Q4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q8(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1+Q8
       deallocate(Q8)
C
       call sumxold21(0,N3,N0,N2,N2,N3,X1,FockB, 1.000)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U1(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call DMATMAT(I1,I2,I3,X1,B2,U1)
       deallocate(B2)
C
       call
     & sum21(N2,N3,N2,N3,V2C0A,U1,-1.000)
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(U2(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,U2)
       deallocate(D1)
       deallocate(B2)
C
       V2C0A=V2C0A+U2
       deallocate(U2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(U3(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,U3)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,N3,N2,N3,V2C0A,U3,-1.000)
       deallocate(U3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U6(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,U6)
       deallocate(D1)
       deallocate(B2)
C
       V2C0A=V2C0A+U6
       deallocate(U6)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3421(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(U7(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,U7)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,N3,N2,N3,V2C0A,U7, 0.500)
       deallocate(U7)
C
       end
