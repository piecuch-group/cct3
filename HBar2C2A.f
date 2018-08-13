       subroutine HBar2C2A(N0,N1,N2,N3,V2C2A,
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
       real*8 V2C2A(N2+1:N3,N2+1:N3,N0+1:N1,N0+1:N1)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::S4(:,:,:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:)
       real*8,allocatable::U3(:,:,:,:)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       call reorder2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N0,N2,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(S4(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       X1=0.0d0
       call sum4123(N0,N2,N2,N3,N0,N1,N0,N1,X1,S4, 1.000)
       deallocate(S4)
C
       call sumxold2413(N0,N3,N0,N2,N2,N3,N0,N1,N0,N1,X1,IntM, 1.000)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(U2(N2+1:N3,N2+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,X1,B2,U2)
       deallocate(B2)
C
       V2C2A=V2C2A-U2
       deallocate(U2)
       deallocate(X1)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:N3,N0+1:N1))
       call reorder2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N2,N3,N2,N3,N0,N1,IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(U1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       I1=K1*K4*K4
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,U1)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,N2,N3,N0,N1,N0,N1,V2C2A,U1, 1.000)
       deallocate(U1)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N1,N3,N2,N3,N0,N1,IntM,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,N3,N0,N1,t2B,D2)
       allocate(U3(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K1*K4
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,U3)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum1423(N2,N3,N2,N3,N0,N1,N0,N1,V2C2A,U3,-1.000)
       deallocate(U3)
C
       end
