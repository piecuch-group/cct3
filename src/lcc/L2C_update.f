       subroutine L2C_update(N0,N1,N2,N3,V2C,
     & K1,K2,K3,K4,
     & FockR,FockB,IntR,IntB,IntM,
     & H1A,H1B,H2A,H2B,H2C,
     & t1A,t1B,t2A,t2B,t2C,
     & l1A,l1B,l2A,l2B,l2C)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 FockR(N3,N3)
       real*8 FockB(N3,N3)
       real*8 IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H1A(N0+1:N3,N0+1:N3)
       real*8 H1B(N0+1:N3,N0+1:N3)
       real*8 H2A(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H2B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H2C(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 l1A(N1+1:N3,N0+1:N1)
       real*8 l1B(N2+1:N3,N0+1:N2)
       real*8 l2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 l2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 l2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
C
       real*8 V2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
C
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:)
       real*8,allocatable::U3(:,:,:,:)
       real*8,allocatable::U4(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:)
       real*8,allocatable::U8(:,:,:,:)
       real*8,allocatable::U9(:,:,:,:)
       real*8,allocatable::U10(:,:,:,:)
       real*8,allocatable::U11(:,:,:,:)
       real*8,allocatable::U12(:,:,:,:)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call old1reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call old1reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(Q1(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q1)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call old1reorder1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(U4(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K4
       call DMATMAT(I1,I2,I3,D1,Q1,U4)
       deallocate(D1)
C
       call
     & old1sum2134(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U4,1.0d0)
       V2C=V2C-U4
       deallocate(U4)
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call old1reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,l2B,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call old1reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(Q2(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,Q2)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U5(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K2
       call DMATMAT(I1,I2,I3,D1,Q2,U5)
       deallocate(D1)
C
       call
     & old1sum3124(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U5,-1.0d0)
       call
     & old1sum4123(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U5,1.0d0)
       deallocate(U5)
       deallocate(Q2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call old1reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,l2C,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call old1reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(Q3(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q3)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call old1reorder1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(U11(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K4
       call DMATMAT(I1,I2,I3,D1,Q3,U11)
       deallocate(D1)
C
       call
     & old1sum2134(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U11,1.0d0/2)
       V2C=V2C-1.0d0/2*U11
       deallocate(U11)
       deallocate(Q3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,l2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(Q4(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q4)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U12(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K2
       call DMATMAT(I1,I2,I3,D1,Q4,U12)
       deallocate(D1)
C
       call
     & old1sum3124(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U12,-1.0d0/2)
       call
     & old1sum4123(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U12,1.0d0/2)
       deallocate(U12)
       deallocate(Q4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call old1reorder1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,H2C,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call old1reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,l1B,B2)
       allocate(U1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,U1)
       deallocate(D1)
       deallocate(B2)
C
       call
     & old1sum2134(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U1,1.0d0)
       V2C=V2C-U1
       deallocate(U1)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,H2C,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call old1reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,l1B,B2)
       allocate(U2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,U2)
       deallocate(D1)
       deallocate(B2)
C
       call
     & old1sum3124(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U2,1.0d0)
       call
     & old1sum4123(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U2,-1.0d0)
       deallocate(U2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,H2B,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,l2B,D2)
       allocate(U3(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,U3)
       deallocate(D1)
       deallocate(D2)
C
       call
     & old1sum2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U3,-1.0d0)
       call
     & old1sum1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U3,1.0d0)
       call
     & old1sum2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U3,1.0d0)
       call
     & old1sum1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U3,-1.0d0)
       deallocate(U3)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call old1reorder12(N0,N3,N0,N3,
     & N0,N2,N0,N2,H1B,B1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,l2C,D2)
       allocate(U6(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K4*K4
       I3=K2
       call DMATMAT(I1,I2,I3,B1,D2,U6)
       deallocate(B1)
       deallocate(D2)
C
       V2C=V2C+U6
       call
     & old1sum1243(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U6,-1.0d0)
       deallocate(U6)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call old1reorder21(N0,N3,N0,N3,
     & N2,N3,N2,N3,H1B,B1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call old1reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,l2C,D2)
       allocate(U7(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K2*K4
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,U7)
       deallocate(B1)
       deallocate(D2)
C
       call
     & old1sum2341(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U7,1.0d0)
       call
     & old1sum1342(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U7,-1.0d0)
       deallocate(U7)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call old1reorder1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N2,N0,N2,H2C,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call old1reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,l2C,D2)
       allocate(U8(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,U8)
       deallocate(D1)
       deallocate(D2)
C
       V2C=V2C+1.0d0/2*U8
       deallocate(U8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,H2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,l2C,D2)
       allocate(U9(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,U9)
       deallocate(D1)
       deallocate(D2)
C
       call
     & old1sum2314(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U9,-1.0d0)
       call
     & old1sum1324(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U9,1.0d0)
       call
     & old1sum2413(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U9,1.0d0)
       call
     & old1sum1423(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U9,-1.0d0)
       deallocate(U9)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call old1reorder3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,H2C,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call old1reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,l2C,D2)
       allocate(U10(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2
       I3=K4*K4
       call DMATMAT(I1,I2,I3,D1,D2,U10)
       deallocate(D1)
       deallocate(D2)
C
       call
     & old1sum3412(N2,N3,N2,N3,N0,N2,N0,N2,V2C,U10,1.0d0/2)
       deallocate(U10)
C
       end
