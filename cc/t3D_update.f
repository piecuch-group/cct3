       subroutine t3D_update(N0,N1,N2,N3,V3D,
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
       real*8 V3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::S1(:,:,:,:)
       real*8,allocatable::S2(:,:,:,:)
       real*8,allocatable::S3(:,:,:,:)
       real*8,allocatable::S4(:,:,:,:)
       real*8,allocatable::S5(:,:,:,:)
       real*8,allocatable::S6(:,:,:,:)
       real*8,allocatable::S7(:,:,:,:)
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::S9(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U1(:,:,:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U4(:,:,:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U10(:,:,:,:,:,:)
       real*8,allocatable::U11(:,:,:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U12(:,:,:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::U15(:,:,:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U16(:,:,:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::U29(:,:,:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::U30(:,:,:,:,:,:)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X1=0.0d0
       call sum2134(N0,N2,N2,N3,N0,N2,N0,N2,X1,S1, 1.000)
       deallocate(S1)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X3=0.0d0
       call sum3124(N0,N2,N2,N3,N0,N2,N0,N2,X3,S2, 1.000)
       deallocate(S2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3142(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S3(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X4=0.0d0
       call sum3124(N2,N3,N2,N3,N2,N3,N0,N2,X4,S3, 1.000)
       deallocate(S3)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S4(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4*K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X2=0.0d0
       call sum4123(N2,N3,N2,N3,N2,N3,N0,N2,X2,S4,-1.000)
       deallocate(S4)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S5(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S5)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N2,N2,N3,N0,N2,N0,N2,X3,S5,-1.000)
       deallocate(S5)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S6(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S6)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N2,N3,N2,N3,N2,N3,N0,N2,X4,S6, 1.000)
       deallocate(S6)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder12(0,N3,0,N3,
     & N2,N3,N0,N2,FockB,B1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S7(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S7)
       deallocate(B1)
       deallocate(D2)
C
       call sum2341(N0,N2,N2,N3,N0,N2,N0,N2,X1,S7, 1.000)
       deallocate(S7)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder3412(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S8(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K4
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S8)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X5(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X5=0.0d0
       call sum2314(N2,N3,N2,N3,N2,N3,N0,N2,X5,S8, 1.000)
       deallocate(S8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder4132(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S9(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S9)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder3124(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,N2,S9,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U11(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,D2,U11)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.000)
       call
     & sum135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11, 1.000)
       call
     & sum234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11, 1.000)
       call
     & sum235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.000)
       call
     & sum134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.000)
       call
     & sum124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11, 1.000)
       call
     & sum126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11, 1.000)
       call
     & sum136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.000)
       call
     & sum234165(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.000)
       call
     & sum236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11, 1.000)
       call
     & sum134265(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11, 1.000)
       call
     & sum124365(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.000)
       call
     & sum126354(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.000)
       call
     & sum136254(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11, 1.000)
       call
     & sum235164(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11, 1.000)
       call
     & sum236154(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.000)
       call
     & sum135264(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11,-1.000)
       call
     & sum125364(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U11, 1.000)
       deallocate(U11)
       deallocate(S9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder3214(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S10(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S10)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X6(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X6=0.0d0
       call sum3412(N2,N3,N2,N3,N2,N3,N0,N2,X6,S10, 1.000)
       deallocate(S10)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S11(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K2
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S11)
       deallocate(D1)
       deallocate(D2)
C
       call sum3412(N0,N2,N2,N3,N0,N2,N0,N2,X1,S11, 0.500)
       deallocate(S11)
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
       call EGEMM1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q1,B1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S12(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S12)
       deallocate(B1)
       deallocate(D2)
C
       call sum2341(N0,N2,N2,N3,N0,N2,N0,N2,X1,S12, 1.000)
       deallocate(S12)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S13(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder3214(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S13,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S15(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X8=0.0d0
       call sum2134(N0,N2,N2,N3,N0,N2,N0,N2,X8,S15, 1.000)
       deallocate(S15)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder2314(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S13,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S14(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X7=0.0d0
       call sum2134(N0,N2,N2,N3,N0,N2,N0,N2,X7,S14, 1.000)
       deallocate(S14)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder4312(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S16(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder2314(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S16,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S17(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N2,N3,N2,N3,N2,N3,N0,N2,X2,S17, 1.000)
       deallocate(S17)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder2314(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N2,N3,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S18(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder2341(N0,N2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S18,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S20(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N2,N3,N2,N3,N2,N3,N0,N2,X4,S20, 1.000)
       deallocate(S20)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder3241(N0,N2,N0,N2,N2,N3,N2,N3,
     & N2,N3,N0,N2,N2,N3,N0,N2,S18,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S19(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       call sum3124(N0,N2,N2,N3,N0,N2,N0,N2,X1,S19, 1.000)
       deallocate(S19)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N1,N1,N3,IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S21(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder3421(N0,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,N2,S21,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S22(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S22)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N2,N2,N3,N0,N2,N0,N2,X3,S22,-1.000)
       deallocate(S22)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U4(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,X3,D2,U4)
       deallocate(D2)
C
       call
     & sum234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4, 1.000)
       call
     & sum134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,-1.000)
       call
     & sum124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4, 1.000)
       call
     & sum235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,-1.000)
       call
     & sum135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4, 1.000)
       call
     & sum125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,-1.000)
       call
     & sum234165(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,-1.000)
       call
     & sum134265(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4, 1.000)
       call
     & sum124365(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,-1.000)
       call
     & sum235164(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4, 1.000)
       call
     & sum135264(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,-1.000)
       call
     & sum125364(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4, 1.000)
       call
     & sum236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4, 1.000)
       call
     & sum136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,-1.000)
       call
     & sum126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4, 1.000)
       call
     & sum236154(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,-1.000)
       call
     & sum136254(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4, 1.000)
       call
     & sum126354(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U4,-1.000)
       deallocate(U4)
       deallocate(X3)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N0,N2,N2,N3,IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,N3,N0,N2,t2B,D2)
       allocate(S23(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S23)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S23,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S24(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N2,N3,N2,N3,N2,N3,N0,N2,X4,S24,-1.000)
       deallocate(S24)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U5(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,X4,D2,U5)
       deallocate(D2)
C
       call
     & sum345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.000)
       call
     & sum245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5, 1.000)
       call
     & sum345216(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5, 1.000)
       call
     & sum245316(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.000)
       call
     & sum145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.000)
       call
     & sum145326(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5, 1.000)
       call
     & sum346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5, 1.000)
       call
     & sum246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.000)
       call
     & sum346215(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.000)
       call
     & sum246315(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5, 1.000)
       call
     & sum146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5, 1.000)
       call
     & sum146325(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.000)
       call
     & sum356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.000)
       call
     & sum256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5, 1.000)
       call
     & sum356214(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5, 1.000)
       call
     & sum256314(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.000)
       call
     & sum156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5,-1.000)
       call
     & sum156324(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U5, 1.000)
       deallocate(U5)
       deallocate(X4)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N0,N2,N0,N2,N2,N3,IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S25(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder3421(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S25,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S27(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S27)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N2,N2,N3,N0,N2,N0,N2,X8,S27, 1.000)
       deallocate(S27)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U16(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,X8,D2,U16)
       deallocate(D2)
C
       call
     & sum124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U16,-1.000)
       call
     & sum125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U16, 1.000)
       call
     & sum124365(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U16, 1.000)
       call
     & sum125364(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U16,-1.000)
       call
     & sum126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U16,-1.000)
       call
     & sum126354(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U16, 1.000)
       deallocate(U16)
       deallocate(X8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder2431(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,N2,S25,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S28(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S28)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N0,N2,N2,N3,N0,N2,N0,N2,X7,S28, 1.000)
       deallocate(S28)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U15(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,X7,D2,U15)
       deallocate(D2)
C
       call
     & sum234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15, 1.000)
       call
     & sum134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15,-1.000)
       call
     & sum235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15,-1.000)
       call
     & sum135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15, 1.000)
       call
     & sum234165(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15,-1.000)
       call
     & sum134265(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15, 1.000)
       call
     & sum235164(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15, 1.000)
       call
     & sum135264(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15,-1.000)
       call
     & sum236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15, 1.000)
       call
     & sum136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15,-1.000)
       call
     & sum236154(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15,-1.000)
       call
     & sum136254(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U15, 1.000)
       deallocate(U15)
       deallocate(X7)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder4231(N0,N2,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N0,N2,N0,N2,S25,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(S35(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder2314(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S35,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S36(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X9=0.0d0
       call sum2134(N0,N2,N2,N3,N0,N2,N0,N2,X9,S36, 1.000)
       deallocate(S36)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder3241(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S25,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S38(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder2314(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,S38,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S39(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N2,N3,N2,N3,N2,N3,N0,N2,X2,S39,-1.000)
       deallocate(S39)
C
       call sumxold1423(N0,N3,N2,N3,N2,N3,N2,N3,N0,N2,X2,IntB, 1.000)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U2(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,X2,D2,U2)
       deallocate(D2)
C
       call
     & sum345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,-1.000)
       call
     & sum245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2, 1.000)
       call
     & sum145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,-1.000)
       call
     & sum346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2, 1.000)
       call
     & sum246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,-1.000)
       call
     & sum146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2, 1.000)
       call
     & sum356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,-1.000)
       call
     & sum256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2, 1.000)
       call
     & sum156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U2,-1.000)
       deallocate(U2)
       deallocate(X2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder2341(N0,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,S25,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(S26(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K4
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S26)
       deallocate(D1)
       deallocate(D2)
C
       call sum2314(N2,N3,N2,N3,N2,N3,N0,N2,X5,S26,-1.000)
       deallocate(S26)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U10(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,X5,D2,U10)
       deallocate(D2)
C
       call
     & sum145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,-0.500)
       call
     & sum245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10, 0.500)
       call
     & sum345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,-0.500)
       call
     & sum146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10, 0.500)
       call
     & sum246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,-0.500)
       call
     & sum346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10, 0.500)
       call
     & sum156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,-0.500)
       call
     & sum256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10, 0.500)
       call
     & sum356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U10,-0.500)
       deallocate(U10)
       deallocate(X5)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder3214(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S35,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S37(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X10=0.0d0
       call sum2134(N0,N2,N2,N3,N0,N2,N0,N2,X10,S37, 1.000)
       deallocate(S37)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N0,N2,N0,N2,IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S31(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S31)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S31,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S32(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N2,N2,N3,N0,N2,N0,N2,X9,S32,-0.500)
       deallocate(S32)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U29(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,X9,D2,U29)
       deallocate(D2)
C
       call
     & sum234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U29,-1.000)
       call
     & sum134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U29, 1.000)
       call
     & sum235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U29, 1.000)
       call
     & sum135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U29,-1.000)
       call
     & sum236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U29,-1.000)
       call
     & sum136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U29, 1.000)
       deallocate(U29)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder4312(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,N2,N0,N2,S31,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S34(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N0,N2,N2,N3,N0,N2,N0,N2,X10,S34,-0.500)
       deallocate(S34)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U30(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,X10,D2,U30)
       deallocate(D2)
C
       call
     & sum124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U30, 1.000)
       call
     & sum125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U30,-1.000)
       call
     & sum126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U30, 1.000)
       deallocate(U30)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N0,N2,N2,N3,IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(S29(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S29)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,S29,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(S30(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       call sum2134(N2,N3,N2,N3,N2,N3,N0,N2,X6,S30, 1.000)
       deallocate(S30)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(U12(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,X6,D2,U12)
       deallocate(D2)
C
       call
     & sum256134(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.000)
       call
     & sum356124(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.000)
       call
     & sum156234(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.000)
       call
     & sum156324(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.000)
       call
     & sum356214(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.000)
       call
     & sum256314(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.000)
       call
     & sum246135(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.000)
       call
     & sum346125(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.000)
       call
     & sum146235(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.000)
       call
     & sum146325(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.000)
       call
     & sum346215(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.000)
       call
     & sum246315(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.000)
       call
     & sum345126(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.000)
       call
     & sum245136(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.000)
       call
     & sum345216(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.000)
       call
     & sum245316(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.000)
       call
     & sum145236(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12, 1.000)
       call
     & sum145326(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U12,-1.000)
       deallocate(U12)
       deallocate(X6)
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
       call EGEMM1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q2,B1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,t2C,D2)
       allocate(S33(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S33)
       deallocate(B1)
       deallocate(D2)
       deallocate(Q2)
C
       call sum2341(N0,N2,N2,N3,N0,N2,N0,N2,X1,S33, 1.000)
       deallocate(S33)
C
       call sumxold3412(N0,N3,N0,N2,N2,N3,N0,N2,N0,N2,X1,IntB, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(U1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,X1,D2,U1)
       deallocate(D2)
C
       call
     & sum234156(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1, 1.000)
       call
     & sum134256(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,-1.000)
       call
     & sum124356(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1, 1.000)
       call
     & sum235146(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,-1.000)
       call
     & sum135246(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1, 1.000)
       call
     & sum125346(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,-1.000)
       call
     & sum236145(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1, 1.000)
       call
     & sum136245(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1,-1.000)
       call
     & sum126345(N2,N3,N2,N3,N2,N3,N0,N2,N0,N2,N0,N2,V3D,U1, 1.000)
       deallocate(U1)
       deallocate(X1)
C
       end
