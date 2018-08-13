       subroutine L1B_update(N0,N1,N2,N3,V1B,
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
       real*8 V1B(N2+1:N3,N0+1:N2)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
C
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q40(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::U1(:,:)
       real*8,allocatable::U2(:,:)
       real*8,allocatable::U3(:,:)
       real*8,allocatable::U4(:,:)
       real*8,allocatable::U6(:,:)
       real*8,allocatable::U8(:,:)
       real*8,allocatable::U11(:,:)
       real*8,allocatable::U14(:,:)
       real*8,allocatable::U15(:,:)
       real*8,allocatable::U16(:,:)
       real*8,allocatable::U18(:,:)
       real*8,allocatable::U22(:,:)
       real*8,allocatable::U20(:,:)
       real*8,allocatable::U24(:,:)
       real*8,allocatable::U26(:,:)
       real*8,allocatable::U30(:,:)
       real*8,allocatable::U28(:,:)
       real*8,allocatable::U32(:,:)
       real*8,allocatable::U33(:,:)
       real*8,allocatable::U34(:,:)
       real*8,allocatable::U36(:,:)
       real*8,allocatable::U38(:,:)
       real*8,allocatable::U41(:,:)
       real*8,allocatable::U44(:,:)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call old1reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,l2A,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call old1reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(Q5(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q5)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call old1reorder2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N0+1:N1))
       call old1reorder21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q5,B2)
       allocate(U6(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K1*K1
       call EGEMM1(I1,I3,D1,B2,U6)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/2*U6
       deallocate(U6)
       deallocate(Q5)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call old1reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,l2A,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call old1reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q7(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q7)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U8(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K3
       call EGEMM1(I1,I3,D1,Q7,U8)
       deallocate(D1)
C
       V1B=V1B+1.0d0/2*U8
       deallocate(U8)
       deallocate(Q7)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call old1reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,l2A,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call old1reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,N1,t2A,D2)
       allocate(Q9(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q9)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call old1reorder21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q9,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call old1reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q10(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q10)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q9)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call old1reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q10,B2)
       allocate(U11(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,U11)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/2*U11
       deallocate(U11)
       deallocate(Q10)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call old1reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,l2A,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call old1reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,t2A,D2)
       allocate(Q12(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q12)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call old1reorder21(N1,N3,N1,N3,
     & N1,N3,N1,N3,Q12,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call old1reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(Q13(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q13)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q12)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U14(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call EGEMM1(I1,I3,D1,Q13,U14)
       deallocate(D1)
C
       V1B=V1B-1.0d0/2*U14
       deallocate(U14)
       deallocate(Q13)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call old1reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,l2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call old1reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,N1,t2B,D2)
       allocate(Q17(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q17)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N2+1:N3,N0+1:N2))
       call old1reorder2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N0,N1,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N0+1:N1))
       call old1reorder21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q17,B2)
       allocate(U18(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K1*K1
       call EGEMM1(I1,I3,D1,B2,U18)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-U18
       deallocate(U18)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call old1reorder21(N0,N1,N0,N1,
     & N0,N1,N0,N1,Q17,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call old1reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q21(N1+1:N3,N0+1:N1))
       I1=K1
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q21)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q17)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call old1reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,Q21,B2)
       allocate(U22(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,U22)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-U22
       deallocate(U22)
       deallocate(Q21)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call old1reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call old1reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,t2B,D2)
       allocate(Q19(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q19)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N1,N3,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U20(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K3
       call EGEMM1(I1,I3,D1,Q19,U20)
       deallocate(D1)
C
       V1B=V1B+U20
       deallocate(U20)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call old1reorder21(N1,N3,N1,N3,
     & N1,N3,N1,N3,Q19,B1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call old1reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,t1A,B2)
       allocate(Q23(N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q23)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q19)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder4213(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,IntM,D1)
       allocate(U24(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call EGEMM1(I1,I3,D1,Q23,U24)
       deallocate(D1)
C
       V1B=V1B-U24
       deallocate(U24)
       deallocate(Q23)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call old1reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,l2B,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call old1reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,t2B,D2)
       allocate(Q25(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q25)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call old1reorder1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N0+1:N2))
       call old1reorder21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q25,B2)
       allocate(U26(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K2*K2
       call EGEMM1(I1,I3,D1,B2,U26)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-U26
       deallocate(U26)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call old1reorder21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q25,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call old1reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q29(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q29)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q25)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call old1reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q29,B2)
       allocate(U30(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,U30)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-U30
       deallocate(U30)
       deallocate(Q29)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call old1reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call old1reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,t2B,D2)
       allocate(Q27(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q27)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N2+1:N3,N2+1:N3))
       call old1reorder21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q27,B2)
       allocate(U28(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K4
       call EGEMM1(I1,I3,D1,B2,U28)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B+U28
       deallocate(U28)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call old1reorder21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q27,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call old1reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q31(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q31)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q27)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U32(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call EGEMM1(I1,I3,D1,Q31,U32)
       deallocate(D1)
C
       V1B=V1B-U32
       deallocate(U32)
       deallocate(Q31)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,l2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(Q35(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q35)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call old1reorder1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N0+1:N2))
       call old1reorder21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q35,B2)
       allocate(U36(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K2*K2
       call EGEMM1(I1,I3,D1,B2,U36)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/2*U36
       deallocate(U36)
       deallocate(Q35)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call old1reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,l2C,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call old1reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(Q37(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q37)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N2,N3,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U38(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K4
       call EGEMM1(I1,I3,D1,Q37,U38)
       deallocate(D1)
C
       V1B=V1B+1.0d0/2*U38
       deallocate(U38)
       deallocate(Q37)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,l2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,t2C,D2)
       allocate(Q39(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q39)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call old1reorder21(N0,N2,N0,N2,
     & N0,N2,N0,N2,Q39,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call old1reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q40(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q40)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q39)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call old1reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,Q40,B2)
       allocate(U41(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,U41)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B-1.0d0/2*U41
       deallocate(U41)
       deallocate(Q40)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call old1reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,l2C,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call old1reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,t2C,D2)
       allocate(Q42(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q42)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call old1reorder21(N2,N3,N2,N3,
     & N2,N3,N2,N3,Q42,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call old1reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,t1B,B2)
       allocate(Q43(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q43)
       deallocate(B1)
       deallocate(B2)
       deallocate(Q42)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,IntB,D1)
       allocate(U44(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call EGEMM1(I1,I3,D1,Q43,U44)
       deallocate(D1)
C
       V1B=V1B-1.0d0/2*U44
       deallocate(U44)
       deallocate(Q43)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder2413(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N1,N3,N2,N3,N0,N2,H2B,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call old1reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,l1A,B2)
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
       allocate(B1(N0+1:N2,N0+1:N2))
       call old1reorder12(N0,N3,N0,N3,
     & N0,N2,N0,N2,H1B,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call old1reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,l1B,B2)
       allocate(U2(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,U2)
       deallocate(B1)
       deallocate(B2)
C
       V1B=V1B-U2
       deallocate(U2)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call old1reorder21(N0,N3,N0,N3,
     & N2,N3,N2,N3,H1B,B1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call old1reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,l1B,B2)
       allocate(U3(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,U3)
       deallocate(B1)
       deallocate(B2)
C
       call
     & old1sum21(N2,N3,N0,N2,V1B,U3,1.0d0)
       deallocate(U3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder1324(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N0,N2,H2C,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call old1reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,l1B,B2)
       allocate(U4(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,U4)
       deallocate(D1)
       deallocate(B2)
C
       V1B=V1B+U4
       deallocate(U4)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       call old1reorder1243(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N1,N1,N3,N0,N2,H2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call old1reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,N3,l2B,D2)
       allocate(U15(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,U15)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B-U15
       deallocate(U15)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:N3))
       call old1reorder2341(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N1,N2,N3,N1,N3,N2,N3,H2B,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call old1reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,N2,l2B,D2)
       allocate(U16(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,U16)
       deallocate(D1)
       deallocate(D2)
C
       call
     & old1sum21(N2,N3,N0,N2,V1B,U16,1.0d0)
       deallocate(U16)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call old1reorder1234(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N0,N2,N2,N3,N0,N2,H2C,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call old1reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,l2C,D2)
       allocate(U33(N2+1:N3,N0+1:N2))
       I1=K2
       I2=K4
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,U33)
       deallocate(D1)
       deallocate(D2)
C
       V1B=V1B-1.0d0/2*U33
       deallocate(U33)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call old1reorder1342(N0,N3,N0,N3,N0,N3,N0,N3,
     & N0,N2,N2,N3,N2,N3,N2,N3,H2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call old1reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,N2,l2C,D2)
       allocate(U34(N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,U34)
       deallocate(D1)
       deallocate(D2)
C
       call
     & old1sum21(N2,N3,N0,N2,V1B,U34,1.0d0/2)
       deallocate(U34)
C
       end
