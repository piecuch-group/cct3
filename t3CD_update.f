       subroutine t3CD_update(N0,N1,N2,N3,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,iPC,
     & FAHH,FAPP,FBHH,FBPP,
     & t1A,t1B,t2A,t2B,t2C,
     & t3C2,t3C3,t3D,
     & V3C2,V3C3,V3D)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 CoeLeft,shift,PP
       real*8 FAHH(N0+1:N1,N0+1:N1)
       real*8 FAPP(N1+1:N3,N1+1:N3)
       real*8 FBHH(N0+1:N2,N0+1:N2)
       real*8 FBPP(N2+1:N3,N2+1:N3)
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 t3C2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1)
       real*8 t3C3(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:N2,M1+1:N2,N0+1:N1)
       real*8 t3D(N2+1:N3,N2+1:N3,N2+1:M2,N0+1:N2,N0+1:N2,M1+1:N2)
       real*8 V3C2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1)
       real*8 V3C3(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:N2,M1+1:N2,N0+1:N1)
       real*8 V3D(N2+1:N3,N2+1:N3,N2+1:M2,N0+1:N2,N0+1:N2,M1+1:N2)
C
       real*8,allocatable::VCPPP(:,:,:)
       real*8,allocatable::tt3C2(:,:,:)
       real*8,allocatable::tt3C3(:,:,:)
       real*8,allocatable::tt3D(:,:,:)
       real*8,allocatable::U(:,:)
       real*8,allocatable::C1(:,:,:)
C
       N3C2=0
       do I=M1+1,N1;do j=N0+2,M1;do k=N0+1,j-1;do A=N1+1,M2
        N3C2=N3C2+1
       enddo;enddo;enddo;enddo
       allocate(tt3C2(N2+1:N3,N2+1:N3,N3C2))
       tt3C2=0.0d0
       I3C2=0 
       do I=M1+1,N1;do j=N0+2,M1;do k=N0+1,j-1;do A=N1+1,M2
        I3C2=I3C2+1
        do g=M2+1,N3;do h=M2+1,N3
         tt3C2(h,g,I3C2)=0.5d0*t3C2(h,g,A,k,j,I)
        enddo;enddo
        do e=N2+1,N3;do f=N2+1,N3
         tt3C2(f,e,I3C2)=tt3C2(f,e,I3C2)-t1B(f,j)*t2B(e,A,k,I)
     &                                  +t1B(f,k)*t2B(e,A,j,I)
        enddo;enddo
       enddo;enddo;enddo;enddo
C
       N3C3=0
       do i=N0+1,N1;do J=M1+1,N2;do k=N0+1,N2;do A=N1+1,M2
        N3C3=N3C3+1
       enddo;enddo;enddo;enddo
       allocate(tt3C3(N2+1:N3,N2+1:N3,N3C3))
       tt3C3=0.0d0
       I3C3=0 
       do i=N0+1,N1;do J=M1+1,N2;do k=N0+1,N2;do A=N1+1,M2
        I3C3=I3C3+1
        do g=M2+1,N3;do h=M2+1,N3
         tt3C3(h,g,I3C3)=0.5d0*t3C3(h,g,A,k,J,i)
        enddo;enddo
        do e=N2+1,N3;do f=N2+1,N3
         tt3C3(f,e,I3C3)=tt3C3(f,e,I3C3)-t1B(f,J)*t2B(e,A,k,i)
     &                                  +t1B(f,k)*t2B(e,A,J,i)
        enddo;enddo
       enddo;enddo;enddo;enddo
C
       N3D=0
       do I=M1+1,N2;do j=N0+2,N2;do k=N0+1,j-1;do A=N2+1,M2
        N3D=N3D+1
       enddo;enddo;enddo;enddo
       allocate(tt3D(N2+1:N3,N2+1:N3,N3D))
       tt3D=0.0d0
       I3D=0
       do I=M1+1,N2;do j=N0+2,N2;do k=N0+1,j-1;do A=N2+1,M2
        I3D=I3D+1
        do g=M2+1,N3;do h=M2+1,N3
         tt3D(h,g,I3D)=0.5d0*t3D(h,g,A,k,j,I)
        enddo;enddo
        do e=N2+1,N3;do f=N2+1,N3
         tt3D(f,e,I3D)=tt3D(f,e,I3D)+t1B(f,I)*t2C(e,A,k,j)
     &                              -t1B(f,j)*t2C(e,A,k,I)
     &                              +t1B(f,k)*t2C(e,A,j,I)
        enddo;enddo
       enddo;enddo;enddo;enddo
C
       allocate(VCPPP(N2+1:N3,N2+1:N3,N2+1:N3))
       do 3000 b=M2+1,N3-1
        read(iPC,rec=b)VCPPP
        allocate(C1(N2+1:N3,N2+1:N3,b+1:N3))
        do c=b+1,N3;do e=N2+1,N3;do f=N2+1,N3
         C1(f,e,c)=VCPPP(f,e,c)
        enddo;enddo;enddo
        I2=N3-b
        I3=K4*K4
C
        I1=N3C2
        allocate(U(b+1:N3,N3C2))
        call EGEMM(I1,I2,I3,tt3C2,C1,U)
!       call EGEMM(I1,I2,I3,tt3C2,VCPPP(1,1,b+1),U)
        I3C2=0
        do I=M1+1,N1;do j=N0+2,M1;do k=N0+1,j-1;do A=N1+1,M2
         I3C2=I3C2+1
         do c=b+1,N3
          V3C2(c,b,A,k,j,I)=V3C2(c,b,A,k,j,I)+U(c,I3C2)
         enddo
        enddo;enddo;enddo;enddo
        deallocate(U)
C
        I1=N3C3
        allocate(U(b+1:N3,N3C3))
!       call EGEMM(I1,I2,I3,tt3C3,VCPPP(1,1,b+1),U)
        call EGEMM(I1,I2,I3,tt3C3,C1,U)
        I3C3=0
        do i=N0+1,N1;do J=M1+1,N2;do k=N0+1,N2;do A=N1+1,M2
         I3C3=I3C3+1
         do c=b+1,N3
          V3C3(c,b,A,k,J,i)=V3C3(c,b,A,k,J,i)+U(c,I3C3)
         enddo
        enddo;enddo;enddo;enddo
        deallocate(U)
C
        I1=N3D
        allocate(U(b+1:N3,N3D))
!       call EGEMM(I1,I2,I3,tt3D,VCPPP(1,1,b+1),U)
        call EGEMM(I1,I2,I3,tt3D,C1,U)
        deallocate(C1)
        I3D=0
        do I=M1+1,N2;do j=N0+2,N2;do k=N0+1,j-1;do A=N2+1,M2
         I3D=I3D+1
         do c=b+1,N3
          V3D(c,b,A,k,j,I)=V3D(c,b,A,k,j,I)+U(c,I3D)
         enddo
        enddo;enddo;enddo;enddo
        deallocate(U)
3000   continue
       deallocate(tt3C2)
       deallocate(tt3C3)
       deallocate(tt3D)
       deallocate(VCPPP)
C
       do I=M1+1,N1;do j=N0+2,M1;do k=N0+1,j-1
       do A=N1+1,M2;do b=M2+1,N3-1;do c=b+1,N3
        CoeLeft=FBPP(c,c)+FBPP(b,b)+FAPP(A,A)
     &         -FBHH(k,k)-FBHH(j,j)-FAHH(I,I)
     &         +shift
!       print*,I,j,k,A,b,c
!       print*,-V3C2(c,b,A,k,j,I)
        t3C2(c,b,A,k,j,I)= t3C2(c,b,A,k,j,I)-V3C2(c,b,A,k,j,I)/CoeLeft
        t3C2(c,b,A,j,k,I)=-t3C2(c,b,A,k,j,I)
        t3C2(b,c,A,k,j,I)=-t3C2(c,b,A,k,j,I)
        t3C2(b,c,A,j,k,I)= t3C2(c,b,A,k,j,I)
       enddo;enddo;enddo
       enddo;enddo;enddo
!return
       do i=N0+1,N1;do J=M1+1,N2;do k=N0+1,N2
       do A=N1+1,M2;do b=M2+1,N3-1;do c=b+1,N3
        if(J.eq.k)cycle
        CoeLeft=FBPP(c,c)+FBPP(b,b)+FAPP(A,A)
     &         -FBHH(k,k)-FBHH(J,J)-FAHH(i,i)
     &         +shift
        t3C3(c,b,A,k,J,i)= t3C3(c,b,A,k,J,i)-V3C3(c,b,A,k,J,i)/CoeLeft
        t3C3(b,c,A,k,J,i)=-t3C3(c,b,A,k,J,i)
       enddo;enddo;enddo
       enddo;enddo;enddo
       do I=M1+1,N2;do j=N0+2,N2;do k=N0+1,j-1
       do A=N2+1,M2;do b=N2+1,N3-1;do c=b+1,N3
        if(I.eq.j)cycle
        if(I.eq.k)cycle
        if(A.eq.b)cycle
        if(A.eq.c)cycle
         CoeLeft=FBPP(c,c)+FBPP(b,b)+FBPP(A,A)
     &          -FBHH(k,k)-FBHH(j,j)-FBHH(I,I)
     &          +shift
        t3D(c,b,A,k,j,I)= t3D(c,b,A,k,j,I)-V3D(c,b,A,k,j,I)/CoeLeft
        t3D(c,b,A,j,k,I)=-t3D(c,b,A,k,j,I)
        t3D(b,c,A,k,j,I)=-t3D(c,b,A,k,j,I)
        t3D(b,c,A,j,k,I)= t3D(c,b,A,k,j,I)
       enddo;enddo;enddo
       enddo;enddo;enddo
C
       end
