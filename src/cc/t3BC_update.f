       subroutine t3BC_update(N0,N1,N2,N3,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,iPB,
     & FAHH,FAPP,FBHH,FBPP,
     & t1A,t1B,t2A,t2B,t2C,
     & t3B1,t3B4,t3C1,t3C4,
     & V3B1,V3B4,V3C1,V3C4)
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
       real*8 t3B1(N2+1:N3,N1+1:N3,N1+1:M2,N0+1:N2,N0+1:N1,M1+1:N1)
       real*8 t3B4(N2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1)
       real*8 t3C1(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:N2,M1+1:N2,N0+1:N1)
       real*8 t3C4(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N1)
       real*8 V3B1(N2+1:N3,N1+1:N3,N1+1:M2,N0+1:N2,N0+1:N1,M1+1:N1)
       real*8 V3B4(N2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1)
       real*8 V3C1(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:N2,M1+1:N2,N0+1:N1)
       real*8 V3C4(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N1)
C
       real*8,allocatable::VBPPP(:,:,:),C1(:,:,:)
       real*8,allocatable::tt3B1(:,:,:)
       real*8,allocatable::tt3B4(:,:,:)
       real*8,allocatable::tt3C1(:,:,:)
       real*8,allocatable::tt3C4(:,:,:)
       real*8,allocatable::U(:,:)
C
       N3B1=0
       do I=M1+1,N1;do j=N0+1,N1;do k=N0+1,N2;do A=N1+1,M2
        N3B1=N3B1+1
       enddo;enddo;enddo;enddo
       allocate(tt3B1(N2+1:N3,N1+1:N3,N3B1))
       tt3B1=0.0d0
       I3B1=0
       do I=M1+1,N1;do j=N0+1,N1;do k=N0+1,N2;do A=N1+1,M2
        I3B1=I3B1+1
        do g=M2+1,N3;do h=M2+1,N3
         tt3B1(h,g,I3B1)=t3B1(h,g,A,k,j,I)
        enddo;enddo
        do e=N1+1,N3;do f=N2+1,N3
         tt3B1(f,e,I3B1)=tt3B1(f,e,I3B1)-t1A(e,I)*t2B(f,A,k,j)
     &                                  +t1A(e,j)*t2B(f,A,k,i)
     &                                  +t1B(f,K)*t2A(e,A,j,i)
        enddo;enddo
       enddo;enddo;enddo;enddo
C
       N3B4=0
       do i=N0+2,M1;do j=N0+1,i-1;do K=M1+1,N2;do A=N1+1,M2
        N3B4=N3B4+1
       enddo;enddo;enddo;enddo
       allocate(tt3B4(N2+1:N3,N1+1:N3,N3B4))
       tt3B4=0.0d0
       I3B4=0
       do i=N0+2,M1;do j=N0+1,i-1;do K=M1+1,N2;do A=N1+1,M2
        I3B4=I3B4+1
        do g=M2+1,N3;do h=M2+1,N3
         tt3B4(h,g,I3B4)=t3B4(h,g,A,K,j,i)
        enddo;enddo
        do e=N1+1,N3;do f=N2+1,N3
         tt3B4(f,e,I3B4)=tt3B4(f,e,I3B4)-t1A(e,i)*t2B(f,A,K,j)
     &                                  +t1A(e,j)*t2B(f,A,K,i)
     &                                  +t1B(f,K)*t2A(e,A,j,i)
        enddo;enddo
       enddo;enddo;enddo;enddo
C
       N3C1=0
       do i=N0+1,N1;do J=M1+1,N2;do k=N0+1,N2;do B=N2+1,M2
        N3C1=N3C1+1
       enddo;enddo;enddo;enddo
       allocate(tt3C1(N2+1:N3,N1+1:N3,N3C1))
       tt3C1=0.0d0
       I3C1=0 
       do i=N0+1,N1;do J=M1+1,N2;do k=N0+1,N2;do B=N2+1,M2
        I3C1=I3C1+1
        do g=M2+1,N3;do h=M2+1,N3
         tt3C1(h,g,I3C1)=t3C1(h,B,g,k,J,i)
        enddo;enddo
        do e=N1+1,N3;do f=N2+1,N3
         tt3C1(f,e,I3C1)=tt3C1(f,e,I3C1)+t1A(e,I)*t2C(f,B,k,J)
     &                                  -t1B(f,J)*t2B(B,e,k,i)
     &                                  +t1B(f,k)*t2B(B,e,J,i)
        enddo;enddo
       enddo;enddo;enddo;enddo
C
       N3C4=0
       do I=M1+1,N1;do j=N0+2,M1;do k=N0+1,j-1;do B=N2+1,M2
        N3C4=N3C4+1
       enddo;enddo;enddo;enddo
       allocate(tt3C4(N2+1:N3,N1+1:N3,N3C4))
       tt3C4=0.0d0
       I3C4=0 
       do I=M1+1,N1;do j=N0+2,M1;do k=N0+1,j-1;do B=N2+1,M2
        I3C4=I3C4+1
        do g=M2+1,N3;do h=M2+1,N3
         tt3C4(h,g,I3C4)=t3C4(h,B,g,k,j,I)
        enddo;enddo
        do e=N1+1,N3;do f=N2+1,N3
         tt3C4(f,e,I3C4)=tt3C4(f,e,I3C4)+t1A(e,I)*t2C(f,B,k,j)
     &                                  -t1B(f,j)*t2B(B,e,k,I)
     &                                  +t1B(f,k)*t2B(B,e,j,I)
        enddo;enddo
       enddo;enddo;enddo;enddo
C      
       allocate(VBPPP(N2+1:N3,N1+1:N3,N2+1:N3))
       do 2000 b=M2+1,N3
        read(iPB,rec=b)VBPPP
        allocate(C1(N2+1:N3,N1+1:N3,M2+1:N3))
        do c=M2+1,N3;do e=N1+1,N3;do f=N2+1,N3
         C1(f,e,c)=VBPPP(f,e,c)
        enddo;enddo;enddo
        I2=K6
        I3=K3*K4
C
        allocate(U(M2+1:N3,N3B1))
        I1=N3B1
        call DMATMAT(I1,I2,I3,tt3B1,C1,U)
        I3B1=0
        do I=M1+1,N1;do j=N0+1,N1;do k=N0+1,N2;do A=N1+1,M2
         I3B1=I3B1+1
         do c=M2+1,N3
         V3B1(c,b,A,k,j,I)=V3B1(c,b,A,k,j,I)+U(c,I3B1)
         enddo
        enddo;enddo;enddo;enddo
        deallocate(U)
C
        allocate(U(M2+1:N3,N3B4))
        I1=N3B4
        call DMATMAT(I1,I2,I3,tt3B4,C1,U)
        I3B4=0
        do i=N0+2,M1;do j=N0+1,i-1;do K=M1+1,N2;do A=N1+1,M2
         I3B4=I3B4+1
         do c=M2+1,N3
         V3B4(c,b,A,K,j,i)=V3B4(c,b,A,K,j,i)+U(c,I3B4)
         enddo
        enddo;enddo;enddo;enddo
        deallocate(U)
C
        I1=N3C1
        allocate(U(M2+1:N3,N3C1))
        call DMATMAT(I1,I2,I3,tt3C1,C1,U)
        I3C1=0 
        do i=N0+1,N1;do J=M1+1,N2;do k=N0+1,N2;do A=N2+1,M2
         I3C1=I3C1+1
         do c=M2+1,N3
          V3C1(c,A,b,k,J,i)=V3C1(c,A,b,k,J,i)+U(c,I3C1)
         enddo
        enddo;enddo;enddo;enddo
        deallocate(U)
C
        I1=N3C4
        allocate(U(M2+1:N3,N3C4))
        call DMATMAT(I1,I2,I3,tt3C4,C1,U)
        deallocate(C1)
        I3C4=0
        do I=M1+1,N1;do j=N0+2,M1;do k=N0+1,j-1;do A=N2+1,M2
         I3C4=I3C4+1
         do c=M2+1,N3
          V3C4(c,A,b,k,j,I)=V3C4(c,A,b,k,j,I)+U(c,I3C4)
         enddo
        enddo;enddo;enddo;enddo
        deallocate(U)
2000   continue
       deallocate(tt3B1)
       deallocate(tt3B4)
       deallocate(tt3C1)
       deallocate(tt3C4)
       deallocate(VBPPP)
C
       do I=M1+1,N1;do j=N0+1,N1;do k=N0+1,N2
       do A=N1+1,M2;do b=N1+1,N3;do c=N2+1,N3
        if(I.eq.j)cycle
        if(A.eq.b)cycle
        CoeLeft=FBPP(c,c)+FAPP(b,b)+FAPP(A,A)
     &         -FBHH(k,k)-FAHH(j,j)-FAHH(I,I)
     &         +shift
        t3B1(c,b,A,k,j,I)=t3B1(c,b,A,k,j,I)-V3B1(c,b,A,k,j,I)/CoeLeft
       enddo;enddo;enddo
       enddo;enddo;enddo
       do i=N0+2,M1;do j=N0+1,i-1;do K=M1+1,N2
       do A=N1+1,M2;do b=N1+1,N3;do c=N2+1,N3
        if(A.eq.b)cycle
        CoeLeft=FBPP(c,c)+FAPP(b,b)+FAPP(A,A)
     &         -FBHH(K,K)-FAHH(j,j)-FAHH(i,i)
     &         +shift
        t3B4(c,b,A,K,j,i)= t3B4(c,b,A,K,j,i)-V3B4(c,b,A,K,j,i)/CoeLeft
        t3B4(c,b,A,K,i,j)=-t3B4(c,b,A,K,j,i)
       enddo;enddo;enddo
       enddo;enddo;enddo
       do i=N0+1,N1;do J=M1+1,N2;do k=N0+1,N2
       do a=N1+1,N3;do B=N2+1,M2;do c=N2+1,N3
!      do i=N0+1,M1;do J=M1+1,N2;do k=N0+1,M1
!      do a=M2+1,N3;do B=N2+1,M2;do c=M2+1,N3
        if(J.eq.k)cycle
        if(B.eq.c)cycle
        CoeLeft=FBPP(c,c)+FBPP(B,B)+FAPP(a,a)
     &         -FBHH(k,k)-FBHH(J,J)-FAHH(i,i)
     &         +shift
        t3C1(c,B,a,k,J,i)= t3C1(c,B,a,k,J,i)-V3C1(c,B,a,k,J,i)/CoeLeft
       enddo;enddo;enddo
       enddo;enddo;enddo
       do I=M1+1,N1;do j=N0+2,M1;do k=N0+1,j-1
       do a=N1+1,N3;do B=N2+1,M2;do c=N2+1,N3
!      do a=M2+1,N3;do B=N2+1,M2;do c=M2+1,N3
        if(B.eq.c)cycle
        CoeLeft=FBPP(c,c)+FBPP(B,B)+FAPP(a,a)
     &         -FBHH(k,k)-FBHH(j,j)-FAHH(I,I)
     &         +shift
        t3C4(c,B,a,k,j,I)= t3C4(c,B,a,k,j,I)-V3C4(c,B,a,k,j,I)/CoeLeft
        t3C4(c,B,a,j,k,I)=-t3C4(c,B,a,k,j,I)
       enddo;enddo;enddo
       enddo;enddo;enddo
C
       end
