       subroutine t3AB_update(N0,N1,N2,N3,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,iPA,
     & FAHH,FAPP,FBHH,FBPP,
     & t1A,t1B,t2A,t2B,t2C,
     & t3A,t3B2,t3B3,
     & V3A,V3B2,V3B3)
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
       real*8 t3A(N1+1:N3,N1+1:N3,N1+1:M2,N0+1:N1,N0+1:N1,M1+1:N1)
       real*8 t3B2(N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1)
       real*8 t3B3(N2+1:M2,M2+1:N3,M2+1:N3,N0+1:N2,N0+1:N1,M1+1:N1)
       real*8 V3A(N1+1:N3,N1+1:N3,N1+1:M2,N0+1:N1,N0+1:N1,M1+1:N1)
       real*8 V3B2(N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1)
       real*8 V3B3(N2+1:M2,M2+1:N3,M2+1:N3,N0+1:N2,N0+1:N1,M1+1:N1)
C
       real*8,allocatable::VAPPP(:,:,:)
       real*8,allocatable::tt3A(:,:,:)
       real*8,allocatable::tt3B2(:,:,:)
       real*8,allocatable::tt3B3(:,:,:)
       real*8,allocatable::U(:,:)
       real*8,allocatable::C1(:,:,:)
C
       N3A=0
       do I=M1+1,N1;do j=N0+2,N1;do k=N0+1,j-1;do A=N1+1,M2
        N3A=N3A+1
       enddo;enddo;enddo;enddo
       allocate(tt3A(N1+1:N3,N1+1:N3,N3A))
       tt3A=0.0d0
       I3A=0
       do I=M1+1,N1;do j=N0+2,N1;do k=N0+1,j-1;do A=N1+1,M2
        I3A=I3A+1
        do g=M2+1,N3;do h=M2+1,N3
         tt3A(h,g,I3A)=0.5d0*t3A(h,g,A,k,j,I)
        enddo;enddo
        do e=N1+1,N3;do f=N1+1,N3
         tt3A(f,e,I3A)=tt3A(f,e,I3A)+t1A(f,I)*t2A(e,A,k,j)
     &                              -t1A(f,j)*t2A(e,A,k,I)
     &                              +t1A(f,k)*t2A(e,A,j,I)
        enddo;enddo
       enddo;enddo;enddo;enddo
C
       N3B2=0
       do i=N0+2,M1;do j=N0+1,i-1;do K=M1+1,N2;do C=N2+1,M2
        N3B2=N3B2+1
       enddo;enddo;enddo;enddo
       allocate(tt3B2(N1+1:N3,N1+1:N3,N3B2))
       tt3B2=0.0d0
       I3B2=0 
       do i=N0+2,M1;do j=N0+1,i-1;do K=M1+1,N2;do C=N2+1,M2
        I3B2=I3B2+1
        do g=M2+1,N3;do h=M2+1,N3
         tt3B2(h,g,I3B2)=0.5d0*t3B2(C,h,g,K,j,i)
        enddo;enddo
        do e=N1+1,N3;do f=N1+1,N3
         tt3B2(f,e,I3B2)=tt3B2(f,e,I3B2)+t1A(e,i)*t2B(C,f,K,j)
     &                                  +t1A(f,j)*t2B(C,e,K,i)
        enddo;enddo               
       enddo;enddo;enddo;enddo
C
       N3B3=0
       do I=M1+1,N1;do j=N0+1,N1;do k=N0+1,N2;do C=N2+1,M2
        N3B3=N3B3+1
       enddo;enddo;enddo;enddo
       allocate(tt3B3(N1+1:N3,N1+1:N3,N3B3))
       tt3B3=0.0d0
       I3B3=0
       do I=M1+1,N1;do j=N0+1,N1;do k=N0+1,N2;do C=N2+1,M2
        I3B3=I3B3+1
        do g=M2+1,N3;do h=M2+1,N3
         tt3B3(h,g,I3B3)=0.5d0*t3B3(C,h,g,k,j,I)
        enddo;enddo
        do e=N1+1,N3;do f=N1+1,N3
         tt3B3(f,e,I3B3)=tt3B3(f,e,I3B3)+t1A(e,I)*t2B(C,f,k,j)
     &                                  +t1A(f,j)*t2B(C,e,k,I)
        enddo;enddo
       enddo;enddo;enddo;enddo
C
       allocate(VAPPP(N1+1:N3,N1+1:N3,N1+1:N3))
       do 1000 b=M2+1,N3-1
        read(iPA,rec=b)VAPPP
        allocate(C1(N1+1:N3,N1+1:N3,b+1:N3))
        do c=b+1,N3;do e=N1+1,N3;do f=N1+1,N3
         C1(f,e,c)=VAPPP(f,e,c)
        enddo;enddo;enddo
        I2=N3-b
        I3=K3*K3
C
        allocate(U(b+1:N3,N3A))
        I1=N3A
!       call EGEMM(I1,I2,I3,tt3A,VAPPP(1,1,b+1),U)
        call EGEMM(I1,I2,I3,tt3A,C1,U)
        I3A=0
        do I=M1+1,N1;do j=N0+2,N1;do k=N0+1,j-1;do A=N1+1,M2
         I3A=I3A+1
         do c=b+1,N3
          V3A(c,b,A,k,j,I)=V3A(c,b,A,k,j,I)+U(c,I3A)
         enddo
        enddo;enddo;enddo;enddo
        deallocate(U)
C
        allocate(U(b+1:N3,N3B2))
        I1=N3B2
!       call EGEMM(I1,I2,I3,tt3B2,VAPPP(1,1,b+1),U)
        call EGEMM(I1,I2,I3,tt3B2,C1,U)
        I3B2=0
        do i=N0+2,M1;do j=N0+1,i-1;do K=M1+1,N2;do C=N2+1,M2
         I3B2=I3B2+1
         do a=b+1,N3
          V3B2(C,a,b,K,j,i)=V3B2(C,a,b,K,j,i)+U(a,I3B2)
         enddo
        enddo;enddo;enddo;enddo
        deallocate(U)
C
        allocate(U(b+1:N3,N3B3))
        I1=N3B3
        call EGEMM(I1,I2,I3,tt3B3,C1,U)
!       call EGEMM(I1,I2,I3,tt3B3,VAPPP(1,1,b+1),U)
        deallocate(C1)
        I3B3=0
        do I=M1+1,N1;do j=N0+1,N1;do k=N0+1,N2;do C=N2+1,M2
         I3B3=I3B3+1
         do a=b+1,N3
          V3B3(C,a,b,k,j,I)=V3B3(C,a,b,k,j,I)+U(a,I3B3)
         enddo
        enddo;enddo;enddo;enddo
        deallocate(U)
1000   continue
       deallocate(tt3A)
       deallocate(tt3B2)
       deallocate(tt3B3)
       deallocate(VAPPP)
C
       do I=M1+1,N1;do j=N0+2,N1;do k=N0+1,j-1
       do A=N1+1,M2;do b=N1+1,N3-1;do c=b+1,N3
        if(I.eq.j)cycle
        if(I.eq.k)cycle
        if(A.eq.b)cycle
        if(A.eq.c)cycle
        CoeLeft=FAPP(c,c)+FAPP(b,b)+FAPP(A,A)
     &         -FAHH(k,k)-FAHH(j,j)-FAHH(I,I)
     &         +shift
        t3A(c,b,A,k,j,I)= t3A(c,b,A,k,j,I)-V3A(c,b,A,k,j,I)/CoeLeft
        t3A(c,b,A,j,k,I)=-t3A(c,b,A,k,j,I)
        t3A(b,c,A,k,j,I)=-t3A(c,b,A,k,j,I)
        t3A(b,c,A,j,k,I)= t3A(c,b,A,k,j,I)
       enddo;enddo;enddo
       enddo;enddo;enddo
       do i=N0+2,M1;do j=N0+1,i-1;do K=M1+1,N2
       do a=M2+1,N3-1;do b=a+1,N3;do C=N2+1,M2
        CoeLeft=FBPP(C,C)+FAPP(b,b)+FAPP(a,a)
     &         -FBHH(K,K)-FAHH(j,j)-FAHH(i,i)
     &         +shift
        t3B2(C,b,a,K,j,i)= t3B2(C,b,a,K,j,i)-V3B2(C,b,a,K,j,i)/CoeLeft
        t3B2(C,b,a,K,i,j)=-t3B2(C,b,a,K,j,i)
        t3B2(C,a,b,K,j,i)=-t3B2(C,b,a,K,j,i)
        t3B2(C,a,b,K,i,j)= t3B2(C,b,a,K,j,i)
       enddo;enddo;enddo
       enddo;enddo;enddo
       do I=M1+1,N1;do j=N0+1,N1;do k=N0+1,N2
       do a=M2+1,N3-1;do b=a+1,N3;do C=N2+1,M2
        if(I.eq.j)cycle
        CoeLeft=FBPP(C,C)+FAPP(b,b)+FAPP(a,a)
     &         -FBHH(k,k)-FAHH(j,j)-FAHH(I,I)
     &         +shift
        t3B3(C,b,a,k,j,I)= t3B3(C,b,a,k,j,I)-V3B3(C,b,a,k,j,I)/CoeLeft
        t3B3(C,a,b,k,j,I)=-t3B3(C,b,a,k,j,I)
       enddo;enddo;enddo
       enddo;enddo;enddo
C
       end
