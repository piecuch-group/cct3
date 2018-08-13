       subroutine t2A_update1(N0,N1,N3,V2A,shift,
     & iPA,K1,K3,FAHH,FAPP,t1A,t2A)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 CoeLeft,shift
       real*8 FAHH(N0+1:N1,N0+1:N1)
       real*8 FAPP(N1+1:N3,N1+1:N3)
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 V2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8,allocatable::VAPPP(:,:,:)
       real*8,allocatable::tt2A(:,:,:)
       real*8,allocatable::U(:,:)
       real*8,allocatable::C1(:,:,:)
C
       N2A=0
       do i=N0+2,N1;do j=N0+1,i-1
        N2A=N2A+1
       enddo;enddo
C
       allocate(tt2A(N1+1:N3,N1+1:N3,N2A))
       tt2A=0.0d0
       I2A=0
       do i=N0+2,N1;do j=N0+1,i-1
        I2A=I2A+1
        do e=N1+1,N3;do f=N1+1,N3
         tt2A(f,e,I2A)=0.5d0*t2A(f,e,j,i)+t1A(e,i)*t1A(f,j)
       enddo;enddo;enddo;enddo
       allocate(VAPPP(N1+1:N3,N1+1:N3,N1+1:N3))
       do a=N1+1,N3-1
        read(iPA,rec=a)VAPPP
        allocate(C1(N1+1:N3,N1+1:N3,a+1:N3))
        do b=a+1,N3;do e=N1+1,N3;do f=N1+1,N3
         C1(f,e,b)=VAPPP(f,e,b)
        enddo;enddo;enddo
        allocate(U(a+1:N3,N2A))
        I1=N2A
        I2=N3-a
        I3=K3*K3
        call EGEMM(I1,I2,I3,tt2A,C1,U)
!       call EGEMM(I1,I2,I3,tt2A,VAPPP(1,1,a+1),U)
        deallocate(C1)
        I2A=0
        do i=N0+2,N1;do j=N0+1,i-1
         I2A=I2A+1
         do b=a+1,N3
         V2A(b,a,j,i)=V2A(b,a,j,i)+U(b,I2A)
        enddo;enddo;enddo
        deallocate(U)
       enddo
       deallocate(VAPPP)
       deallocate(tt2A)
C
       do i=N0+2,N1;do j=N0+1,i-1
       do a=N1+1,N3-1;do b=a+1,N3
         CoeLeft=FAPP(b,b)+FAPP(a,a)
     &          -FAHH(j,j)-FAHH(i,i)
     &          +shift
         t2A(b,a,j,i)=t2A(b,a,j,i)-V2A(b,a,j,i)/CoeLeft
         t2A(b,a,i,j)=-t2A(b,a,j,i)
         t2A(a,b,j,i)=-t2A(b,a,j,i)
         t2A(a,b,i,j)= t2A(b,a,j,i)
       enddo
       enddo
       enddo
       enddo
C
       end
