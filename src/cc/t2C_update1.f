       subroutine t2C_update1(N0,N2,N3,V2C,shift,
     & iPC,K2,K4,FBHH,FBPP,t1B,t2C)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 CoeLeft,shift
       real*8 FBHH(N0+1:N2,N0+1:N2)
       real*8 FBPP(N2+1:N3,N2+1:N3)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 V2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8,allocatable::VCPPP(:,:,:)
       real*8,allocatable::tt2C(:,:,:)
       real*8,allocatable::U(:,:)
       real*8,allocatable::C1(:,:,:)
C
       N2C=0
       do i=N0+2,N2;do j=N0+1,i-1
        N2C=N2C+1
       enddo;enddo
C
       allocate(tt2C(N2+1:N3,N2+1:N3,N2C))
       tt2C=0.0d0
       I2C=0
       do i=N0+2,N2;do j=N0+1,i-1
        I2C=I2C+1
        do e=N2+1,N3;do f=N2+1,N3
         tt2C(f,e,I2C)=0.5d0*t2C(f,e,j,i)+t1B(e,i)*t1B(f,j)
       enddo;enddo;enddo;enddo
C
       allocate(VCPPP(N2+1:N3,N2+1:N3,N2+1:N3))
       do a=N2+1,N3-1
        read(iPC,rec=a)VCPPP
        allocate(C1(N2+1:N3,N2+1:N3,a+1:N3))
        do b=a+1,N3;do e=N2+1,N3;do f=N2+1,N3
         C1(f,e,b)=VCPPP(f,e,b)
        enddo;enddo;enddo
        allocate(U(a+1:N3,N2C))
        U=0.0d0
        I1=N2C
        I2=N3-a
        I3=K4*K4
        call DMATMAT(I1,I2,I3,tt2C,C1,U)
!       call DMATMAT(I1,I2,I3,tt2C,VCPPP(1,1,a+1),U)
        deallocate(C1)
        I2C=0
        do i=N0+2,N2;do j=N0+1,i-1
         I2C=I2C+1
         do b=a+1,N3
          V2C(b,a,j,i)=V2C(b,a,j,i)+U(b,I2C)
         enddo;enddo;enddo
        deallocate(U)
       enddo
       deallocate(VCPPP)
       deallocate(tt2C)
C
       do i=N0+2,N2;do j=N0+1,i-1
       do a=N2+1,N3-1;do b=a+1,N3
         CoeLeft=FBPP(b,b)+FBPP(a,a)
     &          -FBHH(j,j)-FBHH(i,i)
     &          +shift
         t2C(b,a,j,i)= t2C(b,a,j,i)-V2C(b,a,j,i)/CoeLeft
         t2C(b,a,i,j)=-t2C(b,a,j,i)
         t2C(a,b,j,i)=-t2C(b,a,j,i)
         t2C(a,b,i,j)= t2C(b,a,j,i)
       enddo;enddo
       enddo;enddo
C
       end
