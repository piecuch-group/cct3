       subroutine t2B_update1(N0,N1,N2,N3,V2B,shift,
     & iPB,K1,K2,K3,K4,
     & FAHH,FAPP,FBHH,FBPP,t1A,t1B,t2B)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 CoeLeft,shift
       real*8 FAHH(N0+1:N1,N0+1:N1)
       real*8 FAPP(N1+1:N3,N1+1:N3)
       real*8 FBHH(N0+1:N2,N0+1:N2)
       real*8 FBPP(N2+1:N3,N2+1:N3)
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 V2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8,allocatable::VBPPPP(:,:,:)
       real*8,allocatable::D(:,:,:,:)
       real*8,allocatable::U(:,:,:)
C
       allocate(U(N0+1:N2,N0+1:N1,N2+1:N3))
       allocate(D(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       D=0.0d0
       do i=N0+1,N1;do j=N0+1,N2;do e=N1+1,N3;do f=N2+1,N3
        D(f,e,j,i)=t2B(f,e,j,i)+t1A(e,i)*t1B(f,j)
       enddo;enddo;enddo;enddo
       I1=K4
       I2=K1*K2
       I3=K3*K4
       allocate(VBPPPP(N2+1:N3,N1+1:N3,N2+1:N3))
       do a=N1+1,N3
        U=0.0d0
        read(iPB,rec=a)VBPPPP
        call DMATMAT(I1,I2,I3,VBPPPP,D,U)
        do b=N2+1,N3;do i=N0+1,N1;do j=N0+1,N2
         V2B(b,a,j,i)=V2B(b,a,j,i)+U(j,i,b)
        enddo;enddo;enddo
       enddo
       deallocate(VBPPPP)
       deallocate(D)
       deallocate(U)
C
       do i=N0+1,N1
       do j=N0+1,N2
       do a=N1+1,N3
       do b=N2+1,N3
         CoeLeft=FBPP(b,b)
     &          +FAPP(a,a)
     &          -FBHH(j,j)
     &          -FAHH(i,i)
     &          +shift
         t2B(b,a,j,i)=t2B(b,a,j,i)-V2B(b,a,j,i)/CoeLeft
       enddo
       enddo
       enddo
       enddo
C
       end
