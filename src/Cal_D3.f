       subroutine Cal_D3(N0,N1,N2,N3,
     & H2A,H2B,H2C,t2A,t2B,t2C,
     & D3A1,D3A2,D3B1,D3B2,D3C1,D3C2,D3D1,D3D2)
C
       implicit none

       integer n0, n1, n2, n3
       integer a,b,e
       integer i,j,m
       real*8 PP
       real*8 H2A(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H2B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H2C(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 D3A1(N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 D3A2(N1+1:N3,N1+1:N3,N0+1:N1)
       real*8 D3B1(N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 D3B2(N2+1:N3,N1+1:N3,N0+1:N1)
       real*8 D3C1(N2+1:N3,N0+1:N2,N0+1:N1)
       real*8 D3C2(N2+1:N3,N1+1:N3,N0+1:N2)
       real*8 D3D1(N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 D3D2(N2+1:N3,N2+1:N3,N0+1:N2)
C
       do i=N0+1,N1;do j=N0+1,N1;do a=N1+1,N3
        PP=0.0d0
        do e=N1+1,N3
         PP=PP+H2A(e,a,j,i)*t2A(e,a,j,i)
        enddo
        D3A1(a,j,i)=PP
       enddo;enddo;enddo
C
       do i=N0+1,N1;do a=N1+1,N3;do b=N1+1,N3
        PP=0.0d0
        do m=N0+1,N1
         PP=PP+H2A(b,a,m,i)*t2A(b,a,m,i)
        enddo
        D3A2(b,a,i)=-PP
       enddo;enddo;enddo
C
       do i=N0+1,N1;do j=N0+1,N2;do a=N1+1,N3
        PP=0.0d0
        do e=N2+1,N3
         PP=PP+H2B(e,a,j,i)*t2B(e,a,j,i)
        enddo
        D3B1(a,j,i)=PP
       enddo;enddo;enddo
       do i=N0+1,N1;do a=N1+1,N3;do b=N2+1,N3
        PP=0.0d0
        do m=N0+1,N2
         PP=PP+H2B(b,a,m,i)*t2B(b,a,m,i)
        enddo
        D3B2(b,a,i)=-PP
       enddo;enddo;enddo
C
       do i=N0+1,N1;do j=N0+1,N2;do b=N2+1,N3
        PP=0.0d0
        do e=N1+1,N3
         PP=PP+H2B(b,e,j,i)*t2B(b,e,j,i)
        enddo
        D3C1(b,j,i)=PP
       enddo;enddo;enddo
       do j=N0+1,N2;do a=N1+1,N3;do b=N2+1,N3
        PP=0.0d0
        do m=N0+1,N1
         PP=PP+H2B(b,a,j,m)*t2B(b,a,j,m)
        enddo
        D3C2(b,a,j)=-PP
       enddo;enddo;enddo
C
       do i=N0+1,N2;do j=N0+1,N2;do a=N2+1,N3
        PP=0.0d0
        do e=N1+2,N3
         PP=PP+H2C(e,a,j,i)*t2C(e,a,j,i)
        enddo
        D3D1(a,j,i)=PP
       enddo;enddo;enddo
       do i=N0+1,N2;do a=N2+1,N3;do b=N2+1,N3
        PP=0.0d0
        do m=N0+1,N2
         PP=PP+H2C(b,a,m,i)*t2C(b,a,m,i)
        enddo
        D3D2(b,a,i)=-PP
       enddo;enddo;enddo
C
       end subroutine
