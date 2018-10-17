       subroutine L_input(N0,N1,N2,N3,iroot,
     & H1A,H1B,H2A,H2B,H2C,l1A,l1B,l2A,l2B,l2C,
     & V1A,V1B,V2A,V2B,V2C)
C
       integer a,b,c
       real*8 H1A(N0+1:N3,N0+1:N3)
       real*8 H1B(N0+1:N3,N0+1:N3)
       real*8 H2A(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H2B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H2C(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 l1A(N1+1:N3,N0+1:N1)
       real*8 l1B(N2+1:N3,N0+1:N2)
       real*8 l2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 l2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 l2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 V1A(N1+1:N3,N0+1:N1)
       real*8 V1B(N2+1:N3,N0+1:N2)
       real*8 V2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 V2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 V2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
C
       do i=N0+1,N1;do j=N0+1,N1
       do a=N1+1,N3;do b=N1+1,N3
        V2A(b,a,j,i)=-H1A(b,i)*l1A(a,j)
     &               +H1A(a,i)*l1A(b,j)
     &               +H1A(b,j)*l1A(a,i)
     &               -H1A(a,j)*l1A(b,i)
       enddo;enddo;enddo;enddo
C
       do i=N0+1,N1;do j=N0+1,N2
       do a=N1+1,N3;do b=N2+1,N3
        V2B(b,a,j,i)=+H1B(b,j)*l1A(a,i)
     &               +H1A(a,i)*l1B(b,j)
       enddo;enddo;enddo;enddo
C
       do i=N0+1,N2;do j=N0+1,N2
       do a=N2+1,N3;do b=N2+1,N3
        V2C(b,a,j,i)=-H1B(b,i)*l1B(a,j)
     &               +H1B(a,i)*l1B(b,j)
     &               +H1B(b,j)*l1B(a,i)
     &               -H1B(a,j)*l1B(b,i)
       enddo;enddo;enddo;enddo
C
       if(iroot.ne.0)return
       do i=N0+1,N1;do a=N1+1,N3
        V1A(a,i)=H1A(a,i)
       enddo;enddo
       do i=N0+1,N2;do a=N2+1,N3
        V1B(a,i)=H1B(a,i)
       enddo;enddo
       do i=N0+1,N1;do j=N0+1,N1;do a=N1+1,N3;do b=N1+1,N3
        V2A(b,a,j,i)=V2A(b,a,j,i)+H2A(b,a,j,i)
       enddo;enddo;enddo;enddo
       do i=N0+1,N1;do j=N0+1,N2;do a=N1+1,N3;do b=N2+1,N3
        V2B(b,a,j,i)=V2B(b,a,j,i)+H2B(b,a,j,i)
       enddo;enddo;enddo;enddo
       do i=N0+1,N2;do j=N0+1,N2;do a=N2+1,N3;do b=N2+1,N3
        V2C(b,a,j,i)=V2C(b,a,j,i)+H2C(b,a,j,i)
       enddo;enddo;enddo;enddo
C
       end
