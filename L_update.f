       subroutine L_update(N0,N1,N2,N3,shift,H1A,H1B,ECor,
     & l1A,l1B,l2A,l2B,l2C,
     & V1A,V1B,V2A,V2B,V2C)
C
       integer a,b,c
       real*8 shift,PP,ECor
       real*8 H1A(N0+1:N3,N0+1:N3)
       real*8 H1B(N0+1:N3,N0+1:N3)
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
       do i=N0+1,N1;do a=N1+1,N3
         PP=H1A(a,a)-H1A(i,i)-ECor+shift
         l1A(a,i)=l1A(a,i)-V1A(a,i)/PP
       enddo;enddo
       do i=N0+1,N2;do a=N2+1,N3
         PP=H1B(a,a)-H1B(i,i)-ECor+shift
         l1B(a,i)=l1B(a,i)-V1B(a,i)/PP
       enddo;enddo
       do i=N0+1,N1;do j=N0+1,N1
       do a=N1+1,N3;do b=N1+1,N3
         PP=H1A(b,b)+H1A(a,a)-H1A(j,j)-H1A(i,i)-ECor+shift
         l2A(b,a,j,i)= l2A(b,a,j,i)-V2A(b,a,j,i)/PP
       enddo;enddo;enddo;enddo
       do i=N0+1,N1;do j=N0+1,N2
       do a=N1+1,N3;do b=N2+1,N3
         PP=H1B(b,b)+H1A(a,a)-H1B(j,j)-H1A(i,i)-ECor+shift
         l2B(b,a,j,i)= l2B(b,a,j,i)-V2B(b,a,j,i)/PP
       enddo;enddo;enddo;enddo
       do i=N0+1,N2;do j=N0+1,N2
       do a=N2+1,N3;do b=N2+1,N3
         PP=H1B(b,b)+H1B(a,a)-H1B(j,j)-H1B(i,i)-ECor+shift
         l2C(b,a,j,i)= l2C(b,a,j,i)-V2C(b,a,j,i)/PP
       enddo;enddo;enddo;enddo
C
       end
C
