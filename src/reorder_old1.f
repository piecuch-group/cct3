       subroutine VtoHR12(M1,N1,M2,N2,
     & K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2)
       real*8 B(K1+1:L1,K2+1:L2)

       do I1=M1+1,N1
       do I2=M2+1,N2
        B(I1,I2)=A(I1,I2)
       enddo
       enddo

       end

       subroutine VtoHR21(M1,N1,M2,N2,
     & K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2)
       real*8 B(K1+1:L1,K2+1:L2)

       do I1=M1+1,N1
       do I2=M2+1,N2
        B(I2,I1)=-A(I1,I2)
       enddo
       enddo

       end

       subroutine old1reorder12(M1,N1,M2,N2,
     & K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2)
       real*8 B(K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
        B(I1,I2)=A(I1,I2)
       enddo
       enddo

       end

       subroutine old1reorder21(M1,N1,M2,N2,
     & K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2)
       real*8 B(K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
        B(I2,I1)=A(I1,I2)
       enddo
       enddo

       end

       subroutine VtoHR1234(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)

       do I1=M1+1,N1
       do I2=M2+1,N2
       do I3=M3+1,N3
       do I4=M4+1,N4
        B(I1,I2,I3,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine VtoHR1243(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)

       do I1=M1+1,N1
       do I2=M2+1,N2
       do I3=M3+1,N3
       do I4=M4+1,N4
        B(I1,I2,I4,I3)=-A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine VtoHR1324(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)

       do I1=M1+1,N1
       do I2=M2+1,N2
       do I3=M3+1,N3
       do I4=M4+1,N4
        B(I1,I3,I2,I4)=-A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine VtoHR1432(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)

       do I1=M1+1,N1
       do I2=M2+1,N2
       do I3=M3+1,N3
       do I4=M4+1,N4
        B(I1,I4,I3,I2)=-A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine VtoHR2134(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)

       do I1=M1+1,N1
       do I2=M2+1,N2
       do I3=M3+1,N3
       do I4=M4+1,N4
        B(I2,I1,I3,I4)=-A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine VtoHR3214(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)

       do I1=M1+1,N1
       do I2=M2+1,N2
       do I3=M3+1,N3
       do I4=M4+1,N4
        B(I3,I2,I1,I4)=-A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder1234(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I2,I3,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder1243(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K2,L2,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I2,I4,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder1324(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K3,L3,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I3,I2,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder1342(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K3,L3,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I3,I4,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder1423(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K4,L4,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I4,I2,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder1432(M1,N1,M2,N2,M3,N3,M4,N4,
     & K1,L1,K4,L4,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I1,I4,I3,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder2134(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K1,L1,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I1,I3,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder2143(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K1,L1,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I1,I4,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder2314(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K3,L3,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I3,I1,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder2341(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K3,L3,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I3,I4,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder2413(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K4,L4,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I4,I1,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder2431(M1,N1,M2,N2,M3,N3,M4,N4,
     & K2,L2,K4,L4,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I2,I4,I3,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder3124(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K1,L1,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I1,I2,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder3142(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K1,L1,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I1,I4,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder3214(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K2,L2,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I2,I1,I4)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder3241(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K2,L2,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I2,I4,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder3412(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K4,L4,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I4,I1,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder3421(M1,N1,M2,N2,M3,N3,M4,N4,
     & K3,L3,K4,L4,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I3,I4,I2,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder4123(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K1,L1,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I1,I2,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder4132(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K1,L1,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I1,I3,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder4213(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K2,L2,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I2,I1,I3)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder4231(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K2,L2,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I2,I3,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder4312(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K3,L3,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I3,I1,I2)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder4321(M1,N1,M2,N2,M3,N3,M4,N4,
     & K4,L4,K3,L3,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        B(I4,I3,I2,I1)=A(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo

       end

       subroutine VtoHR123456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)

       do I1=M1+1,N1
       do I2=M2+1,N2
       do I3=M3+1,N3
       do I4=M4+1,N4
       do I5=M5+1,N5
       do I6=M6+1,N6
        B(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine VtoHR124356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)

       do I1=M1+1,N1
       do I2=M2+1,N2
       do I3=M3+1,N3
       do I4=M4+1,N4
       do I5=M5+1,N5
       do I6=M6+1,N6
        B(I1,I2,I3,I4,I6,I5)=-A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder123456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder123465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K4,L4,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I3,I4,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder123546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K5,L5,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I3,I5,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder123564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K5,L5,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I3,I5,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder123645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K6,L6,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I3,I6,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder123654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K3,L3,K6,L6,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I3,I6,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder124356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K3,L3,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I4,I3,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder124365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K3,L3,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I4,I3,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder124536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K5,L5,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I4,I5,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder124563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K5,L5,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I4,I5,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder124635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K6,L6,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I4,I6,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder124653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K4,L4,K6,L6,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I4,I6,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder125346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K3,L3,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I5,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder125364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K3,L3,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I5,I3,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder125436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K4,L4,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I5,I4,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder125463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K4,L4,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I5,I4,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder125634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K6,L6,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I5,I6,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder125643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K5,L5,K6,L6,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I5,I6,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder126345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K3,L3,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I6,I3,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder126354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K3,L3,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I6,I3,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder126435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K4,L4,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I6,I4,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder126453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K4,L4,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I6,I4,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder126534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K5,L5,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I6,I5,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder126543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K2,L2,K6,L6,K5,L5,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I2,I6,I5,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder132456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K4,L4,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I2,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder132465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K4,L4,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I2,I4,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder132546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K5,L5,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I2,I5,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder132564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K5,L5,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I2,I5,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder132645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K6,L6,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I2,I6,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder132654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K2,L2,K6,L6,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I2,I6,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder134256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K2,L2,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I4,I2,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder134265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K2,L2,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I4,I2,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder134526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K5,L5,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I4,I5,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder134562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K5,L5,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I4,I5,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder134625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K6,L6,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I4,I6,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder134652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K4,L4,K6,L6,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I4,I6,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder135246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K2,L2,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I5,I2,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder135264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K2,L2,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I5,I2,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder135426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K4,L4,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I5,I4,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder135462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K4,L4,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I5,I4,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder135624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K6,L6,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I5,I6,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder135642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K5,L5,K6,L6,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I5,I6,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder136245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K2,L2,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I6,I2,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder136254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K2,L2,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I6,I2,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder136425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K4,L4,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I6,I4,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder136452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K4,L4,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I6,I4,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder136524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K5,L5,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I6,I5,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder136542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K3,L3,K6,L6,K5,L5,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I3,I6,I5,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder142356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K3,L3,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I2,I3,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder142365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K3,L3,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I2,I3,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder142536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K5,L5,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I2,I5,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder142563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K5,L5,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I2,I5,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder142635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K6,L6,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I2,I6,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder142653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K2,L2,K6,L6,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I2,I6,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder143256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K2,L2,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I3,I2,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder143265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K2,L2,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I3,I2,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder143526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K5,L5,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I3,I5,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder143562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K5,L5,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I3,I5,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder143625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K6,L6,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I3,I6,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder143652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K3,L3,K6,L6,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I3,I6,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder145236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K2,L2,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I5,I2,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder145263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K2,L2,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I5,I2,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder145326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K3,L3,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I5,I3,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder145362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K3,L3,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I5,I3,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder145623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K6,L6,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I5,I6,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder145632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K5,L5,K6,L6,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I5,I6,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder146235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K2,L2,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I6,I2,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder146253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K2,L2,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I6,I2,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder146325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K3,L3,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I6,I3,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder146352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K3,L3,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I6,I3,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder146523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K5,L5,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I6,I5,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder146532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K4,L4,K6,L6,K5,L5,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I4,I6,I5,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder152346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K3,L3,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I2,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder152364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K3,L3,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I2,I3,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder152436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K4,L4,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I2,I4,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder152463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K4,L4,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I2,I4,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder152634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K6,L6,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I2,I6,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder152643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K2,L2,K6,L6,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I2,I6,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder153246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K2,L2,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I3,I2,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder153264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K2,L2,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I3,I2,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder153426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K4,L4,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I3,I4,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder153462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K4,L4,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I3,I4,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder153624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K6,L6,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I3,I6,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder153642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K3,L3,K6,L6,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I3,I6,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder154236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K2,L2,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I4,I2,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder154263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K2,L2,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I4,I2,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder154326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K3,L3,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I4,I3,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder154362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K3,L3,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I4,I3,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder154623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K6,L6,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I4,I6,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder154632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K4,L4,K6,L6,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I4,I6,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder156234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K2,L2,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I6,I2,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder156243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K2,L2,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I6,I2,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder156324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K3,L3,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I6,I3,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder156342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K3,L3,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I6,I3,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder156423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K4,L4,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I6,I4,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder156432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K5,L5,K6,L6,K4,L4,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I5,I6,I4,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder162345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K3,L3,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder162354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K3,L3,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I2,I3,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder162435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K4,L4,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I2,I4,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder162453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K4,L4,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I2,I4,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder162534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K5,L5,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I2,I5,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder162543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K2,L2,K5,L5,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I2,I5,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder163245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K2,L2,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I3,I2,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder163254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K2,L2,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I3,I2,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder163425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K4,L4,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I3,I4,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder163452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K4,L4,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I3,I4,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder163524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K5,L5,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I3,I5,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder163542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K3,L3,K5,L5,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I3,I5,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder164235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K2,L2,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I4,I2,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder164253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K2,L2,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I4,I2,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder164325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K3,L3,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I4,I3,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder164352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K3,L3,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I4,I3,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder164523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K5,L5,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I4,I5,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder164532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K4,L4,K5,L5,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I4,I5,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder165234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K2,L2,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I5,I2,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder165243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K2,L2,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I5,I2,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder165324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K3,L3,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I5,I3,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder165342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K3,L3,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I5,I3,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder165423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K4,L4,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I5,I4,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder165432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K1,L1,K6,L6,K5,L5,K4,L4,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I1,I6,I5,I4,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder213456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K4,L4,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder213465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K4,L4,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I3,I4,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder213546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K5,L5,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I3,I5,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder213564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K5,L5,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I3,I5,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder213645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K6,L6,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I3,I6,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder213654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K3,L3,K6,L6,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I3,I6,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder214356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K3,L3,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I4,I3,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder214365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K3,L3,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I4,I3,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder214536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K5,L5,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I4,I5,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder214563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K5,L5,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I4,I5,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder214635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K6,L6,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I4,I6,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder214653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K4,L4,K6,L6,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I4,I6,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder215346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K3,L3,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I5,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder215364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K3,L3,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I5,I3,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder215436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K4,L4,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I5,I4,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder215463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K4,L4,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I5,I4,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder215634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K6,L6,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I5,I6,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder215643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K5,L5,K6,L6,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I5,I6,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder216345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K3,L3,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I6,I3,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder216354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K3,L3,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I6,I3,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder216435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K4,L4,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I6,I4,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder216453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K4,L4,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I6,I4,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder216534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K5,L5,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I6,I5,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder216543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K1,L1,K6,L6,K5,L5,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I1,I6,I5,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder231456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K4,L4,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I1,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder231465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K4,L4,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I1,I4,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder231546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K5,L5,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I1,I5,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder231564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K5,L5,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I1,I5,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder231645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K6,L6,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I1,I6,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder231654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K1,L1,K6,L6,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I1,I6,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder234156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K1,L1,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I4,I1,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder234165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K1,L1,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I4,I1,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder234516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K5,L5,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I4,I5,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder234561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I4,I5,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder234615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K6,L6,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I4,I6,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder234651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K4,L4,K6,L6,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I4,I6,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder235146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K1,L1,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I5,I1,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder235164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K1,L1,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I5,I1,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder235416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K4,L4,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I5,I4,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder235461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K4,L4,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I5,I4,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder235614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K6,L6,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I5,I6,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder235641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K5,L5,K6,L6,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I5,I6,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder236145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K1,L1,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I6,I1,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder236154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K1,L1,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I6,I1,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder236415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K4,L4,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I6,I4,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder236451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K4,L4,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I6,I4,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder236514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K5,L5,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I6,I5,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder236541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K3,L3,K6,L6,K5,L5,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I3,I6,I5,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder241356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K3,L3,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I1,I3,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder241365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K3,L3,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I1,I3,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder241536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K5,L5,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I1,I5,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder241563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K5,L5,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I1,I5,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder241635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K6,L6,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I1,I6,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder241653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K1,L1,K6,L6,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I1,I6,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder243156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K1,L1,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I3,I1,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder243165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K1,L1,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I3,I1,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder243516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K5,L5,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I3,I5,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder243561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K5,L5,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I3,I5,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder243615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K6,L6,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I3,I6,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder243651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K3,L3,K6,L6,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I3,I6,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder245136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K1,L1,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I5,I1,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder245163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K1,L1,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I5,I1,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder245316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K3,L3,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I5,I3,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder245361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K3,L3,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I5,I3,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder245613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K6,L6,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I5,I6,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder245631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K5,L5,K6,L6,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I5,I6,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder246135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K1,L1,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I6,I1,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder246153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K1,L1,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I6,I1,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder246315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K3,L3,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I6,I3,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder246351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K3,L3,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I6,I3,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder246513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K5,L5,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I6,I5,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder246531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K4,L4,K6,L6,K5,L5,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I4,I6,I5,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder251346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K3,L3,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I1,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder251364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K3,L3,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I1,I3,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder251436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K4,L4,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I1,I4,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder251463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K4,L4,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I1,I4,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder251634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K6,L6,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I1,I6,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder251643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K1,L1,K6,L6,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I1,I6,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder253146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K1,L1,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I3,I1,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder253164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K1,L1,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I3,I1,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder253416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K4,L4,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I3,I4,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder253461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K4,L4,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I3,I4,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder253614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K6,L6,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I3,I6,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder253641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K3,L3,K6,L6,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I3,I6,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder254136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K1,L1,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I4,I1,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder254163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K1,L1,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I4,I1,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder254316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K3,L3,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I4,I3,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder254361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K3,L3,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I4,I3,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder254613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K6,L6,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I4,I6,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder254631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K4,L4,K6,L6,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I4,I6,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder256134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K1,L1,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I6,I1,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder256143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K1,L1,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I6,I1,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder256314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K3,L3,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I6,I3,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder256341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K3,L3,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I6,I3,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder256413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K4,L4,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I6,I4,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder256431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K5,L5,K6,L6,K4,L4,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I5,I6,I4,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder261345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K3,L3,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I1,I3,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder261354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K3,L3,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I1,I3,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder261435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K4,L4,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I1,I4,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder261453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K4,L4,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I1,I4,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder261534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K5,L5,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I1,I5,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder261543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K1,L1,K5,L5,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I1,I5,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder263145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K1,L1,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I3,I1,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder263154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K1,L1,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I3,I1,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder263415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K4,L4,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I3,I4,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder263451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K4,L4,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I3,I4,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder263514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K5,L5,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I3,I5,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder263541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K3,L3,K5,L5,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I3,I5,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder264135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K1,L1,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I4,I1,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder264153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K1,L1,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I4,I1,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder264315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K3,L3,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I4,I3,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder264351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K3,L3,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I4,I3,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder264513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K5,L5,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I4,I5,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder264531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K4,L4,K5,L5,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I4,I5,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder265134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K1,L1,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I5,I1,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder265143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K1,L1,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I5,I1,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder265314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K3,L3,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I5,I3,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder265341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K3,L3,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I5,I3,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder265413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K4,L4,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I5,I4,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder265431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K2,L2,K6,L6,K5,L5,K4,L4,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I2,I6,I5,I4,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder312456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K4,L4,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I2,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder312465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K4,L4,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I2,I4,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder312546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K5,L5,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I2,I5,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder312564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K5,L5,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I2,I5,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder312645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K6,L6,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I2,I6,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder312654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K2,L2,K6,L6,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I2,I6,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder314256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K2,L2,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I4,I2,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder314265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K2,L2,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I4,I2,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder314526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K5,L5,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I4,I5,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder314562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K5,L5,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I4,I5,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder314625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K6,L6,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I4,I6,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder314652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K4,L4,K6,L6,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I4,I6,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder315246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K2,L2,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I5,I2,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder315264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K2,L2,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I5,I2,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder315426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K4,L4,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I5,I4,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder315462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K4,L4,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I5,I4,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder315624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K6,L6,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I5,I6,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder315642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K5,L5,K6,L6,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I5,I6,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder316245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K2,L2,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I6,I2,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder316254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K2,L2,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I6,I2,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder316425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K4,L4,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I6,I4,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder316452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K4,L4,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I6,I4,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder316524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K5,L5,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I6,I5,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder316542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K1,L1,K6,L6,K5,L5,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I1,I6,I5,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder321456(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K4,L4,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I1,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder321465(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K4,L4,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I1,I4,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder321546(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K5,L5,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I1,I5,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder321564(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K5,L5,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I1,I5,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder321645(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K6,L6,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I1,I6,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder321654(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K1,L1,K6,L6,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I1,I6,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder324156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K1,L1,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I4,I1,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder324165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K1,L1,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I4,I1,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder324516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K5,L5,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I4,I5,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder324561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K5,L5,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I4,I5,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder324615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K6,L6,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I4,I6,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder324651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K4,L4,K6,L6,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I4,I6,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder325146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K1,L1,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I5,I1,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder325164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K1,L1,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I5,I1,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder325416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K4,L4,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I5,I4,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder325461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K4,L4,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I5,I4,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder325614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K6,L6,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I5,I6,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder325641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K5,L5,K6,L6,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I5,I6,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder326145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K1,L1,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I6,I1,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder326154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K1,L1,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I6,I1,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder326415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K4,L4,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I6,I4,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder326451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K4,L4,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I6,I4,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder326514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K5,L5,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I6,I5,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder326541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K2,L2,K6,L6,K5,L5,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I2,I6,I5,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder341256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K2,L2,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I1,I2,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder341265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K2,L2,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I1,I2,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder341526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K5,L5,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I1,I5,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder341562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K5,L5,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I1,I5,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder341625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K6,L6,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I1,I6,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder341652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K1,L1,K6,L6,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I1,I6,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder342156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K1,L1,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I2,I1,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder342165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K1,L1,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I2,I1,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder342516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K5,L5,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I2,I5,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder342561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K5,L5,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I2,I5,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder342615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K6,L6,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I2,I6,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder342651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K2,L2,K6,L6,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I2,I6,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder345126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K1,L1,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I5,I1,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder345162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K1,L1,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I5,I1,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder345216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K2,L2,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I5,I2,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder345261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K2,L2,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I5,I2,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder345612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K6,L6,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I5,I6,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder345621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K5,L5,K6,L6,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I5,I6,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder346125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K1,L1,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I6,I1,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder346152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K1,L1,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I6,I1,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder346215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K2,L2,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I6,I2,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder346251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K2,L2,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I6,I2,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder346512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K5,L5,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I6,I5,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder346521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K4,L4,K6,L6,K5,L5,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I4,I6,I5,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder351246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K2,L2,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I1,I2,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder351264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K2,L2,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I1,I2,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder351426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K4,L4,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I1,I4,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder351462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K4,L4,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I1,I4,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder351624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K6,L6,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I1,I6,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder351642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K1,L1,K6,L6,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I1,I6,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder352146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K1,L1,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I2,I1,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder352164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K1,L1,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I2,I1,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder352416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K4,L4,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I2,I4,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder352461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K4,L4,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I2,I4,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder352614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K6,L6,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I2,I6,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder352641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K2,L2,K6,L6,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I2,I6,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder354126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K1,L1,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I4,I1,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder354162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K1,L1,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I4,I1,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder354216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K2,L2,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I4,I2,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder354261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K2,L2,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I4,I2,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder354612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K6,L6,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I4,I6,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder354621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K4,L4,K6,L6,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I4,I6,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder356124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K1,L1,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I6,I1,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder356142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K1,L1,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I6,I1,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder356214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K2,L2,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I6,I2,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder356241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K2,L2,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I6,I2,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder356412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K4,L4,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I6,I4,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder356421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K5,L5,K6,L6,K4,L4,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I5,I6,I4,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder361245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K2,L2,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I1,I2,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder361254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K2,L2,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I1,I2,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder361425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K4,L4,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I1,I4,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder361452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K4,L4,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I1,I4,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder361524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K5,L5,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I1,I5,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder361542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K1,L1,K5,L5,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I1,I5,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder362145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K1,L1,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I2,I1,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder362154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K1,L1,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I2,I1,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder362415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K4,L4,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I2,I4,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder362451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K4,L4,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I2,I4,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder362514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K5,L5,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I2,I5,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder362541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K2,L2,K5,L5,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I2,I5,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder364125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K1,L1,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I4,I1,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder364152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K1,L1,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I4,I1,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder364215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K2,L2,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I4,I2,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder364251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K2,L2,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I4,I2,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder364512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K5,L5,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I4,I5,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder364521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K4,L4,K5,L5,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I4,I5,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder365124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K1,L1,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I5,I1,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder365142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K1,L1,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I5,I1,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder365214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K2,L2,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I5,I2,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder365241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K2,L2,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I5,I2,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder365412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K4,L4,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I5,I4,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder365421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K3,L3,K6,L6,K5,L5,K4,L4,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I3,I6,I5,I4,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder412356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K3,L3,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I2,I3,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder412365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K3,L3,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I2,I3,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder412536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K5,L5,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I2,I5,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder412563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K5,L5,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I2,I5,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder412635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K6,L6,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I2,I6,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder412653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K2,L2,K6,L6,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I2,I6,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder413256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K2,L2,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I3,I2,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder413265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K2,L2,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I3,I2,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder413526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K5,L5,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I3,I5,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder413562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K5,L5,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I3,I5,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder413625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K6,L6,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I3,I6,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder413652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K3,L3,K6,L6,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I3,I6,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder415236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K2,L2,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I5,I2,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder415263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K2,L2,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I5,I2,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder415326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K3,L3,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I5,I3,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder415362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K3,L3,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I5,I3,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder415623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K6,L6,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I5,I6,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder415632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K5,L5,K6,L6,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I5,I6,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder416235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K2,L2,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I6,I2,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder416253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K2,L2,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I6,I2,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder416325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K3,L3,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I6,I3,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder416352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K3,L3,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I6,I3,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder416523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K5,L5,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I6,I5,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder416532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K1,L1,K6,L6,K5,L5,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I1,I6,I5,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder421356(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K3,L3,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I1,I3,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder421365(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K3,L3,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I1,I3,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder421536(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K5,L5,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I1,I5,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder421563(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K5,L5,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I1,I5,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder421635(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K6,L6,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I1,I6,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder421653(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K1,L1,K6,L6,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I1,I6,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder423156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K1,L1,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I3,I1,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder423165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K1,L1,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I3,I1,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder423516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K5,L5,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I3,I5,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder423561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K5,L5,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I3,I5,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder423615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K6,L6,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I3,I6,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder423651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K3,L3,K6,L6,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I3,I6,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder425136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K1,L1,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I5,I1,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder425163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K1,L1,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I5,I1,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder425316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K3,L3,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I5,I3,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder425361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K3,L3,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I5,I3,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder425613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K6,L6,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I5,I6,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder425631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K5,L5,K6,L6,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I5,I6,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder426135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K1,L1,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I6,I1,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder426153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K1,L1,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I6,I1,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder426315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K3,L3,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I6,I3,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder426351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K3,L3,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I6,I3,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder426513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K5,L5,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I6,I5,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder426531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K2,L2,K6,L6,K5,L5,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I2,I6,I5,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder431256(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K2,L2,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I1,I2,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder431265(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K2,L2,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I1,I2,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder431526(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K5,L5,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I1,I5,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder431562(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K5,L5,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I1,I5,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder431625(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K6,L6,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I1,I6,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder431652(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K1,L1,K6,L6,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I1,I6,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder432156(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K1,L1,K5,L5,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I2,I1,I5,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder432165(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K1,L1,K6,L6,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I2,I1,I6,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder432516(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K5,L5,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I2,I5,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder432561(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K5,L5,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I2,I5,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder432615(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K6,L6,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I2,I6,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder432651(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K2,L2,K6,L6,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I2,I6,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder435126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K1,L1,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I5,I1,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder435162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K1,L1,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I5,I1,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder435216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K2,L2,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I5,I2,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder435261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K2,L2,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I5,I2,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder435612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K6,L6,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I5,I6,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder435621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K5,L5,K6,L6,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I5,I6,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder436125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K1,L1,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I6,I1,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder436152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K1,L1,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I6,I1,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder436215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K2,L2,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I6,I2,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder436251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K2,L2,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I6,I2,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder436512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K5,L5,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I6,I5,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder436521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K3,L3,K6,L6,K5,L5,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I3,I6,I5,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder451236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K2,L2,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I1,I2,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder451263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K2,L2,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I1,I2,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder451326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K3,L3,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I1,I3,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder451362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K3,L3,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I1,I3,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder451623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K6,L6,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I1,I6,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder451632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K1,L1,K6,L6,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I1,I6,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder452136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K1,L1,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I2,I1,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder452163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K1,L1,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I2,I1,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder452316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K3,L3,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I2,I3,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder452361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K3,L3,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I2,I3,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder452613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K6,L6,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I2,I6,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder452631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K2,L2,K6,L6,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I2,I6,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder453126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K1,L1,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I3,I1,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder453162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K1,L1,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I3,I1,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder453216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K2,L2,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I3,I2,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder453261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K2,L2,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I3,I2,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder453612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K6,L6,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I3,I6,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder453621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K3,L3,K6,L6,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I3,I6,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder456123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K1,L1,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I6,I1,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder456132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K1,L1,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I6,I1,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder456213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K2,L2,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I6,I2,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder456231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K2,L2,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I6,I2,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder456312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K3,L3,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I6,I3,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder456321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K5,L5,K6,L6,K3,L3,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I5,I6,I3,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder461235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K2,L2,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I1,I2,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder461253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K2,L2,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I1,I2,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder461325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K3,L3,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I1,I3,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder461352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K3,L3,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I1,I3,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder461523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K5,L5,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I1,I5,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder461532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K1,L1,K5,L5,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I1,I5,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder462135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K1,L1,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I2,I1,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder462153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K1,L1,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I2,I1,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder462315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K3,L3,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I2,I3,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder462351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K3,L3,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I2,I3,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder462513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K5,L5,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I2,I5,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder462531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K2,L2,K5,L5,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I2,I5,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder463125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K1,L1,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I3,I1,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder463152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K1,L1,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I3,I1,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder463215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K2,L2,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I3,I2,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder463251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K2,L2,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I3,I2,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder463512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K5,L5,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I3,I5,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder463521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K3,L3,K5,L5,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I3,I5,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder465123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K1,L1,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I5,I1,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder465132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K1,L1,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I5,I1,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder465213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K2,L2,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I5,I2,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder465231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K2,L2,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I5,I2,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder465312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K3,L3,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I5,I3,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder465321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K4,L4,K6,L6,K5,L5,K3,L3,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I4,I6,I5,I3,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder512346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K3,L3,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I2,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder512364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K3,L3,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I2,I3,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder512436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K4,L4,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I2,I4,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder512463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K4,L4,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I2,I4,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder512634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K6,L6,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I2,I6,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder512643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K2,L2,K6,L6,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I2,I6,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder513246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K2,L2,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I3,I2,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder513264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K2,L2,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I3,I2,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder513426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K4,L4,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I3,I4,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder513462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K4,L4,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I3,I4,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder513624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K6,L6,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I3,I6,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder513642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K3,L3,K6,L6,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I3,I6,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder514236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K2,L2,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I4,I2,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder514263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K2,L2,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I4,I2,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder514326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K3,L3,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I4,I3,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder514362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K3,L3,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I4,I3,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder514623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K6,L6,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I4,I6,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder514632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K4,L4,K6,L6,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I4,I6,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder516234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K2,L2,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I6,I2,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder516243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K2,L2,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I6,I2,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder516324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K3,L3,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I6,I3,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder516342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K3,L3,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I6,I3,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder516423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K4,L4,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I6,I4,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder516432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K1,L1,K6,L6,K4,L4,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I1,I6,I4,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder521346(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K3,L3,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I1,I3,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder521364(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K3,L3,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I1,I3,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder521436(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K4,L4,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I1,I4,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder521463(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K4,L4,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I1,I4,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder521634(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K6,L6,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I1,I6,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder521643(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K1,L1,K6,L6,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I1,I6,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder523146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K1,L1,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I3,I1,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder523164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K1,L1,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I3,I1,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder523416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K4,L4,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I3,I4,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder523461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K4,L4,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I3,I4,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder523614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K6,L6,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I3,I6,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder523641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K3,L3,K6,L6,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I3,I6,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder524136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K1,L1,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I4,I1,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder524163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K1,L1,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I4,I1,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder524316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K3,L3,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I4,I3,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder524361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K3,L3,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I4,I3,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder524613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K6,L6,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I4,I6,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder524631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K4,L4,K6,L6,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I4,I6,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder526134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K1,L1,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I6,I1,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder526143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K1,L1,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I6,I1,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder526314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K3,L3,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I6,I3,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder526341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K3,L3,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I6,I3,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder526413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K4,L4,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I6,I4,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder526431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K2,L2,K6,L6,K4,L4,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I2,I6,I4,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder531246(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K2,L2,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I1,I2,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder531264(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K2,L2,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I1,I2,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder531426(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K4,L4,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I1,I4,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder531462(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K4,L4,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I1,I4,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder531624(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K6,L6,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I1,I6,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder531642(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K1,L1,K6,L6,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I1,I6,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder532146(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K1,L1,K4,L4,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I2,I1,I4,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder532164(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K1,L1,K6,L6,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I2,I1,I6,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder532416(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K4,L4,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I2,I4,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder532461(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K4,L4,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I2,I4,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder532614(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K6,L6,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I2,I6,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder532641(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K2,L2,K6,L6,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I2,I6,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder534126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K1,L1,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I4,I1,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder534162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K1,L1,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I4,I1,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder534216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K2,L2,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I4,I2,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder534261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K2,L2,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I4,I2,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder534612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K6,L6,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I4,I6,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder534621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K4,L4,K6,L6,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I4,I6,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder536124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K1,L1,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I6,I1,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder536142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K1,L1,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I6,I1,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder536214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K2,L2,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I6,I2,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder536241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K2,L2,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I6,I2,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder536412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K4,L4,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I6,I4,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder536421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K3,L3,K6,L6,K4,L4,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I3,I6,I4,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder541236(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K2,L2,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I1,I2,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder541263(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K2,L2,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I1,I2,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder541326(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K3,L3,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I1,I3,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder541362(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K3,L3,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I1,I3,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder541623(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K6,L6,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I1,I6,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder541632(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K1,L1,K6,L6,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I1,I6,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder542136(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K1,L1,K3,L3,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I2,I1,I3,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder542163(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K1,L1,K6,L6,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I2,I1,I6,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder542316(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K3,L3,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I2,I3,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder542361(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K3,L3,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I2,I3,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder542613(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K6,L6,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I2,I6,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder542631(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K2,L2,K6,L6,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I2,I6,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder543126(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K1,L1,K2,L2,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I3,I1,I2,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder543162(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K1,L1,K6,L6,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I3,I1,I6,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder543216(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K2,L2,K1,L1,K6,L6,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I3,I2,I1,I6)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder543261(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K2,L2,K6,L6,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I3,I2,I6,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder543612(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K6,L6,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I3,I6,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder543621(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K3,L3,K6,L6,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I3,I6,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder546123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K1,L1,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I6,I1,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder546132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K1,L1,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I6,I1,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder546213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K2,L2,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I6,I2,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder546231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K2,L2,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I6,I2,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder546312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K3,L3,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I6,I3,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder546321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K4,L4,K6,L6,K3,L3,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I4,I6,I3,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder561234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K2,L2,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I1,I2,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder561243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K2,L2,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I1,I2,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder561324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K3,L3,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I1,I3,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder561342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K3,L3,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I1,I3,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder561423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K4,L4,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I1,I4,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder561432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K1,L1,K4,L4,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I1,I4,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder562134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K1,L1,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I2,I1,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder562143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K1,L1,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I2,I1,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder562314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K3,L3,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I2,I3,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder562341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K3,L3,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I2,I3,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder562413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K4,L4,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I2,I4,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder562431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K2,L2,K4,L4,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I2,I4,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder563124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K1,L1,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I3,I1,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder563142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K1,L1,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I3,I1,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder563214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K2,L2,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I3,I2,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder563241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K2,L2,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I3,I2,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder563412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K4,L4,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I3,I4,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder563421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K3,L3,K4,L4,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I3,I4,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder564123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K1,L1,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I4,I1,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder564132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K1,L1,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I4,I1,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder564213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K2,L2,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I4,I2,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder564231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K2,L2,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I4,I2,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder564312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K3,L3,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I4,I3,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder564321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K5,L5,K6,L6,K4,L4,K3,L3,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I5,I6,I4,I3,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder612345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I2,I3,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder612354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K3,L3,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I2,I3,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder612435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K4,L4,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I2,I4,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder612453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K4,L4,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I2,I4,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder612534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K5,L5,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I2,I5,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder612543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K2,L2,K5,L5,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I2,I5,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder613245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K2,L2,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I3,I2,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder613254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K2,L2,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I3,I2,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder613425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K4,L4,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I3,I4,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder613452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K4,L4,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I3,I4,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder613524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K5,L5,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I3,I5,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder613542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K3,L3,K5,L5,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I3,I5,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder614235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K2,L2,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I4,I2,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder614253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K2,L2,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I4,I2,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder614325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K3,L3,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I4,I3,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder614352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K3,L3,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I4,I3,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder614523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K5,L5,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I4,I5,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder614532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K4,L4,K5,L5,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I4,I5,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder615234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K2,L2,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I5,I2,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder615243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K2,L2,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I5,I2,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder615324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K3,L3,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I5,I3,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder615342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K3,L3,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I5,I3,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder615423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K4,L4,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I5,I4,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder615432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K1,L1,K5,L5,K4,L4,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I1,I5,I4,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder621345(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K3,L3,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I1,I3,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder621354(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K3,L3,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I1,I3,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder621435(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K4,L4,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I1,I4,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder621453(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K4,L4,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I1,I4,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder621534(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K5,L5,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I1,I5,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder621543(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K1,L1,K5,L5,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I1,I5,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder623145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K1,L1,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I3,I1,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder623154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K1,L1,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I3,I1,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder623415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K4,L4,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I3,I4,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder623451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K4,L4,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I3,I4,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder623514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K5,L5,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I3,I5,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder623541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K3,L3,K5,L5,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I3,I5,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder624135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K1,L1,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I4,I1,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder624153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K1,L1,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I4,I1,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder624315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K3,L3,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I4,I3,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder624351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K3,L3,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I4,I3,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder624513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K5,L5,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I4,I5,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder624531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K4,L4,K5,L5,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I4,I5,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder625134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K1,L1,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I5,I1,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder625143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K1,L1,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I5,I1,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder625314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K3,L3,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I5,I3,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder625341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K3,L3,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I5,I3,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder625413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K4,L4,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I5,I4,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder625431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K2,L2,K5,L5,K4,L4,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I2,I5,I4,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder631245(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K2,L2,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I1,I2,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder631254(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K2,L2,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I1,I2,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder631425(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K4,L4,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I1,I4,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder631452(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K4,L4,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I1,I4,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder631524(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K5,L5,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I1,I5,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder631542(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K1,L1,K5,L5,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I1,I5,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder632145(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K1,L1,K4,L4,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I2,I1,I4,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder632154(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K1,L1,K5,L5,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I2,I1,I5,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder632415(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K4,L4,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I2,I4,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder632451(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K4,L4,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I2,I4,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder632514(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K5,L5,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I2,I5,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder632541(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K2,L2,K5,L5,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I2,I5,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder634125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K1,L1,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I4,I1,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder634152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K1,L1,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I4,I1,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder634215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K2,L2,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I4,I2,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder634251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K2,L2,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I4,I2,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder634512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K5,L5,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I4,I5,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder634521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K4,L4,K5,L5,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I4,I5,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder635124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K1,L1,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I5,I1,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder635142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K1,L1,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I5,I1,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder635214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K2,L2,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I5,I2,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder635241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K2,L2,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I5,I2,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder635412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K4,L4,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I5,I4,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder635421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K3,L3,K5,L5,K4,L4,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I3,I5,I4,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder641235(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K2,L2,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I1,I2,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder641253(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K2,L2,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I1,I2,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder641325(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K3,L3,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I1,I3,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder641352(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K3,L3,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I1,I3,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder641523(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K5,L5,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I1,I5,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder641532(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K1,L1,K5,L5,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I1,I5,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder642135(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K1,L1,K3,L3,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I2,I1,I3,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder642153(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K1,L1,K5,L5,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I2,I1,I5,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder642315(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K3,L3,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I2,I3,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder642351(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K3,L3,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I2,I3,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder642513(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K5,L5,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I2,I5,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder642531(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K2,L2,K5,L5,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I2,I5,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder643125(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K1,L1,K2,L2,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I3,I1,I2,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder643152(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K1,L1,K5,L5,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I3,I1,I5,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder643215(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K2,L2,K1,L1,K5,L5,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I3,I2,I1,I5)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder643251(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K2,L2,K5,L5,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I3,I2,I5,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder643512(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K5,L5,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I3,I5,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder643521(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K3,L3,K5,L5,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I3,I5,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder645123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K1,L1,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I5,I1,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder645132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K1,L1,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I5,I1,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder645213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K2,L2,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I5,I2,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder645231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K2,L2,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I5,I2,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder645312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K3,L3,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I5,I3,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder645321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K4,L4,K5,L5,K3,L3,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I4,I5,I3,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder651234(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K2,L2,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I1,I2,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder651243(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K2,L2,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I1,I2,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder651324(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K3,L3,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I1,I3,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder651342(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K3,L3,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I1,I3,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder651423(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K4,L4,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I1,I4,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder651432(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K1,L1,K4,L4,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I1,I4,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder652134(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K1,L1,K3,L3,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I2,I1,I3,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder652143(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K1,L1,K4,L4,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I2,I1,I4,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder652314(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K3,L3,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I2,I3,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder652341(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K3,L3,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I2,I3,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder652413(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K4,L4,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I2,I4,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder652431(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K2,L2,K4,L4,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I2,I4,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder653124(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K1,L1,K2,L2,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I3,I1,I2,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder653142(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K1,L1,K4,L4,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I3,I1,I4,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder653214(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K2,L2,K1,L1,K4,L4,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I3,I2,I1,I4)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder653241(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K2,L2,K4,L4,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I3,I2,I4,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder653412(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K4,L4,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I3,I4,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder653421(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K3,L3,K4,L4,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I3,I4,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder654123(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K1,L1,K2,L2,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I4,I1,I2,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder654132(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K1,L1,K3,L3,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I4,I1,I3,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder654213(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K2,L2,K1,L1,K3,L3,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I4,I2,I1,I3)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder654231(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K2,L2,K3,L3,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I4,I2,I3,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder654312(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K3,L3,K1,L1,K2,L2,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I4,I3,I1,I2)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

       subroutine old1reorder654321(M1,N1,M2,N2,M3,N3,M4,N4,M5,N5,M6,N6,
     & K6,L6,K5,L5,K4,L4,K3,L3,K2,L2,K1,L1,A,B)

       real*8 A(M1+1:N1,M2+1:N2,M3+1:N3,M4+1:N4,M5+1:N5,M6+1:N6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)

       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        B(I6,I5,I4,I3,I2,I1)=A(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo

       end

