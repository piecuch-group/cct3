       subroutine old1sum12(K1,L1,K2,L2,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2)
       real*8 B(K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
        A(I1,I2)=A(I1,I2)+C*B(I1,I2)
       enddo
       enddo
C
       end
C
       subroutine old1sum21(K1,L1,K2,L2,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2)
       real*8 B(K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
        A(I1,I2)=A(I1,I2)+C*B(I2,I1)
       enddo
       enddo
C
       end
C
       subroutine old1sum1234(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum1243(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I1,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum1324(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I1,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum1342(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I1,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum1423(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I1,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum1432(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I1,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum2134(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I2,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum2143(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I2,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum2314(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I2,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum2341(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I2,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum2413(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I2,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum2431(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I2,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum3124(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I3,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum3142(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I3,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum3214(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I3,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum3241(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I3,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum3412(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I3,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum3421(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I3,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum4123(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I4,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum4132(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I4,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum4213(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I4,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum4231(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I4,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum4312(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I4,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum4321(K1,L1,K2,L2,K3,L3,K4,L4,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
        A(I1,I2,I3,I4)=A(I1,I2,I3,I4)+C*B(I4,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum123456(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum123465(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum123546(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum123564(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum123645(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum123654(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I3,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum124356(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum124365(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum124536(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum124563(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum124635(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum124653(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I4,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum125346(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum125364(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum125436(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum125463(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum125634(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum125643(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I5,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum126345(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum126354(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum126435(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum126453(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum126534(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum126543(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I2,I6,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum132456(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum132465(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum132546(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum132564(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum132645(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum132654(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I2,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum134256(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum134265(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum134526(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum134562(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum134625(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum134652(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I4,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum135246(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum135264(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum135426(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum135462(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum135624(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum135642(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I5,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum136245(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum136254(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum136425(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum136452(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum136524(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum136542(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I3,I6,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum142356(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum142365(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum142536(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum142563(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum142635(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum142653(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I2,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum143256(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum143265(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum143526(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum143562(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum143625(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum143652(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I3,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum145236(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum145263(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum145326(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum145362(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum145623(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum145632(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I5,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum146235(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum146253(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum146325(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum146352(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum146523(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum146532(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I4,I6,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum152346(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum152364(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum152436(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum152463(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum152634(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum152643(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I2,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum153246(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum153264(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum153426(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum153462(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum153624(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum153642(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I3,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum154236(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum154263(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum154326(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum154362(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum154623(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum154632(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I4,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum156234(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum156243(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum156324(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum156342(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum156423(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum156432(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I5,I6,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum162345(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum162354(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum162435(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum162453(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum162534(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum162543(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I2,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum163245(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum163254(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum163425(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum163452(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum163524(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum163542(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I3,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum164235(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum164253(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum164325(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum164352(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum164523(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum164532(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I4,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum165234(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum165243(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum165324(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum165342(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum165423(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum165432(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I1,I6,I5,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum213456(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum213465(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum213546(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum213564(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum213645(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum213654(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I3,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum214356(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum214365(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum214536(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum214563(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum214635(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum214653(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I4,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum215346(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum215364(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum215436(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum215463(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum215634(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum215643(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I5,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum216345(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum216354(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum216435(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum216453(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum216534(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum216543(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I1,I6,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum231456(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum231465(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum231546(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum231564(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum231645(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum231654(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I1,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum234156(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum234165(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum234516(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum234561(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum234615(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum234651(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I4,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum235146(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum235164(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum235416(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum235461(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum235614(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum235641(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I5,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum236145(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum236154(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum236415(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum236451(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum236514(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum236541(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I3,I6,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum241356(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum241365(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum241536(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum241563(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum241635(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum241653(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I1,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum243156(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum243165(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum243516(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum243561(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum243615(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum243651(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I3,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum245136(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum245163(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum245316(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum245361(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum245613(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum245631(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I5,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum246135(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum246153(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum246315(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum246351(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum246513(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum246531(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I4,I6,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum251346(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum251364(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum251436(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum251463(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum251634(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum251643(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I1,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum253146(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum253164(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum253416(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum253461(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum253614(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum253641(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I3,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum254136(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum254163(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum254316(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum254361(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum254613(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum254631(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I4,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum256134(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum256143(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum256314(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum256341(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum256413(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum256431(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I5,I6,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum261345(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum261354(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum261435(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum261453(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum261534(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum261543(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I1,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum263145(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum263154(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum263415(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum263451(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum263514(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum263541(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I3,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum264135(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum264153(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum264315(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum264351(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum264513(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum264531(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I4,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum265134(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum265143(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum265314(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum265341(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum265413(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum265431(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I2,I6,I5,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum312456(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum312465(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum312546(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum312564(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum312645(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum312654(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I2,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum314256(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum314265(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum314526(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum314562(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum314625(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum314652(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I4,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum315246(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum315264(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum315426(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum315462(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum315624(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum315642(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I5,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum316245(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum316254(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum316425(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum316452(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum316524(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum316542(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I1,I6,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum321456(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I4,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum321465(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I4,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum321546(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I5,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum321564(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I5,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum321645(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I6,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum321654(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I1,I6,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum324156(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum324165(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum324516(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum324561(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum324615(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum324651(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I4,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum325146(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum325164(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum325416(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum325461(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum325614(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum325641(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I5,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum326145(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum326154(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum326415(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum326451(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum326514(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum326541(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I2,I6,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum341256(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum341265(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum341526(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum341562(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum341625(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum341652(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I1,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum342156(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum342165(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum342516(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum342561(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum342615(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum342651(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I2,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum345126(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum345162(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum345216(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum345261(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum345612(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum345621(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I5,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum346125(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum346152(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum346215(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum346251(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum346512(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum346521(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I4,I6,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum351246(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum351264(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum351426(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum351462(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum351624(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum351642(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I1,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum352146(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum352164(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum352416(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum352461(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum352614(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum352641(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I2,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum354126(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum354162(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum354216(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum354261(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum354612(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum354621(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I4,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum356124(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum356142(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum356214(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum356241(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum356412(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum356421(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I5,I6,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum361245(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum361254(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum361425(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum361452(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum361524(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum361542(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I1,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum362145(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum362154(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum362415(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum362451(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum362514(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum362541(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I2,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum364125(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum364152(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum364215(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum364251(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum364512(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum364521(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I4,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum365124(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum365142(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum365214(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum365241(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum365412(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum365421(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K3+1:L3,K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I3,I6,I5,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum412356(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum412365(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum412536(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum412563(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum412635(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum412653(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I2,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum413256(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum413265(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum413526(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum413562(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum413625(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum413652(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I3,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum415236(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum415263(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum415326(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum415362(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum415623(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum415632(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I5,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum416235(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum416253(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum416325(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum416352(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum416523(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum416532(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I1,I6,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum421356(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I3,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum421365(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I3,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum421536(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I5,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum421563(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I5,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum421635(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I6,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum421653(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I1,I6,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum423156(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum423165(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum423516(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum423561(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum423615(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum423651(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I3,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum425136(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum425163(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum425316(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum425361(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum425613(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum425631(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I5,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum426135(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum426153(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum426315(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum426351(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum426513(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum426531(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K2+1:L2,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I2,I6,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum431256(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I2,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum431265(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I2,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum431526(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I5,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum431562(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I5,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum431625(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I6,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum431652(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I1,I6,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum432156(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I1,I5,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum432165(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I1,I6,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum432516(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I5,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum432561(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I5,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum432615(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I6,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum432651(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I2,I6,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum435126(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum435162(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum435216(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum435261(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum435612(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum435621(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I5,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum436125(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum436152(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum436215(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum436251(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum436512(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum436521(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K3+1:L3,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I3,I6,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum451236(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum451263(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum451326(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum451362(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum451623(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum451632(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I1,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum452136(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum452163(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum452316(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum452361(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum452613(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum452631(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I2,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum453126(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum453162(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum453216(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum453261(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum453612(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum453621(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I3,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum456123(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum456132(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum456213(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum456231(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum456312(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum456321(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I5,I6,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum461235(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum461253(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum461325(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum461352(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum461523(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum461532(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I1,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum462135(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum462153(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum462315(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum462351(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum462513(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum462531(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I2,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum463125(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum463152(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum463215(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum463251(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum463512(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum463521(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I3,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum465123(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum465132(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum465213(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum465231(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum465312(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum465321(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K4+1:L4,K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I4,I6,I5,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum512346(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum512364(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum512436(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum512463(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum512634(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum512643(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I2,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum513246(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum513264(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum513426(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum513462(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum513624(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum513642(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I3,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum514236(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum514263(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum514326(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum514362(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum514623(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum514632(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I4,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum516234(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum516243(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum516324(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum516342(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum516423(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum516432(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I1,I6,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum521346(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I3,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum521364(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I3,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum521436(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I4,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum521463(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I4,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum521634(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I6,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum521643(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I1,I6,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum523146(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum523164(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum523416(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum523461(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum523614(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum523641(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I3,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum524136(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum524163(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum524316(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum524361(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum524613(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum524631(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I4,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum526134(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum526143(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum526314(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum526341(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum526413(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum526431(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K2+1:L2,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I2,I6,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum531246(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I2,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum531264(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I2,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum531426(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I4,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum531462(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I4,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum531624(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I6,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum531642(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K1+1:L1,K6+1:L6,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I1,I6,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum532146(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I1,I4,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum532164(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I1,I6,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum532416(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I4,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum532461(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I4,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum532614(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I6,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum532641(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K2+1:L2,K6+1:L6,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I2,I6,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum534126(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum534162(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum534216(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum534261(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum534612(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum534621(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I4,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum536124(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum536142(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum536214(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum536241(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum536412(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum536421(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K3+1:L3,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I3,I6,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum541236(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I2,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum541263(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I2,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum541326(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I3,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum541362(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I3,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum541623(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I6,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum541632(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K1+1:L1,K6+1:L6,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I1,I6,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum542136(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I1,I3,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum542163(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K6+1:L6,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I1,I6,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum542316(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I3,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum542361(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I3,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum542613(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I6,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum542631(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K2+1:L2,K6+1:L6,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I2,I6,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum543126(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I1,I2,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum543162(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K6+1:L6,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I1,I6,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum543216(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K6+1:L6)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I2,I1,I6)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum543261(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K6+1:L6,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I2,I6,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum543612(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I6,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum543621(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K3+1:L3,K6+1:L6,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I3,I6,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum546123(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum546132(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum546213(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum546231(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum546312(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum546321(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K4+1:L4,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I4,I6,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum561234(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum561243(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum561324(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum561342(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum561423(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum561432(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I1,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum562134(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum562143(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum562314(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum562341(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum562413(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum562431(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I2,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum563124(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum563142(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum563214(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum563241(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum563412(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum563421(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I3,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum564123(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum564132(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum564213(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum564231(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum564312(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum564321(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K5+1:L5,K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I5,I6,I4,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum612345(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum612354(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum612435(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum612453(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum612534(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum612543(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I2,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum613245(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum613254(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum613425(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum613452(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum613524(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum613542(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I3,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum614235(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum614253(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum614325(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum614352(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum614523(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum614532(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I4,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum615234(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum615243(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum615324(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum615342(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum615423(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum615432(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I1,I5,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum621345(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I3,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum621354(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I3,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum621435(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I4,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum621453(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I4,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum621534(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I5,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum621543(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I1,I5,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum623145(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum623154(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum623415(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum623451(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum623514(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum623541(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I3,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum624135(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum624153(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum624315(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum624351(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum624513(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum624531(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I4,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum625134(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum625143(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum625314(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum625341(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum625413(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum625431(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K2+1:L2,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I2,I5,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum631245(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I2,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum631254(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I2,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum631425(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I4,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum631452(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K4+1:L4,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I4,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum631524(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I5,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum631542(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K1+1:L1,K5+1:L5,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I1,I5,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum632145(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I1,I4,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum632154(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I1,I5,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum632415(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I4,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum632451(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K4+1:L4,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I4,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum632514(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I5,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum632541(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K2+1:L2,K5+1:L5,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I2,I5,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum634125(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum634152(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum634215(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum634251(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum634512(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum634521(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I4,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum635124(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum635142(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum635214(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum635241(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum635412(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum635421(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K3+1:L3,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I3,I5,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum641235(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I2,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum641253(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K2+1:L2,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I2,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum641325(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I3,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum641352(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K3+1:L3,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I3,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum641523(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I5,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum641532(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K1+1:L1,K5+1:L5,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I1,I5,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum642135(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I1,I3,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum642153(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K1+1:L1,K5+1:L5,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I1,I5,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum642315(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I3,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum642351(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K3+1:L3,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I3,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum642513(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I5,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum642531(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K2+1:L2,K5+1:L5,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I2,I5,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum643125(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I1,I2,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum643152(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K1+1:L1,K5+1:L5,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I1,I5,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum643215(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1,K5+1:L5)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I2,I1,I5)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum643251(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K2+1:L2,K5+1:L5,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I2,I5,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum643512(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I5,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum643521(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K3+1:L3,K5+1:L5,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I3,I5,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum645123(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum645132(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum645213(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum645231(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum645312(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum645321(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K4+1:L4,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I4,I5,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum651234(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I2,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum651243(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K2+1:L2,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I2,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum651324(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I3,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum651342(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K3+1:L3,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I3,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum651423(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I4,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum651432(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K1+1:L1,K4+1:L4,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I1,I4,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum652134(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K3+1:L3,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I1,I3,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum652143(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K1+1:L1,K4+1:L4,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I1,I4,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum652314(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I3,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum652341(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K3+1:L3,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I3,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum652413(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I4,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum652431(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K2+1:L2,K4+1:L4,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I2,I4,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum653124(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K2+1:L2,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I1,I2,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum653142(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K1+1:L1,K4+1:L4,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I1,I4,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum653214(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K1+1:L1,K4+1:L4)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I2,I1,I4)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum653241(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K2+1:L2,K4+1:L4,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I2,I4,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum653412(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I4,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum653421(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K3+1:L3,K4+1:L4,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I3,I4,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum654123(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K2+1:L2,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I1,I2,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum654132(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K1+1:L1,K3+1:L3,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I1,I3,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum654213(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K1+1:L1,K3+1:L3)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I2,I1,I3)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum654231(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K2+1:L2,K3+1:L3,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I2,I3,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum654312(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K1+1:L1,K2+1:L2)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I3,I1,I2)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
       subroutine old1sum654321(K1,L1,K2,L2,K3,L3,K4,L4,K5,L5,K6,L6,A,B,C)
C
       real*8 A(K1+1:L1,K2+1:L2,K3+1:L3,K4+1:L4,K5+1:L5,K6+1:L6)
       real*8 B(K6+1:L6,K5+1:L5,K4+1:L4,K3+1:L3,K2+1:L2,K1+1:L1)
       real*8 C
C
       do I1=K1+1,L1
       do I2=K2+1,L2
       do I3=K3+1,L3
       do I4=K4+1,L4
       do I5=K5+1,L5
       do I6=K6+1,L6
        A(I1,I2,I3,I4,I5,I6)=A(I1,I2,I3,I4,I5,I6)+C*B(I6,I5,I4,I3,I2,I1)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
C
       end
C
