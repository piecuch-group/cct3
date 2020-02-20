       subroutine calculate_energy(N1,N2,N3,N0,
     & FAHP,FBHP,VAHHPP,VBHHPP,VCHHPP,
     & t_order, t_pos, t_up_2,
     & E1A,E1B,E2A,E2B,E2C,E1A1A,E1A1B,E1B1B)

      integer e,f,m,n
      real*8 E1A,E1B,E2A,E2B,E2C,E1A1A,E1B1B,E1A1B

      real*8 FAHP(N1+1:N3,N0+1:N1)
      real*8 FBHP(N2+1:N3,N0+1:N2)
      real*8 VAHHPP(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
      real*8 VBHHPP(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
      real*8 VCHHPP(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
      real*8 t1A(N1+1:N3,N0+1:N1)
      real*8 t1B(N2+1:N3,N0+1:N2)
      real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
      real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
      real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)

      integer t_order
      integer t_pos(t_order + 1)

      real*8 t_up_2(t_pos(6) - 1)

      t1a = reshape(t_up_2(t_pos(1):t_pos(2)-1), (/n3-n1,n1-n0/))
      t1b = reshape(t_up_2(t_pos(2):t_pos(3)-1), (/n3-n2,n2-n0/))
      t2a = reshape(t_up_2(t_pos(3):t_pos(4)-1),
     & (/n3-n1, n3-n1, n1-n0, n1-n0/))
      t2b = reshape(t_up_2(t_pos(4):t_pos(5)-1),
     & (/n3-n2, n3-n1, n2-n0, n1-n0/))
      t2c = reshape(t_up_2(t_pos(5):t_pos(6)-1),
     & (/n3-n2, n3-n2, n2-n0, n2-n0/))

! Caluculate E2A,E2B,E2C
      E2A=0.0d0
      E2B=0.0d0
      E2C=0.0d0
      do m=N0+1,N1
        do n=N0+1,N1
          do e=N1+1,N3
            do f=N1+1,N3
              E2A=E2A+0.25*VAHHPP(f,e,n,m)*t2A(f,e,n,m)
            enddo
          enddo
        enddo
      enddo
      do m=N0+1,N1
        do n=N0+1,N2
          do e=N1+1,N3
            do f=N2+1,N3
              E2B=E2B+VBHHPP(f,e,n,m)*t2B(f,e,n,m)
            enddo
          enddo
        enddo
      enddo
      do m=N0+1,N2
        do n=N0+1,N2
          do e=N2+1,N3
            do f=N2+1,N3
              E2C=E2C+0.25*VCHHPP(f,e,n,m)*t2C(f,e,n,m)
            enddo
          enddo
        enddo
      enddo

C  Calculate E1A,E1B
      E1A=0.0d0
      E1B=0.0d0
      do e= N1+1,N3
        do m= N0+1,N1
          E1A=E1A+FAHP(e,m)*t1A(e,m)
        enddo
      enddo
      do e= N2+1,N3
        do m= N0+1,N2
          E1B=E1B+FBHP(e,m)*t1B(e,m)
        enddo
      enddo

! Caluculate E1A1A,E1A1B,E1B1B
      E1A1A=0.0d0
      E1A1B=0.0d0
      E1B1B=0.0d0
      do m=N0+1,N1
        do n=N0+1,N1
          do e=N1+1,N3
            do f=N1+1,N3
              E1A1A=E1A1A+0.50*VAHHPP(f,e,n,m)*t1A(f,n)*t1A(e,m)
            enddo
          enddo
        enddo
      enddo
      do m=N0+1,N1
        do n=N0+1,N2
          do e=N1+1,N3
            do f=N2+1,N3
              E1A1B=E1A1B+VBHHPP(f,e,n,m)*t1A(e,m)*t1B(f,n)
            enddo
          enddo
        enddo
      enddo
      do m=N0+1,N2
        do n=N0+1,N2
          do e=N2+1,N3
            do f=N2+1,N3
              E1B1B=E1B1B+0.50*VCHHPP(f,e,n,m)*t1B(f,n)*t1B(e,m)
            enddo
          enddo
        enddo
      enddo

      end
