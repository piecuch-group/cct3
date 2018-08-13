subroutine print_header()
    character(len=255) :: hostname
    character(len=255) :: cmd
    character(len=37) :: uuid
    integer(kind=4) :: stat
    character(len=800) :: io

    write(io,'(a)') 'UCCSD building test'
    call print_io(io)
    write(io,'(a)') '==================='
    call print_io(io)

    write(io,'(a)') 'Compilation information'
    write(io,'(a)') '-----------------------'

#ifdef VERSION
    write(io,'(2x,a20,1x,a)') 'Git SHA', &
     VERSION
#endif

    write(io,'(a)') 'Host information'
    write(io,'(a)') '----------------'

end subroutine print_header

subroutine print_calc_params(froz, occ_a, occ_b, total, &
        shift, itol, &
        eref, erepul, &
        diis_space, restart, maxiter, &
        label)

    integer, intent(in) :: froz, occ_a, occ_b, total
    integer, intent(in) :: itol, diis_space
    logical, intent(in) :: restart
    character(len=*) :: label
    real(kind=8), intent(in) :: eref, erepul, shift
    character(len=800) :: io

    write(io,'(a)') 'System information'
    call print_io(io)
    write(io,'(a)') '------------------'
    if (trim(label) /= '') then
        write(io,'(2x,a27,2x,a)') 'Label', trim(label)
    endif
    call print_io(io)
    write(io,'(2x,a27,2x,i16)') 'Frozen orbitals', froz
    call print_io(io)
    write(io,'(2x,a27,2x,i16)') 'Occupied orbitals (alpha)',occ_a
    call print_io(io)
    write(io,'(2x,a27,2x,i16)') 'Occupied orbitals (beta)', occ_b
    call print_io(io)
    write(io,'(2x,a27,2x,i16)') 'Total orbitals', total
    call print_io(io)

    !      write(io,'(2x,a27,2x,i16)')
    !    &  'Occupied active (guess)', m3
    !      write(io,'(2x,a27,2x,i16)')
    !    &  'Unoccupied active (guess)', m4
    !      write(io,'(2x,a27,2x,i16)')
    !    &  'Occupied active', m1
    !      write(io,'(2x,a27,2x,i16)')
    !    &  'Unoccupied active', m2

    write(io,'(a)') 'CC settings'
    call print_io(io)
    write(io,'(a)')  '-----------'
    call print_io(io)
    !      write(io,'(2x,a27,2x,i16)') 'Number of excited states', nroot
    write(io,'(2x,a27,2x,es16.2)') 'Convergence tolerance', 1.0d1 ** (-itol)
    call print_io(io)
    write(io,'(2x,a27,2x,i16)') 'Max. iterations', maxiter
    call print_io(io)
    write(io,'(2x,a27,2x,i16)') 'DIIS space',diis_space
    call print_io(io)
    write(io,'(2x,a27,2x,l16)') 'Restart', restart
    call print_io(io)
    write(io,'(2x,a27,2x,es16.4)') 'Shift energy', shift
    call print_io(io)

    write(io,'(a)') 'Starting energies (Eh)'
    call print_io(io)
    write(io,'(a)')  '----------------------'
    call print_io(io)
    write(io,'(2x,a27,2x,f16.10)') 'Nuclear repulsion', erepul
    call print_io(io)
    write(io,'(2x,a27,2x,f16.10)') 'Reference (HF)', eref+erepul
    call print_io(io)


end subroutine print_calc_params

subroutine print_summary(e_hf, ecor, ccpq_energy)
    real(kind=8), intent(in) :: e_hf, ecor
    real(kind=8), intent(in) :: ccpq_energy(4)
    character(len=800) :: io

    write(io,'(a)') 'CCSDt Calculation Summary (Eh)'
    call print_io(io)
    write(io,'(a)')  '------------------------------'
    call print_io(io)
    write(io,'(a20,f18.12)') 'Reference', e_hf
    call print_io(io)
    write(io,'(a20,f18.12)') 'Correlation', ecor
    call print_io(io)
    write(io,'(a20,f18.12)') 'Total energy', e_hf + ecor
    call print_io(io)

    write(io,'(a)') 'CC(t;3),A Calculation Summary (Eh)'
    call print_io(io)
    write(io,'(a)')  '----------------------------------'
    call print_io(io)
    write(io,'(a20,f18.12)') 'Correlation', ecor + ccpq_energy(1)
    call print_io(io)
    write(io,'(a20,f18.12)') 'Total energy', e_hf + ecor + ccpq_energy(1)
    call print_io(io)

    write(io,'(a)') 'CC(t;3),D Calculation Summary (Eh)'
    call print_io(io)
    write(io,'(a)')  '----------------------------------'
    call print_io(io)
    write(io,'(a20,f18.12)') 'Correlation', ecor + ccpq_energy(4)
    call print_io(io)
    write(io,'(a20,f18.12)') 'Total energy', e_hf + ecor + ccpq_energy(4)
    call print_io(io)

end subroutine print_summary


subroutine print_date(note)
      implicit none
      character(len=*), intent(in) :: note
      character(len=30) :: date
    character(len=800) :: io

      call fdate(date)
      write (io,'(a)') trim(note)//' '//trim(date)
    call print_io(io)


end subroutine print_date

subroutine print_iter_head()
    character(len=800) :: io

    write(io,'(2x,a4,3(a15),a16)') 'It.',  'E (Corr)', 'dE', 'Residuum', 'CPU Time'
    call print_io(io)
    write(io,'(2x,65("-"))')
    call print_io(io)
end subroutine print_iter_head

subroutine print_iteration(iter, ecor, energy_diff, res, prev_time)
    integer, intent(in) :: iter
    real(kind=8), intent(in) :: ecor
    real(kind=8), intent(in) :: energy_diff
    real(kind=8), intent(in) :: res
    real(kind=8), intent(in) :: prev_time
    real(kind=8) :: cputime
    character(len=800) :: io

    real(kind=8) :: nsec
    integer :: nmin

    call cpu_time(cputime)
    nsec=cputime - prev_time
    nmin=int(nsec) / 60
    nsec=nsec-real(nmin, kind=8)*60.0d0

    write(io,'(2x,i4,3(f15.10),i5,'' min'',f5.1,'' s'')') iter,ecor,energy_diff, res,nmin,nsec

    call print_io(io)

end subroutine print_iteration

subroutine abort_cc(msg)
    use, intrinsic :: iso_fortran_env, only: error_unit
    character(len=*), intent(in) :: msg

    write(error_unit, '(a)') msg
    call exit(1)

end subroutine abort_cc

subroutine print_io(str)
    use iso_c_binding, only: c_null_char, c_new_line
    implicit none

    character(len=*), intent(in) :: str

    integer :: i, str_len
    character(len=800) :: buffer

    str_len = sizeof(trim(str))
    buffer = trim(str)
    buffer(str_len+1:str_len+2) = c_new_line

    call print_psi4(buffer(1:str_len+1)//c_null_char)

end subroutine print_io
