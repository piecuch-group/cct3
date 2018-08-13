module system

    type sys_t

        ! Number of frozen orbitals
        integer :: froz

        ! Occupied alpha orbitals
        integer :: occ_a
        ! Occupied beta orbitals
        integer :: occ_b

        ! Total orbitals
        integer :: orbs

        ! Active occupied orbitals
        integer :: actocc
        ! Active unoccupied orbitals
        integer :: actunocc

    end type sys_t

    type calc_t
        ! Maximum iterations
        integer :: maxiter

        ! DIIS space
        integer :: diis_space

        ! Convergence tolerance
        integer :: itol
    end type calc_t

end module system
