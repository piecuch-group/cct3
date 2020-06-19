/*
 *@BEGIN LICENSE
 *
 * , a plugin to:
 *
 * Psi4: an open-source quantum chemistry software package
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Copyright (c) 2014, The Florida State University. All rights reserved.
 *
 *@END LICENSE
 *
 */

#ifndef FORTRAN_H
#define FORTRAN_H

/**
 * This header file wraps F90 routines to be used in C++.
 * It was adapted from the v2rdm_casscf Psi4 plugin
 * (https://github.com/edeprince3/v2rdm_casscf)
 */

#ifndef FC_SYMBOL
#define FC_SYMBOL 2
#endif

#if   FC_SYMBOL==1
#define F77NAME(x) x
#elif FC_SYMBOL==2
#define F77NAME(x) x##_
#endif

// CC fortran interface

extern "C" {
    void F77NAME(cc)(
                    int &froz,
                    int &socc,
                    int &docc,
                    int &orbs,
                    int &actocc,
                    int &actunocc,
                    int &etol,
                    int &maxiter,
                    bool &keep_amps,
                    bool &is_rhf,
                    int &cct3_diis,
                    int &calc_type,
                    double *onebody,
                    double *twobody,
                    double &erepul,
                    double &eref_psi4,
                    double &rval
                    );
};

inline double do_cc(
                int &froz,
                int &socc,
                int &docc,
                int &orbs,
                int &actocc,
                int &actunocc,
                int &etol,
                int &maxiter,
                bool &keep_amps,
                bool &is_rhf,
                int &cct3_diis,
                int &calc_type,
                double *onebody,
                double *twobody,
                double &erepul,
                double &eref_psi4
                ){

        double rval = 0.0;
        F77NAME(cc)(
                        froz,
                        socc,
                        docc,
                        orbs,
                        actocc,
                        actunocc,
                        etol,
                        maxiter,
                        keep_amps,
                        is_rhf,
                        cct3_diis,
                        calc_type,
                        onebody,
                        twobody,
                        erepul,
                        eref_psi4,
                        rval
                        );
        return rval;
};
#endif
