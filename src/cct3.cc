/*
 * @BEGIN LICENSE
 *
 * psi4_cct3 by Emiliano Deustua, a plugin to:
 *
 * Psi4: an open-source quantum chemistry software package
 *
 * Copyright (c) 2007-2020 The Psi4 Developers.
 *
 * The copyrights for code used from other parties are included in
 * the corresponding files.
 *
 * This file is part of Psi4.
 *
 * Psi4 is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * Psi4 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along
 * with Psi4; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * @END LICENSE
 */

/*
 * This file has been generated using Psi4's plugin system and by taking some code from
 * FCIDUMP (https://github.com/edeprince3/v2rdm_casscf).
 */

#include "psi4/psi4-dec.h"
#include "psi4/psifiles.h"
#include "psi4/libdpd/dpd.h"
#include "psi4/libtrans/integraltransform.h"
#include "psi4/libpsio/psio.hpp"
#include "psi4/libmints/wavefunction.h"
#include "psi4/libmints/molecule.h"
#include "psi4/libmints/matrix.h"
#include "psi4/libmints/vector.h"
#include "psi4/libpsi4util/PsiOutStream.h"
#include "psi4/liboptions/liboptions.h"

#include "fortran.h"
#include <cstdarg>


// This allows us to be lazy in getting the spaces in DPD calls
#define ID(x) ints.DPD_ID(x)

namespace psi{ namespace psi4_cct3{

// Insertion sort algorithm adapted from HANDE and Rossetta
void insertion_sort(double* sp_eigv, int* sp_ord, int* sp_ord_inv, int norbs) {

    int i = 0;
    int j = 0;
    int tmp = 0;
    const double tol = 1.0e-9;

    // Initialize arrays
    for (i = 0; i < norbs; i++) {
        sp_ord[i] = i;
        sp_ord_inv[i] = 0;
    }

    // Produce sorting vector
    // sp_ord maps energy rank index -> PSI4 mo index
    for (i = 1; i < norbs; i++) {
        j = i - 1;
        tmp = sp_ord[i];

        while (j >= 0) {
            if (sp_eigv[sp_ord[j]] - sp_eigv[tmp] < tol) {
                break;
            }
            sp_ord[j+1] = sp_ord[j];
            j--;
        }
        sp_ord[j+1] = tmp;
    }

    // Invert sorting
    // sp_ord_inv maps PSI4 mo index -> energy rank index
    for (i = 0; i < norbs; i++) {
        sp_ord_inv[sp_ord[i]] = i;
    }

}

void create_oei(int* sp_ord_inv, SharedMatrix moH, double* onebody, double ints_tolerance, int norb)
{
    // Walk through moH and save the non-zero values
    double tmp_e;
    int offset = 0;
    int i = 0;
    int a = 0;
    for (int h=0; h<moH->nirrep(); ++h) {
        for (int m=0; m<moH->rowdim(h); ++m) {
            for (int n=0; n<=m; ++n) {
                // Read 1e- integral
                tmp_e = moH->get(h, m, n);
                // Translate mo indices from PSI4 ordering to energy
                // ordering
                i = sp_ord_inv[m + offset];
                a = sp_ord_inv[n + offset];
                // Fill onebody array in column major and
                // non-permutationally unique form
                onebody[a+i*norb] = tmp_e;
                onebody[i+a*norb] = tmp_e;
            }
        }
        offset += moH->rowdim(h);
    }
}

void create_tei(int* sp_ord, int nirrep, dpdbuf4& K, double* twobody, double ints_tolerance, int norb)
{
    int a = 0;
    int b = 0;
    int i = 0;
    int j = 0;
    double tmp_e;

    for(int h = 0; h < nirrep; ++h){
        global_dpd_->buf4_mat_irrep_init(&K, h);
        global_dpd_->buf4_mat_irrep_rd(&K, h);
        for(int pq = 0; pq < K.params->rowtot[h]; ++pq){
            int p = K.params->roworb[h][pq][0];
            int q = K.params->roworb[h][pq][1];
            for(int rs = 0; rs < K.params->coltot[h]; ++rs){
                int r = K.params->colorb[h][rs][0];
                int s = K.params->colorb[h][rs][1];

                // Read 2e- integral
                tmp_e = K.matrix[h][pq][rs];

                // Translate mo indices from PSI4 ordering to energy
                // ordering
                i = sp_ord[p];
                a = sp_ord[q];
                j = sp_ord[r];
                b = sp_ord[s];

                // Twobody matrix is now in physical notation form
                // <ij|1/r_12|ab> = (ia||jb)

                // Column major format is required for Fortran
                // interface
                twobody[i + j*norb + a*norb*norb + b*norb*norb*norb] = tmp_e;
            }
        }
        global_dpd_->buf4_mat_irrep_close(&K, h);
    }
}

void read_eigv_and_order(double* sp_eigv, int* sp_ord, int* sp_ord_inv,
        Dimension norb, int norbs,
        const std::shared_ptr<Vector> eigv)
{
    int iorb = 0;
    for (int h=0; h<norb.n(); ++h) {
        for (int i=0; i<norb[h]; ++i) {
            sp_eigv[iorb] = eigv->get(h,i);
            iorb++;
        }
    }

    insertion_sort(sp_eigv, sp_ord, sp_ord_inv, norbs);
}

// Very simple wrapper for handling Fortran I/O
extern "C"
void F77NAME(print_psi4) (const char *string)
{
    psi::outfile->Printf("%s", string);
}



// Read options
extern "C" PSI_API
int read_options(std::string name, Options &options)
{
    if (name == "PSI4_CCT3" || options.read_globals()) {
        // [TODO] add shifts, diis control and rhf vs rohf

        /*- Number of frozen core molecular orbitals. -*/
        options.add_int("FROZ", 0);

        /*- Number of active occupied molecular orbitals counting from the Fermi level down (e.g. HOMO, HOMO-1, HOMO-2, etc.). -*/
        options.add_int("ACT_OCC", 1);

        /*- Number of active unnocupied molecular orbitals counting from the Fermi level up (e.g. LUMO, LUMO+1, LUMO+2, etc.). -*/
        options.add_int("ACT_UNOCC", 1);

        /*- Energy convergence tolerance given as 10^-ETOL. -*/
        options.add_int("ETOL", 7);

        // Dimension of DIIS subspace
        options.add_int("CCT3_DIIS", 5);

        /*- Maximum number of CC iterations. -*/
        options.add_int("MAX_ITER", 100);

        /*- Do write the converged cluster amplitudes (T vectors) to the file amplitudes.moe. -*/
        options.add_bool("KEEP_AMPS", false);

        /*- Invoke CCSD, CR-CC(2,3), CCSDt, and CC(t;3) calculations, respectively. -*/
        options.add_str("CALC_TYPE", "CCSD", "CCSD CR-CC CCSD3A CCT3");
    }

    return true;
}


extern "C" PSI_API
SharedWavefunction psi4_cct3(SharedWavefunction ref_wfn, Options& options)
{
    // Definitions
    double ints_tol = 0.0;
    double scf_e;
    double rep_e;
    std::array<double, 3> dipole_field = {0.0, 0.0, 0.0};

    // Aux vars
    int i;

    // Integrals for CC
    double *onebody = NULL;
    double *twobody = NULL;
    double *sp_eigv = NULL;
    int *sp_ord = NULL;
    int *sp_ord_inv = NULL;

    // Read options
    int froz = options.get_int("FROZ");
    int actocc = options.get_int("ACT_OCC");
    int actunocc = options.get_int("ACT_UNOCC");
    int etol = options.get_int("ETOL");
    int maxiter = options.get_int("MAX_ITER");
    int cct3_diis = options.get_int("CCT3_DIIS");
    int calc_type;

    bool keep_amps = options.get_bool("KEEP_AMPS");
    bool is_rhf = false;

    // Grab the global (default) PSIO object, for file I/O
    std::shared_ptr<PSIO> psio(_default_psio_lib_);

    // Have the reference (SCF) wavefunction, ref_wfn
    if(!ref_wfn) throw PSIEXCEPTION("SCF has not been run yet!");

    // Check for SCF type
    if (options.get_str("REFERENCE") == "RHF") {
        is_rhf = true;
    } else if (options.get_str("REFERENCE") == "ROHF") {
        is_rhf = false;
    } else {
        throw PSIEXCEPTION("SCF reference type not supported by CC(t;3)");
    }

    // Get calculation type number.
    if (options.get_str("CALC_TYPE") == "CCSD") {
        calc_type = 1;
    } else if (options.get_str("CALC_TYPE") == "CR-CC") {
        calc_type = 2;
    } else if (options.get_str("CALC_TYPE") == "CCSD3A") {
        calc_type = 3;
    } else if (options.get_str("CALC_TYPE") == "CCT3") {
        calc_type = 4;
    } else {
        calc_type = 1;
    }


    // Get reference energy and grab the molecule pointer
    scf_e = ref_wfn->energy();
    std::shared_ptr<Molecule> molecule = ref_wfn->molecule();

    // Read quantum system sizes
    Dimension nalphapi = ref_wfn->nalphapi();
    Dimension nbetapi = ref_wfn->nbetapi();
    Dimension soccpi = ref_wfn->soccpi();
    Dimension doccpi = ref_wfn->doccpi();
    Dimension norb = ref_wfn->nmopi();


    int nalpha = nalphapi.sum();
    int nbeta = nbetapi.sum();


    int socc = soccpi.sum();
    int docc = doccpi.sum();
    int norbs = norb.sum();

    //std::cout << nalpha << "  " << nbeta << "\n";
    //std::cout << socc << "  " << docc << "  " << norbs << "\n";

    //exit(1);
    if (froz > docc) {
        throw PSIEXCEPTION("Number of frozen orbitals is too large!");
    }

    if (actocc > docc - froz) {
        throw PSIEXCEPTION("Number of active occupied orbitals cannot be larger than then amount of available orbitals (i.e. CC(t;3) is not capable of doing CCSDT).");
    }


    // Initialize integral arrays for Fortran
    onebody = new double[norbs*norbs];
    twobody = new double[norbs*norbs*norbs*norbs];
    sp_eigv = new double[norbs];
    sp_ord = new int[norbs];
    sp_ord_inv = new int[norbs];

    for (i = 0; i < norbs; i++) {
        sp_eigv[i] = 0;
        sp_ord[i] = 0;
          sp_ord_inv[i] = 0;
    }

    for (i = 0; i < norbs*norbs; i++) {
        onebody[i] = 0.0;
    }
    for (i = 0; i < norbs*norbs*norbs*norbs; i++) {
        twobody[i] = 0.0;
    }

    // Quickly check that there are no open shell orbitals here...
    int nirrep  = ref_wfn->nirrep();

    std::vector<std::shared_ptr<MOSpace> > spaces;
    spaces.push_back(MOSpace::all);
    IntegralTransform ints(ref_wfn, spaces, IntegralTransform::TransformationType::Restricted);
    ints.transform_tei(MOSpace::all, MOSpace::all, MOSpace::all, MOSpace::all);

    // Use the IntegralTransform object's DPD instance, for convenience
    dpd_set_default(ints.get_dpd_id());


    // Read SCF MO eigenvalues and order them by energy
    read_eigv_and_order(sp_eigv, sp_ord, sp_ord_inv, norb, norbs, ref_wfn->epsilon_a());

    // Now, loop over the DPD buffer, printing the integrals
    dpdbuf4 K;
    psio->open(PSIF_LIBTRANS_DPD, PSIO_OPEN_OLD);

    // We need all permutations for our CC program
    global_dpd_->buf4_init(&K, PSIF_LIBTRANS_DPD, 0,
            ID("[A,A]"), ID("[A,A]"),
            ID("[A>=A]+"), ID("[A>=A]+"),
            0, "MO Ints (AA|AA)");

    // Create two electron integral array and sort
    create_tei(sp_ord_inv, nirrep, K, twobody, ints_tol, norbs);

    global_dpd_->buf4_close(&K);

    SharedMatrix moH(new Matrix(PSIF_MO_FZC, norb, norb));
    moH->load(psio, PSIF_OEI);

    // Create one electron integral array and sort
    create_oei(sp_ord_inv, moH, onebody, ints_tol, norbs);

    // Get nuclear repulsion energy
    rep_e = molecule->nuclear_repulsion_energy(dipole_field);

    psio->close(PSIF_LIBTRANS_DPD, PSIO_OPEN_OLD);

    // Execute CC program
    double corl_e = do_cc(
            froz,
            socc,
            docc,
            norbs,
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
            rep_e,
            scf_e
            );

    ref_wfn->set_scalar_variable("CURRENT CORRELATION ENERGY", corl_e);
    ref_wfn->set_energy(scf_e + corl_e);
    return ref_wfn;
}

}} // End Namespaces
