#
# @BEGIN LICENSE
#
# cct3 by J. Emiliano Deustua, a plugin to:
#
# Psi4: an open-source quantum chemistry software package
#
# Copyright (c) 2007-2020 The Psi4 Developers.
#
# The copyrights for code used from other parties are included in
# the corresponding files.
#
# This file is part of Psi4.
#
# Psi4 is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, version 3.
#
# Psi4 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License along
# with Psi4; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#
# @END LICENSE
#

import psi4
import psi4.driver.p4util as p4util
from psi4.driver.procrouting import proc_util

def run_cct3(name, **kwargs):
    r"""Function encoding sequence of PSI module and plugin calls so that
    cct3 can be called via :py:func:`~driver.energy`. For post-scf plugins.

    >>> energy('cct3')

    """
    lowername = name.lower()
    kwargs = p4util.kwargs_lower(kwargs)

    optstash = p4util.OptionsState(
        ['CCT3', 'CALC_TYPE'])

    # Your plugin's psi4 run sequence goes here
    if lowername == "cct3":
        pass
    elif lowername == "ccsd":
        psi4.core.set_local_option("CCT3", "CALC_TYPE", "CCSD")
    elif lowername == "cr-cc(2,3)":
        psi4.core.set_local_option("CCT3", "CALC_TYPE", "CR-CC")
    elif lowername == "ccsd_lct":  # CCSDt
        psi4.core.set_local_option("CCT3", "CALC_TYPE", "CCSD3A")
    elif lowername == "cc(t;3)":
        psi4.core.set_local_option("CCT3", "CALC_TYPE", "CCT3")

    # Compute a SCF reference, a wavefunction is return which holds the molecule used, orbitals
    # Fock matrices, and more
    ref_wfn = kwargs.get('ref_wfn', None)
    if ref_wfn is None:
        ref_wfn = psi4.driver.scf_helper(name, **kwargs)

    # Ensure IWL files have been written when not using DF/CD
    proc_util.check_iwl_file_from_scf_type(psi4.core.get_option('SCF', 'SCF_TYPE'), ref_wfn)

    # Call the Psi4 plugin
    # Please note that setting the reference wavefunction in this way is ONLY for plugins
    cct3_wfn = psi4.core.plugin('cct3.so', ref_wfn)

    # Shove variables into global space
    for k, v in cct3_wfn.variables().items():
        psi4.core.set_variable(k, v)

    optstash.restore()

    return cct3_wfn


# Integration with driver routines
psi4.driver.procedures['energy']['cct3'] = run_cct3
# CCSD is managed method. add `qc_module='cct3'` to access through this plugin
psi4.driver.procedures['energy']['cr-cc(2,3)'] = run_cct3
psi4.driver.procedures['energy']['ccsd_lct'] = run_cct3
psi4.driver.procedures['energy']['cc(t;3)'] = run_cct3

