CC(t;3) PSI4 plugin
===================

CC(t;3) is a quantum chemistry method that consists in the computation of the
iterative active-space coupled-cluster approach with singles, doubles, and
active-space triples, denominated CCSDt, and in the subsequent energy correction
based on the method of moments of coupled-cluster equations.

Compilation
-----------

A working version of PSI4 version 1.1 is required. To compile the plugin:

::

   $ git clone https://gitlab.msu.edu/piecuch-group/psi4_cct3
   $ cd psi4_cct3
   $ `psi4 --plugin-compile`
   $ make

Running
-------

In order to run a CCSDt calculation, the following options have to be set within
the scheme

::

   set psi4-cct3 {
      option value
      ...
   }

froz
   Number of frozen core molecular orbitals.
act_occ
   Number of active occupied molecular orbitals counting from the fermi vaccum
   down.
act_unocc
   Number of active unnocupied molecular orbitals counting from the fermi vaccum
   up.
etol
   Energy convergence tolerance given as 10^-ETOL. Default is 10^-7
max_iter
   Maximum number of iterations. Default is 1000.
keep_amps
   If true, write down the converged cluster amplitudes to the file
   ``amplitudes.moe``.

