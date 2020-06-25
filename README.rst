CC(t;3) PSI4 Plugin 
===================
.. image:: https://travis-ci.org/piecuch-group/cct3.svg?branch=dev
    :target: https://travis-ci.org/piecuch-group/cct3

The CCT3 plugin to PSI4 [1]_ is capable of executing a number of closed-
and open-shell coupled-cluster (CC) calculations with up to triply excited (T3)
clusters. Among them is the active-space CC approach abbreviated as CCSDt
[2]_, [3]_, [4]_, [5]_, which approximates full CCSDT by selecting the dominant T\ :sub:`3` amplitudes
via active orbitals, and the CC(t;3) method, which corrects the CCSDt energies
for the remaining, predominantly dynamical, triple excitations that have not
been captured by CCSDt [6]_, [7]_. The CC(t;3) approach belongs to a larger family
of methods that rely on the generalized form of biorthogonal moment expansions
defining the CC(P;Q) formalism [6]_, [7]_.

The CCSDt method alone is already very useful, since it can reproduce
electronic energies of the near-CCSDT quality at a small fraction of the
computational cost, while accurately describing selected multireference
situations, such as single bond breaking. CC(t;3) improves the CCSDt energetics
even further, being practically as accurate as full CCSDT for both relative and
total electronic energies, at a cost which is essentially the same at that of
CCSDt. The systematic convergence of the CCSDt and CC(t;3) calculations toward
CCSDT should be emphasized here too. For example, CCSDt becomes full CCSDT when
all orbitals used to select T3 amplitudes are active. The same applies to
CC(t;3) (when all orbitals used to select T3 amplitudes are active, the triples
correction to CCSDt becomes zero).

The CCT3 plugin can also be used to run CCSD and CR-CC(2,3) calculations. This
can be done by making the active orbital set, which is defined by the user in
the input, empty, since in this case CCSDt = CCSD and CC(t;3) = CR-CC(2,3). We
recall that CR-CC(2,3) is a completely renormalized triples correction to CCSD,
which improves the results obtained with the conventional CCSD(T) approach
without resorting to any multireference concepts and being at most twice as
expensive as CCSD(T) [8]_, [9]_, [10]_.

Build
-----

To compile this plugin, a working version of PSI4 version 1.1 or greater is
required. The easiest way to get a working copy is via the `conda
<https://conda.io/docs/>`_ or `anaconda
<https://www.continuum.io/downloads#linux>`_ environment (more on this `here
<http://www.psicode.org/psi4manual/1.3.2/conda.html#faq-psi4pkg>`_). To use it,
first create a new python 3 environment and activate it via:

.. code-block:: bash

   $ conda create -n p4env python=3.7 psi4 psi4-dev -c psi4 -c psi4/label/dev
   $ source activate p4env

Next, get the source code for the CC(t;3) plugin and compile it using the
following lines:

.. code-block:: bash

   $ git clone https://github.com/piecuch-group/cct3
   $ cd cct3
   $ `psi4 --plugin-compile`
   $ make

Once this step is done, you should have a working copy of the plugin. You can
run a test example with:

.. code-block:: bash

   $ psi4 examples/H8-0.1.dat

Run
---

In order to run a CCSD, CR-CC(2,3), CCSDt, or CC(t;3) calculation, the following
options have to be set within
the scheme

::

   set psi4-cct3 {
      option value
      ...
   }

froz
   Number of frozen core molecular orbitals.
act_occ
   Number of active occupied molecular orbitals counting from the Fermi level
   down (e.g. HOMO, HOMO-1, HOMO-2, etc.).
act_unocc
   Number of active unnocupied molecular orbitals counting from the Fermi level
   up (e.g. LUMO, LUMO+1, LUMO+2, etc.).
etol
   Energy convergence tolerance given as 10^-ETOL. Default is 10^-7
max_iter
   Maximum number of iterations. Default is 100.
keep_amps
   If true, write down the converged cluster amplitudes to the file
   ``amplitudes.moe``.
calc_type
   Can be set to ``CCSD``, ``CR-CC``, ``CCSD3A``, or ``CCT3``. These options invoke CCSD, CR-CC(2,3), CCSDt, and CC(t;3) calculations, respectively. It not specified, the default is CCSD.

References
----------

.. [1] \J.E. Deustua, J. Shen, P. Piecuch, "CCT3: A PSI4 Plugin Which Performs Active-Space Coupled-Cluster CCSDt Calculations and Which Can Determine Noniterative Corrections to CCSDt Defining the CC(t;3) Approach."
.. [2] \P. Piecuch, "Active-Space Coupled-Cluster Methods," *Mol. Phys.* **108**, 2987-3015 (2010). DOI: http://dx.doi.org/10.1080/00268976.2010.522608.

.. [3] \N. Oliphant and L. Adamowicz, "The Implementation of the Multireference Coupled-Cluster Method Based on the Single-Reference Formalism," *J. Chem. Phys.* **96**, 3739-3744 (1992). https://doi.org/10.1063/1.461878.
.. [4] \P. Piecuch, N. Oliphant, and L. Adamowicz, "A State-Selective Multi-Reference Coupled-Cluster Theory Employing the Single-Reference Formalism," *J. Chem. Phys.* **99**, 1875-1900 (1993). DOI: http://dx.doi.org/10.1063/1.466179.
.. [5] \P. Piecuch, S.A. Kucharski, and R.J. Barlett, "Coupled-Cluster Methods with Internal and Semi-Internal Triply and Quadruply Excited Clusters: CCSDt and CCSDtq Approaches," *J. Chem. Phys.* **110**, 6103-6122 (1999). DOI: http://dx.doi.org/10.1063/1.478517.
.. [6] \J. Shen and P. Piecuch, "Biorthogonal Moment Expansions in Coupled-Cluster Theory: Review of Key Concepts and Merging the Renormalized and Active-Space Coupled-Cluster Methods," *Chem. Phys.* **401**, 180-202 (2012). DOI: http://dx.doi.org/10.1016/j.chemphys.2011.11.033.
.. [7] \J. Shen and P. Piecuch, "Combining Active-Space Coupled-Cluster Methods with Moment Energy Corrections via the CC(P;Q) Methodology, with Benchmark Calculations for Biradical Transition States," *J. Chem. Phys.* **136**, 144104-1 - 144104-16 (2012). DOI: http://dx.doi.org/10.1063/1.3700802.
.. [8] \P. Piecuch and M. Wloch, "Renormalized Coupled-Cluster Methods Exploiting Left Eigenstates of the Similarity-Transformed Hamiltonian," *J. Chem. Phys.* **123**, 224105-1 - 224105-10 (2005). DOI: http://dx.doi.org/10.1063/1.2137318.
.. [9] \P. Piecuch, M. Wloch, J.R. Gour, and A. Kinal, "Single-Reference, Size-Extensive, Non-Iterative Coupled-cluster Approaches to Bond Breaking and Biradicals," *Chem. Phys. Lett.* **418**, 467-474 (2006). DOI: http://dx.doi.org/10.1016/j.cplett.2005.10.116.
.. [10] \M. Wloch, J.R. Gour, and P. Piecuch, "Extension of the Renormalized Coupled-Cluster Methods Exploiting Left Eigenstates of the Similarity-Transformed Hamiltonian to Open-Shell Systems: A Benchmark Study," *J. Phys. Chem. A* **111**, 11359-11382 (2007). DOI: http://dx.doi.org/10.1021/jp0725351.
