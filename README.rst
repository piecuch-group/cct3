PGCC (Piecuch Group Coupled-Cluster)
====================================

This code is used in conjuction with the automatic CC code generator to perform
CC calculations.

Usage
-----

The executable is invoked using

::

   pgcc [OPTIONS] CONFIGURATION_FILE

with the following options

-o file, --output file                 Output filename. If not set, the program's output will be directed to stdout.

The configuration file accepts the following options:

nfroz, core
  Number of frozen spin-orbitals.

occ, occupied
  Number of occupied spin-orbitals.

unocc, unoccupied
  Number of unoccupied spin-orbitals.

mult, multiplicity
  System's spin multiplicity.

maxiter, max_iterations
  Maximum amount of iterations.

diis, diis_space
  Number of cycles between DIIS extrapolations.

tol, tolerance
  Convergence tolerance (10^-tol).

label
  Calculation label

onebody
  One-body integrals file name.

twobody
  Two-body integrals file name.
