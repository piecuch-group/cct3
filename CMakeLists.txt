#
# @BEGIN LICENSE
#
# moint by Psi4 Developer, a plugin to:
#
# Psi4: an open-source quantum chemistry software package
#
# Copyright (c) 2007-2017 The Psi4 Developers.
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

cmake_minimum_required(VERSION 3.1 FATAL_ERROR)

project(psi4-cct3 CXX Fortran)

find_package(psi4 1.1 REQUIRED)

#SET(GCC_COVERAGE_COMPILE_FLAGS "-g -rdynamic")
#SET(GCC_COVERAGE_LINK_FLAGS    "-g -rdynamic")

#SET( CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} ${GCC_COVERAGE_COMPILE_FLAGS}" )
SET( CMAKE_Fortran_FLAGS  "${CMAKE_CXX_FLAGS} -g -fbacktrace -ffixed-line-length-132" )
#SET( CMAKE_EXE_LINKER_FLAGS  "${CMAKE_EXE_LINKER_FLAGS} ${GCC_COVERAGE_LINK_FLAGS}" )

#base.a
#DIIS.a
#lw.a
#sj-CCSD-PE.a)

#set(my_lib_dir "/home/edeustua/Downloads/psi4/tests/plugins/cct3")

add_psi4_plugin(psi4_cct3
	cct3.cc

Cal_D3.f
cc_utils.f
dgemm.f
energy.f
hbar/HBar0A2A.f
hbar/HBar0A2C.f
hbar/HBar0A4A.f
hbar/HBar0A4C.f
hbar/HBar0A4E.f
hbar/HBar1A1A.f
hbar/HBar1A3A0.f
hbar/HBar1A3A.f
hbar/HBar1A3C0.f
hbar/HBar1A3C.f
hbar/HBar1B1B.f
hbar/HBar1B3B0.f
hbar/HBar1B3B.f
hbar/HBar1B3D0.f
hbar/HBar1B3D.f
hbar/HBar2A0A.f
hbar/HBar2A2A.f
hbar/HBar2A2C.f
hbar/HBar2B2B0.f
hbar/HBar2B2B.f
hbar/HBar2C0A.f
hbar/HBar2C2A.f
hbar/HBar2C2C.f
hbar/HBar3A1A0.f
hbar/HBar3A1A.f
hbar/HBar3B1B0.f
hbar/HBar3B1B.f
hbar/HBar3C1A0.f
hbar/HBar3C1A.f
hbar/HBar3D1B0.f
hbar/HBar3D1B.f
hbar/HBar4A0A.f
hbar/HBar4C0A.f
hbar/HBar4E0A.f
lcc/L1A_update.f
lcc/L1B_update.f
lcc/L2A_update.f
lcc/L2B_update.f
lcc/L2C_update.f
lcc/L3A_update.f
lcc/L3B_update.f
lcc/L3C_update.f
lcc/L3D_update.f
lcc/L_input.f
lcc/L_update.f
reorder_old1.f
sum_old1.f
cc/t1A00_update.f
cc/t1A01_update.f
cc/t1A10_update.f
cc/t1A11_update.f
cc/t1A_update.f
cc/t1B00_update.f
cc/t1B01_update.f
cc/t1B10_update.f
cc/t1B11_update.f
cc/t1B_update.f
cc/t2A0000_update.f
cc/t2A0010_update.f
cc/t2A0011_update.f
cc/t2A1000_update.f
cc/t2A1010_update.f
cc/t2A1011_update.f
cc/t2A1100_update.f
cc/t2A1110_update.f
cc/t2A1111_update.f
cc/t2A_update1.f
cc/t2A_update.f
cc/t2B0000_update.f
cc/t2B0001_update.f
cc/t2B0010_update.f
cc/t2B0011_update.f
cc/t2B0100_update.f
cc/t2B0101_update.f
cc/t2B0110_update.f
cc/t2B0111_update.f
cc/t2B1000_update.f
cc/t2B1001_update.f
cc/t2B1010_update.f
cc/t2B1011_update.f
cc/t2B1100_update.f
cc/t2B1101_update.f
cc/t2B1110_update.f
cc/t2B1111_update.f
cc/t2B_update1.f
cc/t2B_update.f
cc/t2C0000_update.f
cc/t2C0010_update.f
cc/t2C0011_update.f
cc/t2C1000_update.f
cc/t2C1010_update.f
cc/t2C1011_update.f
cc/t2C1100_update.f
cc/t2C1110_update.f
cc/t2C1111_update.f
cc/t2C_update1.f
cc/t2C_update.f
cc/t3A100100_update.f
cc/t3A100110_update.f
cc/t3A100111_update.f
cc/t3A110100_update.f
cc/t3A110110_update.f
cc/t3A110111_update.f
cc/t3A111100_update.f
cc/t3A111110_update.f
cc/t3A111111_update.f
cc/t3AB_update.f
cc/t3A_update.f
cc/t3B001001_update.f
cc/t3B001100_update.f
cc/t3B001101_update.f
cc/t3B001110_update.f
cc/t3B001111_update.f
cc/t3B100001_update.f
cc/t3B100100_update.f
cc/t3B100101_update.f
cc/t3B100110_update.f
cc/t3B100111_update.f
cc/t3B101001_update.f
cc/t3B101100_update.f
cc/t3B101101_update.f
cc/t3B101110_update.f
cc/t3B101111_update.f
cc/t3B110001_update.f
cc/t3B110100_update.f
cc/t3B110101_update.f
cc/t3B110110_update.f
cc/t3B110111_update.f
cc/t3B111001_update.f
cc/t3B111100_update.f
cc/t3B111101_update.f
cc/t3B111110_update.f
cc/t3B111111_update.f
cc/t3BC_update.f
cc/t3B_update.f
cc/t3C010010_update.f
cc/t3C010011_update.f
cc/t3C010100_update.f
cc/t3C010110_update.f
cc/t3C010111_update.f
cc/t3C011010_update.f
cc/t3C011011_update.f
cc/t3C011100_update.f
cc/t3C011110_update.f
cc/t3C011111_update.f
cc/t3C100010_update.f
cc/t3C100011_update.f
cc/t3C100100_update.f
cc/t3C100110_update.f
cc/t3C100111_update.f
cc/t3C110010_update.f
cc/t3C110011_update.f
cc/t3C110100_update.f
cc/t3C110110_update.f
cc/t3C110111_update.f
cc/t3C111010_update.f
cc/t3C111011_update.f
cc/t3C111100_update.f
cc/t3C111110_update.f
cc/t3C111111_update.f
cc/t3CD_update.f
cc/t3C_update.f
cc/t3D100100_update.f
cc/t3D100110_update.f
cc/t3D100111_update.f
cc/t3D110100_update.f
cc/t3D110110_update.f
cc/t3D110111_update.f
cc/t3D111100_update.f
cc/t3D111110_update.f
cc/t3D111111_update.f
cc/t3D_update.f
update_clusters.f
update_lambdas.f

ccsdt.f90
correction.f90
diis.f90
hbar.f90
integrals.f90
lcc.f90
printing.F90
solve.f90
utils.f90
wrapper_psi4.f90)


#target_link_libraries(moint PRIVATE base.a DIIS.a lw.a sj-CCSD-PE.a)
#target_link_libraries(cct3 PRIVATE
#	${my_lib_dir}/all_libs.a)
#${my_lib_dir}/base.a
#${my_lib_dir}/DIIS.a
#${my_lib_dir}/lw.a
#${my_lib_dir}/sj-CCSD-PE.a
