cmake_policy(SET CMP0048 NEW)  # project_VERSION* variables populated from project(... VERSION x.x.x) string
project(psi4_cct3
        VERSION 0.1
        LANGUAGES CXX Fortran)
set(psi4_cct3_AUTHORS      "Emiliano Deustua")
set(psi4_cct3_DESCRIPTION  "Active-space coupled-cluster CCSDt and non-iterative corrections to CCSDt defining the CC(t;3) approach plugin to Psi4")
set(psi4_cct3_URL          "https://github.com/piecuch-group/psi4_cct3")
set(psi4_cct3_LICENSE      "LGPL-3.0")

cmake_minimum_required(VERSION 3.3 FATAL_ERROR)
list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

# <<<  Prepare  >>>

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

set(TargetOpenMP_FIND_COMPONENTS "CXX Fortran")
find_package(psi4 1.1 REQUIRED)

set(PN ${PROJECT_NAME})

# PYMOD_INSTALL_LIBDIR If set, installs python modules to
#                      ${CMAKE_INSTALL_LIBDIR}${PYMOD_INSTALL_LIBDIR}/psi4_cct3 rather than
#                      the default read off parent Psi4

#   install alongside psi4 module by default, but overrideable
get_filename_component(psi4_CMAKE_INSTALL_PREFIX ${psi4_INCLUDE_DIR} DIRECTORY)
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set(CMAKE_INSTALL_PREFIX ${psi4_CMAKE_INSTALL_PREFIX} CACHE PATH "Install path" FORCE)
endif()
message(STATUS "Plugin install prefix: ${CMAKE_INSTALL_PREFIX}")

if(PYMOD_INSTALL_LIBDIR)
    set(PYMOD_INSTALL_FULLDIR "${CMAKE_INSTALL_LIBDIR}${PYMOD_INSTALL_LIBDIR}/${PN}")
else()
    file(RELATIVE_PATH _tmp ${psi4_CMAKE_INSTALL_PREFIX} ${psi4_LIBRARY})
    #   e.g., _tmp = lib/psi4/core.so
    get_filename_component(_tmp2 ${_tmp} DIRECTORY)
    get_filename_component(_tmp3 ${_tmp2} DIRECTORY)
    set(PYMOD_INSTALL_FULLDIR "${_tmp3}/${PN}")
endif()
message(STATUS "Plugin module install: ${PYMOD_INSTALL_FULLDIR}")


include(custom_static_library)

# <<<  Build  >>>

# Debug
# set(CMAKE_Fortran_FLAGS  "${CMAKE_CXX_FLAGS} -fcheck=all -g -fbacktrace -ffixed-line-length-132" )

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffixed-line-length-132")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -extend-source 132")
endif()

add_psi4_plugin(psi4_cct3
    src/cct3.cc
    src/Cal_D3.f
    src/cc_utils.f
    src/dgemm.f
    src/energy.f
    src/reorder_old1.f
    src/sum_old1.f
    src/update_ccsd.f
    src/update_clusters.f
    src/update_lambdas.f
    src/hbar/HBar0A2A.f
    src/hbar/HBar0A2C.f
    src/hbar/HBar0A4A.f
    src/hbar/HBar0A4C.f
    src/hbar/HBar0A4E.f
    src/hbar/HBar1A1A.f
    src/hbar/HBar1A3A0.f
    src/hbar/HBar1A3A.f
    src/hbar/HBar1A3C0.f
    src/hbar/HBar1A3C.f
    src/hbar/HBar1B1B.f
    src/hbar/HBar1B3B0.f
    src/hbar/HBar1B3B.f
    src/hbar/HBar1B3D0.f
    src/hbar/HBar1B3D.f
    src/hbar/HBar2A0A.f
    src/hbar/HBar2A2A.f
    src/hbar/HBar2A2C.f
    src/hbar/HBar2B2B0.f
    src/hbar/HBar2B2B.f
    src/hbar/HBar2C0A.f
    src/hbar/HBar2C2A.f
    src/hbar/HBar2C2C.f
    src/hbar/HBar3A1A0.f
    src/hbar/HBar3A1A.f
    src/hbar/HBar3B1B0.f
    src/hbar/HBar3B1B.f
    src/hbar/HBar3C1A0.f
    src/hbar/HBar3C1A.f
    src/hbar/HBar3D1B0.f
    src/hbar/HBar3D1B.f
    src/hbar/HBar4A0A.f
    src/hbar/HBar4C0A.f
    src/hbar/HBar4E0A.f
    src/cc/t1A00_update.f
    src/cc/t1A01_update.f
    src/cc/t1A10_update.f
    src/cc/t1A11_update.f
    src/cc/t1A_update.f
    src/cc/t1B00_update.f
    src/cc/t1B01_update.f
    src/cc/t1B10_update.f
    src/cc/t1B11_update.f
    src/cc/t1B_update.f
    src/cc/t2A0000_update.f
    src/cc/t2A0010_update.f
    src/cc/t2A0011_update.f
    src/cc/t2A1000_update.f
    src/cc/t2A1010_update.f
    src/cc/t2A1011_update.f
    src/cc/t2A1100_update.f
    src/cc/t2A1110_update.f
    src/cc/t2A1111_update.f
    src/cc/t2A_update1.f
    src/cc/t2A_update.f
    src/cc/t2B0000_update.f
    src/cc/t2B0001_update.f
    src/cc/t2B0010_update.f
    src/cc/t2B0011_update.f
    src/cc/t2B0100_update.f
    src/cc/t2B0101_update.f
    src/cc/t2B0110_update.f
    src/cc/t2B0111_update.f
    src/cc/t2B1000_update.f
    src/cc/t2B1001_update.f
    src/cc/t2B1010_update.f
    src/cc/t2B1011_update.f
    src/cc/t2B1100_update.f
    src/cc/t2B1101_update.f
    src/cc/t2B1110_update.f
    src/cc/t2B1111_update.f
    src/cc/t2B_update1.f
    src/cc/t2B_update.f
    src/cc/t2C0000_update.f
    src/cc/t2C0010_update.f
    src/cc/t2C0011_update.f
    src/cc/t2C1000_update.f
    src/cc/t2C1010_update.f
    src/cc/t2C1011_update.f
    src/cc/t2C1100_update.f
    src/cc/t2C1110_update.f
    src/cc/t2C1111_update.f
    src/cc/t2C_update1.f
    src/cc/t2C_update.f
    src/cc/t3A100100_update.f
    src/cc/t3A100110_update.f
    src/cc/t3A100111_update.f
    src/cc/t3A110100_update.f
    src/cc/t3A110110_update.f
    src/cc/t3A110111_update.f
    src/cc/t3A111100_update.f
    src/cc/t3A111110_update.f
    src/cc/t3A111111_update.f
    src/cc/t3AB_update.f
    src/cc/t3A_update.f
    src/cc/t3B001001_update.f
    src/cc/t3B001100_update.f
    src/cc/t3B001101_update.f
    src/cc/t3B001110_update.f
    src/cc/t3B001111_update.f
    src/cc/t3B100001_update.f
    src/cc/t3B100100_update.f
    src/cc/t3B100101_update.f
    src/cc/t3B100110_update.f
    src/cc/t3B100111_update.f
    src/cc/t3B101001_update.f
    src/cc/t3B101100_update.f
    src/cc/t3B101101_update.f
    src/cc/t3B101110_update.f
    src/cc/t3B101111_update.f
    src/cc/t3B110001_update.f
    src/cc/t3B110100_update.f
    src/cc/t3B110101_update.f
    src/cc/t3B110110_update.f
    src/cc/t3B110111_update.f
    src/cc/t3B111001_update.f
    src/cc/t3B111100_update.f
    src/cc/t3B111101_update.f
    src/cc/t3B111110_update.f
    src/cc/t3B111111_update.f
    src/cc/t3BC_update.f
    src/cc/t3B_update.f
    src/cc/t3C010010_update.f
    src/cc/t3C010011_update.f
    src/cc/t3C010100_update.f
    src/cc/t3C010110_update.f
    src/cc/t3C010111_update.f
    src/cc/t3C011010_update.f
    src/cc/t3C011011_update.f
    src/cc/t3C011100_update.f
    src/cc/t3C011110_update.f
    src/cc/t3C011111_update.f
    src/cc/t3C100010_update.f
    src/cc/t3C100011_update.f
    src/cc/t3C100100_update.f
    src/cc/t3C100110_update.f
    src/cc/t3C100111_update.f
    src/cc/t3C110010_update.f
    src/cc/t3C110011_update.f
    src/cc/t3C110100_update.f
    src/cc/t3C110110_update.f
    src/cc/t3C110111_update.f
    src/cc/t3C111010_update.f
    src/cc/t3C111011_update.f
    src/cc/t3C111100_update.f
    src/cc/t3C111110_update.f
    src/cc/t3C111111_update.f
    src/cc/t3CD_update.f
    src/cc/t3C_update.f
    src/cc/t3D100100_update.f
    src/cc/t3D100110_update.f
    src/cc/t3D100111_update.f
    src/cc/t3D110100_update.f
    src/cc/t3D110110_update.f
    src/cc/t3D110111_update.f
    src/cc/t3D111100_update.f
    src/cc/t3D111110_update.f
    src/cc/t3D111111_update.f
    src/cc/t3D_update.f
    src/ccsd/t1A_update.f
    src/ccsd/t1B_update.f
    src/ccsd/t2A_update1.f
    src/ccsd/t2A_update.f
    src/ccsd/t2B_update1.f
    src/ccsd/t2B_update.f
    src/ccsd/t2C_update1.f
    src/ccsd/t2C_update.f
    src/lcc/L1A_update.f
    src/lcc/L1B_update.f
    src/lcc/L2A_update.f
    src/lcc/L2B_update.f
    src/lcc/L2C_update.f
    src/lcc/L3A_update.f
    src/lcc/L3B_update.f
    src/lcc/L3C_update.f
    src/lcc/L3D_update.f
    src/lcc/L_input.f
    src/lcc/L_update.f
    src/const.f90
    src/correction.f90
    src/diis.f90
    src/driver.f90
    src/hbar.f90
    src/integrals.f90
    src/printing.f90
    src/solver.f90
    src/system.f90
    src/utils.f90
    src/wrapper_psi4.f90
)

target_link_libraries(psi4_cct3 PRIVATE ${LIBC_INTERJECT})

configure_file(__init__.py.in __init__.py @ONLY)

# <<<  Install  >>>

install(TARGETS psi4_cct3
        EXPORT "${PN}Targets"
        LIBRARY DESTINATION ${PYMOD_INSTALL_FULLDIR})

install(FILES pymodule.py LICENSE README.rst
              ${CMAKE_CURRENT_BINARY_DIR}/__init__.py
        DESTINATION ${PYMOD_INSTALL_FULLDIR})

install(DIRECTORY tests/
        DESTINATION ${PYMOD_INSTALL_FULLDIR}/tests
        FILES_MATCHING PATTERN "conftest.py"
                       PATTERN "test_*.py"
        PATTERN ".pytest_cache" EXCLUDE
        PATTERN "__pycache__" EXCLUDE)

# <<< Export Config >>>

# GNUInstallDirs "DATADIR" wrong here; CMake search path wants "share".
set(CMAKECONFIG_INSTALL_DIR "share/cmake/${PN}")
configure_package_config_file(cmake/${PN}Config.cmake.in
                              "${CMAKE_CURRENT_BINARY_DIR}/${PN}Config.cmake"
                              INSTALL_DESTINATION ${CMAKECONFIG_INSTALL_DIR})
write_basic_package_version_file(${CMAKE_CURRENT_BINARY_DIR}/${PN}ConfigVersion.cmake
                                 VERSION ${${PN}_VERSION}
                                 COMPATIBILITY AnyNewerVersion)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PN}Config.cmake
              ${CMAKE_CURRENT_BINARY_DIR}/${PN}ConfigVersion.cmake
        DESTINATION ${CMAKECONFIG_INSTALL_DIR})
install(EXPORT "${PN}Targets"
        NAMESPACE "${PN}::"
        DESTINATION ${CMAKECONFIG_INSTALL_DIR})

