import pytest

from psi4.tests.utils import compare_values
import psi4_cct3


@pytest.mark.parametrize("call,keyw,eneprim,enesec", [
    ("ccsd", {"qc_module": "cct3"}, "CCSD", "CCSD"),
    ("CR-cc(2,3)", {}, "CR-CC(2,3)", "CCSD"),
    ("CCSD_lct", {}, "CCSDt", "CCSDt"),
    ("CC(T;3)", {}, "CC(t;3)", "CCSDt"),
    ("psi4_cct3", {"psi4_cct3__calc_type": "ccsd"}, "CCSD", "CCSD"),
    ("psi4_cct3", {"psi4_cct3__calc_type": "cr-cc"}, "CR-CC(2,3)", "CCSD"),
    ("psi4_cct3", {"psi4_cct3__calc_type": "ccsd3a"}, "CCSDt", "CCSDt"),
    ("psi4_cct3", {"psi4_cct3__calc_type": "cct3"}, "CC(t;3)", "CCSDt"),
])
def test_cct3_methods(call, keyw, eneprim, enesec):
    import psi4
    psi4.geometry("""
        units bohr
        h -2.514213562373  -1.000000000000   0.000000000000
        h -2.514213562373   1.000000000000   0.000000000000
        h  2.514213562373  -1.000000000000   0.000000000000
        h  2.514213562373   1.000000000000   0.000000000000
        h -1.000000000000  -2.414213562373   0.000000000000
        h -1.000000000000   2.414213562373   0.000000000000
        h  1.000000000000  -2.414213562373   0.000000000000
        h  1.000000000000   2.414213562373   0.000000000000
        symmetry d2h
    """)

    def basisspec_psi4_yo__anonymous1234(mol, role):
        bas = """
            cartesian
            ****
            H   0
            S   3  1.0000
                  4.50038     0.0704800
                  0.681277    0.407890
                  0.151374    0.647670
            ****
        """
        mol.set_basis_all_atoms("mbs_my", role=role)
        return {"mbs_my": bas}

    psi4.driver.qcdb.libmintsbasisset.basishorde["ANONYMOUS1234"] = basisspec_psi4_yo__anonymous1234

    psi4.set_options({
        "psi4_cct3__froz": 0,
        "psi4_cct3__act_occ": 1,
        "psi4_cct3__act_unocc": 1,
        "psi4_cct3__etol": 16,
        "basis": "anonymous1234",
    })
    psi4.set_options(keyw)

    ene, wfn = psi4.energy(call, return_wfn=True)
    psi4.core.print_variables()
    
    ref = {
        "total": {
            "HF":         -4.093745312228,
            "CCSD":       -4.216648883330,
            "CR-CC(2,3)": -4.219314222609,
            "CCSDt":      -4.220106352850,
            "CC(t;3)":    -4.220587742726,
        },
        "corl": {
            "CCSD":       -0.122903571102,
            "CR-CC(2,3)": -0.125568910381,
            "CCSDt":      -0.126361040622,
            "CC(t;3)":    -0.126842430498,
        },
    }

    atol = 10
    
    for item in [
        wfn.variable("current reference energy"),
        psi4.core.variable("current reference energy"),
        wfn.variable("hf total energy"),
        psi4.core.variable("hf total energy"),
    ]:
        assert compare_values(ref["total"]["HF"], item, atol, "HF energy")
    
    for item in [
        wfn.energy(),
        wfn.variable("current energy"),
        psi4.core.variable("current energy"),
        wfn.variable(f"{eneprim} total energy"),
        psi4.core.variable(f"{eneprim} total energy"),
        ene,
    ]:
        assert compare_values(ref["total"][eneprim], item, atol, f"{eneprim} energy")
    
    for item in [
        wfn.variable("current correlation energy"),
        psi4.core.variable("current correlation energy"),
        wfn.variable(f"{eneprim} correlation energy"),
        psi4.core.variable(f"{eneprim} correlation energy"),
    ]:
        assert compare_values(ref["corl"][eneprim], item, atol, f"{eneprim} corl energy")
    
    for item in [
        wfn.variable(f"{enesec} total energy"),
        psi4.core.variable(f"{enesec} total energy"),
    ]:
        assert compare_values(ref["total"][enesec], item, atol, f"{enesec} energy")
    
    for item in [
        wfn.variable(f"{enesec} correlation energy"),
        psi4.core.variable(f"{enesec} correlation energy"),
    ]:
        assert compare_values(ref["corl"][enesec], item, atol, f"{enesec} corl energy")
