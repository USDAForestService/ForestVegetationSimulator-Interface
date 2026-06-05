"""
koa_equations.py
================
Component equations for the FVS-HI (Hawaii variant) koa growth & yield model,
implemented for two parameter/equation lineages so they can be stress-tested
against each other through an identical stepping engine.

LINEAGE A  ("PR / manuscript")
    Exactly the equations shipped in Ben Rice's PR #30 HiGy.R (v0.2.0) and
    mirrored in koa_projection.py. dDBH/dHT use the BAL^2/ln(size) + sqrt(BAPH*size)
    form; survival is the exp(-exp(eta)) cloglog on the 18.133 anchor; conditional
    Duan CFs 1.026 / 1.030.

LINEAGE B  ("FVS_FINAL / M1-S1")
    The May-2026 stress-test recommendation in koa_FVS_FINAL_parameters.csv:
    increment M1 form (sqrt(SDI) + rHT + log(CR*size) + two BYI terms), survival
    S1 cloglog with ln(YIP) offset, and MARGINAL Duan CFs 1.391 / 1.334.

All equations are metric: DBH cm, HT m, BAPH/BAL m2/ha, BYI Mg/ha, TPH /ha.
Coefficients are transcribed verbatim from the cited sources; see comments.
"""
import numpy as np

ln = np.log
def _sqrt(x): return np.sqrt(np.maximum(x, 0.0))

# ----------------------------------------------------------------------------
# SHARED STATIC EQUATIONS  (identical across lineages: both use the FINAL HT;
# HCB differs between lineages so it is defined per-lineage below)
# ----------------------------------------------------------------------------
# Total height: BYI-modified Chapman-Richards  (HiGy.R ht.pred.parm 'site';
# identical to koa_FVS_FINAL_parameters.csv and koa_projection.predict_HT)
HT_P = dict(a0=19.832, a1=0.106, b=0.044, c=0.863, g1=-0.198, g2=0.479)

def predict_HT(dbh, baph, qmd, byi, use_byi=True):
    a0, a1, b, c, g1, g2 = (HT_P[k] for k in ("a0", "a1", "b", "c", "g1", "g2"))
    rdbh = dbh / np.maximum(qmd, 1e-6)
    inter = a0 + a1 * byi / 100.0 if use_byi else a0
    ht = inter * (1 - np.exp(-b * dbh)) ** c * np.exp(g1 * ln(baph + 1) + g2 * rdbh)
    return np.maximum(ht, 1.37)

# BAL allocation (logistic on rDBH) -- used to derive per-tree BAL from BAPH in
# the stand-level cohort projector. (HiGy.R has no analogue; from FINAL CSV.)
def bal_fraction(rdbh):
    return 1.0 / (1.0 + np.exp(-1.842 + 3.956 * rdbh))

# Height to crown base (identical 'site' parms in HiGy.R and FINAL CSV)
HCB_P = dict(b0=0.1684, b1=1.0146, b2=-0.376, b3=-0.0078, b4=-0.3734, b5=-0.221)
def predict_HCB(dbh, ht, bal, baph, byi):
    p = HCB_P
    eta = (p["b0"] + p["b1"] * _sqrt(ht / 100.0)
           + p["b2"] * ln(np.maximum(ht / np.maximum(dbh, 0.1), 0.5))
           + p["b3"] * _sqrt(bal * baph + 1)
           + p["b4"] * ln(baph + 1)
           + p["b5"] * ln(np.maximum(byi, 1) / 100.0))
    hcb = ht / (1.0 + np.exp(-eta))
    return np.minimum(np.maximum(hcb, 0.0), 0.95 * ht)

# ----------------------------------------------------------------------------
# LINEAGE A  -- PR HiGy.R v0.2.0 (== koa_projection.py)
# ----------------------------------------------------------------------------
class LineageA:
    name = "A_PR_manuscript"
    CF_DDBH, CF_DHT = 1.026, 1.030          # conditional (HiGy.R)
    ddbh_cap, dht_cap = 4.0, 2.0            # HiGy.R pmin(pmax(.,0),4) / ...,2)

    # HCB 'site' parms -- HiGy.R hcb.pred.parm (NOTE: differs from koa_projection)
    HCB = dict(b0=0.1684, b1=1.0146, b2=-0.376, b3=-0.0078, b4=-0.3734, b5=-0.221)
    # dDBH 'site' parms -- HiGy.R ddbh.parm
    DDBH = dict(b0=-2.4704737, b1=0.2072221, b2=-0.0159616, b3=-0.0016893,
                b4=-0.2972574, b5=-0.4470330, b6=-0.0158403, b7=0.0188938, b8=0.4530166)
    # dHT 'site' parms -- HiGy.R dht.parm
    DHT = dict(b0=-3.382162, b1=0.272454, b2=-0.105319, b3=-0.000829,
               b4=-0.071718, b5=-1.483889, b6=0.033035, b7=0.017887, b8=0.433224)
    # survival 'site' parms -- HiGy.R surv.parm; P(survive)=exp(-exp(eta))
    SURV = dict(b0=18.133, b1=0.199, b2=-5.718, b3=7.640, b4=15.678, b5=-3.396,
                b6=3.039, b7=-25.102)

    @classmethod
    def predict_HCB(cls, dbh, ht, bal, baph, byi):
        p = cls.HCB
        eta = (p["b0"] + p["b1"] * _sqrt(ht / 100.0)
               + p["b2"] * ln(np.maximum(ht / np.maximum(dbh, 0.1), 0.5))
               + p["b3"] * _sqrt(bal * baph + 1)
               + p["b4"] * ln(baph + 1)
               + p["b5"] * ln(np.maximum(byi, 1) / 100.0))
        hcb = ht / (1.0 + np.exp(-eta))
        return np.minimum(np.maximum(hcb, 0.0), 0.95 * ht)

    @classmethod
    def dDBH(cls, dbh, baph, bal, cr, byi, planted, sdi=None, rht=None):
        p = cls.DDBH
        lp = (p["b0"] + p["b1"] * ln(dbh + 1) + p["b2"] * dbh
              + p["b3"] * bal**2 / ln(dbh + 5) + p["b4"] * ln(bal + 1)
              + p["b5"] * ln(np.maximum(cr, 0.01)) + p["b6"] * _sqrt(baph * dbh)
              + p["b7"] * planted * np.minimum(dbh, 40) + p["b8"] * ln(np.maximum(byi, 1)))
        return np.clip(np.exp(lp) * cls.CF_DDBH, 0, cls.ddbh_cap)

    @classmethod
    def dHT(cls, ht, baph, bal, cr, byi, planted, sdi=None, rht=None):
        p = cls.DHT
        lp = (p["b0"] + p["b1"] * ln(ht + 1) + p["b2"] * ht
              + p["b3"] * bal**2 / ln(ht + 5) + p["b4"] * ln(bal + 1)
              + p["b5"] * ln(np.maximum(cr, 0.01)) + p["b6"] * _sqrt(baph * ht)
              + p["b7"] * _sqrt(planted * np.minimum(ht, 20)) + p["b8"] * ln(np.maximum(byi, 1)))
        return np.clip(np.exp(lp) * cls.CF_DHT, 0, cls.dht_cap)

    @classmethod
    def surv_annual(cls, dbh, ht, cr, rht, byi, sdi=None, planted=None, yip=1.0):
        p = cls.SURV
        ht = np.maximum(ht, 0.1); dbh = np.maximum(dbh, 0.1)
        eta = (p["b0"] + p["b1"] * ht + p["b2"] * ln(ht) + p["b3"] * rht
               + p["b4"] * ln(np.clip(cr, 0.01, 0.99)) + p["b5"] * ln(ht / dbh)
               + p["b6"] * ln(np.maximum(byi, 1) / 100.0) + p["b7"] * (byi / 1000.0))
        return np.clip(np.exp(-np.exp(eta)), 0, 1)   # annual; no YIP offset in HiGy.R

    @classmethod
    def surv_annual_stable(cls, dbh, ht, cr, rht, byi, sdi=None, planted=None,
                           yip=1.0, cr_lim=(0.20, 0.55), rht_lim=(0.30, 0.70),
                           floor=0.90):
        """Operational stabilizer: clamp CR and rHT to the cohort-calibration
        domain (where the published cloglog fit is sane) and floor annual
        survival. Outside CR>~0.55 / rHT>~0.70 the raw model collapses
        (see diagnostics); FVS feeds it the full range, so clamp at evaluation."""
        cr_c = np.clip(cr, *cr_lim); rht_c = np.clip(rht, *rht_lim)
        s = cls.surv_annual(dbh, ht, cr_c, rht_c, byi)
        return np.clip(np.maximum(s, floor), 0, 1)

# ----------------------------------------------------------------------------
# LINEAGE B  -- FVS_FINAL M1/S1 recommendation (koa_FVS_FINAL_parameters.csv)
# ----------------------------------------------------------------------------
class LineageB:
    name = "B_FVS_FINAL_M1S1"
    CF_DDBH, CF_DHT = 1.391, 1.334          # MARGINAL CFs (stress_test_summary)
    ddbh_cap, dht_cap = 4.0, 2.0

    HCB = LineageA.HCB                       # FINAL CSV HCB == HiGy.R HCB
    # dDBH M1_BYI -- FINAL CSV
    DDBH = dict(b0=0.675575, b1=-0.722509, b2=-0.010397, b3=-0.268313, b4=0.512349,
                b5=-0.023877, b6=1.154764, b7=0.057437, b8=1.726263, b9=-4.529999)
    # dHT M1_BYI -- FINAL CSV
    DHT = dict(b0=-1.018003, b1=1.916395, b2=-0.115641, b3=-0.017570, b4=-1.314187,
               b5=0.014844, b6=0.341214, b7=0.037215, b8=2.129420, b9=-8.171071)
    # survival S1_BYI -- FINAL CSV; cloglog P(alive)=1-exp(-exp(eta+ln(YIP)))
    SURV = dict(b0=36.767230, b1=0.335877, b2=-7.566182, b3=7.372809, b4=16.294807,
                b5=-3.594212, b6=3.194631, b7=-26.781613)

    predict_HCB = LineageA.predict_HCB.__func__   # same HCB

    @classmethod
    def dDBH(cls, dbh, baph, bal, cr, byi, planted, sdi=0.0, rht=0.5):
        p = cls.DDBH
        lp = (p["b0"] + p["b1"] * ln(dbh + 1) + p["b2"] * dbh + p["b3"] * ln(bal + 1)
              + p["b4"] * ln(np.maximum(cr * dbh, 0.001)) + p["b5"] * _sqrt(sdi)
              + p["b6"] * rht + p["b7"] * planted * dbh
              + p["b8"] * ln(np.maximum(byi, 1) / 100.0) + p["b9"] * byi / 1000.0)
        return np.clip(np.exp(lp) * cls.CF_DDBH, 0, cls.ddbh_cap)

    @classmethod
    def dHT(cls, ht, baph, bal, cr, byi, planted, sdi=0.0, rht=0.5):
        p = cls.DHT
        lp = (p["b0"] + p["b1"] * ln(ht + 1) + p["b2"] * ht + p["b3"] * ln(bal + 1)
              + p["b4"] * ln(np.maximum(cr * ht, 0.001)) + p["b5"] * _sqrt(sdi)
              + p["b6"] * rht + p["b7"] * planted * ht
              + p["b8"] * ln(np.maximum(byi, 1) / 100.0) + p["b9"] * byi / 1000.0)
        return np.clip(np.exp(lp) * cls.CF_DHT, 0, cls.dht_cap)

    @classmethod
    def surv_annual(cls, dbh, ht, cr, rht, byi, sdi=None, planted=None, yip=1.0):
        p = cls.SURV
        ht = np.maximum(ht, 0.1); dbh = np.maximum(dbh, 0.1)
        slender = ht / (dbh / 100.0)                  # HT_m / DBH_m
        eta = (p["b0"] + p["b1"] * ht + p["b2"] * ln(ht) + p["b3"] * rht
               + p["b4"] * ln(np.clip(cr, 0.01, 0.99)) + p["b5"] * ln(np.maximum(slender, 1))
               + p["b6"] * ln(np.maximum(byi, 1) / 100.0) + p["b7"] * (byi / 1000.0))
        # P(alive) = 1 - exp(-exp(eta + ln(YIP)))
        return np.clip(1.0 - np.exp(-np.exp(eta + ln(yip))), 0, 1)


LINEAGES = {"A": LineageA, "B": LineageB}
