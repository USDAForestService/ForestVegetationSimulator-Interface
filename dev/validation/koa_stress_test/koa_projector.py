"""
koa_projector.py
================
Two stepping engines for the koa component equations:

1. project_cohort(...)      -- stand-level even-aged cohort. With bounded=True it
   reproduces koa_projection.py's operational guardrails (self-thinning at
   60->55% SDImax, 0.65/0.35 dynamic-static HT blend, biological ceiling
   min(HT, 25+BYI/55), hard DBH max). With bounded=False it strips those
   guardrails to expose the *raw* increment behavior that FVS-HI/HiGy.R would
   show, since HiGYOneStand applies none of them (only per-step increment caps,
   FVS density mortality, and TreeSzCp size caps).

2. project_psp(...)         -- individual-tree list engine mirroring
   HiGYOneStand(): per-tree BA, BAL = cumsum(BA) over descending DBH, plot QMD /
   SDI / htmax, HCB->CR, annual dDBH/dHT, annual survival -> expf decay, 2.5%/yr
   crown-recession limit, size caps. Used to project real PSP tree lists and
   validate against observed remeasurements.

SDI (metric Reineke, used as a lineage-B covariate and for the envelope):
   SDI = TPH * (QMD/25)^1.6     (matches HiGy.R comment expf*(qmd/25)^1.6)
"""
import numpy as np, pandas as pd
from koa_equations import predict_HT, predict_HCB, bal_fraction, LINEAGES

SDI_MAX = 500.0            # upper boundary of FIA koa SDI (stress_test_summary)
FORM_FACTOR = 0.40         # V = BAPH * meanHT * FF
def sdi_of(tph, qmd): return tph * (np.maximum(qmd, 0.1) / 25.0) ** 1.6


# ---------------------------------------------------------------------------
def project_cohort(byi, planted, lineage, bounded=True, surv_mode="cohort",
                   init_dbh=None, init_tph=None, init_age=1, max_age=100,
                   ddbh_mult=1.0, dht_mult=1.0, mort_mult=1.0, sdi_max=SDI_MAX):
    # surv_mode: 'cohort'=raw eqn floored at 0.5/yr (koa_projection.py crutch),
    #            'raw'=raw eqn no floor (FVS behavior), 'stable'=clamped+floored
    L = LINEAGES[lineage]
    dbh_max = 60.0 if planted else 90.0
    if init_dbh is None: init_dbh = 6.0 if planted else 8.0
    if init_tph is None: init_tph = 1200.0 if planted else 500.0
    ages = list(range(init_age, max_age + 1)); n = len(ages)
    rec = {k: np.zeros(n) for k in ["QMD","HT","BAPH","TPH","CR","SDI","HCB","VOL"]}
    rec["age"] = np.array(ages)

    DBH, TPH = float(init_dbh), float(init_tph)
    QMD = DBH; BAPH = TPH * np.pi/4 * (DBH/100)**2
    HT = float(predict_HT(DBH, max(BAPH,0.1), QMD, byi)); CR = 0.65
    for step, age in enumerate(ages):
        SDI = sdi_of(TPH, QMD)
        BAL_avg = BAPH * bal_fraction(1.0)
        BAL_dom = BAPH * bal_fraction(1.5)
        HCB = float(predict_HCB(DBH, HT, BAL_avg, BAPH, byi))
        CR = max(0.20, (HT-HCB)/HT) if HT > 0.1 else 0.65
        rHT = 0.50
        rec["QMD"][step], rec["HT"][step], rec["BAPH"][step] = QMD, HT, BAPH
        rec["TPH"][step], rec["CR"][step], rec["SDI"][step] = TPH, CR, SDI
        rec["HCB"][step], rec["VOL"][step] = HCB, BAPH*HT*FORM_FACTOR
        if step == n-1: break

        if bounded:                                  # self-thinning guardrail
            pct = SDI / sdi_max
            if pct > 0.60:
                tr = 0.55/pct; tphn = TPH*tr
                DBH *= (TPH/max(tphn,1))**0.08; QMD = DBH; TPH = tphn
                BAPH = TPH*np.pi/4*(QMD/100)**2

        if surv_mode == "stable":
            ps = float(L.surv_annual_stable(DBH, HT, CR, rHT, byi))
            ps = np.clip(1.0 - mort_mult*(1.0-ps), 0.0, 1.0)
        elif surv_mode == "raw":
            ps = float(L.surv_annual(DBH, HT, CR, rHT, byi, sdi=SDI, planted=planted))
            ps = np.clip(1.0 - mort_mult*(1.0-ps), 0.0, 1.0)
        else:  # 'cohort' (koa_projection.py): floor at 0.50/yr
            ps = float(L.surv_annual(DBH, HT, CR, rHT, byi, sdi=SDI, planted=planted))
            ps = np.clip(1.0 - mort_mult*(1.0-ps), 0.50, 1.0)
        TPH *= ps
        if TPH < 5:
            for s2 in range(step+1, n):
                for k in rec:
                    if k != "age": rec[k][s2] = rec[k][step]
            break
        BAPH = TPH*np.pi/4*(QMD/100)**2

        dD = float(L.dDBH(DBH, BAPH, BAL_avg, CR, byi, planted, sdi=SDI, rht=rHT))*ddbh_mult
        dH = float(L.dHT(HT, BAPH, BAL_avg, CR, byi, planted, sdi=SDI, rht=rHT))*dht_mult
        DBH += dD; QMD = DBH; HT += dH
        if bounded:                                  # HT blend + ceiling + hard cap
            ht_static = float(predict_HT(DBH, BAPH, max(BAL_dom,0.1)*0+QMD, QMD, byi))
            HT = 0.65*HT + 0.35*ht_static
            HT = min(HT, 25 + byi/55)
            DBH = min(DBH, dbh_max); QMD = DBH
        BAPH = TPH*np.pi/4*(QMD/100)**2
    return pd.DataFrame(rec)


# ---------------------------------------------------------------------------
def project_psp(trees, byi, planted, lineage, n_years, surv_mode="raw",
                use_size_caps=True, dbh_max=None, ht_max=92*0.3048):
    """trees: DataFrame with columns dbh(cm), ht(m), cr(0-1), expf(/ha). One plot.
    surv_mode: 'raw' = HiGy.R/FINAL survival; 'stable' = clamped+floored."""
    L = LINEAGES[lineage]
    surv = L.surv_annual_stable if surv_mode == "stable" else L.surv_annual
    if dbh_max is None: dbh_max = (60.0 if planted else 90.0)
    t = trees.copy().reset_index(drop=True)
    for _ in range(int(n_years)):
        t["ba"] = (t.dbh**2 * 0.00007854) * t.expf
        t = t.sort_values("dbh", ascending=False).reset_index(drop=True)
        t["bal"] = t.ba.cumsum() - t.ba
        baph = t.ba.sum(); tph = t.expf.sum()
        qmd = np.sqrt(baph/(0.00007854*tph)) if tph > 0 else 0.0
        sdi = sdi_of(tph, qmd); htmax = t.ht.max()
        # HCB -> CR
        hcb = predict_HCB(t.dbh.values, t.ht.values, t.bal.values, baph, byi)
        t["cr"] = np.clip(1 - hcb/np.maximum(t.ht, 0.1), 0.05, 0.95)
        rht = t.ht.values/max(htmax, 0.1)
        # increments
        below_bh = t.ht.values < 1.3716
        dD = L.dDBH(t.dbh.values, baph, t.bal.values, t.cr.values, byi, planted, sdi=sdi, rht=rht)
        dH = L.dHT(t.ht.values, baph, t.bal.values, t.cr.values, byi, planted, sdi=sdi, rht=rht)
        dD = np.where(below_bh, 0.0, dD)
        # survival -> expf decay
        ps = surv(t.dbh.values, t.ht.values, t.cr.values, rht, byi, sdi=sdi, planted=planted)
        t["expf"] = np.maximum(t.expf*ps, 1e-5)
        nd = t.dbh.values + dD; nh = t.ht.values + dH
        if use_size_caps:
            nd = np.where(nd > dbh_max, t.dbh.values, nd)
            nh = np.where(nh > ht_max,  t.ht.values, nh)
        t["dbh"] = nd; t["ht"] = nh
    baph = (t.dbh**2*0.00007854*t.expf).sum(); tph = t.expf.sum()
    qmd = np.sqrt(baph/(0.00007854*tph)) if tph > 0 else 0.0
    return dict(QMD=qmd, BAPH=baph, TPH=tph, SDI=sdi_of(tph, qmd),
                HTmax=t.ht.max(), VOL=baph*t.ht.mean()*FORM_FACTOR)
