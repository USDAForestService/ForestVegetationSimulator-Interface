"""
run_benchmarks.py  -- dual-lineage FVS-HI stress test.
Outputs CSVs + console summary used by the report.
"""
import os, numpy as np, pandas as pd
from koa_equations import predict_HT
from koa_projector import project_cohort, project_psp, sdi_of, SDI_MAX

DATA = "/sessions/elegant-lucid-cannon/mnt/Koa"
OUT  = "/sessions/elegant-lucid-cannon/mnt/outputs/koa_harness/results"
os.makedirs(OUT, exist_ok=True)
BYIS = [100, 264, 450]; LAB = {100:"Low",264:"Med",450:"High"}

# ===========================================================================
# 1. COHORT projections: bounded vs unbounded x lineage A/B  (100 yr)
# ===========================================================================
rows = []
for lin in ("A", "B"):
    for planted in (0, 1):
        for byi in BYIS:
            for bounded in (True, False):
                df = project_cohort(byi, planted, lin, bounded=bounded, max_age=100)
                for snap in (20, 40, 60, 100):
                    r = df[df.age == snap]
                    if len(r):
                        r = r.iloc[0]
                        rows.append(dict(lineage=lin, origin="plt" if planted else "nat",
                                         BYI=byi, bounded=bounded, age=snap,
                                         QMD=round(r.QMD,1), HT=round(r.HT,1),
                                         BAPH=round(r.BAPH,1), TPH=round(r.TPH,0),
                                         SDI=round(r.SDI,0), VOL=round(r.VOL,1),
                                         SDI_pct=round(r.SDI/SDI_MAX*100,0)))
cohort = pd.DataFrame(rows)
cohort.to_csv(f"{OUT}/cohort_projections.csv", index=False)

# Runaway flag: unbounded age-40 QMD vs the manuscript Table-8 expectation (~42 cm)
print("="*78); print("1. COHORT LONG-TERM BEHAVIOR (natural, age 40, QMD cm)")
print("   Manuscript Table 8 expectation at age 40 ~ 42 cm")
piv = cohort[(cohort.origin=="nat") & (cohort.age==40)].pivot_table(
        index="BYI", columns=["lineage","bounded"], values="QMD")
print(piv.to_string())

# ===========================================================================
# 2. BAKUZIS / SDI envelope adherence (decade snapshots, bounded runs)
# ===========================================================================
env = []
for lin in ("A","B"):
    for planted in (0,1):
        for byi in BYIS:
            df = project_cohort(byi, planted, lin, bounded=True, max_age=100)
            dec = df[df.age.isin([10,20,30,40,50,60,70,80,90,100])]
            within = (dec.SDI <= 1.10*SDI_MAX).mean()*100
            env.append(dict(lineage=lin, origin="plt" if planted else "nat", BYI=byi,
                            max_SDI=round(dec.SDI.max(),0),
                            within_110pct_SDImax=round(within,0)))
envdf = pd.DataFrame(env); envdf.to_csv(f"{OUT}/bakuzis_envelope.csv", index=False)
print("\n"+"="*78); print("2. BAKUZIS / SDI ENVELOPE (bounded; % decade snaps within 110% SDImax)")
print(envdf.to_string(index=False))

# ===========================================================================
# 3. REAL PSP VALIDATION: project first measurement -> observed last
# ===========================================================================
psp = pd.read_csv(f"{DATA}/AK_PSP_data_compiled.csv")
psp = psp[(psp.DBH_cm > 0) & psp.TPA.notna()]
val = []
for pid, g in psp.groupby("PSP"):
    seqs = sorted(g.MeasSeq.unique())
    if len(seqs) < 2: continue
    g0 = g[g.MeasSeq == seqs[0]]; g1 = g[g.MeasSeq == seqs[-1]]
    yrs = g1.Yfloor.max() - g0.Yfloor.max()
    if pd.isna(yrs) or yrs < 2 or yrs > 60: continue
    yrs = int(round(yrs))
    # initial tree list
    tl = pd.DataFrame(dict(dbh=g0.DBH_cm.values,
                           ht=np.where(g0.HT_m.values > 1.37, g0.HT_m.values, np.nan),
                           expf=g0.TPA.values))
    tl["ht"] = tl.ht.fillna(pd.Series(
        predict_HT(tl.dbh.values, max((tl.dbh**2*0.00007854*tl.expf).sum(),0.1),
                   max(np.sqrt((tl.dbh**2*0.00007854*tl.expf).sum()/(0.00007854*tl.expf.sum())),1),
                   264)))
    tl["cr"] = 0.5
    # observed final stand
    baph_o = (g1.DBH_cm**2*0.00007854*g1.TPA).sum(); tph_o = g1.TPA.sum()
    qmd_o = np.sqrt(baph_o/(0.00007854*tph_o)) if tph_o>0 else np.nan
    rec = dict(PSP=pid, years=int(yrs), n0=len(g0), QMD_obs=round(qmd_o,1),
               BAPH_obs=round(baph_o,1), TPH_obs=round(tph_o,0))
    for lin in ("A","B"):
        try:
            p = project_psp(tl, byi=264, planted=0, lineage=lin, n_years=yrs)
            rec[f"QMD_{lin}"]=round(p["QMD"],1); rec[f"BAPH_{lin}"]=round(p["BAPH"],1)
        except Exception as e:
            rec[f"QMD_{lin}"]=np.nan; rec[f"BAPH_{lin}"]=np.nan
    val.append(rec)
valdf = pd.DataFrame(val)
valdf.to_csv(f"{OUT}/psp_validation.csv", index=False)

def bias(pred, obs):
    m = pred.notna() & obs.notna()
    d = pred[m]-obs[m]
    return dict(n=int(m.sum()), bias=round(d.mean(),2),
                bias_pct=round(100*d.mean()/obs[m].mean(),1),
                rmse=round(np.sqrt((d**2).mean()),2))
print("\n"+"="*78); print(f"3. REAL PSP VALIDATION  (n={len(valdf)} remeasured plots, BYI=264 assumed)")
for lin in ("A","B"):
    print(f"   Lineage {lin}: QMD {bias(valdf[f'QMD_{lin}'], valdf.QMD_obs)} | "
          f"BAPH {bias(valdf[f'BAPH_{lin}'], valdf.BAPH_obs)}")
print(f"\nWrote: cohort_projections.csv, bakuzis_envelope.csv, psp_validation.csv -> {OUT}")
