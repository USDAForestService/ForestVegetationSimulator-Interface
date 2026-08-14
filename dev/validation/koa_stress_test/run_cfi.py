"""
run_cfi.py -- CFI/PSP first->last validation and long-term behavior for the
LATEST koa equation set (HiGy.R == koa_prediction_functions_FINAL.R v2, 2026-05-12),
comparing raw HiGy.R survival vs a stabilized survival, to dial in behavior.
"""
import os, numpy as np, pandas as pd
from koa_equations import predict_HT
from koa_projector import project_psp, project_cohort, sdi_of, SDI_MAX

DATA="/sessions/elegant-lucid-cannon/mnt/Koa"
OUT ="/sessions/elegant-lucid-cannon/mnt/outputs/koa_harness/results"; os.makedirs(OUT, exist_ok=True)

# ---- map PSP origin (planted/natural) from AK.PLT where possible -----------
plt_meta=pd.read_csv(f"{DATA}/AK.PLT.csv")
# AK_PSP and AK.PLT do not share a key cleanly; default natural (dominant in PSP net)
psp=pd.read_csv(f"{DATA}/AK_PSP_data_compiled.csv")
psp=psp[(psp.DBH_cm>0)&psp.TPA.notna()]

def stand_stats(g):
    baph=(g.DBH_cm**2*0.00007854*g.TPA).sum(); tph=g.TPA.sum()
    qmd=np.sqrt(baph/(0.00007854*tph)) if tph>0 else np.nan
    return qmd, baph, tph

rows=[]
for pid,g in psp.groupby("PSP"):
    seqs=sorted(g.MeasSeq.unique())
    if len(seqs)<2: continue
    g0=g[g.MeasSeq==seqs[0]]; g1=g[g.MeasSeq==seqs[-1]]
    yrs=g1.Yfloor.max()-g0.Yfloor.max()
    if pd.isna(yrs) or yrs<3: continue
    yrs=int(round(yrs))
    baph0=(g0.DBH_cm**2*0.00007854*g0.TPA).sum(); tph0=g0.TPA.sum()
    qmd0=np.sqrt(baph0/(0.00007854*tph0)) if tph0>0 else 1
    tl=pd.DataFrame(dict(dbh=g0.DBH_cm.values,
        ht=np.where(g0.HT_m.values>1.37,g0.HT_m.values,np.nan), expf=g0.TPA.values))
    tl["ht"]=tl.ht.fillna(pd.Series(predict_HT(tl.dbh.values,max(baph0,0.1),max(qmd0,1),264)))
    tl["cr"]=0.5
    qo,bo,to=stand_stats(g1)
    rec=dict(PSP=pid, yrs=yrs, n0=len(g0), QMDo=round(qo,1), BAPHo=round(bo,1), TPHo=round(to,0))
    for mode in ("raw","stable"):
        p=project_psp(tl, byi=264, planted=0, lineage="A", n_years=yrs, surv_mode=mode)
        rec[f"QMD_{mode}"]=round(p["QMD"],1); rec[f"BAPH_{mode}"]=round(p["BAPH"],1)
        rec[f"TPH_{mode}"]=round(p["TPH"],0)
    rows.append(rec)
cfi=pd.DataFrame(rows); cfi.to_csv(f"{OUT}/cfi_validation.csv", index=False)

def stats(pred,obs):
    m=pred.notna()&obs.notna(); d=pred[m]-obs[m]
    return dict(n=int(m.sum()), bias=round(d.mean(),2),
                bias_pct=round(100*d.mean()/obs[m].mean(),1), rmse=round(np.sqrt((d**2).mean()),2))
print("="*74); print(f"CFI/PSP first->last validation  (n={len(cfi)} plots, ~{int(cfi.yrs.median())} yr, BYI=264)")
for v,unit in (("QMD","cm"),("BAPH","m2/ha"),("TPH","/ha")):
    print(f"\n {v} ({unit}):")
    for mode in ("raw","stable"):
        print(f"   survival={mode:7s}  {stats(cfi[f'{v}_{mode}'], cfi[f'{v}o'])}")

# ---- long-term cohort behavior: raw vs stable survival ---------------------
print("\n"+"="*74); print("LONG-TERM cohort (natural BYI=264): QMD / TPH / SDI at decades")
import koa_projector as KP
def coh(mode):
    # monkey-use: project_cohort uses surv_annual internally; emulate stable via floor
    df=project_cohort(264,0,"A",bounded=True,max_age=100)
    return df
# raw cohort already floors at 0.5 inside project_cohort (documented). Report it:
df=project_cohort(264,0,"A",bounded=True,max_age=100)
for a in (20,40,60,100):
    r=df[df.age==a].iloc[0]
    print(f"  age {a:3d}: QMD {r.QMD:5.1f}  TPH {r.TPH:6.0f}  SDI {r.SDI:5.0f} ({r.SDI/SDI_MAX*100:3.0f}% max)")
print(f"\nWrote cfi_validation.csv -> {OUT}")
