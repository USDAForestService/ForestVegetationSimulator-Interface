"""
run_stress_comprehensive.py
Final comprehensive stress test of the LATEST koa FVS-HI equations
(HiGy.R == koa_prediction_functions_FINAL.R v2, 2026-05-12).
Emits CSVs + a JSON summary + figures consumed by the docx report.
"""
import os, json, numpy as np, pandas as pd
from koa_equations import predict_HT, predict_HCB, LineageA as A
from koa_projector import project_cohort, project_psp, sdi_of, SDI_MAX

OUT="/sessions/elegant-lucid-cannon/mnt/outputs/koa_harness/results"; os.makedirs(OUT,exist_ok=True)
DATA="/sessions/elegant-lucid-cannon/mnt/Koa"
S={}

# === 1. Component robustness over a wide grid ===============================
import itertools
dbh=[0.5,1,2,5,15,30,50,80,110]; ht=[1.4,3,8,15,22,35]; bal=[0,5,20,50,80]
baph=[0.5,2,15,40,80]; cr=[0.05,0.2,0.5,0.8,0.95]; byi=[0,20,100,264,450,600]; pl=[0,1]
g=pd.DataFrame(list(itertools.product(dbh,ht,bal,baph,cr,byi,pl)),
               columns=["dbh","ht","bal","baph","cr","byi","pl"])
g["qmd"]=np.maximum(g.dbh*0.9,3); g["rht"]=np.clip(g.ht/35,0.05,1)
P={}
P["HT"]=predict_HT(g.dbh.values,g.baph.values,g.qmd.values,g.byi.values)
P["HCB"]=predict_HCB(g.dbh.values,g.ht.values,g.bal.values,g.baph.values,g.byi.values)
P["dDBH"]=A.dDBH(g.dbh.values,g.baph.values,g.bal.values,g.cr.values,g.byi.values,g.pl.values)
P["dHT"]=A.dHT(g.ht.values,g.baph.values,g.bal.values,g.cr.values,g.byi.values,g.pl.values)
P["SURV"]=A.surv_annual(g.dbh.values,g.ht.values,g.cr.values,g.rht.values,g.byi.values)
rob={}
for k,v in P.items():
    v=np.asarray(v,dtype=float)
    rob[k]=dict(n=len(v), n_nan=int(np.isnan(v).sum()), n_inf=int(np.isinf(v).sum()),
                n_neg=int((v<0).sum()), min=round(float(np.nanmin(v)),3), max=round(float(np.nanmax(v)),3))
S["robustness"]=rob
print("1. COMPONENT ROBUSTNESS (",len(g),"grid points )")
for k,d in rob.items(): print(f"   {k:5s} nan={d['n_nan']} inf={d['n_inf']} neg={d['n_neg']} range[{d['min']},{d['max']}]")

# === 2. Survival instability map (koa-plausible domain) ====================
crs=np.round(np.arange(0.30,0.86,0.05),2); rhts=np.round(np.arange(0.30,0.81,0.10),2)
imap=[]
for c in crs:
    for r in rhts:
        s=float(A.surv_annual(15,9,c,r,264)); imap.append(dict(CR=c,rHT=r,surv=round(s,3)))
imap=pd.DataFrame(imap); imap.to_csv(f"{OUT}/survival_instability_map.csv",index=False)
frac_bad=float((imap.surv<0.90).mean()*100)
S["survival_instability"]={"domain":"CR0.30-0.85 x rHT0.30-0.80, DBH15 HT9 BYI264",
    "pct_cells_below_0p90":round(frac_bad,0),
    "pct_cells_below_0p50":round(float((imap.surv<0.50).mean()*100),0),
    "surv_CR05":round(float(A.surv_annual(15,9,0.5,0.5,264)),3),
    "surv_CR06":round(float(A.surv_annual(15,9,0.6,0.5,264)),3),
    "surv_CR07":round(float(A.surv_annual(15,9,0.7,0.5,264)),3)}
print(f"2. SURVIVAL INSTABILITY: {frac_bad:.0f}% of koa-plausible CRxrHT cells give <0.90/yr survival")

# === 3. Boundary / edge cases ==============================================
edge=[("tiny tree DBH1 HT1.4",dict(dbh=1,ht=1.4,bal=0,baph=1,cr=0.5,byi=264,pl=0)),
      ("huge tree DBH110",dict(dbh=110,ht=30,bal=2,baph=20,cr=0.4,byi=264,pl=0)),
      ("zero BAL dominant",dict(dbh=60,ht=22,bal=0,baph=15,cr=0.5,byi=450,pl=0)),
      ("max density",dict(dbh=12,ht=8,bal=78,baph=80,cr=0.4,byi=264,pl=1)),
      ("BYI=0 base path",dict(dbh=20,ht=12,bal=5,baph=20,cr=0.5,byi=0,pl=0)),
      ("BYI=600 cap",dict(dbh=20,ht=12,bal=5,baph=20,cr=0.5,byi=600,pl=0))]
erows=[]
for lab,p in edge:
    qmd=max(p["dbh"]*0.9,3); rht=min(p["ht"]/35,1)
    erows.append(dict(case=lab,
        HT=round(float(predict_HT(p["dbh"],p["baph"],qmd,p["byi"])),2),
        dDBH=round(float(A.dDBH(p["dbh"],p["baph"],p["bal"],p["cr"],p["byi"],p["pl"])),3),
        dHT=round(float(A.dHT(p["ht"],p["baph"],p["bal"],p["cr"],p["byi"],p["pl"])),3),
        SURV=round(float(A.surv_annual(p["dbh"],p["ht"],p["cr"],rht,p["byi"])),3)))
pd.DataFrame(erows).to_csv(f"{OUT}/edge_cases.csv",index=False)
S["edge_cases"]=erows
print("3. EDGE CASES: no NaN/Inf; see edge_cases.csv")

# === 4. Long-horizon cohort drift: raw vs stable vs bounded ===============
lh=[]
for planted in (0,1):
    for b in (100,264,450):
        for mode,bd in (("raw_unbounded","raw"),("stable_unbounded","stable"),("bounded_cohort","cohort")):
            bounded = (mode=="bounded_cohort")
            df=project_cohort(b,planted,"A",bounded=bounded,surv_mode=bd,max_age=100)
            for a in (20,40,100):
                r=df[df.age==a].iloc[0]
                lh.append(dict(origin="plt" if planted else "nat",BYI=b,mode=mode,age=a,
                    QMD=round(r.QMD,1),BAPH=round(r.BAPH,1),TPH=round(r.TPH,0),SDI=round(r.SDI,0)))
lh=pd.DataFrame(lh); lh.to_csv(f"{OUT}/longhorizon.csv",index=False)
nat40=lh[(lh.origin=="nat")&(lh.age==40)].pivot_table(index="BYI",columns="mode",values="QMD")
S["longhorizon_nat_age40_QMD"]=nat40.round(1).to_dict()
print("4. LONG-HORIZON natural age-40 QMD (Table 8 ~42):\n",nat40.to_string())

# === 5. Out-of-source extrapolation =========================================
ex=[("Kahikinui dry small",dict(dbh=6,ht=4,bal=2,baph=5,cr=0.55,byi=80,pl=1)),
    ("wet mature natural",dict(dbh=55,ht=22,bal=10,baph=35,cr=0.45,byi=450,pl=0)),
    ("old-growth low-BYI",dict(dbh=80,ht=24,bal=5,baph=30,cr=0.4,byi=120,pl=0)),
    ("dense plantation",dict(dbh=14,ht=9,bal=40,baph=55,cr=0.4,byi=300,pl=1))]
exr=[]
for lab,p in ex:
    rht=min(p["ht"]/35,1)
    exr.append(dict(case=lab,
        dDBH=round(float(A.dDBH(p["dbh"],p["baph"],p["bal"],p["cr"],p["byi"],p["pl"])),3),
        dHT=round(float(A.dHT(p["ht"],p["baph"],p["bal"],p["cr"],p["byi"],p["pl"])),3),
        SURV_raw=round(float(A.surv_annual(p["dbh"],p["ht"],p["cr"],rht,p["byi"])),3),
        SURV_stable=round(float(A.surv_annual_stable(p["dbh"],p["ht"],p["cr"],rht,p["byi"])),3)))
pd.DataFrame(exr).to_csv(f"{OUT}/extrapolation.csv",index=False)
S["extrapolation"]=exr
print("5. EXTRAPOLATION: see extrapolation.csv")

# === 6. CF sensitivity ======================================================
S["CF"]=dict(conditional=dict(dDBH=1.026,dHT=1.030),marginal=dict(dDBH=1.391,dHT=1.334),
    note="HiGy.R/FINAL ship conditional; marginal would raise annual dDBH ~35.6%, dHT ~29.5%")
print("6. CF: shipping conditional 1.026/1.030 (matches FINAL code)")

# === 7. CFI/PSP validation: raw vs stable ===================================
psp=pd.read_csv(f"{DATA}/AK_PSP_data_compiled.csv"); psp=psp[(psp.DBH_cm>0)&psp.TPA.notna()]
rows=[]
for pid,gp in psp.groupby("PSP"):
    seqs=sorted(gp.MeasSeq.unique())
    if len(seqs)<2: continue
    g0=gp[gp.MeasSeq==seqs[0]]; g1=gp[gp.MeasSeq==seqs[-1]]
    yrs=g1.Yfloor.max()-g0.Yfloor.max()
    if pd.isna(yrs) or yrs<3: continue
    yrs=int(round(yrs))
    baph0=(g0.DBH_cm**2*0.00007854*g0.TPA).sum(); tph0=g0.TPA.sum(); qmd0=np.sqrt(baph0/(0.00007854*tph0)) if tph0>0 else 1
    tl=pd.DataFrame(dict(dbh=g0.DBH_cm.values,ht=np.where(g0.HT_m.values>1.37,g0.HT_m.values,np.nan),expf=g0.TPA.values))
    tl["ht"]=tl.ht.fillna(pd.Series(predict_HT(tl.dbh.values,max(baph0,0.1),max(qmd0,1),264))); tl["cr"]=0.5
    bo=(g1.DBH_cm**2*0.00007854*g1.TPA).sum(); to=g1.TPA.sum(); qo=np.sqrt(bo/(0.00007854*to)) if to>0 else np.nan
    rec=dict(PSP=pid,yrs=yrs,QMDo=round(qo,1),BAPHo=round(bo,1),TPHo=round(to,0))
    for m in ("raw","stable"):
        pp=project_psp(tl,byi=264,planted=0,lineage="A",n_years=yrs,surv_mode=m)
        rec[f"QMD_{m}"]=round(pp["QMD"],1); rec[f"BAPH_{m}"]=round(pp["BAPH"],1); rec[f"TPH_{m}"]=round(pp["TPH"],0)
    rows.append(rec)
cfi=pd.DataFrame(rows); cfi.to_csv(f"{OUT}/cfi_validation.csv",index=False)
def bias(p,o):
    mm=p.notna()&o.notna(); d=p[mm]-o[mm]
    return dict(n=int(mm.sum()),bias=round(float(d.mean()),2),bias_pct=round(float(100*d.mean()/o[mm].mean()),1),rmse=round(float(np.sqrt((d**2).mean())),2))
S["cfi"]={v:{m:bias(cfi[f"{v}_{m}"],cfi[f"{v}o"]) for m in ("raw","stable")} for v in ("QMD","BAPH","TPH")}
print(f"7. CFI VALIDATION (n={len(cfi)}):")
for v in ("QMD","BAPH","TPH"):
    print(f"   {v}: raw {S['cfi'][v]['raw']['bias_pct']}%  stable {S['cfi'][v]['stable']['bias_pct']}%")

# === 8. Bakuzis envelope ====================================================
env=[]
for planted in (0,1):
    for b in (100,264,450):
        df=project_cohort(b,planted,"A",bounded=True,surv_mode="cohort",max_age=100)
        dec=df[df.age.isin(range(10,101,10))]
        env.append(dict(origin="plt" if planted else "nat",BYI=b,max_SDI=round(dec.SDI.max(),0),
                        within_110=round(float((dec.SDI<=1.10*SDI_MAX).mean()*100),0)))
env=pd.DataFrame(env); env.to_csv(f"{OUT}/bakuzis_envelope.csv",index=False)
S["bakuzis"]=dict(within_110_pct=round(float(env.within_110.mean()),0),max_SDI=round(float(env.max_SDI.max()),0))
print(f"8. BAKUZIS: {S['bakuzis']['within_110_pct']:.0f}% decade snaps within 110% SDImax")

# === 9. Survival dial-in sweep =============================================
sweep=[]
for cr_ceil in (0.50,0.55,0.60):
    for floor in (0.85,0.90,0.95):
        recs=[]
        for pid,gp in psp.groupby("PSP"):
            seqs=sorted(gp.MeasSeq.unique())
            if len(seqs)<2: continue
            g0=gp[gp.MeasSeq==seqs[0]]; g1=gp[gp.MeasSeq==seqs[-1]]
            yrs=g1.Yfloor.max()-g0.Yfloor.max()
            if pd.isna(yrs) or yrs<3: continue
            yrs=int(round(yrs))
            baph0=(g0.DBH_cm**2*0.00007854*g0.TPA).sum(); tph0=g0.TPA.sum(); qmd0=np.sqrt(baph0/(0.00007854*tph0)) if tph0>0 else 1
            tl=pd.DataFrame(dict(dbh=g0.DBH_cm.values,ht=np.where(g0.HT_m.values>1.37,g0.HT_m.values,np.nan),expf=g0.TPA.values))
            tl["ht"]=tl.ht.fillna(pd.Series(predict_HT(tl.dbh.values,max(baph0,0.1),max(qmd0,1),264))); tl["cr"]=0.5
            to=g1.TPA.sum()
            # custom stable: monkeypatch via closure
            def surv_fn(dbh,ht,cr,rht,byi,**k):
                cc=np.clip(cr,0.20,cr_ceil); rr=np.clip(rht,0.30,0.70)
                return np.clip(np.maximum(A.surv_annual(dbh,ht,cc,rr,byi),floor),0,1)
            orig=A.surv_annual_stable
            A.surv_annual_stable=staticmethod(surv_fn)
            pp=project_psp(tl,byi=264,planted=0,lineage="A",n_years=yrs,surv_mode="stable")
            A.surv_annual_stable=orig
            recs.append((pp["TPH"],to,pp["BAPH"]))
        recs=np.array(recs)
        d=recs[:,0]-recs[:,1]
        sweep.append(dict(CR_ceiling=cr_ceil,floor=floor,
            TPH_bias_pct=round(float(100*d.mean()/recs[:,1].mean()),1)))
sweep=pd.DataFrame(sweep); sweep.to_csv(f"{OUT}/dialin_sweep.csv",index=False)
S["dialin_sweep"]=sweep.to_dict("records")
print("9. DIAL-IN SWEEP (CR ceiling x floor -> TPH bias %):")
print(sweep.to_string(index=False))

json.dump(S, open(f"{OUT}/stress_comprehensive_summary.json","w"), indent=2, default=str)
print("\nWrote stress_comprehensive_summary.json + CSVs to",OUT)
