# FVS-HI (HiGy.R) stress test, CFI validation, and survival dial-in

**Prepared for:** Ben Rice (Midgard) / FVS-HI PR #30
**By:** Aaron Weiskittel, with Cowork assist
**Date:** 2026-06-05  **Branch:** `koa-stress-test`

---

## 0. Which equations are current (resolved)

The latest finalized koa code, `koa_prediction_functions_FINAL.R` (v2,
"2026-05-12 post-pivot"), uses the NEW increment form, conditional Duan CFs
(1.026/1.030), and the published survival fit (intercept 18.133). **HiGy.R
v0.2.0 in this PR matches that file coefficient-for-coefficient** for HT, HCB,
dDBH, dHT, survival, and CFs. So HiGy.R is on the latest iteration. The
`FVS_FINAL` README TL;DR (OLD form / marginal CFs) and `koa_FVS_FINAL_parameters.csv`
(M1/S1) are pre-pivot and stale; see `discrepancies.md`. Port fidelity of the
harness to HiGy.R is machine-precision (`fidelity_check.R`, max diff ~1e-14 over
3,240 points).

## 1. Headline finding: the survival model collapses stands in FVS

The published survival cloglog is hypersensitive to crown ratio and BYI. At
DBH=12, HT=8, rHT=0.5, BYI=264 (`results/fig_survival_dialin.png`):

| CR | 0.4 | 0.5 | 0.6 | 0.7 |
|---|---:|---:|---:|---:|
| annual survival | 0.993 | 0.802 | 0.021 | 0.000 |
| 30-yr cumulative | 0.82 | 0.001 | ~0 | ~0 |

Koa crown ratios are routinely 0.5-0.8, so applied per-tree in FVS the model
predicts near-immediate death for well-crowned trees. BYI response is also
non-monotonic (0.49/yr at BYI=100). The cohort projector in
`koa_projection.py` only behaves because it fixes rHT=0.5, clamps CR>=0.20, and
floors annual survival at 0.50/yr; FVS/`HiGYOneStand` applies the raw model and
has none of those crutches.

## 2. CFI / PSP validation (first -> last measurement)

Individual-tree projection of the koa CFI/PSP network (13 plots with usable
remeasurements, ~9-14 yr spans, BYI=264 assumed) from first measurement to the
observed last, raw HiGy.R survival vs the stabilized survival:

| Variable | survival | bias | bias % | RMSE |
|---|---|---:|---:|---:|
| QMD (cm)    | raw    | -3.2 | -13.3% | 5.4 |
| QMD (cm)    | stable | -6.7 | -27.5% | 8.1 |
| BAPH (m2/ha)| raw    | -9.0 | **-99.9%** | 9.7 |
| BAPH (m2/ha)| stable | -6.4 | -71.0% | 7.5 |
| TPH (/ha)   | raw    | -186 | **-99.9%** | 199 |
| TPH (/ha)   | stable | -17  | **-9.3%** | 170 |

Raw survival wipes out the stands (TPH/BAPH -99.9%). The stabilizer recovers
realistic density (TPH bias -9.3%). The residual BAPH under-prediction (-71%) is
largely the **missing ingrowth submodel**: these are young building koa stands
where observed BAPH/TPH rise through recruitment that a no-ingrowth projection
cannot reproduce. `results/cfi_validation.csv` has the per-plot detail.

## 3. The dial-in (stabilized survival)

`koa_equations.LineageA.surv_annual_stable` clamps the survival covariates to the
cohort-calibration domain where the published fit is sane and floors annual
survival:

- CR clamped to [0.20, 0.55], rHT clamped to [0.30, 0.70]
- annual survival floored at 0.90 (max 10%/yr mortality)

These bounds are tunable and are an **operational stopgap**, not a refit. They
take TPH bias from -99.9% to -9.3% while keeping QMD within ~25%. The clamp
ceiling on CR (0.55) is the single most important lever; raising the floor toward
0.95/yr further protects young dense stands.

## 4. Long-term behavior (bounded cohort, natural, BYI=264)

With the cohort guardrails (self-thinning, HT blend, ceiling) the 100-yr
trajectory is sensible: QMD 35 -> 57 -> 75 -> 84 cm at age 20/40/60/100, SDI
peaks near 20% of SDImax then declines with self-thinning (within envelope). The
age-100 TPH falls to single digits (single-tree cohort artifact); long-term
density is governed by whatever mortality FVS applies, which is exactly why the
survival fix in section 3 matters.

## 5. Recommendation (updated)

1. **Do not ship the raw survival cloglog unguarded.** Either (a) ship the
   stabilizer (clamp CR/rHT + floor) as an interim, or (b) refit survival for
   individual-tree use. The raw model collapses real stands.
2. **Equations otherwise are current and acceptable** for HT/HCB/dDBH/dHT; keep
   the NEW form + conditional CFs (matches the FINAL code).
3. **Add an ingrowth submodel** (or document the no-ingrowth limitation): it is
   the main residual gap in young-stand BAPH/TPH.
4. **Fix the four code issues** (HCB unit-order bug in `customRun_fvsRunHi.R`,
   `Hi.GY` -> `AcadianGYOneStand` call, tree-size cap units, species scope).
5. **Reconcile the FVS_FINAL README** with its own code (see `discrepancies.md`).

## 6. Reproduce
```bash
cd fvsOL/inst/extdata/koa_stress_test
Rscript fidelity_check.R     # port-fidelity (base R)
python3 run_benchmarks.py    # cohorts + Bakuzis
python3 run_cfi.py           # CFI first->last validation + survival dial-in
```
