# Discrepancies found in the koa equation provenance (2026-06-05)

Flagged per CRSF workspace rules. None block the harness; all should be resolved
before FVS-HI release.

## 1. The May-12 FVS_FINAL package contradicts itself

`FVS_FINAL_2026_05_12/README.md` "TL;DR for Ben" instructs:
1. use the **OLD** equation form,
2. use the **marginal** Duan CFs (1.391 / 1.334).

But the actual drop-in code in the same folder,
`koa_prediction_functions_FINAL.R` (v2, header "2026-05-12 post-pivot"),
implements:
1. the **NEW** form (`b0 + b1*log(size+1) + b2*size + b3*BAL^2/log(size+5) +
   b4*log(BAL+1) + b5*log(CR) + b6*sqrt(BAPH*size) + b7*Planted*size +
   b8*log(BYI)`), and
2. the **conditional** CFs (1.026 / 1.030),
3. the **published** survival fit (intercept 18.133), explicitly rejecting the
   n=6,489 refit (S1, intercept 36.767) because it "predicts 30 percent annual
   mortality at age 40 [and] breaks operational projection."

So the "post-pivot" code reversed the README's recommendation. The
`koa_FVS_FINAL_parameters.csv` (M1 sqrt(SDI)+rHT form, marginal CFs, S1
survival) reflects the pre-pivot decision and is **not** what the FINAL code or
HiGy.R use.

**Consequence:** the equations Ben shipped in `HiGy.R` v0.2.0 match
`koa_prediction_functions_FINAL.R` (the latest code) coefficient-for-coefficient
for HT, HCB, dDBH, dHT, survival, and CFs. HiGy.R is therefore on the **latest**
equation iteration. The README TL;DR and parameters.csv are stale.

**Action:** update `FVS_FINAL_2026_05_12/README.md` so the TL;DR matches the code
(NEW form, conditional CFs, published survival), or delete the stale guidance.

## 2. Survival model is unstable outside the cohort-average regime

The published survival cloglog (used by HiGy.R and FINAL code) has very large
coefficients and is hypersensitive to crown ratio and BYI. At DBH=12, HT=8,
rHT=0.5, BYI=264 the annual survival is 0.80 at CR=0.5 but 0.02 at CR=0.6 and
0.00 at CR>=0.7; BYI response is non-monotonic (0.49/yr at BYI=100). Koa crown
ratios are routinely 0.5-0.8, so applied per-tree in FVS this drives near-total
stand collapse (CFI validation BAPH/TPH bias -99.9%). The cohort projector hides
this by fixing rHT=0.5, clamping CR>=0.20, and flooring annual survival at 0.50.
See REPORT.md sections 4-6 and `koa_SURVIVAL_NOTE` below.

**Action:** refit survival for individual-tree application, or ship the
operational stabilizer (clamp CR/rHT to calibration domain + annual-survival
floor) documented in the harness; do not ship the raw cloglog unguarded.

## 3. Minor: parameters.csv survival n and slenderness units

`koa_FVS_FINAL_parameters.csv` S1 notes "log(HT/(DBH/100)) ... HT_m/DBH_m" while
the FINAL code comment uses "HT/DBH, m/cm, mixed units." Confirm the slenderness
unit convention used at fitting before any refit.
