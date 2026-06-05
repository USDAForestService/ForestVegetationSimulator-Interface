# FVS-HI (HiGy.R) stress test and refinement review

**Prepared for:** Ben Rice (Midgard Natural Resources) / FVS-HI PR #30
**By:** Aaron Weiskittel, with Cowork assist
**Date:** 2026-06-05
**Branch:** `koa-stress-test` (off MidgardNaturalResources `main`, PR #30 head)

---

## 1. Scope and method

This branch adds a self-contained harness (`koa_stress_test/`) that exercises the
FVS-HI component equations independent of the compiled FVS variant, so the koa
growth logic can be benchmarked and refined before it ships. Two equation
lineages are run through one stepping engine:

- **Lineage A — "PR / manuscript":** the equations exactly as written in this
  PR's `HiGy.R` v0.2.0 (and identical to `koa_projection.py`). dDBH/dHT use the
  `BAL^2/ln(size)` + `sqrt(BAPH*size)` form; survival is `exp(-exp(eta))` on the
  18.133 anchor; conditional Duan CFs 1.026/1.030.
- **Lineage B — "FVS_FINAL / M1-S1":** the May-2026 stress-test recommendation
  in `koa_FVS_FINAL_parameters.csv`: increment M1 form (`sqrt(SDI)`, `rHT`,
  `log(CR*size)`, two BYI terms), survival S1 cloglog with `ln(YIP)` offset, and
  **marginal** Duan CFs 1.391/1.334.

Engines: a stand-level cohort projector run both **bounded** (reproducing
`koa_projection.py`'s self-thinning, 0.65/0.35 HT blend, and `min(HT,25+BYI/55)`
ceiling) and **raw/FVS-like** (no guardrails, mirroring what `HiGYOneStand`
actually applies); and an individual-tree-list engine mirroring `HiGYOneStand`
for real plots. Data: synthetic low/med/high BYI cohorts and 14 remeasured koa
PSPs (`AK_PSP_data_compiled.csv`).

Files: `koa_equations.py`, `koa_projector.py`, `run_benchmarks.py`,
`fidelity_check.R`, `results/`.

## 2. Port fidelity — PASS

`fidelity_check.R` evaluates the **verbatim** `HiGy.R` `pred_ht`/`pred_hcb`/
`ddbh`/`dht`/`surv_prob` bodies over 3,240 input combinations; the Python port
reproduces them to machine precision:

| Component | max \|R − Python\| |
|---|---|
| HT | 4.6e-14 |
| HCB | 5.0e-14 |
| dDBH | 4.9e-15 |
| dHT | 4.9e-15 |
| Survival | 1.5e-15 |

So the harness's Lineage A *is* `HiGy.R`, and the R code faithfully matches the
manuscript/Python reference it was ported from.

## 3. The lineage divergence (the headline)

`HiGy.R` ships the **manuscript/Python lineage**, not the May-2026 FVS_FINAL
recommendation. Coefficient evidence: dDBH/dHT/survival in `HiGy.R` are identical
to `koa_projection.py`, whereas `koa_FVS_FINAL_parameters.csv` +
`stress_test_summary.md` recommend the M1/S1 form with marginal CFs. HT agrees
across all three; HCB in the PR matches the FINAL refit (b0=0.1684...). Net: the
PR did not adopt the form/CF switch the May handoff concluded should ship. This
is a decision for you and Aaron, informed by the benchmarks below.

## 4. Long-term behavior (100-yr cohorts)

Natural cohort QMD (cm) at age 40 (manuscript Table 8 expectation ≈ 42 cm):

| BYI | A raw | A bounded | B raw | B bounded |
|----:|------:|----------:|------:|----------:|
| 100 | 43.0 | 43.4 | 13.4 | 13.4 |
| 264 | 56.3 | 56.9 | 21.2 | 21.2 |
| 450 | 52.8 | 57.2 | 22.4 | 22.4 |

- **Lineage A** lands near Table 8 on low sites and the operational guardrails
  barely bind for natural-origin stands (raw ≈ bounded), i.e. for realistic
  natural starting densities `HiGy.R` does **not** show the small-tree runaway
  documented for QMD=8 cm planted starts; the per-step caps plus rising BAL hold
  it. The runaway risk is concentrated at very small initial DBH and on dense
  planted starts — the regime to test in true FVS runs.
- **Lineage B (M1)** grows much slower (QMD 13–22 at age 40) and accumulates
  almost no density (max SDI 3–15). This is highly sensitive to the `sqrt(SDI)`
  and `rHT` covariate **definitions**, which are not fully pinned down in the
  FINAL artifacts. Reproducing M1 cleanly in FVS will require the exact SDI units
  (Reineke ref 25 vs 25.4 cm; per-ha vs per-plot) and rHT definition used at
  fitting — a portability risk if B is chosen.

## 5. Bakuzis / SDI envelope

Bounded runs for both lineages keep 100% of decade snapshots within 110% of
SDImax (500). Lineage A high-BYI stands push SDI to ~250–280 before
self-thinning; Lineage B never approaches the envelope because it under-grows.
See `results/fig_cohort_trajectories.png` and `bakuzis_envelope.csv`.

## 6. Real-plot validation (14 remeasured koa PSPs, BYI=264 assumed)

Individual-tree projection (`HIGYOneStand`-style) from first to last measurement:

| Lineage | QMD bias | QMD bias% | BAPH bias | BAPH bias% |
|---|---:|---:|---:|---:|
| A (PR) | −3.5 cm | −14.7% | −8.5 m²/ha | **−99.9%** |
| B (M1) | +3.4 cm | +14.5% | −2.1 m²/ha | −24.7% |

**Key finding:** Lineage A's survival model **collapses stands** in tree-list
mode — near-total mortality (BAPH → ~0) — because annual survival compounds and
the model is hypersensitive to BYI through the large `−25.1·BYI/1000` term.
With per-plot BYI unknown (assumed 264 here), this swings mortality enormously.
Lineage B survival (S1, with the `ln(YIP)` offset and bounded coding) is far more
stable. This is the single most important refinement target for `HiGy.R`.

*Caveats:* n=14 PSPs, BYI assumed constant, natural origin assumed; treat as
directional. The cohort engine masked this because it floored annual survival at
0.50 — the tree-list engine (faithful to `HiGYOneStand`) does not, exposing the
collapse.

## 7. Code issues carried from the static review (still open)

1. **Unit-order bug, first-cycle HCB imputation** in `customRun_fvsRunHi.R`: HT
   converted to feet *before* `calc_hcb` (which expects metres) → biased dubbed
   crown ratios. Compute HCB while metric, convert to feet last.
2. **`Hi.GY()` calls `AcadianGYOneStand()`** instead of `HiGYOneStand()` — latent.
3. **Tree-size cap units:** `tree.size.cap` AK=(90,92) multiplied by `in.to.cm`/
   `ft.to.m`; if 90 was cm (FIA), the DBH cap is effectively disabled (228 cm).
4. **Species scope vs GUI:** only AK is parameterized; ʻōhiʻa/sandalwood map to
   `OT` and are held static. Document or add surrogates.

## 8. Recommendation

1. **Fix survival before anything else.** The PR (Lineage A) survival is the
   clearest failure mode (stand collapse, BYI hypersensitivity). Either adopt the
   S1 cloglog form (with the `ln(YIP)` offset and a documented BYI scale/cap), or
   re-anchor the A survival and add a BYI guard. Re-run §6 until BAPH bias is
   within ±25%.
2. **Keep Lineage A increment + HT** for now: it tracks Table 8 and validates
   acceptably for natural stands, and porting M1 is blocked by undocumented
   SDI/rHT definitions. If you want the marginal CFs (1.391/1.334) instead of
   conditional, that is a separate, low-risk swap — test it explicitly.
3. **Add the operational guardrails to FVS-HI** or confirm FVS supplies them:
   the manuscript HT blend, biological ceiling, and SDI-based self-thinning are
   what keep small-DBH/dense-planted starts from running away. The per-step
   increment caps alone are not sufficient at QMD < ~5 cm.
4. **Fix the four code issues in §7.**
5. **Validate in true FVS** on the dense-planted, small-DBH regime (the one this
   equation-level harness cannot fully exercise) before release.

## 9. Reproduce

```bash
cd fvsOL/inst/extdata/koa_stress_test
Rscript fidelity_check.R          # port-fidelity check (base R)
python3 run_benchmarks.py         # cohorts + Bakuzis + PSP validation -> results/
```
Python needs numpy/pandas/matplotlib; R needs no packages.
