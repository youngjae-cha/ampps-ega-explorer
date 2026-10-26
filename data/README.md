# Bundled tutorial inputs

These are small aggregate fixtures copied from the existing project on 2026-09-07, not new raw-data extraction or newly fitted scientific evidence.

- `gss_year.csv`: inherited 47-row annual aggregate outcomes, lagged outcomes, and `MobilityLag`; copied from `app_v2/data`.
- `gss_year_cell_source.csv`: original pipeline's cell-source labels (`extract`/`carried`), not a validated interpolation algorithm.
- `landscape_reference.csv`: saved 44-outcome OLS results from `v4_draft/02_gss_landscape_v4.csv`; used to test the implementation, never used in place of live model fitting.
- `typical_network.csv`, `item_cor_respondent.csv`, `ega_membership.csv`: saved respondent-level EGA structure, zero-order correlations, and membership from `app_v2/data`.
- `item_stability.csv`: saved item-level bootstrap stability from `ega_v2_outputs`.
- `effect_landscape.csv`: earlier saved table used **only for item labels**, not current estimates.

The annual aggregate model input and respondent-level network are different sources/analytic levels. The application does not recreate respondent-level EGA by treating years as respondents. Inherited annual values are analyzed unchanged; the application does not impute missing values. Classical OLS uncertainty reproduces the tutorial specification without adjusting for inherited interpolation or possible serial dependence.

`orientation_key.csv` supplies the documented display directions for the GSS example. HAPPY/HAPMAR increase from very happy (1) to not too happy (3), HEALTH from excellent (1) to poor (4), LIFE from exciting (1) to dull (3), SATJOB from satisfied (1) to dissatisfied (4), and NEWS from daily (1) to never (5); multiplying their coefficient, t statistic and interval endpoints by -1 makes higher displayed values mean more of the stated positive content. RC marks these changes and the three inherited-code reversals below. FINRELA already runs from below-average to above-average income and stays +1. Other outcomes are not silently assigned a direction. Sources are recorded per key; no direction was selected from fitted signs, correlations or PCA.

The inherited analysis swaps source questionnaire codes 2 and 3 for TRUST, HELPFUL, FAIR and FINALTER. Its fitted coding is TRUST: 1 = can trust, 2 = depends, 3 = cannot trust; HELPFUL: 1 = helpful, 2 = depends, 3 = looking out for self; FAIR: 1 = take advantage, 2 = depends, 3 = fair; FINALTER: 1 = better, 2 = stayed same, 3 = worse. The documented display therefore reverses TRUST, HELPFUL and FINALTER (-1) and retains FAIR (+1). This coding lineage was checked on 2026-10-25 against the existing project script `R code and datafiles for OSF/Study1-GSS-1-EGA-data cannot provided.R` (lines 53–76) and retained `data_filtered.csv`: its unweighted item-by-year means match the current annual cells labeled `extract` within 5e-15 (28 survey years each for TRUST/HELPFUL/FAIR; 32 for FINALTER). The check did not regenerate carried/interpolated cells or inspect the raw SAV. Here `verified` denotes the documented inherited code mapping and display direction; placing `depends` at code 2 does not establish a psychological midpoint or equal intervals. No input values or models were changed.

Display alignment never changes the stored input, fitted raw coefficients, SE, p-values or |t|. Both raw and oriented estimates are exported, together with active and documented keys. For a reversal the displayed interval is [-raw upper, -raw lower], not an inverted interval. This operation aligns item meaning; it does not claim a common construct, scale, respondent base or expected predictor effect.

The app can separately analyze a user's own numeric CSV and estimate live EGA using supplied outcome rows. No uploaded data are included in exported bundles unless explicitly requested.
# Item wording in the map

`item_wordings.csv` supplies the eleven GSS neighborhood items' questions, response options, respondent scopes, and public codebook links from the existing project item documentation. The map displays these fields and the reproducibility ZIP retains them in `item_annotations.csv`. The other outcomes retain their available labels and measurement notes. These records describe questionnaire items; the supplied annual values and their coding remain unchanged.
