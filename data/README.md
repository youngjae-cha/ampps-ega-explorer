# Bundled tutorial inputs

These are small aggregate fixtures copied from the existing project on 2026-09-07, not new raw-data extraction or newly fitted scientific evidence.

- `gss_year.csv`: inherited 47-row annual aggregate outcomes, lagged outcomes, and `MobilityLag`; copied from `app_v2/data`.
- `gss_year_cell_source.csv`: original pipeline's cell-source labels (`extract`/`carried`), not a validated interpolation algorithm.
- `landscape_reference.csv`: saved 44-outcome OLS results from `v4_draft/02_gss_landscape_v4.csv`; used to test the implementation, never used in place of live model fitting.
- `typical_network.csv`, `item_cor_respondent.csv`, `ega_membership.csv`: saved respondent-level EGA structure, zero-order correlations, and membership from `app_v2/data`.
- `item_stability.csv`: saved item-level bootstrap stability from `ega_v2_outputs`.
- `effect_landscape.csv`: earlier saved table used **only for item labels**, not current estimates.

The annual aggregate model input and respondent-level network are different sources/analytic levels. The application does not recreate respondent-level EGA by treating years as respondents. Inherited annual values are analyzed unchanged; the application does not impute missing values. Classical OLS uncertainty reproduces the tutorial specification without adjusting for inherited interpolation or possible serial dependence. Coding is retained, and direction alignment requires an explicit documented key.

The app can separately analyze a user's own numeric CSV and estimate live EGA using supplied outcome rows. No uploaded data are included in exported bundles unless explicitly requested.
