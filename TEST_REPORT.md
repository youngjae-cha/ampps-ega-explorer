# Version 3.0.1 verification

## Manuscript display alignment, 2026-10-03

The Map screen now retains a separate color for each EGA community, labels all outcomes, marks focal outcomes with diamonds, and outlines the current comparison. Community colors and node coordinates remain fixed when boundaries change. The Results navigation button uses the tutorial wording. Eleven GSS neighborhood items carry question wording, response options, respondent scope, and a codebook source in the hover display and exported item annotations. These display records use the project's existing item documentation; no annual values, graph memberships, regression fits, or reference calculations were changed.

The following checks passed after these changes: `test_map_display.R`, `test_review_ui.R`, `test_boundary_ui.R`, `test_reference_server.R`, `test_server.R`, and `test_input_export.R`. They cover the rendered Plotly widget, 44 labels, four community colors, three focal diamonds, eight outlined community neighbors, stable colors/coordinates/fits across boundary changes, escaped uploaded labels, metadata in exports, uploaded-data analysis, reference reproduction, and stale-result blocking. Local browser inspection confirmed the updated map and tutorial button.

## Version 3.0.0 baseline verification

Local verification on 2026-10-02, macOS, R 4.4.1. This release starts from public commit `8fb2cd86de62b3a339c8d282df7366803c0ad84f` (1.0.4-review) in a separate checkout. The regression, EGA and input-provenance engines and historical tutorial inputs retain their source contents. New work concerns the conditional rank-sum reference, comparison reporting, plotted overview, state/export integration and reproducible package setup.

## Automated checks

All twelve R scripts below and the JavaScript navigation test passed in this release. Generated logs stay in ignored `qa/v3-tests/`.

| Script | Verified behavior |
| --- | --- |
| `tests/test_model.R` | All 44 fits; independent OLS/HC3 references; invalid inputs; CSV escaping; numerical export reproduction. |
| `tests/test_ega.R` | Archived graph and boundaries; actual synthetic-data EGA and bootstrap; small-network compatibility; reproducibility. |
| `tests/test_server.R` | GSS, uploaded files and pasted data; stale model/file blocking; fixed fits across boundaries; annotation isolation. |
| `tests/test_provenance.R` | Source-label counts, inherited-lag counts, all 44 source-restricted fits and saved reference values. |
| `tests/test_input_export.R` | Real server ZIP extraction and execution; all 44 source-restricted estimates reproduce; raw-input opt-in and GSS/upload separation. |
| `tests/test_review_ui.R` | Individual estimates and intervals; semantic direction; three display modes; native Plotly box/raw-point overview; 3/8/33 example groups; uploaded-label isolation. |
| `tests/test_orientation.R` | Documented reversals, unchanged raw fits and absolute t/p, interval endpoint reversal, user keys and exported reproduction. |
| `tests/test_navigation.R` | Successful builds advance; failed builds remain; busy controls reset; setting changes do not advance. |
| `tests/test_boundary_ui.R` | Default EGA community, optional boundary controls, active boundary labels/reset, all exported boundaries and unchanged fits. |
| `tests/test_concentration.R` | 45 independent Wilcoxon distribution comparisons (k=2–10); direct tied-subset enumeration; correct attainable floor/rate; single/full reports; invalid input rejection; resource cap; descriptive top-subset ties. |
| `tests/test_reference_server.R` | GSS without reference p; claim-specific family/subset; both statistical-condition routes; required declarations; stale choice invalidation; signed direction changes; unchanged fits; reference-free stale exports; exported exact distribution re-execution. |
| `tests/test_runtime.R` | All 195 locked versions match; no qs/tidyverse dependency or precomputed RDS; missing/mismatched direct packages detected. |
| `tests/test_navigation.js` | Next-tab activation, success-only build navigation, focus/scroll, reduced motion and duplicate-build guard. |

The initial runtime check compared version strings literally; it was corrected to use R's package-version comparison so `1.84.0-0` and `1.84.0.0` are correctly treated as the same version. A ggplot-to-Plotly boxplot conversion failed in the browser and was replaced by native Plotly boxplots with observed points; a rendered-widget regression check was added.

## Actual local browser checks

- App starts without `precomputed_results.rds` and without qs installed.
- GSS Build → Map displays the 11-outcome focal community. Results shows raw points/boxplots and the individual estimates. Interpret retains the concrete rank comparison and attaches no reference probability.
- A pasted synthetic 120-row, eight-outcome dataset executes new EGA and common-model fits through the interface. Its selected community contains four outcomes.
- The interface accepts an explicit four-outcome family, fixed report Item1, |t| statistic, stated fixed-label symmetry justification and independent-specification record. It produces W=2, reference p=.5, four subsets, minimum attainable p=.25 and attainable rate at .05 of zero.
- Interpret carries the same claim, family, subset, metric, assumption and calculation. The reproducibility ZIP download completes in the browser. These numbers are software fixtures, not manuscript findings.

File-upload analysis also executes in the actual Shiny-server tests. Browser QA used the application's CSV-paste route; the browser automation's file-chooser event timed out, so that separate browser-picker interaction is not claimed as verified.

## Packages and release scope

`renv.lock` fixes 195 direct/transitive package versions, including R 4.4.1 as the tested runtime. The local library was populated from already installed packages, then `install_dependencies.R` and `renv::restore` were actually executed against it. All versions matched and restore reported synchronization. This verifies local restoration against the lock; it is not a fresh network install on an empty machine. No user-wide package versions were changed. The launcher checks direct versions and uses cli messages; app-screen messages use Shiny.

`manifest.json` is rebuilt from an explicit file allowlist and the lock. It is a deployment artifact, not evidence of a successful cloud deployment. This document records local verification; public release status is tracked separately in the deployment dashboard.

Expected nonfatal warnings remain for a ggplot2 binary built under a later R patch version and the existing Plotly hover-text aesthetic in other ggplot displays. Their widgets and numerical outputs pass the tests. The Linux/cloud deployment has its own execution environment and requires separate deployment verification.

No manuscript, response letter, SI, respondent-level GSS data, private paths, credentials, generated QA logs or local package library is included in the committed app source.
