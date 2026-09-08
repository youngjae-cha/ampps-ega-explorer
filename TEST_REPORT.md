# Review-release checks

Release: **1.0.3-review**. Local checks performed on 2026-09-08 with R 4.4.1 on macOS; direct package versions are in `package_versions.csv`, and the deployment dependency closure is in `manifest.json`.

| Check | Local result |
| --- | --- |
| `tests/test_model.R` | Pass: all 44 fits, OLS/HC3 references, invalid inputs, escaped exports and numerical reproduction |
| `tests/test_ega.R` | Pass: archived graph/rings, live preview and bootstrap, compatibility regressions, reproducibility |
| `tests/test_server.R` | Pass: real Shiny server, new uploads/paste, stale-state blocking, boundary invariance, annotation isolation |
| `tests/test_provenance.R` | Pass: source labels, inherited-lag counts, all 44 sensitivity fits and reference values |
| `tests/test_input_export.R` | Pass: actual export extraction/re-execution, raw-input opt-in and demo/upload separation |
| `tests/test_review_ui.R` | Pass: privacy wording, readable estimate table, all metric views, variable focal counts and user-label isolation |
| `tests/test_orientation.R` | Pass: six documented reversals, raw-fit preservation, correctly reversed interval endpoints, unchanged absolute t and p-values, explicit upload keys, and exported reproduction |
| `tests/test_navigation.R` | Pass: successful demo/upload builds advance, failed builds stay put, busy state resets, and setting changes do not advance |
| `tests/test_navigation.js` | Pass: immediate Next activation, top-of-step focus/scroll, reduced-motion behavior, and duplicate-build prevention |

Local Chrome checks also confirmed successful Build → Map, Map → Results → Interpret, and switching between the documented and original coding views. HAPPY changes from original b = −.0076 to displayed b = +.0076 with the corresponding interval reversal; its absolute t remains 4.01. The deployed session is checked separately after publication.

Observed non-fatal warnings: ggplot2 was built with a newer R patch version; `geom_point` reports the Plotly hover-text aesthetic as unknown. Interactive plot outputs still render in the server tests. The deployed Linux runtime is a separate test target; these local passes are not evidence that a cloud build has succeeded.

Public-source preparation excludes manuscripts, internal review records, respondent-level records, credentials, generated QA folders, and archives. The four numerical/network modules retain the tested calculations; hosting changes update display labels and privacy wording, not estimates or graph membership.
