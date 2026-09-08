# EGA Explorer

A four-step application for putting reported findings in the context of unreported outcomes in secondary data.

**Research preview · 1.0.4-review.** This repository contains the app, small aggregate tutorial inputs, synthetic upload examples, and software tests. It does not contain the manuscript, reviewer correspondence, respondent-level GSS records, or private research files.

## Try the four steps

1. **Define:** specify the focal outcomes, predictor, and model. Include other candidate outcomes meeting the same coding and data-availability requirements. The supplied GSS example has these inputs prepared.
2. **Map:** begin with the focal EGA communities, selected automatically. No distance setting is required. An optional collapsed section compares predefined 15/25/35% distance rings; all three are computed and exported. If you change the view, its label remains visible beside the map, with a button to return to the EGA communities.
3. **Results:** compare reported and unreported outcomes, then read each original-unit coefficient with its interval. Absolute t is statistical prominence, not effect size.
4. **Interpret & export:** describe what a comparison adds, distinguish related outcomes from justified substitutes, and download the complete estimates and reporting record.

For a first upload test, select **My CSV data → Try a synthetic dataset** in the app. Download the example CSV, upload it as analysis data, and choose `X` as predictor and `Item01`–`Item12` as outcomes. The optional descriptions file supplies labels. These are artificial software-test data, not new scientific evidence.

## Privacy

The shared web app is publicly accessible. Uploaded and pasted data are sent to its hosting server for computation. **Use public or synthetic data only. Do not upload confidential, identifiable, or restricted data.** The app has no cross-session results gallery and does not deliberately save inputs to a persistent database, but this is not a security or data-retention guarantee. Downloaded reports contain variable names, estimates, and any notes you enter.

Use a local copy for sensitive data only after checking your own institution's policies and computer/storage configuration. Uploaded rows are excluded from reproducibility downloads unless explicitly requested. The bundled aggregate example is included when exporting that example.

## Run locally

Install R, then run these commands from this directory:

```sh
Rscript install_dependencies.R
Rscript run_app.R
```

Open `http://127.0.0.1:7860`. The local launcher binds only to loopback. `AMPPS_PORT` selects another port. The app never installs packages automatically. The installer obtains missing CRAN packages; `manifest.json` records the deployment package versions separately.

## Deploy on Posit Connect Cloud

Choose **Publish → From GitHub**, select this repository and its `main` branch, choose **Shiny for R**, and select **app.R** as the primary file. The root `manifest.json` contains the deployment dependencies. Give the GitHub integration access to this repository only. Free-plan publication is public.

The deployed Linux build must be checked separately from the local tests. For a review release, turn off automatic republishing, record the tested commit, and update intentionally after fixes. Do not use `run_app.R` as the hosted entry point: Connect Cloud runs `app.R` itself.

## Methods and limits

- Uploaded data use actual EGAnet TMFG and Walktrap. The map uses outcome data, without the focal predictor. Bootstrap mode adds a typical structure and item stability; preview, bootstrap, and archived maps are labeled separately.
- Retained graph edges are reweighted with zero-order outcome correlations. Distance length is `1/abs(r)`. Rings use the minimum path distance to any focal outcome and type-7 quantiles of finite nonfocal distances, retaining ties and all focal outcomes. All boundaries are exported.
- A community comparison is the union of communities containing focal outcomes. Community membership does not establish conceptual equivalence or statistical exchangeability.
- Regressions use one numeric predictor, optional shared numeric covariates, and optional supplied own-lag columns. Classical OLS and HC3 are supported; neither addresses serial dependence, survey design, or clustering automatically. Generalized, multilevel, and survey-weighted models are not implemented.
- Original inputs and fits are retained. Step 3 defaults to **Display direction → Documented semantic key**, with an **Original coding** view available. The GSS key aligns six monotone items (HAPPY, HAPMAR, HEALTH, LIFE, SATJOB, NEWS; marked **RC**) and retains FINRELA's already-increasing direction. TRUST, FAIR and HELPFUL retain original coding because their `depends` category is not ordinal; FINALTER remains unverified because source and inherited recoding need reconciliation. The other outcomes remain original unless an explicit key is supplied. Alignment follows documented item meaning, never fitted signs, correlations or PCA. It does not make distinct constructs equivalent.
- Coefficients, signed t statistics and both interval endpoints use the same direction key throughout the plots, tables, card and readable report. SE, p-values, |t|, sample sizes and raw fits stay unchanged. Exports retain raw plus `oriented_*` fields, the active key, and the documented key. Optional upload metadata may provide `orientation_multiplier` (-1/+1), `high_value_means`, and `orientation_source`; these are labeled user-declared.
- Optional Bonferroni sensitivities name the boundary and complete analyzable universe separately. They assume valid individual tests and a justified family, do not debias coefficients, and are not cherry-picking verdicts. Rank-null testing is not part of this app.
- Changing analysis settings invalidates old results and blocks export until recomputation. Boundary changes reuse the same fits and graph layout. Each session keeps its own uploaded data and annotations.

See [data provenance](data/README.md) for the archived GSS map, inherited annual inputs, and source-restricted sensitivity. The aggregate tutorial does not certify the original study or demonstrate the validity of inference for all datasets.

## Software verification

```sh
Rscript tests/test_model.R
Rscript tests/test_ega.R
Rscript tests/test_server.R
Rscript tests/test_provenance.R
Rscript tests/test_input_export.R
Rscript tests/test_review_ui.R
Rscript tests/test_orientation.R
Rscript tests/test_navigation.R
Rscript tests/test_boundary_ui.R
```

The tests compare all 44 example fits against saved references; check 44 nodes, 262 edges, and the 11/10/14/18 community/ring sizes; execute live EGA on synthetic uploads; and verify state invalidation and export reproduction. Local compatibility guards for EGAnet 2.3.0 repair upstream small-network edge cases without modifying the installed namespace. Tests check agreement with the native algorithm where it works. These are software checks, not a claim that a recovered community is substantively correct.

When reporting a problem, include the app version, which step failed, what you clicked, and a screenshot or error message. **Do not attach confidential data to a public GitHub issue.**
