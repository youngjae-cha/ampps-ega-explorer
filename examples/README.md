# Try the upload workflow

1. Select **Your CSV** and upload `upload_example.csv` as the analysis file.
2. Use `X` as predictor and `Item01`–`Item12` as candidate outcomes. No covariate or own-lag term is needed. Choose any focal outcomes to explore their empirical neighbors.
3. Optionally upload `metadata_example.csv`. The network can use the same analysis rows; no separate network file is required.
4. Build the analysis, inspect communities and boundaries, then compare estimates and export a report.

This fixture contains 200 simulated rows and two correlated generating blocks. Its labels and coefficients are arbitrary software examples, not evidence about real constructs, researcher behavior, or the method's scientific performance. A recovered community is not certified merely because a generating block is named in the metadata.

Recreate the exact CSV fixture using base R:

```sh
Rscript examples/make_upload_example.R
```

The script uses seed `20260907`. The fixture has no missing cells, so the first upload exercise demonstrates the basic workflow without requiring missing-data decisions. Missingness handling is tested separately in the automated model and network tests.
