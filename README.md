# mvher — Missing Value Handling for Entity Resolution

[![R package](https://img.shields.io/badge/R-package-blue)](https://github.com/yourusername/mvher)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

`mvher` is a standalone R package that provides principled missing-value
handling for **entity resolution** (record linkage, de-duplication) pipelines.

## The problem

Standard string-similarity metrics treat two empty strings as *perfectly
matching* (`sim = 1`).  In an entity resolution classifier this sends a
spurious "definitely a match" signal for pairs where both records simply lack a
field — the *both-null=1 artefact*.  Under entity-disjoint train/test splits
this artefact creates systematic covariate shift when missingness is
entity-correlated (e.g., one data source consistently omits a field).

## What mvher provides

| Module | Functions | Purpose |
|---|---|---|
| **Diagnosis** | `mv_field_summary()` | Per-column empty/NA rate |
| | `mv_pair_summary()` | Pair-level any/both-missing counts |
| | `mv_detect_mechanism()` | Chi-sq test → MCAR vs MAR/MNAR |
| | `mv_entity_correlation()` | One-way ICC of missingness indicator |
| **Field comparison** | `mv_compare_fields()` | Vector-level comparison with 8 MV strategies |
| | `mv_pair_features()` | Full feature matrix from candidate pairs |
| **Imputation** | `mv_fit_imputer()` | Fit imputer on training matrix |
| | `mv_apply_imputer()` | Apply imputer (train or test) |
| | `mv_impute()` | One-step fit + apply |
| **Guidance** | `mv_strategy_table()` | 15 strategies × 9 property axes |
| | `mv_recommend()` | Decision-rule recommendation |
| **Utilities** | `norm_str()` | Lower, trim, NA→"" |

## Field-comparison strategies (`mv_pair_features(..., method = ...)`)

| Strategy | Effect on empty pairs |
|---|---|
| `"current"` | Both-empty → sim=1 (backward-compat, semantically wrong) |
| `"neutral"` | Any-empty → sim=0.5 (Fellegi-Sunter absent level) |
| `"zero"` | Any-empty → sim=0.0 (conservative) |
| `"asymmetric"` | Both-empty→0, one-empty→0.5, else raw |
| `"indicator"` | Raw sim + `{col}_any_miss` / `{col}_both_miss` columns |
| `"full"` | Neutral (0.5) + indicator columns **(recommended)** |
| `"fellegi_sunter"` | Integer γ-level 0–4 |
| `"complete_cases"` | NA for missing pairs (requires downstream handling) |

## Imputation strategies (`mv_fit_imputer(..., strategy = ...)`)

`"mean"`, `"median"`, `"zero"`, `"half"` (0.5), `"hot_deck"`, `"knn"`, `"none"`

## Installation

```r
# Install from GitHub (recommended)
# install.packages("remotes")
remotes::install_github("yourusername/mvher")

# Or install locally from the package directory
devtools::install("path/to/mvher")
```

Dependencies: **data.table** ≥ 1.14.0, **stringdist** ≥ 0.9.0

## Quick start

```r
library(mvher)

records <- data.frame(
  id        = 1:4,
  entity_id = c(1L, 1L, 2L, 2L),
  name      = c("Alice Smith", "Alice Smyth", NA, "Bob Jones"),
  city      = c("London", "Londen", "Paris", NA)
)
pairs <- data.frame(id1 = c(1, 1, 3), id2 = c(2, 3, 4))

# 1. Diagnose missingness
mv_field_summary(records, c("name", "city"))
mv_entity_correlation(records, c("name", "city"))

# 2. Get strategy recommendation
mv_recommend(field_miss_rate = 0.25, entity_corr = 0.5, split_type = "entity")
# → field_method = "full", imputer_strategy = "mean"

# 3. Extract features with 'full' strategy
feats <- mv_pair_features(records, pairs,
                          text_cols = c("name", "city"),
                          method    = "full")
# feats$X: JW + LV + _any_miss + _both_miss per column
# feats$y: 1/0 match labels

# 4. Impute residual NAs (fit on train, apply to both)
imp    <- mv_fit_imputer(feats$X, strategy = "mean")
X_clean <- mv_apply_imputer(feats$X, imp)
```

## The creative finding: entity-correlated missingness + entity-disjoint splits

When missingness is entity-correlated (ICC > 0.3) and evaluation uses
entity-disjoint splits, the both-null=1 artefact causes covariate shift:

- Training entities may have low missingness → few both-null pairs → model
  sees low both-null frequency.
- Test entities from a missing-heavy source → many both-null pairs → model
  receives inflated match signals.

**Fix:** use `method = "full"`.  The `{col}_both_miss` indicator column allows
the classifier to learn a *negative* weight for the spurious signal instead of
treating it as a positive similarity.

See `vignette("mvher-intro")` for a complete walkthrough.

## Package structure

```
mvher/
├── DESCRIPTION
├── NAMESPACE
├── R/
│   ├── mvher-package.R     # package-level documentation
│   ├── utils.R             # norm_str(), internal helpers
│   ├── missingness.R       # mv_field_summary, mv_pair_summary, mv_detect_mechanism, mv_entity_correlation
│   ├── field_comparison.R  # mv_compare_fields, mv_pair_features
│   ├── imputation.R        # mv_fit_imputer, mv_apply_imputer, mv_impute
│   └── strategies.R        # mv_strategy_table, mv_recommend
├── man/                    # Rd documentation (13 files)
├── tests/testthat/         # unit tests (30+ assertions)
└── vignettes/mvher-intro.Rmd
```

## References

- Fellegi, I.P. & Sunter, A.B. (1969). A theory for record linkage. *JASA*, 64(328), 1183–1210.
- Little, R.J.A. & Rubin, D.B. (2002). *Statistical Analysis with Missing Data*, 2nd ed. Wiley.
- Christen, P. (2012). *Data Matching*. Springer.
- van Buuren, S. & Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. *JSS*, 45(3).

## License

MIT
