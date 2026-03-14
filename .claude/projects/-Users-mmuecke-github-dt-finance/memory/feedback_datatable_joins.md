---
name: data.table join style
description: Prefer data.table idiomatic subsetting join syntax over merge(); only use merge() when required
type: feedback
---

Use data.table's idiomatic `X[Y, on = ...]` join syntax instead of `merge()`. Only use `merge()` where strictly required (e.g., full outer joins or when column name conflicts make subsetting awkward).

**Why:** User prefers idiomatic data.table style throughout the project.
**How to apply:** When writing joins in R code for this project, default to `dt1[dt2, on = "key"]` or `dt1[dt2, on = "key", nomatch = NULL]` (for inner joins).
