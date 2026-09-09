# Vendored OSF data

With the exception of `n6cp3.rds`, the `.rds` files in this directory are
byte-for-byte snapshots of objects fetched from the
[Serocalculator Data Repository](https://osf.io/ne8pc/) on OSF, downloaded on
2026-08-07.
Vignettes read them from here instead of fetching from OSF at render time,
so a docs build no longer depends on OSF being reachable (issue #648).

`n6cp3.rds` is a privacy-preserving corrected derivative. Its assay and
demographic fields are unchanged from the OSF object, but its row-level
`index_id` values have been replaced with opaque participant IDs derived from
the restricted SEES source. The source identifier is used only in memory and
is discarded before the file is written. This restores linkage between a
participant's biomarker rows without exposing the source identifier (issue
#650).

Each file is named after the OSF resource id it was downloaded from
(`https://osf.io/download/<id>/`):

| File        | OSF id   | Contents                                             |
| ----------- | -------- | ----------------------------------------------------- |
| `n6cp3.rds` | `n6cp3`  | Corrected typhoid cross-sectional population data    |
| `hqy4v.rds` | `hqy4v`  | Typhoid noise parameters                             |
| `u5gxh.rds` | `u5gxh`  | Scrub typhus antibody-decay curve parameters         |
| `h5js4.rds` | `h5js4`  | Scrub typhus cross-sectional population data         |

There is no `rtw5k.rds` here.
`typhoid_curves_nostrat_100` (the package's own bundled example data) is
`load_sr_params("https://osf.io/download/rtw5k/")` filtered to
`iter %in% 1:100`, so a vignette that only needs `iter < 50` from `rtw5k`
filters the bundled object further instead of fetching or vendoring a second
copy of it --- the two are identical once filtered (same reasoning as the
methodology article's simulation sections, per NEWS.md).

That reasoning does not cover every consumer, though.
A vignette that averages over the **full** posterior is not served by a
100-draw subset: the two are identical only where the subset is what was
wanted.
`vignettes/articles/enteric_fever_example.Rmd` is that case --- the published
v1.4.1 build read all draws of `rtw5k`,
and the bundled object gives a coarser Monte Carlo approximation of the
marginal density.
Vendoring `rtw5k.rds` by the refresh procedure below would close it;
tracked in
[issue #682](https://github.com/UCD-SERG/serocalculator/issues/682).

To refresh a byte-for-byte snapshot after the upstream OSF object changes,
re-download it and overwrite the file here:

```r
download.file(
  "https://osf.io/download/<id>/",
  "vignettes/precomputed/osf/<id>.rds",
  mode = "wb" # required: the URL has no .rds extension, so Windows can't
              # infer binary mode on its own and would CRLF-corrupt the file
)
```

Do not refresh `n6cp3.rds` with that command because doing so would restore
the broken row-level IDs. Instead, an authorized maintainer must set
`SEES_REDACTED_DATA` to the redacted SEES source extract and run:

```r
devtools::load_all()
source("data-raw/sees_datacleaning.R")
source("data-raw/sees_pop_data_pakistan_100.R")
```

The regeneration script reads only the fields needed for the public
derivative and never writes the restricted subject identifier.

After regenerating and reviewing the corrected artifact, an authorized
maintainer can publish it as a new version of the existing OSF file:

```r
install.packages("osfr") # if needed
osfr::osf_auth()
source("data-raw/upload_sees_pop_data_to_osf.R")
```

The upload script checks the project, folder path, filename, and existing file
GUID before overwriting. It then downloads OSF file `n6cp3` and verifies that it
is byte-for-byte identical to the local corrected RDS.

`hqy4v.rds` had drifted from the package's own `example_noise_params_sees`
data object: the `y.high` column read `1000` here and `5e+06` there.
That was stale package data rather than two sources disagreeing.
`data-raw/example_noise_params_pk.R` builds both
`example_noise_params_sees` and `example_noise_params_pk` by downloading
this same OSF id, so the `.rda` files were simply generated from an earlier
state of it.
Both have been regenerated to match, and this snapshot preserves whatever
`hqy4v` currently serves.

Regenerating changed no result,
which is worth recording so the next reader does not have to re-derive it.
`y.high` is the upper limit of detection.
Both likelihood implementations use it first to *classify* an observation:
`src/serocalc.c` in its uncensored guard (`yLo < y && y < yHi`, `:24`, `:71`)
and its `yHi <= y` right-censoring branch (`:54`, `:101`),
and `src/serocalc_joint.c` where `y >= yHi` sets `JOINT_RIGHT_CENSORED`
(`:201`).
Then, on the censored branch only, its value is passed into the term being
evaluated: the probability functions `prbB()` and `prbF()` are evaluated at
`yHi` itself (`src/serocalc.c:57`, `:104` --- these are probability functions
rather than densities, per the source's own comments at `:130` and `:157`),
and in the joint engine `yHi` reaches `cond_term()` (`:210`, `:245`) and
`ystar_support()` (`:224`).
So an observation below the bound never sees the value at all.
The largest *measured* antibody concentration in any dataset the package ships
or vendors is about `219` (in `n6cp3.rds`; the largest in `data/` is `135`),
so no observation reaches `1000`, let alone `5e+06`:
under either value no observation is right-censored and the likelihood is
identical.
Note this is a claim about measurements, not about every number in `data/` ---
`typhoid_curves_nostrat_100`'s `y1` (a fitted curve parameter, not an
observation) runs far higher, and censoring never compares against it.
Which value `hqy4v` *should* carry is a separate question for whoever owns
the OSF data, tracked in
[issue #684](https://github.com/UCD-SERG/serocalculator/issues/684).
`inst/extdata/example_noise_params.csv` and `inst/extdata/example_noise_params.rds`
are regenerated to match, so no copy of these parameters is left behind.
They are not independent fixtures: they hold the same four Pakistan rows as
`example_noise_params_pk` and agree with it to fifteen significant figures on
every column except `y.high`, and the `.csv` still carries the row-index column
`write.csv()` left in it, so they are a stale export of the same OSF-derived
subset rather than hand-authored data.
No `data-raw/` script writes them (unlike `example_pop_data.csv`/`.rds`, which
`data-raw/sees_pop_data_pakistan_100.R` does), which is why they had drifted.
The `.csv` writes the value as `1000.0` so it still parses as a double;
a bare `1000` reads back as an integer and changes the column's type.
