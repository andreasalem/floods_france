# Code

Two tracks, with different guarantees:

| Directory | What it is | Guarantee |
|---|---|---|
| [`casd_export_2025-07/`](casd_export_2025-07/) | The exact code that produced the thesis results, exported from the CASD secure enclave on 2025-07-09. | **Provenance.** Byte-frozen — never edited, never "improved". This is the record of what actually ran on the confidential data. |
| [`pipeline/`](pipeline/) | A clean-room rewrite of the same pipeline in R: numbered scripts, portable paths, `renv`-pinned. | **Legibility + partial reproducibility.** The public-data stages (GASPAR treatment, flood-risk rasters) run end-to-end outside the enclave. The confidential-data stages are faithful rewrites that cannot be executed outside CASD; they carry an explicit verification status and will be checked against the frozen export on the next enclave session. |

If you want to know *what the thesis did*, read `casd_export_2025-07/` (run order in its
README). If you want to *run or build on* the flood-exposure construction, use `pipeline/`.

Any divergence between the rewrite and the frozen export — including suspected mistakes in the
original code — is documented in this repository's GitHub issues, never silently patched.
