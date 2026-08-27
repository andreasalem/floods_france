# Computational environment

The pipeline ran **inside the CASD secure enclave** (Windows, project directory
`C:/Users/Public/Documents/Fontaine/Floods_Shock`). The enclave does not allow exporting a
lockfile, and no `sessionInfo()` / `about` snapshot was captured before the 2025-07-09 code
export — so exact versions are **TODO-verify**: recover them from the enclave session
(`sessionInfo()` in R, `about` + `ado dir` in Stata) on the next CASD login and record them
here.

## R

R ≥ 4.x (enclave version TODO-verify). Packages used across the scripts, by role:

- **Data handling:** `tidyverse` (incl. `dplyr`, `tidyr`, `readr`, `purrr`, `ggplot2`),
  `data.table`, `arrow`, `haven`, `readxl`, `jsonlite`, `httr`, `lubridate`, `slider`,
  `fastDummies`, `Hmisc`, `pacman`
- **Spatial:** `sf`, `terra`, `leaflet`, `rnaturalearth`
- **Estimation:** `fixest`, `did`, `panelr` (plus `fect`, `DIDmultiplegtDYN`, `HonestDiD`
  where called in the analysis scripts)
- **Output/figures:** `xtable`, `kableExtra`, `tableone`, `codebookr`, `scales`,
  `RColorBrewer`, `patchwork`, `ggrepel`, `gganimate`, `transformr`, `geoarrow`, `tictoc`

Install (approximate, outside the enclave):

```r
install.packages(c("tidyverse","data.table","arrow","haven","readxl","jsonlite","httr",
  "lubridate","slider","fastDummies","Hmisc","pacman","sf","terra","leaflet",
  "rnaturalearth","fixest","did","panelr","fect","HonestDiD","xtable","kableExtra",
  "tableone","codebookr","scales","RColorBrewer","patchwork","ggrepel","tictoc"))
```

## Stata

Stata ≥ 17 (enclave version TODO-verify). Community packages installed via `ssc install`:

`boottest`, `cem`, `csdid`, `did_imputation`, `did_multiplegt`, `egenmore`,
`eventstudyinteract`, `ftools`, `kmatch`, `lpdid`, `reghdfe`, `winsor`

The main specification (`Analysis2Stata.do`) is LP-DiD via `lpdid`, which depends on
`reghdfe` + `ftools`.

## Why there is no lockfile

`renv`/`pip`-style pinning was not available in the enclave workflow, and the code cannot be
re-run outside CASD (confidential inputs). This file is the honest substitute: the package
surface is exact (scanned from the scripts); versions await the next enclave session.
