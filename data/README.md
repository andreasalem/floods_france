# Data

**No data files are committed to this repository** (`data/` is gitignored). Confidential
microdata never leave [CASD](https://www.casd.eu/); public files are documented here so the
public-data pipeline stages can be re-run.

## Confidential (CASD secure enclave only)

| Source | Content | Access |
|---|---|---|
| DADS Postes (INSEE) | Matched employer–employee panel, all French establishments, 2010–2019 | CASD project agreement |
| FARE / FICUS (INSEE–DGFiP) | Firm financial statements | CASD project agreement |

## Public

The vintages below are the ones used in the thesis (downloaded 2024–2025 and preserved
locally by the author). Current downloads from the listed portals may differ from those
vintages; the pipeline records which vintage it was run on. Download scripts land in
`code/pipeline/` with the public-data stages.

| Source | Files used | What it is | Where to get it |
|---|---|---|---|
| GASPAR (MTE/Géorisques) | `catnat_gaspar.csv`, `risq_gaspar.csv`, `azi_gaspar.csv`, `pprn_gaspar.csv` | CatNat ministerial disaster declarations and risk registries by commune | [georisques.gouv.fr](https://www.georisques.gouv.fr/donnees/bases-de-donnees) |
| JRC river flood hazard maps (Dottori et al.) | `Europe_RP{10,20,100,500}_filled_depth.tif`, `Europe_permanent_water_bodies.tif` | Modeled flood depth rasters by return period, Europe | [JRC data catalogue](https://data.jrc.ec.europa.eu/collection/id-0054) |
| TRI 2020 (Géorisques) | `tri_2020_sig_di/` shapefiles | Territoires à Risques importants d'Inondation — mapped flood-risk zones | georisques.gouv.fr |
| ONRN | `ONRN_Entreprises_EAIP_2019.csv` | Share of enterprises located in the EAIP flood-prone envelope, by commune | [onrn.fr](https://www.onrn.fr/) |
| INSEE | `v_commune_2024.csv`, populations légales (`donnees_communes.csv` etc.), `zones_emploi_2020.csv` | Commune code tables, legal populations, employment zones | [insee.fr](https://www.insee.fr/) |
| Administrative boundaries | `communes-20220101-shp/`, `departements-20180101-shp/`, `regions-20180101-shp/` | Commune/département/région shapefiles | [data.gouv.fr](https://www.data.gouv.fr/) (OSM-derived *découpage administratif*) |
