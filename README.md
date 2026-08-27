# After the Flood — The Impact of Flooding on Employment and Wages

M2 thesis, Paris School of Economics (2025). Author: Andrea Salem.

**📄 Read the paper:** [`Salem_2025_The_Impact_of_Flooding_on_Employment_and_Wages.pdf`](Salem_2025_The_Impact_of_Flooding_on_Employment_and_Wages.pdf)

## Abstract

Rising tail risks from extreme weather events elevate flood risk to the forefront of France's
climate governance debates. This paper quantifies the causal impact of extreme floods on labor
markets and firm dynamics. Leveraging matched employer–employee data on all French establishments
(2010–2019) and ministerial disaster declarations under the French CatNat insurance regime, I
implement a Local Projection Difference-in-Differences design to address staggered treatment and
dynamic responses. Flood shocks reduce employment by roughly 2% and wages by 1% on average in
establishments affected by extreme flooding over the five years following a disaster, with losses
concentrated in commerce, construction, and local services, and among small, undiversified firms.
Industrial and agricultural establishments exhibit resilience. Commune-level results are more
muted, consistent with aggregate recovery dynamics, insurance transfers, and spatial reallocation
of economic activity.

**Keywords:** Firm performance, Wages, Natural disasters · **JEL:** D22, Q54, R11

## Repository structure

```
├── Salem_2025_...pdf     # compiled thesis (as submitted)
├── manuscript/           # full LaTeX source (chapters, tables, figures, bibliography)
├── code/                 # analysis pipeline (R + Stata), exported from CASD — see code/README.md
└── output/tables/        # regression output exported from the secure enclave
```

## Data availability

The microdata are **confidential** and are accessed through [CASD](https://www.casd.eu/)
(Centre d'accès sécurisé aux données). No microdata are or ever will be contained in this
repository; the code is published for transparency and cannot be run without CASD access.

| Source | Content | Access |
|---|---|---|
| DADS Postes (INSEE) | Matched employer–employee panel, all French establishments | CASD |
| FARE / FICUS (INSEE–DGFiP) | Firm financial statements | CASD |
| GASPAR (MTE) | CatNat ministerial disaster declarations | Public ([georisques.gouv.fr](https://www.georisques.gouv.fr/)) |
| Flood-risk rasters (EAIP / TRI) | Flood-prone area extent used for risk exposure | Public |

## Reproducibility

Scripts were written and executed inside the CASD secure enclave; paths therefore point to the
enclave file system (e.g. `C:/Users/Public/Documents/...`) and are not portable by design. The
run order and per-script documentation are in [`code/README.md`](code/README.md); the full
R + Stata package surface is in [`code/ENVIRONMENT.md`](code/ENVIRONMENT.md). Code is
MIT-licensed; the manuscript text is © the author (see `LICENSE`).

## Related work

This thesis feeds the flood-risk pillar of my PhD research agenda (CASD-based follow-ups on
adaptation, insurance regimes, and belief formation after disasters).
