# Build notes and errata — frozen manuscript source

`paper/src/` is the LaTeX source **as submitted** (August 26, 2025). It is deliberately kept
frozen: known quirks are recorded here rather than fixed in the source, so the repository
remains an exact record of the submitted document.

## Verification (2026-08-27)

The source was compiled locally (pdflatex → bibtex → pdflatex ×2, matching the submitted
PDF's own pdfTeX producer metadata) and compared against the submitted PDF
([`Salem_2025_The_Impact_of_Flooding_on_Employment_and_Wages.pdf`](Salem_2025_The_Impact_of_Flooding_on_Employment_and_Wages.pdf)):

- **Page count 52/52; text word-for-word identical.** Only differences: the title-page date
  (`titlepage.tex` uses `\today`) and sub-line line-break drift on 6 pages from TeX Live
  version differences. No words added, removed, or changed.
- `chapters/2. Literature Review.tex` is **intentionally empty**: the submitted thesis has no
  Literature Review chapter (the empty `\input` drops it, which is why *Institutional
  Background* is chapter 2 in the PDF while source filenames keep the older numbering).
- `makeglossaries` was not run: the glossary contains a single placeholder entry and the
  submitted PDF has no list of abbreviations — skipping it reproduces the submitted output.

## Known source quirks (present in the original submitted build too; non-fatal)

- `\mathbbm{1}` used without loading the `bbm` package (ch. 5) — the indicator function
  renders in a plain font.
- `\texit{Vigicrues}` typo for `\textit` (ch. 3).
- `\justify` used without `ragged2e`.
- `\minitoc` / `\adjustmtc` calls without the `minitoc` package.
- BibTeX: malformed author field in `indaco2021hurricanes` (trailing comma), producing bibtex
  error messages; the rendered bibliography is nonetheless correct.

## Rebuilding

A TeX Live/TinyTeX install needs these extra packages beyond a minimal setup: `epigraph`,
`blindtext`, `was` (for `gensymb`), `tocbibind`, `multibib`, `chngcntr`, `titling`,
`tocloft`, `glossaries`, `nextpage`. Compile `src/main.tex` with pdflatex; expect the
non-fatal errors listed above (pdflatex exits non-zero but produces the complete PDF).

## Substantive errata

Found by the public-data pipeline's differential audit (2026-08-28); full evidence in
[`code/pipeline/verification/VERIFICATION.md`](../code/pipeline/verification/VERIFICATION.md).
No estimate, table, or the risk index itself is affected by either item.

1. **Depth-distribution sentence** ([issue #5](https://github.com/andreasalem/floods_france/issues/5)).
   The Appendix states that "90% of all flooded cells fall between 0.07 and 3 meters in water
   depth." Measured on the RP100 France raster actually used (4,648,816 flooded cells), the
   share in (0.07, 3] m is **70.5%**, and the central-90% interval is **[0.34, 6.74] m**
   (median 1.84 m). The qualitative point (right-skewed distribution) stands; the figures do not.
2. **Raster cell size** ([issue #4](https://github.com/andreasalem/floods_france/issues/4)).
   The Appendix describes "a grid of 90m × 90m cells"; the projected raster the index is
   computed from has 100m × 100m cells.

If the code rewrite surfaces further material issues, they will be documented in a GitHub
issue and summarized here — see the protocol in [`../code/README.md`](../code/README.md).
