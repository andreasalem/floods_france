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

None recorded. If the code rewrite (`code/pipeline/`) surfaces a material issue in the
analysis, it will be documented in a GitHub issue and summarized here — see the protocol in
[`../code/README.md`](../code/README.md).
