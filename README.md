# Infant mortality and inequality in Argentina, 1980–2024

Data and R code for the article **"Stalled decline and widening provincial inequalities in infant mortality in Argentina, 1980–2024"** (submitted to the *Revista Panamericana de Salud Pública / Pan American Journal of Public Health*).

The study updates Bossio JC, Sanchis I, Herrero MB, Armando GA, Arias SJ. *Mortalidad infantil y desigualdades sociales en Argentina, 1980-2017.* Rev Panam Salud Publica. 2020;44:e127. https://doi.org/10.26633/RPSP.2020.127

## Contents

| Folder | Content |
|---|---|
| `data_raw/` | Aggregated input data from official sources (see below) |
| `data/` | Analytical datasets built by `R/01_preparar.R`: national series 1980–2024, province-year panel 2005–2024, census indicators by province |
| `R/` | Analysis scripts (run in order with `R/ejecutar_todo.R`) |
| `resultados/` | Outputs: `T*.csv` (main analysis), `R*.csv` (additional analyses for the article), `tablas_resultados.xlsx`, `tablas_rpsp.xlsx` |
| `figuras_rpsp/` | Article figures (PNG 600 dpi, PDF, EPS) and the data behind each figure (`datos_figuras_rpsp.xlsx`) |
| `supplementary/` | Supplementary material of the article (Tables S1–S7, Figures S1–S3, supplementary methods) |

## How to reproduce

1. Install R (≥ 4.3) and the packages:
   ```r
   install.packages(c("tidyverse", "sm", "pracma", "strucchange", "segmented", "MASS",
                      "broom", "openxlsx", "patchwork", "ggrepel", "scales"))
   ```
2. Set the working directory to the repository root and run:
   ```r
   source("R/ejecutar_todo.R")
   ```
   Runtime is about 2–3 minutes. Bootstrap and simulation steps use fixed seeds.

| Script | Purpose |
|---|---|
| `00_descargar_bases.R` | (Optional) downloads the original DEIS, INDEC and census files to `data_raw/originales/` |
| `01_preparar.R` | Builds the analytical datasets in `data/` |
| `02_analisis.R` | Trends and breakpoints (IMR, NMR, PNMR), Gini index, concentration index, SII/RII, UBN quartiles, convergence, decomposition by cause, 2022 census cross-section |
| `03_figuras.R` | Figures of the original (Spanish-language) analysis |
| `04_analisis_rpsp.R` | Robustness of the 2020 breakpoint, observed vs expected deaths 2020–2024, inequality by triennium, coefficient of variation net of Poisson noise, Kitagawa decomposition by birthweight, UN IGME regional context |
| `05_figuras_rpsp.R` | Figures of the article (English) |
| `funciones_tendencia.R` | Trend procedure: smoothing (LOESS, spline, moving average), Bai–Perron breakpoints on log rates, annual percent change by period |
| `funciones_desigualdad.R` | Gini index (smoothed Lorenz curve, bootstrap CI), concentration index, SII and RII |

Script comments and some variable names are in Spanish (e.g., `TMI` = infant mortality rate, `TMN` = neonatal, `TMPN` = postneonatal, `NBI` = unsatisfied basic needs, `NV` = live births, `def` = infant deaths).

## Data sources

| File | Source |
|---|---|
| `deis_agregado_prov_2005_2024.csv` | Dirección de Estadísticas e Información en Salud (DEIS), Ministry of Health of Argentina: open data on deaths and live births, aggregated by province of residence (infant deaths, ICD-10 cause groups, birth characteristics) |
| `deis_anuarios_neonatal_posneonatal_2005_2024.csv` | DEIS, *Estadísticas vitales. Información básica* (yearbooks): neonatal and postneonatal deaths by province |
| `deis_anuarios_peso_nacer_2011_2024.csv` | DEIS yearbooks: national live births and infant deaths by birthweight (for deaths, column `g500_999` = "< 1 000 g") |
| `serie_nacional_1980_2004_articulo2020.csv` | National series 1980–2004 from Bossio et al. 2020 (DEIS data) |
| `indec_nbi_hogares_1980_2022.csv`, `indec_nbi_personas_2010_2022.csv` | INDEC, unsatisfied basic needs (UBN), censuses 1980–2022 |
| `censo2022_cobertura_salud.csv` | INDEC, National Census 2022, health insurance coverage |
| `igme_lac_2000_2024.txt` | UN IGME estimates (infant and neonatal mortality), UNICEF data warehouse, accessed 6 Oct 2026 |
| `codigos_provincias.csv` | Province codes and names |

All data are public, aggregated and anonymized. Original sources: https://www.argentina.gob.ar/salud/deis · https://www.indec.gob.ar · https://childmortality.org

## License

Code: MIT License (see `LICENSE`). Data derived from official public sources; please cite the original sources and the article.

## Citation

[Authors]. Stalled decline and widening provincial inequalities in infant mortality in Argentina, 1980–2024. Rev Panam Salud Publica. [year; volume: e-number. DOI — to be completed after publication].

## Use of AI

Claude (Anthropic) was used to assist with programming the additional analyses (`04_analisis_rpsp.R`, `05_figuras_rpsp.R`); all code and results were reviewed by the authors.
