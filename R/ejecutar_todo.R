# Reproduce the full analysis / Reproduce todo el análisis.
# Set the working directory to the repository root (the folder containing R/, data_raw/ ...).
# Packages: tidyverse, sm, pracma, strucchange, segmented, MASS, broom, openxlsx, patchwork, ggrepel, scales
# install.packages(c("tidyverse","sm","pracma","strucchange","segmented","MASS","broom","openxlsx","patchwork","ggrepel","scales"))
source("R/01_preparar.R", encoding = "UTF-8")       # data_raw/ -> data/
source("R/02_analisis.R", encoding = "UTF-8")       # trends, Gini, concentration index, quartiles, census 2022 -> resultados/T*.csv
source("R/03_figuras.R",  encoding = "UTF-8")       # figures of the Spanish-language analysis -> figuras/
source("R/04_analisis_rpsp.R", encoding = "UTF-8")  # additional analyses for the RPSP article -> resultados/R*.csv
Sys.setenv(FIG_LANG = "es"); source("R/05_figuras_rpsp.R", encoding = "UTF-8")   # article figures (Spanish, as submitted) -> figuras_rpsp_es/
Sys.setenv(FIG_LANG = "en"); source("R/05_figuras_rpsp.R", encoding = "UTF-8")   # same figures with English labels -> figuras_rpsp/
