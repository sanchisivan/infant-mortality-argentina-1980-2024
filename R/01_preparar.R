# =============================================================================
# 01_preparar.R — Construye las bases analíticas a partir de data_raw/
#   data/serie_nacional_1980_2024.csv
#   data/panel_provincial_2005_2024.csv
#   data/indicadores_censales_provincia.csv
# Ejecutar con el directorio de trabajo en la carpeta "analisis"
# =============================================================================
suppressPackageStartupMessages(library(tidyverse))
dir.create("data", showWarnings = FALSE)
cc <- cols(prov = col_character())
cod <- read_csv("data_raw/codigos_provincias.csv", col_types = cc)
ag  <- read_csv("data_raw/deis_agregado_prov_2005_2024.csv", col_types = cc) %>% select(-neo, -pneo)  # neo/posneo se toman de los anuarios
neo_df <- read_csv("data_raw/deis_anuarios_neonatal_posneonatal_2005_2024.csv", col_types = cc)

# ---- Serie nacional (incluye otros países y lugar no especificado, como DEIS)
vars <- c("nv","def","m20","edb","edns","bpn","pns","pret","gns","P","Q","R","J","AB","VY","OT")
nac05 <- ag %>% group_by(anio) %>% summarise(across(all_of(vars), sum), .groups = "drop")
nac80 <- read_csv("data_raw/serie_nacional_1980_2004_articulo2020.csv", show_col_types = FALSE)
nn <- neo_df %>% filter(prov == "00") %>% select(anio, neo, posneo)
nac <- bind_rows(nac80, nac05) %>% left_join(nn, by = "anio") %>%
  mutate(TMI = def / nv * 1000, TMN = neo / nv * 1000, TMPN = posneo / nv * 1000,
         fuente = if_else(anio < 2005, "Bossio et al. 2020 (DEIS)", "DEIS datos abiertos / anuarios"))
write_csv(nac, "data/serie_nacional_1980_2024.csv")

# ---- Indicadores censales (NBI 1980-2022, cobertura de salud 2022)
nh <- read_delim("data_raw/indec_nbi_hogares_1980_2022.csv", delim = ";", show_col_types = FALSE)
np <- read_delim("data_raw/indec_nbi_personas_2010_2022.csv", delim = ";", show_col_types = FALSE) %>% select(-nbimas1_2010)
cob <- read_delim("data_raw/censo2022_cobertura_salud.csv", delim = ";", col_types = cols(cod = col_character())) %>% select(-cod)
cen <- cod %>% left_join(nh, by = c("nbi_nombre" = "provincia")) %>%
  left_join(np, by = c("nbi_nombre" = "provincia")) %>%
  left_join(cob, by = c("nbi_nombre" = "provincia")) %>%
  mutate(pnbi2010 = nbi2010 / pob2010 * 100, pnbi2022 = nbi2022 / pob2022 * 100,
         pnbimas1_2022 = nbimas1_2022 / pob2022 * 100, p_sincob2022 = sin_cobertura / pob2022 * 100)
stopifnot(nrow(cen) == 24, !anyNA(cen$h2022), !anyNA(cen$pnbi2022), !anyNA(cen$p_sincob2022))
write_csv(cen, "data/indicadores_censales_provincia.csv")

# ---- Panel provincial 2005-2024 (24 jurisdicciones) + NBI interpolado anual
interp <- cen %>% rowwise() %>% reframe(prov = prov, anio = 2005:2024,
  nbi_hog_interp = approx(c(2001, 2010, 2022), c(h2001, h2010, h2022), xout = 2005:2024, rule = 2)$y,
  nbi_pers_interp = approx(c(2010, 2022), c(pnbi2010, pnbi2022), xout = 2005:2024, rule = 2)$y)
pr <- ag %>% filter(prov %in% cod$prov) %>% inner_join(cod, by = "prov") %>%
  left_join(neo_df %>% select(anio, prov, neo, posneo), by = c("anio", "prov")) %>%
  left_join(interp, by = c("prov", "anio")) %>%
  mutate(TMI = def / nv * 1000, TMN = neo / nv * 1000, TMPN = posneo / nv * 1000,
         p_m20 = m20 / nv * 100, p_edb = edb / (nv - edns) * 100,
         p_bpn = bpn / (nv - pns) * 100, p_pret = pret / (nv - gns) * 100)
# controles: neonatal + posneonatal = total (salvo 1 defunción de edad ignorada)
stopifnot(all(abs(pr$def - pr$neo - pr$posneo) <= 1, na.rm = TRUE))
write_csv(pr, "data/panel_provincial_2005_2024.csv")
cat("Bases preparadas: ", nrow(nac), "años nacionales;", nrow(pr), "filas provincia-año\n")
