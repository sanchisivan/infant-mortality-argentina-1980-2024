# =============================================================================
# 02_analisis.R — Tendencias, desigualdad y cruce con el Censo 2022
# Salidas: resultados/*.csv y resultados/tablas_resultados.xlsx
# =============================================================================
suppressPackageStartupMessages({library(tidyverse); library(openxlsx)})
source("R/funciones_tendencia.R", encoding = "UTF-8"); source("R/funciones_desigualdad.R", encoding = "UTF-8")
dir.create("resultados", showWarnings = FALSE)
cc <- cols(prov = col_character())
nac <- read_csv("data/serie_nacional_1980_2024.csv", show_col_types = FALSE)
pr  <- read_csv("data/panel_provincial_2005_2024.csv", col_types = cc)
cen <- read_csv("data/indicadores_censales_provincia.csv", col_types = cc)
OUT <- list()

# -----------------------------------------------------------------------------
# A. Tendencias (procedimiento del script: suavizado + períodos + VAP)
# -----------------------------------------------------------------------------
serie <- function(df, x) data.frame(AÑO = df$anio, TASA = df[[x]])
TEND <- list(
  TMI_1980_2024 = tendencia_serie(serie(nac, "TMI"), "TMI 1980-2024", h = 5),
  TMI_2005_2024 = tendencia_serie(serie(filter(nac, anio >= 2005), "TMI"), "TMI 2005-2024", h = 4),
  TMN_2005_2024 = tendencia_serie(serie(filter(nac, anio >= 2005), "TMN"), "TMN 2005-2024", h = 4),
  TMPN_2005_2024 = tendencia_serie(serie(filter(nac, anio >= 2005), "TMPN"), "TMPN 2005-2024", h = 4))

# -----------------------------------------------------------------------------
# B. Desigualdad entre jurisdicciones: Gini (app) para TMI, TMN y TMPN
# -----------------------------------------------------------------------------
G <- pr %>% group_by(anio) %>% group_modify(~{
  a <- gini_app(.x$def, .x$nv); b <- gini_app(.x$neo, .x$nv); c <- gini_app(.x$posneo, .x$nv)
  tibble(TMI = sum(.x$def) / sum(.x$nv) * 1000,
         Gini_TMI = a[1], Gini_TMI_LI = a[2], Gini_TMI_LS = a[3],
         Gini_TMN = b[1], Gini_TMN_LI = b[2], Gini_TMN_LS = b[3],
         Gini_TMPN = c[1], Gini_TMPN_LI = c[2], Gini_TMPN_LS = c[3],
         TMI_max = max(.x$TMI), prov_max = .x$provincia[which.max(.x$TMI)],
         TMI_min = min(.x$TMI), prov_min = .x$provincia[which.min(.x$TMI)],
         razon_max_min = max(.x$TMI) / min(.x$TMI)) }) %>% ungroup()

# -----------------------------------------------------------------------------
# C. Desigualdad asociada a condiciones de vida (NBI): IC, SII, RII anuales
# -----------------------------------------------------------------------------
IC <- pr %>% group_by(anio) %>% group_modify(~{
  ic <- conc_boot(.x$def, .x$nv, .x$nbi_hog_interp)
  icp <- conc_index(.x$def, .x$nv, .x$nbi_pers_interp)
  sr <- sii_rii(.x$def, .x$nv, .x$nbi_hog_interp)
  as_tibble(as.list(c(ic, IC_NBIpersonas = icp, sr))) }) %>% ungroup()
DES <- G %>% left_join(IC, by = "anio")

# Tendencia de los índices de desigualdad (mismo procedimiento; IC en valor absoluto)
TEND$Gini_TMI <- tendencia_serie(data.frame(AÑO = G$anio, TASA = G$Gini_TMI), "Gini TMI", h = 4)
TEND$Gini_TMN <- tendencia_serie(data.frame(AÑO = G$anio, TASA = G$Gini_TMN), "Gini TMN", h = 4)
TEND$Gini_TMPN <- tendencia_serie(data.frame(AÑO = G$anio, TASA = G$Gini_TMPN), "Gini TMPN", h = 4)
TEND$IC_abs <- tendencia_serie(data.frame(AÑO = IC$anio, TASA = abs(IC$IC)), "|IC| NBI", h = 4)

# Trienios censales: 2009-2011 (Censo 2010) y 2021-2023 (Censo 2022)
trienio <- function(a, b) pr %>% filter(anio >= a, anio <= b) %>% group_by(prov) %>%
  summarise(across(c(nv, def, neo, posneo, m20, edb, edns, bpn, pns, pret, gns), sum), .groups = "drop") %>%
  left_join(cen, by = "prov")
CEN <- list()
for (cfg in list(list(2009, 2011, "2010"), list(2021, 2023, "2022"))) {
  g <- trienio(cfg[[1]], cfg[[2]])
  ind <- list(`NBI personas` = g[[paste0("pnbi", cfg[[3]])]], `NBI hogares` = g[[paste0("h", cfg[[3]])]])
  if (cfg[[3]] == "2022") ind[["% sin cobertura de salud"]] <- g$p_sincob2022
  for (nm in names(ind)) for (comp in c("TMI", "TMN", "TMPN")) {
    num <- g[[c(TMI = "def", TMN = "neo", TMPN = "posneo")[comp]]]
    ic <- conc_boot(num, g$nv, ind[[nm]]); sr <- sii_rii(num, g$nv, ind[[nm]])
    q <- ntile(ind[[nm]], 4)
    tq <- tapply(num, q, sum) / tapply(g$nv, q, sum) * 1000
    CEN[[length(CEN) + 1]] <- tibble(periodo = paste0(cfg[[1]], "-", cfg[[2]]), censo = paste("Censo", cfg[[3]]),
      indicador_social = nm, componente = comp, tasa_total = sum(num) / sum(g$nv) * 1000,
      IC = ic[1], IC_LI = ic[2], IC_LS = ic[3], SII = sr[1], SII_LI = sr[2], SII_LS = sr[3], RII = sr[4], RII_LI = sr[5], RII_LS = sr[6],
      tasa_Q1 = tq[1], tasa_Q4 = tq[4], razon_Q4_Q1 = tq[4] / tq[1], dif_Q4_Q1 = tq[4] - tq[1])
  }
}
CEN <- bind_rows(CEN)

# -----------------------------------------------------------------------------
# D. TMI por cuartil de NBI (cuartiles fijos según NBI personas 2010)
# -----------------------------------------------------------------------------
cuart <- cen %>% mutate(cuartil = paste0("Q", ntile(pnbi2010, 4))) %>% select(prov, provincia, cuartil, pnbi2010)
QT <- pr %>% left_join(select(cuart, prov, cuartil), by = "prov") %>% group_by(anio, cuartil) %>%
  summarise(nv = sum(nv), def = sum(def), neo = sum(neo), posneo = sum(posneo), .groups = "drop") %>%
  mutate(TMI = def / nv * 1000, TMN = neo / nv * 1000, TMPN = posneo / nv * 1000)
QW <- QT %>% select(anio, cuartil, TMI) %>% pivot_wider(names_from = cuartil, values_from = TMI) %>%
  mutate(razon_Q4_Q1 = Q4 / Q1, dif_Q4_Q1 = Q4 - Q1)
TEND$razon_Q4_Q1 <- tendencia_serie(data.frame(AÑO = QW$anio, TASA = QW$razon_Q4_Q1), "Razón Q4/Q1", h = 4)
for (q in paste0("Q", 1:4)) TEND[[paste0("TMI_", q)]] <- tendencia_serie(serie(filter(QT, cuartil == q), "TMI"), paste("TMI", q), h = 4)
# VAP por cuartil en períodos fijos (para comparar entre cuartiles)
VAPQ <- expand_grid(Q = paste0("Q", 1:4), per = list(c(2005, 2024), c(2005, 2014), c(2014, 2024), c(2018, 2024))) %>%
  pmap_dfr(function(Q, per) { r <- analisis_periodo_especifico_mejorada(serie(QT[QT$cuartil == Q, ], "TMI"), per[1], per[2])
    tibble(cuartil = Q, periodo = paste0(per[1], "-", per[2]), VAP = r$vpa, LI = r$vpa_li, LS = r$vpa_ls, p = r$p_value, tendencia = r$tendencia) })

# -----------------------------------------------------------------------------
# E. Cruce transversal con el Censo 2022 (TMI 2021-2023)
# -----------------------------------------------------------------------------
g22 <- trienio(2021, 2023) %>% mutate(TMI = def / nv * 1000, TMN = neo / nv * 1000, TMPN = posneo / nv * 1000,
  p_m20 = m20 / nv * 100, p_edb = edb / (nv - edns) * 100, p_bpn = bpn / (nv - pns) * 100, p_pret = pret / (nv - gns) * 100)
g10 <- trienio(2009, 2011) %>% transmute(prov, TMI_2009_2011 = def / nv * 1000)
g22 <- g22 %>% left_join(g10, by = "prov") %>% mutate(var_TMI = (TMI / TMI_2009_2011 - 1) * 100, var_NBI = (pnbi2022 / pnbi2010 - 1) * 100)
COR <- expand_grid(tasa = c("TMI", "TMN", "TMPN"), variable = c("pnbi2022", "pnbimas1_2022", "p_sincob2022", "p_m20", "p_edb", "p_bpn", "p_pret")) %>%
  pmap_dfr(function(tasa, variable) { t <- suppressWarnings(cor.test(g22[[variable]], g22[[tasa]], method = "spearman"))
    tibble(tasa, variable, rho = unname(t$estimate), p = t$p.value) })
t1 <- suppressWarnings(cor.test(g22$var_NBI, g22$var_TMI, method = "spearman"))
COR <- bind_rows(COR, tibble(tasa = "Var% TMI 2010-2022", variable = "Var% NBI 2010-2022", rho = unname(t1$estimate), p = t1$p.value))
REG <- map_dfr(c("def ~ pnbi2022", "def ~ p_sincob2022", "def ~ pnbi2022 + p_sincob2022", "neo ~ pnbi2022", "posneo ~ pnbi2022"), function(f) {
  m <- glm(as.formula(paste(f, "+ offset(log(nv))")), family = quasipoisson, data = g22); ci <- confint.default(m)
  tibble(modelo = f, variable = names(coef(m))[-1], RR_por_10pp = exp(10 * coef(m)[-1]),
         LI = exp(10 * ci[-1, 1]), LS = exp(10 * ci[-1, 2]), p = summary(m)$coefficients[-1, 4], dispersion = summary(m)$dispersion) })

# -----------------------------------------------------------------------------
# F. Causas y características de los nacimientos (nacional)
# -----------------------------------------------------------------------------
CAU <- nac %>% filter(anio >= 2005) %>% select(anio, nv, def, P, Q, R, J, AB, VY, OT) %>%
  mutate(across(c(P, Q, R, J, AB, VY, OT), list(tasa = ~ .x / nv * 1000, pct = ~ .x / def * 100)))
NAC <- nac %>% filter(anio >= 2005) %>% transmute(anio, nv, indice_nv_2014 = nv / nv[anio == 2014] * 100,
  p_m20 = m20 / nv * 100, p_bpn = bpn / (nv - pns) * 100, p_pret = pret / (nv - gns) * 100, p_edb = edb / (nv - edns) * 100)


# -----------------------------------------------------------------------------
# G. Tendencia por provincia y convergencia (beta y sigma)
# -----------------------------------------------------------------------------
VAPP <- pr %>% group_by(prov, provincia, region) %>% group_modify(~{
  d <- data.frame(AÑO = .x$anio, TASA = .x$TMI)
  a <- analisis_periodo_especifico_mejorada(d, 2005, 2024); b <- analisis_periodo_especifico_mejorada(d, 2005, 2014)
  c <- analisis_periodo_especifico_mejorada(d, 2014, 2024)
  tibble(TMI_2005_2007 = sum(.x$def[.x$anio <= 2007]) / sum(.x$nv[.x$anio <= 2007]) * 1000,
         TMI_2022_2024 = sum(.x$def[.x$anio >= 2022]) / sum(.x$nv[.x$anio >= 2022]) * 1000,
         VAP_2005_2024 = a$vpa, LI_2005_2024 = a$vpa_li, LS_2005_2024 = a$vpa_ls, p_2005_2024 = a$p_value, tend_2005_2024 = a$tendencia,
         VAP_2005_2014 = b$vpa, p_2005_2014 = b$p_value, VAP_2014_2024 = c$vpa, p_2014_2024 = c$p_value, tend_2014_2024 = c$tendencia) }) %>% ungroup() %>%
  left_join(cen %>% select(prov, pnbi2010, pnbi2022), by = "prov")
# Convergencia beta: ¿las provincias con TMI inicial más alta descendieron más rápido?
beta1 <- lm(VAP_2005_2014 ~ log(TMI_2005_2007), data = VAPP)
tmi1314 <- pr %>% filter(anio %in% 2012:2014) %>% group_by(prov) %>% summarise(TMI_2012_2014 = sum(def) / sum(nv) * 1000)
VAPP <- VAPP %>% left_join(tmi1314, by = "prov")
beta2 <- lm(VAP_2014_2024 ~ log(TMI_2012_2014), data = VAPP)
CONV <- bind_rows(tidy(beta1, conf.int = TRUE) %>% mutate(periodo = "2005-2014"), tidy(beta2, conf.int = TRUE) %>% mutate(periodo = "2014-2024")) %>%
  filter(term != "(Intercept)") %>% mutate(r2 = c(summary(beta1)$r.squared, summary(beta2)$r.squared))
# Convergencia sigma: coeficiente de variación de la TMI provincial (ponderado por NV)
SIG <- pr %>% group_by(anio) %>% summarise(media = weighted.mean(TMI, nv), CV = sqrt(sum(nv * (TMI - media)^2) / sum(nv)) / media)
TEND$CV_provincial <- tendencia_serie(data.frame(AÑO = SIG$anio, TASA = SIG$CV), "CV provincial", h = 4)

# -----------------------------------------------------------------------------
# H. Descomposición del cambio de la TMI (componentes y grupos de causas)
# -----------------------------------------------------------------------------
decomp <- function(a, b) {
  x <- nac %>% filter(anio %in% c(a, b)) %>% arrange(anio)
  comp <- c(Neonatal = "neo", Posneonatal = "posneo", `Perinatales (P00-P96)` = "P", `Malformaciones congénitas (Q)` = "Q",
            `Respiratorias (J)` = "J", `Mal definidas (R)` = "R", `Infecciosas (A-B)` = "AB", `Externas (V-Y)` = "VY", `Otras` = "OT")
  map_dfr(names(comp), function(k) { t <- x[[comp[k]]] / x$nv * 1000
    tibble(periodo = paste0(a, "-", b), clasificacion = ifelse(k %in% c("Neonatal", "Posneonatal"), "Edad", "Causa"), grupo = k,
           tasa_inicial = t[1], tasa_final = t[2], cambio_absoluto = t[2] - t[1], cambio_pct = (t[2] / t[1] - 1) * 100) }) %>%
    group_by(clasificacion) %>% mutate(contribucion_pct = cambio_absoluto / sum(cambio_absoluto) * 100) %>% ungroup()
}
DESC <- bind_rows(decomp(2005, 2014), decomp(2014, 2024), decomp(2020, 2024))

# -----------------------------------------------------------------------------
# Exportación
# -----------------------------------------------------------------------------
TT <- bind_rows(lapply(TEND, `[[`, "tabla"))
SEL <- tibble(serie = names(TEND), metodo_elegido = sapply(TEND, `[[`, "mejor"),
              quiebres = sapply(TEND, function(t) paste(t$resultados[[t$mejor]]$quiebres_detectados, collapse = ", ")))
SUAV <- bind_rows(lapply(names(TEND), function(n) mutate(TEND[[n]]$tabla_suavizados, serie = n)))
TTO <- TT %>% filter(metodo == "original")
tabs <- list(T1_serie_nacional = nac, T2_tendencias_periodos = TT, T2a_periodos_serie_original = TTO, T2b_metodo_elegido = SEL, T2c_series_suavizadas = SUAV,
  T3_desigualdad_anual = DES, T4_desigualdad_censos = CEN, T5_TMI_cuartiles_NBI = QW, T5b_cuartiles_largo = QT,
  T6_VAP_cuartiles = VAPQ, T7_provincias_2021_2023 = g22 %>% select(prov, provincia, region, nv, def, neo, posneo, TMI, TMN, TMPN, TMI_2009_2011, var_TMI, pnbi2010, pnbi2022, var_NBI, p_sincob2022, p_m20, p_edb, p_bpn, p_pret),
  T8_correlaciones_censo2022 = COR, T9_poisson_censo2022 = REG, T10_causas = CAU, T11_nacimientos = NAC,
  T12_cuartiles_provincias = cuart, T13_VAP_provincias = VAPP, T14_convergencia_beta = CONV, T15_convergencia_sigma = SIG, T16_descomposicion = DESC)
for (n in names(tabs)) write_csv(tabs[[n]], file.path("resultados", paste0(n, ".csv")))
wb <- createWorkbook(); for (n in names(tabs)) { addWorksheet(wb, substr(n, 1, 31)); writeData(wb, substr(n, 1, 31), tabs[[n]]) }
saveWorkbook(wb, "resultados/tablas_resultados.xlsx", overwrite = TRUE)
saveRDS(list(TEND = TEND, DES = DES, CEN = CEN, QT = QT, QW = QW, g22 = g22, cuart = cuart, VAPP = VAPP, CONV = CONV, SIG = SIG, DESC = DESC), "resultados/objetos_analisis.rds")

# Resumen en consola
print(as.data.frame(SEL)); print(sapply(TEND, `[[`, 'scores'))
print(as.data.frame(TT %>% filter(elegido) %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))
print(as.data.frame(DES %>% select(anio, TMI, Gini_TMI, Gini_TMI_LI, Gini_TMI_LS, Gini_TMN, Gini_TMPN, IC, IC_LI, IC_LS, RII) %>% mutate(across(-anio, ~ round(.x, 3)))))
print(as.data.frame(CEN %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))
print(as.data.frame(VAPQ)); print(as.data.frame(COR %>% mutate(across(where(is.numeric), ~ round(.x, 3))))); print(as.data.frame(REG)); print(as.data.frame(CONV)); print(as.data.frame(DESC %>% mutate(across(where(is.numeric), ~ round(.x, 2))))); print(as.data.frame(VAPP %>% arrange(VAP_2014_2024) %>% select(provincia, TMI_2005_2007, TMI_2022_2024, VAP_2005_2024, VAP_2014_2024, p_2014_2024, tend_2014_2024)))
