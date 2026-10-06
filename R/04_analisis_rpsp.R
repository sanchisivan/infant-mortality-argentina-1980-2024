# =============================================================================
# 04_analisis_rpsp.R — Análisis adicionales para el artículo (Rev Panam Salud Publica)
#   A. Robustez del quiebre 2020 (IC del quiebre; regresión segmentada de Poisson;
#      cambio de pendiente con y sin 2020-2021)
#   B. Defunciones infantiles en exceso 2020-2024 respecto de la tendencia 2005-2019
#      (nacional, componentes y cuartiles de NBI)
#   C. Desigualdad según NBI: SII/RII/IC en trienios; CV provincial corregido por
#      ruido de Poisson (caída de nacimientos)
#   D. Descomposición de Kitagawa por peso al nacer (anuarios DEIS 2011-2024)
#   E. Contexto regional: UN IGME, tasa anual de reducción por país
# Requiere haber corrido 01_preparar.R y 02_analisis.R. Salida: resultados/R*.csv
# y resultados/tablas_rpsp.xlsx
# =============================================================================
suppressPackageStartupMessages({library(tidyverse); library(openxlsx); library(strucchange); library(segmented); library(MASS)})
select <- dplyr::select
source("R/funciones_tendencia.R", encoding = "UTF-8"); source("R/funciones_desigualdad.R", encoding = "UTF-8")
set.seed(2026)
cc <- cols(prov = col_character())
nac <- read_csv("data/serie_nacional_1980_2024.csv", show_col_types = FALSE)
pr  <- read_csv("data/panel_provincial_2005_2024.csv", col_types = cc)
cen <- read_csv("data/indicadores_censales_provincia.csv", col_types = cc)
cuart <- cen %>% mutate(cuartil = paste0("Q", ntile(pnbi2010, 4))) %>% select(prov, cuartil)
pr <- pr %>% left_join(cuart, by = "prov")
R <- list()
apc <- function(b) (exp(b) - 1) * 100

# -----------------------------------------------------------------------------
# A. Robustez del quiebre
# -----------------------------------------------------------------------------
# A1. IC95% de la fecha de quiebre (strucchange, log TMI, mismo h que 02_analisis)
bp_ci <- function(d, h) {
  d <- arrange(d, anio); y <- log(d$tasa); x <- d$anio
  bp <- breakpoints(y ~ x, h = h); nb <- breakpoints(bp)$breakpoints
  if (all(is.na(nb))) return(tibble())
  ci <- confint(bp, breaks = length(nb))$confint
  tibble(quiebre = x[ci[, 2]], LI = x[pmax(1, ci[, 1])], LS = x[pmin(length(x), ci[, 3])])
}
R$A1_IC_quiebres <- bind_rows(
  bp_ci(transmute(nac, anio, tasa = TMI), 5) %>% mutate(serie = "TMI 1980-2024"),
  bp_ci(transmute(filter(nac, anio >= 2005), anio, tasa = TMI), 4) %>% mutate(serie = "TMI 2005-2024"),
  bp_ci(transmute(filter(nac, anio >= 2005), anio, tasa = TMN), 4) %>% mutate(serie = "TMN 2005-2024"),
  bp_ci(transmute(filter(nac, anio >= 2005), anio, tasa = TMPN), 4) %>% mutate(serie = "TMPN 2005-2024")) %>%
  relocate(serie)
# Nota: strucchange ubica el quiebre como último año del segmento previo (serie invertida
# en 02_analisis); aquí la serie va en orden cronológico: quiebre = último año antes del cambio.

# A2. Regresión segmentada de Poisson (cuasi-Poisson) con offset log(NV), 2005-2024
seg_pois <- function(d, y, psi = 2019) {
  d$y <- d[[y]]; m0 <- glm(y ~ anio + offset(log(nv)), family = quasipoisson, data = d)
  ms <- segmented(m0, seg.Z = ~anio, psi = psi)
  dv <- davies.test(glm(y ~ anio + offset(log(nv)), family = poisson, data = d), seg.Z = ~anio)
  sl <- slope(ms)$anio; ps <- confint(ms)
  tibble(componente = y, quiebre = ps[1, 1], q_LI = ps[1, 2], q_LS = ps[1, 3],
         APC_antes = apc(sl[1, 1]), APC_antes_LI = apc(sl[1, 4]), APC_antes_LS = apc(sl[1, 5]),
         APC_despues = apc(sl[2, 1]), APC_despues_LI = apc(sl[2, 4]), APC_despues_LS = apc(sl[2, 5]),
         p_davies = dv$p.value, dispersion = summary(ms)$dispersion)
}
n05 <- filter(nac, anio >= 2005)
R$A2_segmentada_poisson <- bind_rows(seg_pois(n05, "def"), seg_pois(n05, "neo", 2015), seg_pois(n05, "posneo"))

# A3. Cambio de pendiente desde 2019 (bisagra), con y sin 2020-2021 (COVID)
hinge <- function(d, y, k = 2019, excl = NULL, lab = "") {
  d <- filter(d, !anio %in% excl); d$y <- d[[y]]; d$post <- pmax(0, d$anio - k)
  m <- glm(y ~ anio + post + offset(log(nv)), family = quasipoisson, data = d); s <- summary(m)$coefficients; ci <- confint.default(m)
  tibble(componente = y, muestra = lab, APC_previa = apc(s["anio", 1]),
         APC_posterior = apc(s["anio", 1] + s["post", 1]), cambio_pendiente = s["post", 1],
         cambio_LI = ci["post", 1], cambio_LS = ci["post", 2], p_cambio = s["post", 4])
}
R$A3_bisagra_2019 <- bind_rows(lapply(c("def", "neo", "posneo"), function(v) bind_rows(
  hinge(n05, v, lab = "2005-2024"), hinge(n05, v, excl = 2020:2021, lab = "sin 2020-2021"))))

# -----------------------------------------------------------------------------
# B. Defunciones en exceso 2020-2024 vs tendencia previa (cuasi-Poisson, offset NV)
#    Intervalos de predicción por simulación: coeficientes ~ N(b, V) y conteos con
#    sobredispersión (binomial negativa con varianza phi*mu)
# -----------------------------------------------------------------------------
exceso <- function(d, y, base = 2005:2019, pred = 2020:2024, B = 10000) {
  d$y <- d[[y]]; db <- filter(d, anio %in% base); dp <- filter(d, anio %in% pred)
  m <- glm(y ~ anio + offset(log(nv)), family = quasipoisson, data = db); phi <- max(1.0001, summary(m)$dispersion)
  X <- cbind(1, dp$anio); bs <- MASS::mvrnorm(B, coef(m), vcov(m))
  mu <- exp(bs %*% t(X)) * matrix(dp$nv, B, nrow(dp), byrow = TRUE)
  sim <- matrix(rnbinom(length(mu), mu = mu, size = mu / (phi - 1)), B)
  esp <- colMeans(mu)
  anual <- tibble(anio = dp$anio, observadas = dp$y, esperadas = esp,
                  PI_LI = apply(sim, 2, quantile, .025), PI_LS = apply(sim, 2, quantile, .975),
                  exceso = observadas - esperadas, tasa_obs = observadas / dp$nv * 1000, tasa_esp = esperadas / dp$nv * 1000,
                  razon_OE = observadas / esperadas)
  tot <- rowSums(sim); obs <- sum(dp$y)
  acum <- tibble(anio = NA, observadas = obs, esperadas = sum(esp), PI_LI = quantile(tot, .025), PI_LS = quantile(tot, .975),
                 exceso = obs - sum(esp), exceso_LI = obs - quantile(tot, .975), exceso_LS = obs - quantile(tot, .025),
                 tasa_obs = obs / sum(dp$nv) * 1000, tasa_esp = sum(esp) / sum(dp$nv) * 1000, razon_OE = obs / sum(esp),
                 APC_base = apc(coef(m)[2]))
  bind_rows(anual, acum) %>% mutate(serie = y, base = paste(range(base), collapse = "-"))
}
quart_series <- pr %>% group_by(cuartil, anio) %>% summarise(nv = sum(nv), def = sum(def), neo = sum(neo), posneo = sum(posneo), .groups = "drop")
EX <- list(exceso(nac, "def") %>% mutate(grupo = "Argentina"),
           exceso(nac, "neo") %>% mutate(grupo = "Argentina"),
           exceso(nac, "posneo") %>% mutate(grupo = "Argentina"),
           exceso(nac, "def", base = 2010:2019) %>% mutate(grupo = "Argentina"))
for (q in paste0("Q", 1:4)) EX[[length(EX) + 1]] <- exceso(filter(quart_series, cuartil == q), "def") %>% mutate(grupo = q)
R$B_exceso <- bind_rows(EX) %>% mutate(serie = recode(serie, def = "TMI", neo = "TMN", posneo = "TMPN")) %>% relocate(grupo, serie, base)

# B2. Exceso en el cuartil de mayor NBI (Q4) por componente y grupo de causa (2005-2019 -> 2020-2024)
q4 <- pr %>% filter(cuartil == "Q4") %>% group_by(anio) %>% summarise(across(c(nv, def, neo, posneo, P, Q, J, R, AB, VY, OT), sum), .groups = "drop")
R$B2_exceso_Q4_componentes <- map_dfr(c("def", "neo", "posneo", "P", "Q", "J", "R"), function(v)
  exceso(q4, v) %>% filter(is.na(anio)) %>% mutate(serie = v)) %>% mutate(grupo = "Q4") %>% relocate(grupo, serie)

# -----------------------------------------------------------------------------
# C. Desigualdad según NBI en trienios + CV corregido por ruido
# -----------------------------------------------------------------------------
trien <- list(`2005-2007` = 2005:2007, `2013-2015` = 2013:2015, `2017-2019` = 2017:2019, `2022-2024` = 2022:2024)
C1 <- list()
for (tn in names(trien)) {
  g <- pr %>% filter(anio %in% trien[[tn]]) %>% group_by(prov) %>%
    summarise(nv = sum(nv), def = sum(def), neo = sum(neo), posneo = sum(posneo), nbi = mean(nbi_hog_interp), .groups = "drop")
  for (comp in c("def", "neo", "posneo")) {
    ic <- conc_boot(g[[comp]], g$nv, g$nbi, B = 1000); sr <- sii_rii(g[[comp]], g$nv, g$nbi)
    gi <- gini_app(g[[comp]], g$nv, B = 1000)
    C1[[length(C1) + 1]] <- tibble(trienio = tn, componente = comp, tasa = sum(g[[comp]]) / sum(g$nv) * 1000,
      Gini = gi[1], Gini_LI = gi[2], Gini_LS = gi[3], CIx = ic[1], CIx_LI = ic[2], CIx_LS = ic[3],
      SII = sr[1], SII_LI = sr[2], SII_LS = sr[3], RII = sr[4], RII_LI = sr[5], RII_LS = sr[6])
  }
}
R$C1_desigualdad_trienios <- bind_rows(C1) %>% mutate(componente = recode(componente, def = "TMI", neo = "TMN", posneo = "TMPN"))

# Tendencia de SII y RII anuales (serie original, mismo procedimiento que 02_analisis)
des <- read_csv("resultados/T3_desigualdad_anual.csv", show_col_types = FALSE)
tt <- function(v, nm) tendencia_serie(data.frame(AÑO = des$anio, TASA = v), nm, h = 4)$tabla %>% filter(metodo == "original")
R$C2_tendencia_SII_RII <- bind_rows(tt(des$SII, "SII TMI"), tt(des$RII, "RII TMI"))

# CV provincial observado vs. esperado sólo por azar (Poisson) y CV "de señal"
R$C3_CV_ruido <- pr %>% group_by(anio) %>% summarise(
  tasa = sum(def) / sum(nv), CV_obs = sqrt(sum(nv * (def / nv - tasa)^2) / sum(nv)) / tasa,
  CV_ruido = sqrt(sum(nv * (tasa / nv)) / sum(nv)) / tasa, nv = sum(nv), .groups = "drop") %>%
  mutate(CV_senal = sqrt(pmax(0, CV_obs^2 - CV_ruido^2)))
R$C3b_tendencia_CV_senal <- tendencia_serie(data.frame(AÑO = R$C3_CV_ruido$anio, TASA = R$C3_CV_ruido$CV_senal), "CV señal", h = 4)$tabla %>%
  filter(metodo == "original")
# CV en trienios móviles (tasas provinciales de 3 años)
R$C3c_CV_trienal <- map_dfr(2007:2024, function(a) pr %>% filter(anio %in% (a - 2):a) %>% group_by(prov) %>%
  summarise(nv = sum(nv), def = sum(def), .groups = "drop") %>% mutate(t = def / nv) %>%
  summarise(anio_fin = a, CV = sqrt(sum(nv * (t - sum(def) / sum(nv))^2) / sum(nv)) / (sum(def) / sum(nv))))
R$C3d_tendencia_CV_trienal <- tendencia_serie(data.frame(AÑO = R$C3c_CV_trienal$anio_fin, TASA = R$C3c_CV_trienal$CV), "CV trienal", h = 4)$tabla %>%
  filter(metodo == "original")

# -----------------------------------------------------------------------------
# D. Kitagawa por peso al nacer (nacional). Defunciones de peso no especificado y
#    NV de peso no especificado redistribuidos proporcionalmente dentro de cada año.
# -----------------------------------------------------------------------------
pw <- read_csv("data_raw/deis_anuarios_peso_nacer_2011_2024.csv", show_col_types = FALSE)
cats <- c(`<1000` = "lt1000", `1000-1499` = "g1000_1499", `1500-2499` = "g1500_2499", `≥2500` = "g2500mas")
pw2 <- pw %>% mutate(lt1000 = coalesce(lt500, 0) + g500_999, g1500_2499 = g1500_1999 + g2000_2499,
                     g2500mas = g2500_2999 + g3000_3499 + g3500mas) %>%
  select(anio, tipo, total, all_of(unname(cats)), sin_esp)
stopifnot(all(abs(rowSums(pw2[, c(unname(cats), "sin_esp")]) - pw2$total) <= 1))
long <- pw2 %>% pivot_longer(all_of(unname(cats)), names_to = "peso", values_to = "n") %>%
  group_by(anio, tipo) %>% mutate(n_red = n + sin_esp * n / sum(n)) %>% ungroup() %>%
  select(anio, tipo, peso, n, n_red) %>% pivot_wider(names_from = tipo, values_from = c(n, n_red))
kitagawa <- function(a, b, col_d = "n_red_def", col_n = "n_red_nv") {
  agg <- function(y) long %>% filter(anio %in% y) %>% group_by(peso) %>% summarise(d = sum(.data[[col_d]]), n = sum(.data[[col_n]]), .groups = "drop") %>%
    mutate(c = n / sum(n), r = d / n * 1000)
  x <- agg(a) %>% inner_join(agg(b), by = "peso", suffix = c("1", "2"))
  comp <- sum((x$c2 - x$c1) * (x$r1 + x$r2) / 2)
  tasa <- sum((x$r2 - x$r1) * (x$c1 + x$c2) / 2)
  T1 <- sum(x$c1 * x$r1); T2 <- sum(x$c2 * x$r2)
  list(resumen = tibble(periodo = paste(range(a), collapse = "-") %>% paste("vs", paste(range(b), collapse = "-")),
                        TMI_1 = T1, TMI_2 = T2, cambio = T2 - T1, efecto_composicion = comp, efecto_tasas = tasa,
                        pct_composicion = comp / (T2 - T1) * 100, pct_tasas = tasa / (T2 - T1) * 100),
       detalle = x %>% mutate(periodo = paste(range(a), collapse = "-") %>% paste("vs", paste(range(b), collapse = "-")),
                              contrib_comp = (c2 - c1) * (r1 + r2) / 2, contrib_tasa = (r2 - r1) * (c1 + c2) / 2))
}
K <- list(kitagawa(2013:2015, 2022:2024), kitagawa(2013:2015, 2017:2019), kitagawa(2017:2019, 2022:2024))
Ks <- kitagawa(2013:2015, 2022:2024, "n_def", "n_nv")   # sensibilidad: sólo peso especificado
R$D1_kitagawa <- bind_rows(map(K, "resumen"), Ks$resumen %>% mutate(periodo = paste(periodo, "(sólo peso especificado)")))
R$D2_kitagawa_detalle <- bind_rows(map(K, "detalle")) %>% mutate(peso = names(cats)[match(peso, cats)])
R$D3_peso_anual <- long %>% group_by(anio) %>% mutate(pct_nv = n_red_nv / sum(n_red_nv) * 100, tasa_esp = n_red_def / n_red_nv * 1000) %>%
  ungroup() %>% mutate(peso = names(cats)[match(peso, cats)]) %>% left_join(pw2 %>% filter(tipo == "def") %>%
  transmute(anio, pct_def_peso_sin_esp = sin_esp / total * 100), by = "anio")

# -----------------------------------------------------------------------------
# E. Contexto regional (UN IGME, estimaciones suavizadas) — tasa anual de reducción
# -----------------------------------------------------------------------------
ig <- read_lines("data_raw/igme_lac_2000_2024.txt") %>% .[-1] %>% str_split_fixed("\\|", 3) %>% as_tibble(.name_repair = ~c("iso3", "ind", "v")) %>%
  filter(v != str_dup(",", 24)) %>% mutate(v = str_split(v, ",")) %>% unnest_longer(v) %>% group_by(iso3, ind) %>%
  mutate(anio = 1999 + row_number(), valor = as.numeric(v)) %>% ungroup() %>% select(-v)
arr <- function(x, a, b) -log(x[b - 1999] / x[a - 1999]) / (b - a) * 100   # ARR (%/año), positivo = descenso
R$E_IGME_ARR <- ig %>% group_by(iso3, ind) %>% arrange(anio, .by_group = TRUE) %>%
  summarise(valor_2015 = valor[anio == 2015], valor_2019 = valor[anio == 2019], valor_2024 = valor[anio == 2024],
            ARR_2010_2015 = arr(valor, 2010, 2015), ARR_2015_2019 = arr(valor, 2015, 2019), ARR_2019_2024 = arr(valor, 2019, 2024), .groups = "drop") %>%
  mutate(indicador = recode(ind, CME_MRY0 = "TMI", CME_MRM0 = "TMN")) %>% select(-ind) %>% relocate(iso3, indicador)
arrDEIS <- function(v) { x <- setNames(nac[[v]], nac$anio); c(ARR_2010_2015 = -log(x["2015"] / x["2010"]) / 5 * 100,
  ARR_2015_2019 = -log(x["2019"] / x["2015"]) / 4 * 100, ARR_2019_2024 = -log(x["2024"] / x["2019"]) / 5 * 100) }
R$E_DEIS_ARR <- bind_rows(TMI = arrDEIS("TMI"), TMN = arrDEIS("TMN"), .id = "indicador") %>% mutate(fuente = "DEIS (Argentina)")
R$E_IGME_serie <- ig %>% mutate(indicador = recode(ind, CME_MRY0 = "TMI", CME_MRM0 = "TMN")) %>% select(iso3, indicador, anio, valor)

# -----------------------------------------------------------------------------
for (n in names(R)) write_csv(R[[n]], file.path("resultados", paste0("R", n, ".csv")))
wb <- createWorkbook(); for (n in names(R)) { addWorksheet(wb, substr(n, 1, 31)); writeData(wb, substr(n, 1, 31), R[[n]]) }
saveWorkbook(wb, "resultados/tablas_rpsp.xlsx", overwrite = TRUE)
options(width = 200); for (n in names(R)) { if (n == "E_IGME_serie") next; cat("\n==", n, "\n"); print(as.data.frame(mutate(R[[n]], across(where(is.numeric), ~ round(.x, 3))))) }
