# =============================================================================
# 05_figuras_rpsp.R — Figuras del manuscrito para Rev Panam Salud Publica
# Idioma según la variable de entorno FIG_LANG ("en" por defecto, o "es"):
#   en: figuras_rpsp/    (Figure1_trends, ...)     + datos_figuras_rpsp.xlsx
#   es: figuras_rpsp_es/ (Figura1_tendencias, ...) + datos_figuras_rpsp_es.xlsx
#       (coma decimal, espacio para miles y signo menos "−", según normas RPSP)
# Salida: PNG 600 dpi, PDF y EPS editables. Requiere 01, 02 y 04.
# Uso: FIG_LANG=es Rscript R/05_figuras_rpsp.R  (desde la carpeta analisis/)
# =============================================================================
suppressPackageStartupMessages({library(tidyverse); library(patchwork); library(openxlsx); library(MASS)})
select <- dplyr::select
IDIOMA <- tolower(Sys.getenv("FIG_LANG", "en"))
if (!IDIOMA %in% c("en", "es")) stop("FIG_LANG debe ser 'en' o 'es'")
ES <- IDIOMA == "es"
tx <- function(en, es) if (ES) es else en                       # texto según idioma
OUT <- tx("figuras_rpsp", "figuras_rpsp_es")
FNAME <- c(F1 = tx("Figure1_trends", "Figura1_tendencias"), F2 = tx("Figure2_observed_expected", "Figura2_observada_esperada"),
           F3 = tx("Figure3_inequality", "Figura3_desigualdad"), S1 = tx("FigureS1_smoothed_IMR", "FiguraS1_TMI_suavizada"),
           S2 = tx("FigureS2_birthweight", "FiguraS2_peso_al_nacer"), S3 = tx("FigureS3_regional_IGME", "FiguraS3_IGME_regional"))
# Números en español: coma decimal, espacio para miles, signo menos U+2212
LAB <- if (ES) scales::label_number(decimal.mark = ",", big.mark = " ", style_negative = "minus") else waiver()
fnum <- function(x, d = 1, plus = FALSE)
  paste0(ifelse(x < 0, "−", ifelse(plus & x > 0, "+", "")), formatC(abs(x), format = "f", digits = d, big.mark = " ", decimal.mark = ","))
SER <- c(IMR = tx("IMR", "TMI"), NMR = tx("NMR", "TMN"), PNMR = tx("PNMR", "TMPN"))   # etiquetas de las series
dir.create(OUT, showWarnings = FALSE)
cc <- cols(prov = col_character())
nac <- read_csv("data/serie_nacional_1980_2024.csv", show_col_types = FALSE)
pr  <- read_csv("data/panel_provincial_2005_2024.csv", col_types = cc)
cen <- read_csv("data/indicadores_censales_provincia.csv", col_types = cc)
per <- read_csv("resultados/T2a_periodos_serie_original.csv", show_col_types = FALSE)
des <- read_csv("resultados/T3_desigualdad_anual.csv", show_col_types = FALSE)
cvr <- read_csv("resultados/RC3_CV_ruido.csv", show_col_types = FALSE)
ex  <- read_csv("resultados/RB_exceso.csv", show_col_types = FALSE)
DATA <- list()

COL <- c(IMR = "#2a78d6", NMR = "#eb6834", PNMR = "#1baf7a")
QCOL <- c(Q1 = "#9ec5f4", Q2 = "#5598e7", Q3 = "#256abf", Q4 = "#0d366b")
th <- theme_minimal(base_size = 9, base_family = "sans") +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
        axis.line = element_line(colour = "grey40", linewidth = 0.3), axis.ticks = element_line(colour = "grey40", linewidth = 0.3),
        plot.title = element_text(face = "bold", size = 9), plot.tag = element_text(face = "bold", size = 10),
        legend.position = "none", strip.text = element_text(face = "bold", size = 8.5))
save_fig <- function(p, name, w, h) {
  ggsave(file.path(OUT, paste0(name, ".png")), p, width = w, height = h, dpi = 600, bg = "white")
  ggsave(file.path(OUT, paste0(name, ".pdf")), p, width = w, height = h, device = cairo_pdf)
  ggsave(file.path(OUT, paste0(name, ".eps")), p, width = w, height = h, device = cairo_ps, fallback_resolution = 600)
}
# Segmentos log-lineales de cada período (serie original, períodos de T2a)
segs <- function(serie_name, df, ycol, lab) {
  p <- per %>% filter(serie == serie_name)
  map_dfr(seq_len(nrow(p)), function(i) { d <- df %>% filter(anio >= p$desde[i], anio <= p$hasta[i])
    m <- lm(log(d[[ycol]]) ~ d$anio)
    tibble(serie = lab, periodo = i, anio = d$anio, ajuste = exp(fitted(m)), desde = p$desde[i], hasta = p$hasta[i],
           APC = p$VAP[i], LI = p$LI[i], LS = p$LS[i], pval = p$p[i]) })
}
apc_lab <- if (!ES) {
  function(a, l, u, p) gsub("-", "−", sprintf("APC %s%s\n(%s to %s)", ifelse(a > 0, "+", ""), formatC(a, format = "f", digits = 1),
                                       formatC(l, format = "f", digits = 1), formatC(u, format = "f", digits = 1)), fixed = TRUE) %>% paste0(ifelse(p < 0.05, "*", ""))
} else {
  function(a, l, u, p) paste0(sprintf("VAP %s\n(%s a %s)", fnum(a, 1, plus = TRUE), fnum(l, 1), fnum(u, 1)), ifelse(p < 0.05, "*", ""))
}
YLAB_RATE <- tx("Deaths per 1 000 live births", "Defunciones por 1 000 nacidos vivos")

# -----------------------------------------------------------------------------
# Figure 1. IMR 1980-2024; NMR and PNMR 2005-2024, with joinpoint segments
# -----------------------------------------------------------------------------
s1 <- segs("TMI 1980-2024", nac, "TMI", "IMR")
lab1 <- s1 %>% group_by(periodo) %>% summarise(x = mean(range(anio)), y = max(ajuste), APC = APC[1], LI = LI[1], LS = LS[1], pval = pval[1],
                                               desde = desde[1], hasta = hasta[1])
lab1$y <- c(34, 26, 18, 12.8); lab1$x[2] <- 2001.5; lab1$x[4] <- 2020.3   # 1997–2004: por encima de la serie
pA <- ggplot() + geom_point(data = nac, aes(anio, TMI), colour = COL["IMR"], size = 1.1, alpha = 0.7) +
  geom_line(data = s1, aes(anio, ajuste, group = periodo), colour = "grey15", linewidth = 0.6) +
  geom_vline(xintercept = c(1997, 2004, 2020), linetype = "dotted", colour = "grey55", linewidth = 0.3) +
  geom_text(data = lab1, aes(x, y, label = paste0(desde, "–", hasta, "\n", apc_lab(APC, LI, LS, pval))), size = 2.3, lineheight = 0.9, colour = "grey15") +
  scale_x_continuous(breaks = seq(1980, 2024, 5), expand = expansion(mult = c(0.01, 0.06))) + scale_y_continuous(limits = c(0, 37), expand = c(0, 0), labels = LAB) +
  labs(x = NULL, y = YLAB_RATE, tag = "A", title = tx("Infant mortality rate (IMR), 1980–2024", "Tasa de mortalidad infantil (TMI), 1980–2024")) + th
s2 <- bind_rows(segs("TMN 2005-2024", filter(nac, anio >= 2005), "TMN", "NMR"), segs("TMPN 2005-2024", filter(nac, anio >= 2005), "TMPN", "PNMR"))
n2 <- nac %>% filter(anio >= 2005) %>% select(anio, NMR = TMN, PNMR = TMPN) %>% pivot_longer(-anio, names_to = "serie", values_to = "tasa")
lab2 <- s2 %>% group_by(serie, periodo) %>% summarise(x = mean(range(anio)), APC = APC[1], LI = LI[1], LS = LS[1], pval = pval[1], desde = desde[1], hasta = hasta[1], .groups = "drop") %>%
  mutate(y = c(9.6, 7.6, 1.2, 1.2), x = c(2009.5, 2019.5, 2011, 2021.3))
pB <- ggplot() + geom_point(data = n2, aes(anio, tasa, colour = serie, shape = serie), size = 1.3) +
  geom_line(data = s2, aes(anio, ajuste, group = interaction(serie, periodo), colour = serie), linewidth = 0.6) +
  geom_text(data = lab2, aes(x, y, label = paste0(SER[serie], " ", desde, "–", hasta, "\n", apc_lab(APC, LI, LS, pval)), colour = serie), size = 2.2, lineheight = 0.9) +
  scale_colour_manual(values = COL) + scale_shape_manual(values = c(NMR = 16, PNMR = 17)) +
  scale_x_continuous(breaks = seq(2005, 2024, 3), expand = expansion(mult = c(0.03, 0.09))) + scale_y_continuous(limits = c(0, 10.2), expand = c(0, 0), labels = LAB) +
  labs(x = NULL, y = YLAB_RATE, tag = "B", title = tx("NMR and PNMR, 2005–2024", "TMN y TMPN, 2005–2024")) + th
f1 <- pA + pB + plot_layout(widths = c(1.35, 1))
save_fig(f1, FNAME[["F1"]], 7.2, 3.2)
DATA$Fig1_observed <- nac %>% select(year = anio, live_births = nv, infant_deaths = def, IMR = TMI, NMR = TMN, PNMR = TMPN)
DATA$Fig1_segments <- bind_rows(s1, s2) %>% rename(year = anio, fitted = ajuste, from = desde, to = hasta)

# -----------------------------------------------------------------------------
# Figure 2. Inequality indices 2005-2024
# -----------------------------------------------------------------------------
seg_ind <- function(v, name) {   # periodos del análisis de tendencia (serie original)
  p <- if (name %in% per$serie) per %>% filter(serie == name) else read_csv("resultados/RC2_tendencia_SII_RII.csv", show_col_types = FALSE) %>%
    bind_rows(read_csv("resultados/RC3b_tendencia_CV_senal.csv", show_col_types = FALSE)) %>% filter(serie == name)
  map_dfr(seq_len(nrow(p)), function(i) { d <- v %>% filter(anio >= p$desde[i], anio <= p$hasta[i]); m <- lm(log(d$y) ~ d$anio)
    tibble(periodo = i, anio = d$anio, ajuste = exp(fitted(m)), APC = p$VAP[i], LI = p$LI[i], LS = p$LS[i], pval = p$p[i], desde = p$desde[i], hasta = p$hasta[i]) })
}
pan <- function(v, name, ttl, ylab, tag, lo = NULL, hi = NULL, ylim = NULL, labpos) {
  s <- seg_ind(v, name)
  lb <- s %>% group_by(periodo) %>% summarise(x = mean(range(anio)), APC = APC[1], LI = LI[1], LS = LS[1], pval = pval[1], desde = desde[1], hasta = hasta[1])
  lb$y <- labpos
  g <- ggplot(v, aes(anio, y))
  if (!is.null(lo)) g <- g + geom_ribbon(aes(ymin = lo, ymax = hi), fill = COL["IMR"], alpha = 0.15)
  g + geom_point(colour = COL["IMR"], size = 1.1) + geom_line(data = s, aes(anio, ajuste, group = periodo), colour = "grey15", linewidth = 0.55) +
    geom_text(data = lb, aes(x, y, label = paste0(desde, "–", hasta, "\n", apc_lab(APC, LI, LS, pval))), size = 2.1, lineheight = 0.9, colour = "grey15") +
    scale_x_continuous(breaks = seq(2005, 2024, 5)) + (if (ES) scale_y_continuous(labels = LAB)) + coord_cartesian(ylim = ylim) +
    labs(x = NULL, y = ylab, title = ttl, tag = tag) + th
}
vG <- des %>% transmute(anio, y = Gini_TMI, lo = Gini_TMI_LI, hi = Gini_TMI_LS)
vC <- cvr %>% transmute(anio, y = CV_senal)
vS <- des %>% transmute(anio, y = SII, lo = SII_LI, hi = SII_LS)
vR <- des %>% transmute(anio, y = RII, lo = RII_LI, hi = RII_LS)
g1 <- pan(vG, "Gini TMI", tx("Gini index (between provinces)", "Índice de Gini (entre provincias)"), "Gini", "A", vG$lo, vG$hi, c(0.05, 0.14), c(0.125, 0.125, 0.135))
g2 <- pan(vC, "CV señal", tx("Coefficient of variation, net of random noise", "Coef. de variación, neto del ruido aleatorio"), "CV", "B", ylim = c(0.08, 0.24), labpos = c(0.225, 0.225))
g3 <- pan(vS, "SII TMI", tx("Slope index of inequality (UBN)", "Índice de desigualdad de la pendiente (NBI)"), YLAB_RATE, "C", vS$lo, vS$hi, c(0, 10.5), c(10, 10))
g4 <- pan(vR, "RII TMI", tx("Relative index of inequality (UBN)", "Índice relativo de desigualdad (NBI)"), tx("Rate ratio", "Razón de tasas"), "D", vR$lo, vR$hi, c(1, 2.3), c(2.2, 2.2))
f2 <- (g1 + g2) / (g3 + g4)
save_fig(f2, FNAME[["F3"]], 7.2, 5.4)
DATA$Fig3_inequality <- des %>% select(year = anio, IMR = TMI, Gini = Gini_TMI, Gini_LCL = Gini_TMI_LI, Gini_UCL = Gini_TMI_LS, SII, SII_LCL = SII_LI, SII_UCL = SII_LS,
  RII, RII_LCL = RII_LI, RII_UCL = RII_LS, CIx = IC, CIx_LCL = IC_LI, CIx_UCL = IC_LS) %>% left_join(cvr %>% select(year = anio, CV_observed = CV_obs, CV_noise = CV_ruido, CV_net = CV_senal), by = "year")

# -----------------------------------------------------------------------------
# Figure 3. Observed vs expected (2005-2019 trend) infant deaths, 2020-2024
# -----------------------------------------------------------------------------
cuart <- cen %>% mutate(cuartil = paste0("Q", ntile(pnbi2010, 4))) %>% select(prov, cuartil)
qs <- pr %>% left_join(cuart, by = "prov") %>% group_by(grupo = cuartil, anio) %>% summarise(nv = sum(nv), def = sum(def), .groups = "drop") %>%
  bind_rows(nac %>% filter(anio >= 2005) %>% transmute(grupo = "Argentina", anio, nv, def))
set.seed(2026)
fit_pred <- function(d) {
  m <- glm(def ~ anio + offset(log(nv)), family = quasipoisson, data = filter(d, anio <= 2019)); phi <- max(1.0001, summary(m)$dispersion)
  bs <- mvrnorm(10000, coef(m), vcov(m)); X <- cbind(1, d$anio)
  mu <- exp(bs %*% t(X)) * matrix(d$nv, 10000, nrow(d), byrow = TRUE)
  sim <- matrix(rnbinom(length(mu), mu = mu, size = mu / (phi - 1)), 10000)
  d %>% mutate(exp = colMeans(mu) / nv * 1000, lo = apply(sim, 2, quantile, .025) / nv * 1000, hi = apply(sim, 2, quantile, .975) / nv * 1000, obs = def / nv * 1000)
}
F3 <- qs %>% group_split(grupo) %>% map_dfr(fit_pred)
oe <- ex %>% filter(serie == "TMI", base == "2005-2019", is.na(anio))
oe <- if (!ES) {
  oe %>% transmute(grupo, lab = sprintf("2020–2024: O/E %.2f\n(95%% PI %.2f to %.2f)", razon_OE, OE_LI, OE_LS))
} else {
  oe %>% transmute(grupo, lab = sprintf("2020–2024: O/E %s\n(IP95%% %s a %s)", fnum(razon_OE, 2), fnum(OE_LI, 2), fnum(OE_LS, 2)))
}
F3 <- F3 %>% mutate(grupo = factor(grupo, levels = c("Argentina", "Q1", "Q2", "Q3", "Q4"),
  labels = tx(c("Argentina", "Q1 (lowest UBN)", "Q2", "Q3", "Q4 (highest UBN)"), c("Argentina", "C1 (menor NBI)", "C2", "C3", "C4 (mayor NBI)"))))
oe <- oe %>% mutate(grupo = factor(grupo, levels = c("Argentina", "Q1", "Q2", "Q3", "Q4"), labels = levels(F3$grupo)))
f3 <- ggplot(F3, aes(anio)) +
  geom_ribbon(data = filter(F3, anio >= 2020), aes(ymin = lo, ymax = hi), fill = "grey70", alpha = 0.45) +
  geom_line(aes(y = exp), colour = "grey30", linewidth = 0.5, linetype = "dashed") +
  geom_point(aes(y = obs, colour = anio >= 2020), size = 1.2) +
  geom_vline(xintercept = 2019.5, colour = "grey60", linewidth = 0.3, linetype = "dotted") +
  geom_text(data = oe, aes(x = 2004.3, y = 3.2, label = lab), hjust = 0, size = 1.95, lineheight = 0.9, colour = "grey15") +
  scale_colour_manual(values = c(`FALSE` = "#86b6ef", `TRUE` = "#0d366b")) +
  facet_wrap(~grupo, nrow = 1) + scale_x_continuous(breaks = c(2005, 2012, 2019, 2024)) + scale_y_continuous(limits = c(2, 18.5), expand = c(0, 0), labels = LAB) +
  labs(x = NULL, y = tx("Infant deaths per 1 000 live births", "Defunciones infantiles\npor 1 000 nacidos vivos")) + th + theme(axis.text.x = element_text(size = 6.5), panel.spacing.x = unit(1, "lines"))
save_fig(f3, FNAME[["F2"]], 7.2, 2.7)
DATA$Fig2_observed_expected <- F3 %>% transmute(group = grupo, year = anio, live_births = nv, infant_deaths = def, IMR_observed = obs,
  IMR_expected = exp, PI95_lower = ifelse(anio >= 2020, lo, NA), PI95_upper = ifelse(anio >= 2020, hi, NA))

# -----------------------------------------------------------------------------
# Supplementary figures
# -----------------------------------------------------------------------------
# S1. Sensitivity: original and smoothed IMR series 1980-2024 (English version of FS1 in 03_figuras.R)
sm1 <- read_csv("resultados/T2c_series_suavizadas.csv", show_col_types = FALSE) %>% filter(serie == "TMI_1980_2024") %>%
  select(-serie) %>% pivot_longer(-AÑO, names_to = "method", values_to = "rate") %>%
  mutate(method = factor(method, levels = c("TASA_ORIGINAL", "TASA_LOESS", "TASA_SPLINE", "TASA_MEDIA_MOVIL"),
                         labels = tx(c("Original", "LOESS", "Smoothing spline", "3-year moving average"),
                                     c("Original", "LOESS", "Spline de suavizado", "Media móvil de 3 años"))))
fs0 <- ggplot(sm1, aes(AÑO, rate, colour = method, linetype = method)) + geom_line(linewidth = 0.6) +
  geom_vline(xintercept = c(1997, 2004, 2020), linetype = "dotted", colour = "grey55", linewidth = 0.3) +
  scale_y_log10(labels = LAB) + scale_x_continuous(breaks = seq(1980, 2024, 5)) +
  scale_colour_manual(values = c("grey10", "#2a78d6", "#eb6834", "#1baf7a"), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted"), name = NULL) +
  labs(x = NULL, y = tx("Infant deaths per 1 000 live births (log scale)", "Defunciones infantiles por 1 000 nacidos vivos\n(escala logarítmica)")) + th + theme(legend.position = "bottom")
save_fig(fs0, FNAME[["S1"]], 6.5, 3.8)
DATA$FigS1_smoothed <- sm1 %>% pivot_wider(names_from = method, values_from = rate) %>% rename(year = AÑO)

# S1. Regional context (UN IGME) — IMR annual rate of reduction 2015-2019 vs 2019-2024
PAIS_ES <- c(ARG = "Argentina", BHS = "Bahamas", BLZ = "Belice", BOL = "Bolivia", BRA = "Brasil", BRB = "Barbados", CAN = "Canadá",
             CHL = "Chile", COL = "Colombia", CRI = "Costa Rica", CUB = "Cuba", DOM = "República Dominicana", ECU = "Ecuador",
             GTM = "Guatemala", GUY = "Guyana", HND = "Honduras", HTI = "Haití", JAM = "Jamaica", MEX = "México", NIC = "Nicaragua",
             PAN = "Panamá", PER = "Perú", PRY = "Paraguay", SLV = "El Salvador", SUR = "Suriname", TTO = "Trinidad y Tabago",
             URY = "Uruguay", USA = "Estados Unidos de América", VEN = "Venezuela", LAC = "América Latina y el Caribe")
pais <- function(iso) if (ES) unname(coalesce(PAIS_ES[iso], iso)) else iso
igm <- read_csv("resultados/RE_IGME_ARR.csv", show_col_types = FALSE) %>% filter(indicador == "TMI") %>%
  mutate(lab = fct_reorder(pais(iso3), ARR_2019_2024))
deis <- read_csv("resultados/RE_DEIS_ARR.csv", show_col_types = FALSE) %>% filter(indicador == "TMI")
fs1 <- ggplot(igm, aes(y = lab)) + geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_segment(aes(x = ARR_2015_2019, xend = ARR_2019_2024, yend = lab), colour = "grey75", linewidth = 0.5) +
  geom_point(aes(x = ARR_2015_2019), colour = "#86b6ef", size = 1.6) + geom_point(aes(x = ARR_2019_2024), colour = "#0d366b", size = 1.6) +
  geom_point(data = tibble(lab = pais("ARG"), x = deis[[grep("2019_2024", names(deis))]]), aes(x = x), shape = 4, size = 2.2, colour = "#eb6834", stroke = 0.9) +
  (if (ES) scale_x_continuous(labels = LAB)) +
  labs(x = tx("Annual rate of reduction of IMR (%/year)", "Tasa anual de reducción de la TMI (%/año)"), y = NULL) + th
save_fig(fs1, FNAME[["S3"]], 6.5, 5.5)
DATA$FigS3_IGME <- read_csv("resultados/RE_IGME_ARR.csv", show_col_types = FALSE)
# S2. Birthweight composition and weight-specific mortality
PESO <- c("<1000", "1000-1499", "1500-2499", "≥2500")
PESO_LAB <- if (ES) c("<1 000", "1 000–1 499", "1 500–2 499", "≥2 500") else waiver()
pw <- read_csv("resultados/RD3_peso_anual.csv", show_col_types = FALSE) %>% mutate(peso = factor(peso, levels = c("<1000", "1000-1499", "1500-2499", "≥2500")))
fs2a <- ggplot(filter(pw, peso != "≥2500"), aes(anio, pct_nv, colour = peso)) + geom_line(linewidth = 0.6) + geom_point(size = 1) +
  scale_colour_manual(values = c("<1000" = "#0d366b", "1000-1499" = "#256abf", "1500-2499" = "#5598e7", "≥2500" = "#9ec5f4"), limits = PESO, labels = PESO_LAB, name = tx("Birthweight (g)", "Peso al nacer (g)")) +
  (if (ES) scale_y_continuous(labels = LAB)) +
  labs(x = NULL, y = tx("% of live births", "% de nacidos vivos"), tag = "A") + th + theme(legend.position = "top")
fs2b <- ggplot(pw, aes(anio, tasa_esp, colour = peso)) + geom_line(linewidth = 0.6) + geom_point(size = 1) + scale_y_log10(labels = LAB) +
  scale_colour_manual(values = c("<1000" = "#0d366b", "1000-1499" = "#256abf", "1500-2499" = "#5598e7", "≥2500" = "#9ec5f4"), limits = PESO, labels = PESO_LAB, name = tx("Birthweight (g)", "Peso al nacer (g)")) +
  labs(x = NULL, y = tx("Deaths per 1 000 live births (log scale)", "Defunciones por 1 000 nacidos vivos\n(escala logarítmica)"), tag = "B") + th + theme(legend.position = "top")
save_fig((fs2a + labs(title = tx("Share of live births", "Proporción de nacidos vivos")) + fs2b + labs(title = tx("Weight-specific mortality", "Mortalidad específica por peso al nacer"))) +
           plot_layout(guides = "collect") & theme(legend.position = "bottom"), FNAME[["S2"]], 7.2, 3.5)
DATA$FigS2_birthweight <- pw

# Datos de las figuras (en español: hojas y columnas traducidas)
if (ES) {
  COLS_ES <- c(year = "año", live_births = "nacidos_vivos", infant_deaths = "defunciones_infantiles", IMR = "TMI", NMR = "TMN", PNMR = "TMPN",
               fitted = "ajuste", from = "desde", to = "hasta", APC = "VAP", LI = "VAP_LI_IC95", LS = "VAP_LS_IC95", pval = "p",
               Gini_LCL = "Gini_LI_IC95", Gini_UCL = "Gini_LS_IC95", SII = "IDP", SII_LCL = "IDP_LI_IC95", SII_UCL = "IDP_LS_IC95",
               RII = "IRD", RII_LCL = "IRD_LI_IC95", RII_UCL = "IRD_LS_IC95", CIx = "ICx", CIx_LCL = "ICx_LI_IC95", CIx_UCL = "ICx_LS_IC95",
               CV_observed = "CV_observado", CV_noise = "CV_ruido", CV_net = "CV_neto", group = "grupo", IMR_observed = "TMI_observada",
               IMR_expected = "TMI_esperada", PI95_lower = "IP95_inferior", PI95_upper = "IP95_superior", anio = "año",
               ARR_2010_2015 = "TAR_2010_2015", ARR_2015_2019 = "TAR_2015_2019", ARR_2019_2024 = "TAR_2019_2024")
  ren <- function(d) { n <- names(d); i <- n %in% names(COLS_ES); n[i] <- COLS_ES[n[i]]; setNames(d, n) }
  DATA$Fig1_segments$serie <- SER[DATA$Fig1_segments$serie]
  DATA$FigS3_IGME <- DATA$FigS3_IGME %>% mutate(`país` = pais(iso3), .after = iso3)
  DATA <- map(DATA, ren)
  names(DATA) <- c(Fig1_observed = "Fig1_observada", Fig1_segments = "Fig1_segmentos", Fig3_inequality = "Fig3_desigualdad",
                   Fig2_observed_expected = "Fig2_observada_esperada", FigS1_smoothed = "FigS1_suavizada", FigS3_IGME = "FigS3_IGME_regional",
                   FigS2_birthweight = "FigS2_peso_al_nacer")[names(DATA)]
}
wb <- createWorkbook(); for (n in names(DATA)) { addWorksheet(wb, n); writeData(wb, n, DATA[[n]]) }
saveWorkbook(wb, file.path(OUT, tx("datos_figuras_rpsp.xlsx", "datos_figuras_rpsp_es.xlsx")), overwrite = TRUE)
cat("Figuras listas (", IDIOMA, "): ", OUT, "\n", sep = "")
