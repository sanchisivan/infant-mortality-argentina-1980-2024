# =============================================================================
# 05_figuras_rpsp.R — Figuras del manuscrito (inglés) para Rev Panam Salud Publica
# Salida: figuras_rpsp/ (PNG 600 dpi, PDF y EPS editables) + datos_figuras_rpsp.xlsx
# Requiere 01, 02 y 04.
# =============================================================================
suppressPackageStartupMessages({library(tidyverse); library(patchwork); library(openxlsx); library(MASS)})
select <- dplyr::select
dir.create("figuras_rpsp", showWarnings = FALSE)
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
  ggsave(file.path("figuras_rpsp", paste0(name, ".png")), p, width = w, height = h, dpi = 600, bg = "white")
  ggsave(file.path("figuras_rpsp", paste0(name, ".pdf")), p, width = w, height = h, device = cairo_pdf)
  ggsave(file.path("figuras_rpsp", paste0(name, ".eps")), p, width = w, height = h, device = cairo_ps, fallback_resolution = 600)
}
# Segmentos log-lineales de cada período (serie original, períodos de T2a)
segs <- function(serie_name, df, ycol, lab) {
  p <- per %>% filter(serie == serie_name)
  map_dfr(seq_len(nrow(p)), function(i) { d <- df %>% filter(anio >= p$desde[i], anio <= p$hasta[i])
    m <- lm(log(d[[ycol]]) ~ d$anio)
    tibble(serie = lab, periodo = i, anio = d$anio, ajuste = exp(fitted(m)), desde = p$desde[i], hasta = p$hasta[i],
           APC = p$VAP[i], LI = p$LI[i], LS = p$LS[i], pval = p$p[i]) })
}
apc_lab <- function(a, l, u, p) gsub("-", "\u2212", sprintf("APC %s%s\n(%s to %s)", ifelse(a > 0, "+", ""), formatC(a, format = "f", digits = 1),
                                       formatC(l, format = "f", digits = 1), formatC(u, format = "f", digits = 1)), fixed = TRUE) %>% paste0(ifelse(p < 0.05, "*", ""))

# -----------------------------------------------------------------------------
# Figure 1. IMR 1980-2024; NMR and PNMR 2005-2024, with joinpoint segments
# -----------------------------------------------------------------------------
s1 <- segs("TMI 1980-2024", nac, "TMI", "IMR")
lab1 <- s1 %>% group_by(periodo) %>% summarise(x = mean(range(anio)), y = max(ajuste), APC = APC[1], LI = LI[1], LS = LS[1], pval = pval[1],
                                               desde = desde[1], hasta = hasta[1])
lab1$y <- c(34, 22, 18, 12.8); lab1$x[4] <- 2020.3
pA <- ggplot() + geom_point(data = nac, aes(anio, TMI), colour = COL["IMR"], size = 1.1, alpha = 0.7) +
  geom_line(data = s1, aes(anio, ajuste, group = periodo), colour = "grey15", linewidth = 0.6) +
  geom_vline(xintercept = c(1997, 2004, 2020), linetype = "dotted", colour = "grey55", linewidth = 0.3) +
  geom_text(data = lab1, aes(x, y, label = paste0(desde, "–", hasta, "\n", apc_lab(APC, LI, LS, pval))), size = 2.3, lineheight = 0.9, colour = "grey15") +
  scale_x_continuous(breaks = seq(1980, 2024, 5), expand = expansion(mult = c(0.01, 0.06))) + scale_y_continuous(limits = c(0, 37), expand = c(0, 0)) +
  labs(x = NULL, y = "Deaths per 1 000 live births", tag = "A", title = "Infant mortality rate (IMR), 1980–2024") + th
s2 <- bind_rows(segs("TMN 2005-2024", filter(nac, anio >= 2005), "TMN", "NMR"), segs("TMPN 2005-2024", filter(nac, anio >= 2005), "TMPN", "PNMR"))
n2 <- nac %>% filter(anio >= 2005) %>% select(anio, NMR = TMN, PNMR = TMPN) %>% pivot_longer(-anio, names_to = "serie", values_to = "tasa")
lab2 <- s2 %>% group_by(serie, periodo) %>% summarise(x = mean(range(anio)), APC = APC[1], LI = LI[1], LS = LS[1], pval = pval[1], desde = desde[1], hasta = hasta[1], .groups = "drop") %>%
  mutate(y = c(9.6, 7.6, 1.2, 1.2), x = c(2009.5, 2019.5, 2011, 2021.3))
pB <- ggplot() + geom_point(data = n2, aes(anio, tasa, colour = serie, shape = serie), size = 1.3) +
  geom_line(data = s2, aes(anio, ajuste, group = interaction(serie, periodo), colour = serie), linewidth = 0.6) +
  geom_text(data = lab2, aes(x, y, label = paste0(serie, " ", desde, "–", hasta, "\n", apc_lab(APC, LI, LS, pval)), colour = serie), size = 2.2, lineheight = 0.9) +
  scale_colour_manual(values = COL) + scale_shape_manual(values = c(NMR = 16, PNMR = 17)) +
  scale_x_continuous(breaks = seq(2005, 2024, 3), expand = expansion(mult = c(0.03, 0.09))) + scale_y_continuous(limits = c(0, 10.2), expand = c(0, 0)) +
  labs(x = NULL, y = "Deaths per 1 000 live births", tag = "B", title = "NMR and PNMR, 2005–2024") + th
f1 <- pA + pB + plot_layout(widths = c(1.35, 1))
save_fig(f1, "Figure1_trends", 7.2, 3.2)
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
    scale_x_continuous(breaks = seq(2005, 2024, 5)) + coord_cartesian(ylim = ylim) +
    labs(x = NULL, y = ylab, title = ttl, tag = tag) + th
}
vG <- des %>% transmute(anio, y = Gini_TMI, lo = Gini_TMI_LI, hi = Gini_TMI_LS)
vC <- cvr %>% transmute(anio, y = CV_senal)
vS <- des %>% transmute(anio, y = SII, lo = SII_LI, hi = SII_LS)
vR <- des %>% transmute(anio, y = RII, lo = RII_LI, hi = RII_LS)
g1 <- pan(vG, "Gini TMI", "Gini index (between provinces)", "Gini", "A", vG$lo, vG$hi, c(0.05, 0.14), c(0.125, 0.125, 0.135))
g2 <- pan(vC, "CV señal", "Coefficient of variation, net of random noise", "CV", "B", ylim = c(0.08, 0.24), labpos = c(0.225, 0.225))
g3 <- pan(vS, "SII TMI", "Slope index of inequality (UBN)", "Deaths per 1 000 live births", "C", vS$lo, vS$hi, c(0, 10.5), c(10, 10))
g4 <- pan(vR, "RII TMI", "Relative index of inequality (UBN)", "Rate ratio", "D", vR$lo, vR$hi, c(1, 2.3), c(2.2, 2.2))
f2 <- (g1 + g2) / (g3 + g4)
save_fig(f2, "Figure3_inequality", 7.2, 5.4)
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
oe <- ex %>% filter(serie == "TMI", base == "2005-2019", is.na(anio)) %>%
  transmute(grupo, lab = gsub("-", "\u2212", fixed = TRUE, x = sprintf("2020–2024: O/E %.2f\nexcess %s%.0f (%s%.0f to %s%.0f)", razon_OE, ifelse(exceso > 0, "+", ""), exceso,
                                   ifelse(exceso_LI > 0, "+", ""), exceso_LI, ifelse(exceso_LS > 0, "+", ""), exceso_LS)))
F3 <- F3 %>% mutate(grupo = factor(grupo, levels = c("Argentina", "Q1", "Q2", "Q3", "Q4"),
  labels = c("Argentina", "Q1 (lowest UBN)", "Q2", "Q3", "Q4 (highest UBN)")))
oe <- oe %>% mutate(grupo = factor(grupo, levels = c("Argentina", "Q1", "Q2", "Q3", "Q4"), labels = levels(F3$grupo)))
f3 <- ggplot(F3, aes(anio)) +
  geom_ribbon(data = filter(F3, anio >= 2020), aes(ymin = lo, ymax = hi), fill = "grey70", alpha = 0.45) +
  geom_line(aes(y = exp), colour = "grey30", linewidth = 0.5, linetype = "dashed") +
  geom_point(aes(y = obs, colour = anio >= 2020), size = 1.2) +
  geom_vline(xintercept = 2019.5, colour = "grey60", linewidth = 0.3, linetype = "dotted") +
  geom_text(data = oe, aes(x = 2005, y = 3.2, label = lab), hjust = 0, size = 2.1, lineheight = 0.9, colour = "grey15") +
  scale_colour_manual(values = c(`FALSE` = "#86b6ef", `TRUE` = "#0d366b")) +
  facet_wrap(~grupo, nrow = 1) + scale_x_continuous(breaks = c(2005, 2012, 2019, 2024)) + scale_y_continuous(limits = c(2, 18.5), expand = c(0, 0)) +
  labs(x = NULL, y = "Infant deaths per 1 000 live births") + th + theme(axis.text.x = element_text(size = 6.5))
save_fig(f3, "Figure2_observed_expected", 7.2, 2.7)
DATA$Fig2_observed_expected <- F3 %>% transmute(group = grupo, year = anio, live_births = nv, infant_deaths = def, IMR_observed = obs,
  IMR_expected = exp, PI95_lower = ifelse(anio >= 2020, lo, NA), PI95_upper = ifelse(anio >= 2020, hi, NA))

# -----------------------------------------------------------------------------
# Supplementary figures
# -----------------------------------------------------------------------------
# S1. Sensitivity: original and smoothed IMR series 1980-2024 (English version of FS1 in 03_figuras.R)
sm1 <- read_csv("resultados/T2c_series_suavizadas.csv", show_col_types = FALSE) %>% filter(serie == "TMI_1980_2024") %>%
  select(-serie) %>% pivot_longer(-AÑO, names_to = "method", values_to = "rate") %>%
  mutate(method = factor(method, levels = c("TASA_ORIGINAL", "TASA_LOESS", "TASA_SPLINE", "TASA_MEDIA_MOVIL"),
                         labels = c("Original", "LOESS", "Smoothing spline", "3-year moving average")))
fs0 <- ggplot(sm1, aes(AÑO, rate, colour = method, linetype = method)) + geom_line(linewidth = 0.6) +
  geom_vline(xintercept = c(1997, 2004, 2020), linetype = "dotted", colour = "grey55", linewidth = 0.3) +
  scale_y_log10() + scale_x_continuous(breaks = seq(1980, 2024, 5)) +
  scale_colour_manual(values = c("grey10", "#2a78d6", "#eb6834", "#1baf7a"), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted"), name = NULL) +
  labs(x = NULL, y = "Infant deaths per 1 000 live births (log scale)") + th + theme(legend.position = "bottom")
save_fig(fs0, "FigureS1_smoothed_IMR", 6.5, 3.8)
DATA$FigS1_smoothed <- sm1 %>% pivot_wider(names_from = method, values_from = rate) %>% rename(year = AÑO)

# S1. Regional context (UN IGME) — IMR annual rate of reduction 2015-2019 vs 2019-2024
igm <- read_csv("resultados/RE_IGME_ARR.csv", show_col_types = FALSE) %>% filter(indicador == "TMI") %>%
  mutate(iso3 = fct_reorder(iso3, ARR_2019_2024))
deis <- read_csv("resultados/RE_DEIS_ARR.csv", show_col_types = FALSE) %>% filter(indicador == "TMI")
fs1 <- ggplot(igm, aes(y = iso3)) + geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_segment(aes(x = ARR_2015_2019, xend = ARR_2019_2024, yend = iso3), colour = "grey75", linewidth = 0.5) +
  geom_point(aes(x = ARR_2015_2019), colour = "#86b6ef", size = 1.6) + geom_point(aes(x = ARR_2019_2024), colour = "#0d366b", size = 1.6) +
  geom_point(data = tibble(iso3 = "ARG", x = deis[[grep("2019_2024", names(deis))]]), aes(x = x), shape = 4, size = 2.2, colour = "#eb6834", stroke = 0.9) +
  labs(x = "Annual rate of reduction of IMR (%/year)", y = NULL) + th
save_fig(fs1, "FigureS3_regional_IGME", 6.5, 5.5)
DATA$FigS3_IGME <- read_csv("resultados/RE_IGME_ARR.csv", show_col_types = FALSE)
# S2. Birthweight composition and weight-specific mortality
pw <- read_csv("resultados/RD3_peso_anual.csv", show_col_types = FALSE) %>% mutate(peso = factor(peso, levels = c("<1000", "1000-1499", "1500-2499", "≥2500")))
fs2a <- ggplot(filter(pw, peso != "≥2500"), aes(anio, pct_nv, colour = peso)) + geom_line(linewidth = 0.6) + geom_point(size = 1) +
  scale_colour_manual(values = c("<1000" = "#0d366b", "1000-1499" = "#256abf", "1500-2499" = "#5598e7", "≥2500" = "#9ec5f4"), limits = c("<1000", "1000-1499", "1500-2499", "≥2500"), name = "Birthweight (g)") +
  labs(x = NULL, y = "% of live births", tag = "A") + th + theme(legend.position = "top")
fs2b <- ggplot(pw, aes(anio, tasa_esp, colour = peso)) + geom_line(linewidth = 0.6) + geom_point(size = 1) + scale_y_log10() +
  scale_colour_manual(values = c("<1000" = "#0d366b", "1000-1499" = "#256abf", "1500-2499" = "#5598e7", "≥2500" = "#9ec5f4"), limits = c("<1000", "1000-1499", "1500-2499", "≥2500"), name = "Birthweight (g)") +
  labs(x = NULL, y = "Deaths per 1 000 live births (log scale)", tag = "B") + th + theme(legend.position = "top")
save_fig((fs2a + labs(title = "Share of live births") + fs2b + labs(title = "Weight-specific mortality")) + plot_layout(guides = "collect") & theme(legend.position = "bottom"), "FigureS2_birthweight", 7.2, 3.5)
DATA$FigS2_birthweight <- pw

wb <- createWorkbook(); for (n in names(DATA)) { addWorksheet(wb, n); writeData(wb, n, DATA[[n]]) }
saveWorkbook(wb, "figuras_rpsp/datos_figuras_rpsp.xlsx", overwrite = TRUE)
cat("Figuras listas\n")
