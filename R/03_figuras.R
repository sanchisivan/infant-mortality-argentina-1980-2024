# =============================================================================
# 03_figuras.R — Figuras (ggplot2, PNG 300 dpi) y datos de cada figura
# Salidas: figuras/F*.png y figuras/datos_figuras.xlsx (una hoja por figura)
# =============================================================================
suppressPackageStartupMessages({library(tidyverse); library(patchwork); library(ggrepel); library(openxlsx)})
source("R/funciones_tendencia.R", encoding = "UTF-8"); source("R/funciones_desigualdad.R", encoding = "UTF-8")
dir.create("figuras", showWarnings = FALSE)
O <- readRDS("resultados/objetos_analisis.rds")
nac <- read_csv("data/serie_nacional_1980_2024.csv", show_col_types = FALSE)
pr  <- read_csv("data/panel_provincial_2005_2024.csv", col_types = cols(prov = col_character()))
cen <- read_csv("data/indicadores_censales_provincia.csv", col_types = cols(prov = col_character()))
AZ <- "#2a78d6"; NA_ <- "#eb6834"; GR <- "#8a8a86"; SEQ <- c("#a9cdf2", "#5b9de6", "#2a78d6", "#0d3f80")
coma <- function(d = 0) function(x) formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")
tema <- theme_minimal(base_size = 9) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", size = 10),
  plot.title.position = "plot", legend.position = "bottom", axis.title = element_text(size = 8.5))
DATOS <- list()
guardar <- function(p, nombre, w, h, datos) { ggsave(file.path("figuras", paste0(nombre, ".png")), p, width = w, height = h, dpi = 300, bg = "white"); DATOS[[nombre]] <<- datos }

# Ajuste por períodos (serie original) para dibujar
ajuste_periodos <- function(datos, periodos) bind_rows(lapply(periodos, function(p) {
  d <- filter(datos, AÑO >= p$inicio_real, AÑO <= p$fin_real); m <- lm(log(TASA) ~ AÑO, d)
  tibble(AÑO = d$AÑO, ajuste = exp(fitted(m)), periodo = paste0(p$inicio_real, "-", p$fin_real), VAP = p$vpa, LI = p$vpa_li, LS = p$vpa_ls, p = p$p_value) }))
etiqueta <- function(a) a %>% group_by(periodo) %>% summarise(AÑO = mean(AÑO), y = max(ajuste), VAP = first(VAP), LI = first(LI), LS = first(LS), p = first(p), .groups = "drop") %>%
  mutate(lab = sprintf("%s\nVAP %s%%\n(%s; %s)%s", periodo, coma(2)(VAP), coma(2)(LI), coma(2)(LS), ifelse(p < 0.05, "*", "")))


# Capas para dibujar los períodos (serie original) de un resultado de tendencia_serie()
capas_periodos <- function(datos, res, color = NA_, signo = 1, dec = 1, pos_y = c("arriba", "abajo"), tam = 2.2) {
  pos_y <- match.arg(pos_y)
  r <- res$resultados$original
  a <- ajuste_periodos(datos, r$periodos) %>% mutate(ajuste = signo * ajuste)
  e <- a %>% group_by(periodo) %>% summarise(AÑO = mean(AÑO), y = if (pos_y == "arriba") max(ajuste) else min(ajuste),
        VAP = first(VAP), LI = first(LI), LS = first(LS), p = first(p), .groups = "drop") %>%
    mutate(lab = sprintf("%s\nVAP %s%%%s", periodo, coma(dec)(VAP), ifelse(p < 0.05, "*", "")))
  rng <- diff(range(signo * datos$TASA, na.rm = TRUE))
  list(geom_vline(xintercept = r$quiebres_detectados, linetype = "dashed", color = GR, linewidth = .35),
       geom_line(data = a, aes(x = AÑO, y = ajuste, group = periodo), color = color, linewidth = 1.1, inherit.aes = FALSE),
       geom_text(data = e, aes(x = AÑO, y = y + ifelse(pos_y == "arriba", 1, -1) * rng * .12, label = lab), size = tam, lineheight = .85, color = "#333333", inherit.aes = FALSE))
}
nota_vap <- "Líneas: tendencia por período (regresión log-lineal sobre la serie original; quiebres por strucchange/BIC).\nVAP: variación anual promedio. * p < 0,05."

# ---- F1: TMI 1980-2024 y nacidos vivos
d1 <- data.frame(AÑO = nac$anio, TASA = nac$TMI)
r1 <- O$TEND$TMI_1980_2024$resultados$original
a1 <- ajuste_periodos(d1, r1$periodos); e1 <- etiqueta(a1)
p1a <- ggplot(d1, aes(AÑO, TASA)) + geom_point(color = AZ, size = 1.6) +
  geom_line(data = a1, aes(y = ajuste, group = periodo), color = NA_, linewidth = 1) +
  geom_vline(xintercept = r1$quiebres_detectados, linetype = "dashed", color = GR, linewidth = .4) +
  geom_text(data = e1, aes(y = y * 1.18, label = lab), size = 2.3, lineheight = .9, color = "#333333") +
  scale_y_log10(labels = coma(0), breaks = c(8, 10, 15, 20, 30)) + labs(x = NULL, y = "TMI por 1.000 NV (escala log)",
  title = "Figura 1. Mortalidad infantil y nacimientos. Argentina, 1980-2024") + tema
p1b <- ggplot(nac, aes(anio, nv / 1000)) + geom_line(color = "#52514e", linewidth = 1) + geom_point(color = "#52514e", size = 1.1) +
  annotate("text", x = 2014, y = 777 + 45, label = "2014: 777 mil", size = 2.4) + annotate("text", x = 2021.5, y = 413 - 45, label = "2024: 413 mil", size = 2.4) +
  labs(x = NULL, y = "Nacidos vivos (miles)") + scale_y_continuous(labels = coma(0), limits = c(350, 850)) + tema
guardar(p1a / p1b + plot_layout(heights = c(2.2, 1)), "F1_TMI_periodos_nacimientos_1980_2024", 6.5, 5.6,
        nac %>% select(anio, nv, def, TMI) %>% left_join(a1 %>% rename(anio = AÑO), by = "anio"))

# ---- F2: Gini (app) e índice de concentración, con períodos
D <- O$DES
dG <- data.frame(AÑO = D$anio, TASA = D$Gini_TMI); dI <- data.frame(AÑO = D$anio, TASA = abs(D$IC))
p2a <- ggplot(D, aes(anio, Gini_TMI)) + geom_ribbon(aes(ymin = Gini_TMI_LI, ymax = Gini_TMI_LS), fill = AZ, alpha = .15) +
  geom_point(color = AZ, size = 1.5) + capas_periodos(dG, O$TEND$Gini_TMI, color = AZ) + scale_y_continuous(labels = coma(2), limits = c(0.05, .16)) +
  labs(x = NULL, y = NULL, subtitle = "Índice de Gini entre jurisdicciones (IC95%)") + tema
p2b <- ggplot(D, aes(anio, IC)) + geom_ribbon(aes(ymin = IC_LI, ymax = IC_LS), fill = NA_, alpha = .15) +
  geom_point(color = NA_, size = 1.5) + capas_periodos(dI, O$TEND$IC_abs, color = NA_, signo = -1, pos_y = "abajo") + scale_y_continuous(labels = coma(2), limits = c(-.13, -0.02)) +
  labs(x = NULL, y = NULL, subtitle = "Índice de concentración según NBI (IC95%)") + tema
guardar((p2a | p2b) + plot_annotation(title = "Figura 2. Desigualdad en la mortalidad infantil. Argentina, 2005-2024", caption = paste(nota_vap, "Para el IC, la VAP se estima sobre |IC| (VAP negativa = menor desigualdad)."), theme = tema),
        "F2_Gini_IC_2005_2024", 7.4, 3.7, D %>% select(anio, TMI, Gini_TMI, Gini_TMI_LI, Gini_TMI_LS, IC, IC_LI, IC_LS))

# ---- F3: TMI por cuartil de NBI (con períodos) y razón Q4/Q1
QT <- O$QT; QW <- O$QW
labQ <- c(Q1 = "Q1 (menor NBI)", Q2 = "Q2", Q3 = "Q3", Q4 = "Q4 (mayor NBI)")
p3q <- lapply(paste0("Q", 1:4), function(q) { d <- filter(QT, cuartil == q); dd <- data.frame(AÑO = d$anio, TASA = d$TMI)
  ggplot(d, aes(anio, TMI)) + geom_point(color = SEQ[as.integer(sub("Q", "", q))], size = 1.3) + capas_periodos(dd, O$TEND[[paste0("TMI_", q)]], color = SEQ[as.integer(sub("Q", "", q))], tam = 1.9) +
    scale_y_continuous(limits = c(5.5, 19), labels = coma(0)) + labs(x = NULL, y = if (q %in% c("Q1", "Q3")) "TMI por 1.000 NV" else NULL, subtitle = labQ[q]) + tema })
dR <- data.frame(AÑO = QW$anio, TASA = QW$razon_Q4_Q1)
p3b <- ggplot(QW, aes(anio, razon_Q4_Q1)) + geom_hline(yintercept = 1, color = "#555555") + geom_point(color = NA_, size = 1.5) +
  capas_periodos(dR, O$TEND$razon_Q4_Q1, color = NA_) + scale_y_continuous(labels = coma(1), limits = c(1, 1.8)) + labs(x = NULL, y = NULL, subtitle = "Razón de tasas Q4/Q1") + tema
guardar(((p3q[[1]] | p3q[[2]]) / (p3q[[3]] | p3q[[4]]) | p3b) + plot_layout(widths = c(2, 1)) +
        plot_annotation(title = "Figura 3. Mortalidad infantil según cuartil de NBI provincial (censo 2010), 2005-2024", caption = nota_vap, theme = tema),
        "F3_TMI_cuartiles_NBI", 8.2, 5, QW)

# ---- F4: TMI 2021-2023 vs Censo 2022
g <- O$g22
f4 <- function(x, xl) ggplot(g, aes(.data[[x]], TMI)) + geom_smooth(method = "lm", mapping = aes(weight = nv), se = FALSE, color = GR, linetype = "dashed", linewidth = .5, formula = y ~ x) +
  geom_point(aes(size = nv), color = AZ, alpha = .75) + geom_text_repel(aes(label = provincia), size = 2.2, color = "#333333", max.overlaps = 30, seed = 1) +
  scale_size_area(max_size = 6, guide = "none") + scale_x_continuous(labels = coma(0)) + labs(x = xl, y = "TMI 2021-2023 por 1.000 NV") + tema
guardar((f4("pnbi2022", "% población en hogares con NBI (2022)") | f4("p_sincob2022", "% población sin cobertura de salud (2022)")) +
        plot_annotation(title = "Figura 4. TMI provincial 2021-2023 e indicadores del Censo 2022", theme = tema), "F4_dispersion_TMI_censo2022", 7.6, 3.9,
        g %>% select(provincia, nv, def, TMI, pnbi2022, p_sincob2022))

# ---- F5: curvas de concentración
cc <- bind_rows(lapply(list(list(2009, 2011, "pnbi2010", "2009-2011 (NBI 2010)"), list(2021, 2023, "pnbi2022", "2021-2023 (NBI 2022)")), function(k) {
  t <- pr %>% filter(anio >= k[[1]], anio <= k[[2]]) %>% group_by(prov) %>% summarise(nv = sum(nv), def = sum(def)) %>% left_join(cen, by = "prov") %>% arrange(desc(.data[[k[[3]]]]))
  ic <- conc_index(t$def, t$nv, t[[k[[3]]]])
  tibble(serie = sprintf("%s; IC = %s", k[[4]], coma(3)(ic)), provincia = c("", t$provincia), P = c(0, cumsum(t$nv) / sum(t$nv)), D = c(0, cumsum(t$def) / sum(t$def))) }))
p5 <- ggplot(cc, aes(P, D, color = serie)) + geom_abline(color = "#555555") + geom_line(linewidth = 1) + scale_color_manual(values = c(SEQ[2], NA_), name = NULL) +
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) + coord_equal() +
  labs(x = "Nacidos vivos acumulados\n(jurisdicciones de mayor a menor NBI)", y = "Defunciones infantiles acumuladas", title = "Figura 5. Curvas de concentración") + tema + theme(legend.direction = "vertical")
guardar(p5, "F5_curvas_concentracion", 4.2, 4.6, cc)

# ---- F6: neonatal y posneonatal (con períodos)
n6 <- nac %>% filter(anio >= 2005)
f6 <- function(var, res, col, sub, lim, dec = 2, ylab = NULL, x = n6, xv = "anio") { dd <- data.frame(AÑO = x[[xv]], TASA = x[[var]])
  ggplot(x, aes(.data[[xv]], .data[[var]])) + geom_point(color = col, size = 1.4) + capas_periodos(dd, res, color = col, tam = 1.9) +
    scale_y_continuous(limits = lim, labels = coma(dec)) + labs(x = NULL, y = ylab, subtitle = sub) + tema }
p6 <- (f6("TMN", O$TEND$TMN_2005_2024, AZ, "Tasa de mortalidad neonatal", c(4, 10), 0, "Tasa por 1.000 NV") |
       f6("TMPN", O$TEND$TMPN_2005_2024, NA_, "Tasa de mortalidad posneonatal", c(1.5, 5.5), 0)) /
      (f6("Gini_TMN", O$TEND$Gini_TMN, AZ, "Gini neonatal entre jurisdicciones", c(.05, .16), x = D) |
       f6("Gini_TMPN", O$TEND$Gini_TMPN, NA_, "Gini posneonatal entre jurisdicciones", c(.05, .17), x = D))
guardar(p6 + plot_annotation(title = "Figura 6. Mortalidad neonatal y posneonatal: nivel y desigualdad. Argentina, 2005-2024", caption = nota_vap, theme = tema),
        "F6_neonatal_posneonatal", 7.4, 5.4,
        n6 %>% select(anio, nv, neo, posneo, TMN, TMPN) %>% left_join(D %>% select(anio, Gini_TMN, Gini_TMN_LI, Gini_TMN_LS, Gini_TMPN, Gini_TMPN_LI, Gini_TMPN_LS), by = "anio"))

# ---- F7: características de los nacimientos
b <- nac %>% filter(anio >= 2005) %>% transmute(anio, `% madres <20 años` = m20 / nv * 100, `% bajo peso (<2.500 g)` = bpn / (nv - pns) * 100,
                                                  `% pretérmino (<37 semanas)` = pret / (nv - gns) * 100) %>% pivot_longer(-anio)
p7 <- ggplot(b, aes(anio, value)) + geom_line(color = AZ, linewidth = 1) + facet_wrap(~name, scales = "free_y") + expand_limits(y = 0) +
  scale_y_continuous(labels = coma(0)) + labs(x = NULL, y = "%", title = "Figura 7. Características de los nacimientos. Argentina, 2005-2024") + tema
guardar(p7, "F7_nacimientos_caracteristicas", 7.2, 2.8, b %>% pivot_wider(names_from = name, values_from = value))

# ---- F8: convergencia entre provincias (beta y sigma)
V <- O$VAPP
fb <- function(xv, yv, xl, sub) { t <- cor.test(log(V[[xv]]), V[[yv]]); 
  ggplot(V, aes(.data[[xv]], .data[[yv]])) + geom_hline(yintercept = 0, color = "#555555") + geom_smooth(method = "lm", formula = y ~ log(x), se = TRUE, color = GR, fill = "#dddddd", linewidth = .6) +
    geom_point(color = AZ, size = 1.8) + geom_text_repel(aes(label = provincia), size = 2, color = "#333333", seed = 1, max.overlaps = 30) +
    scale_y_continuous(labels = coma(1)) + labs(x = xl, y = "VAP de la TMI (%)", subtitle = sprintf("%s (r = %s; p = %s)", sub, coma(2)(t$estimate), sub(".", ",", formatC(t$p.value, format = "f", digits = 3), fixed = TRUE))) + tema }
dS <- data.frame(AÑO = O$SIG$anio, TASA = O$SIG$CV)
p8c <- ggplot(O$SIG, aes(anio, CV)) + geom_point(color = NA_, size = 1.5) + capas_periodos(dS, O$TEND$CV_provincial, color = NA_) +
  scale_y_continuous(labels = coma(2), limits = c(.1, .26)) + labs(x = NULL, y = "Coeficiente de variación", subtitle = "Convergencia sigma: dispersión de la TMI provincial") + tema
guardar((fb("TMI_2005_2007", "VAP_2005_2014", "TMI 2005-2007 (escala log)", "Convergencia beta 2005-2014") + scale_x_log10() |
         fb("TMI_2012_2014", "VAP_2014_2024", "TMI 2012-2014 (escala log)", "Convergencia beta 2014-2024") + scale_x_log10()) / p8c +
        plot_annotation(title = "Figura 8. Convergencia de la mortalidad infantil entre provincias", caption = nota_vap, theme = tema),
        "F8_convergencia_provincias", 7.6, 6.4, V %>% select(provincia, TMI_2005_2007, TMI_2012_2014, TMI_2022_2024, VAP_2005_2014, VAP_2014_2024, VAP_2005_2024))
DATOS[["F8b_sigma"]] <- O$SIG

# ---- F9: descomposición del cambio de la TMI
d9 <- O$DESC %>% filter(periodo != "2020-2024") %>% mutate(grupo = fct_reorder(grupo, cambio_absoluto))
p9 <- ggplot(d9, aes(cambio_absoluto, grupo, fill = periodo)) + geom_col(position = position_dodge(width = .75), width = .7) + geom_vline(xintercept = 0, color = "#555555") +
  facet_grid(clasificacion ~ ., scales = "free_y", space = "free_y") + scale_fill_manual(values = c(SEQ[2], NA_), name = NULL) +
  scale_x_continuous(labels = coma(1)) + labs(x = "Cambio absoluto de la tasa (por 1.000 NV)", y = NULL,
  title = "Figura 9. Contribución de componentes y grupos de causas al descenso de la TMI") + tema
guardar(p9, "F9_descomposicion_cambio_TMI", 6.6, 4.4, O$DESC)

# ---- F8 (sensibilidad): series original y suavizadas, TMI 1980-2024 (como en el script)
s8 <- O$TEND$TMI_1980_2024$tabla_suavizados %>% pivot_longer(-AÑO, names_to = "Metodo", values_to = "Tasa") %>%
  mutate(Metodo = factor(Metodo, levels = c("TASA_ORIGINAL", "TASA_LOESS", "TASA_SPLINE", "TASA_MEDIA_MOVIL"), labels = c("Original", "LOESS", "Spline", "Media móvil")))
p8 <- ggplot(s8, aes(AÑO, Tasa, color = Metodo, linetype = Metodo)) + geom_line(linewidth = .8) + scale_y_log10(labels = coma(0)) +
  scale_color_manual(values = c("black", NA_, AZ, "#1baf7a"), name = NULL) + scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted"), name = NULL) +
  labs(x = NULL, y = "TMI por 1.000 NV (escala log)", title = "Figura S1. Comparación de métodos de suavizado. TMI 1980-2024") + tema
guardar(p8, "FS1_suavizados_TMI", 6.5, 3.8, O$TEND$TMI_1980_2024$tabla_suavizados)

wb <- createWorkbook(); for (n in names(DATOS)) { s <- substr(n, 1, 31); addWorksheet(wb, s); writeData(wb, s, DATOS[[n]]) }
saveWorkbook(wb, "figuras/datos_figuras.xlsx", overwrite = TRUE)
cat("Figuras generadas:", length(DATOS), "\n")
