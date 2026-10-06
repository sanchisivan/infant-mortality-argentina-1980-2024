# =============================================================================
# Funciones de análisis de tendencia y puntos de quiebre
# Adaptadas de "Analisis Tendencia Suavizado Quiebres y Segmentos Deep Seek.R"
# (procedimiento final: suavizado -> períodos -> VAP log-lineal -> transiciones
#  -> selección del método con mayor puntaje)
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(broom); library(strucchange)})

# 1. Suavizado de la serie (igual al script original) --------------------------
suavizar_serie_temporal <- function(datos, metodo = "loess", span = 0.3) {
  datos_limpios <- datos[complete.cases(datos$TASA), ]
  años_limpios <- datos_limpios$AÑO
  tasas_limpios <- datos_limpios$TASA
  if (metodo == "loess") {
    modelo_loess <- loess(log(tasas_limpios) ~ años_limpios, span = span)
    tasas_suavizadas <- exp(predict(modelo_loess))
  } else if (metodo == "spline") {
    modelo_spline <- smooth.spline(años_limpios, log(tasas_limpios))
    tasas_suavizadas <- exp(predict(modelo_spline, años_limpios)$y)
  } else if (metodo == "media_movil") {
    tasas_suavizadas <- stats::filter(tasas_limpios, filter = rep(1/3, 3), sides = 2)
    tasas_suavizadas[1] <- mean(tasas_limpios[1:min(2, length(tasas_limpios))])
    tasas_suavizadas[length(tasas_suavizadas)] <- mean(tasas_limpios[max(1, length(tasas_limpios) - 1):length(tasas_limpios)])
  }
  data.frame(AÑO = años_limpios, TASA_ORIGINAL = tasas_limpios, TASA_SUAVIZADA = as.numeric(tasas_suavizadas))
}

generar_tabla_suavizados_completa <- function(datos) {
  loess_data <- suavizar_serie_temporal(datos, "loess")
  spline_data <- suavizar_serie_temporal(datos, "spline")
  mm_data <- suavizar_serie_temporal(datos, "media_movil")
  loess_data %>% rename(TASA_LOESS = TASA_SUAVIZADA) %>%
    left_join(spline_data %>% dplyr::select(AÑO, TASA_SPLINE = TASA_SUAVIZADA), by = "AÑO") %>%
    left_join(mm_data %>% dplyr::select(AÑO, TASA_MEDIA_MOVIL = TASA_SUAVIZADA), by = "AÑO")
}

# 2. VAP de un período: regresión log-lineal (igual al script original) --------
analisis_periodo_especifico_mejorada <- function(datos, año_inicio, año_fin) {
  periodo_data <- datos %>% filter(AÑO >= año_inicio & AÑO <= año_fin) %>% arrange(AÑO) %>%
    filter(complete.cases(TASA) & TASA > 0)
  años_periodo <- periodo_data$AÑO; tasas_periodo <- periodo_data$TASA
  vacio <- function(t) list(periodo = paste(año_inicio, "-", año_fin), vpa = NA, vpa_li = NA, vpa_ls = NA,
                            p_value = NA, tendencia = t, n = length(tasas_periodo), inicio_real = año_inicio, fin_real = año_fin)
  if (length(tasas_periodo) < 3) return(vacio("DATOS_INSUFICIENTES"))
  modelo <- lm(log(tasas_periodo) ~ años_periodo)
  n <- length(tasas_periodo)
  tcritico <- qt(0.975, n - 2)
  cf <- summary(modelo)$coefficients
  beta <- c(cf[2, 1], cf[2, 1] - tcritico * cf[2, 2], cf[2, 1] + tcritico * cf[2, 2])
  vpa <- round(((-1 + exp(beta)) * 100), 4)
  p_value <- glance(modelo)$p.value
  tendencia <- if (p_value < 0.05) ifelse(vpa[1] > 0, "AUMENTO", "DESCENSO") else "ESTABLE"
  list(periodo = paste(año_inicio, "-", año_fin), vpa = vpa[1], vpa_li = vpa[2], vpa_ls = vpa[3],
       p_value = p_value, tendencia = tendencia, n = n, inicio_real = año_inicio, fin_real = año_fin)
}

# 3. Identificación de puntos de quiebre ---------------------------------------
# Como en analizar_breakpoints() del script: strucchange::breakpoints, número de
# quiebres por BIC, recorriendo la serie desde el último punto hacia el inicial.
# Se aplica sobre log(TASA) (coherente con la VAP log-lineal). h = segmento mínimo.
identificar_quiebres <- function(datos, h = 5) {
  d <- datos %>% filter(!is.na(TASA)) %>% arrange(desc(AÑO))      # desde el último punto
  y <- log(d$TASA); x <- d$AÑO
  bp <- breakpoints(y ~ x, h = h)
  bpb <- breakpoints(bp)$breakpoints
  if (all(is.na(bpb))) return(numeric(0))
  # en la serie invertida, el quiebre en la posición k separa x[k] de x[k+1]: el nuevo período comienza en x[k]
  sort(x[bpb])
}

periodos_desde_quiebres <- function(datos, quiebres) {
  lim <- sort(unique(c(min(datos$AÑO), quiebres, max(datos$AÑO))))
  lapply(seq_len(length(lim) - 1), function(i) analisis_periodo_especifico_mejorada(datos, lim[i], lim[i + 1]))
}

# 4. Transiciones entre períodos consecutivos (igual al script, sin impresión) --
analizar_transiciones_periodos <- function(periodos) {
  if (length(periodos) < 2) return(list(total_transiciones = 0, transiciones_significativas = 0, detalle = list()))
  sig <- 0; det <- list()
  for (i in 2:length(periodos)) {
    a <- periodos[[i - 1]]; b <- periodos[[i]]
    cs <- !is.na(a$p_value) && !is.na(b$p_value) && a$p_value < 0.05 && b$p_value < 0.05 && a$tendencia != b$tendencia
    if (cs) sig <- sig + 1
    det[[i - 1]] <- list(transicion = paste(a$periodo, "→", b$periodo), cambio_tendencia = a$tendencia != b$tendencia,
                         significativo = cs, año_transicion = b$inicio_real)
  }
  list(total_transiciones = length(periodos) - 1, transiciones_significativas = sig, detalle = det)
}

# 5. Análisis completo para una serie (períodos -> transiciones -> quiebres) ----
analisis_completo <- function(datos, h = 5, quiebres = NULL) {
  if (is.null(quiebres)) quiebres <- identificar_quiebres(datos, h)
  periodos <- periodos_desde_quiebres(datos, quiebres)
  no_sig <- which(sapply(periodos, function(p) is.na(p$p_value) || p$p_value >= 0.05))
  trans <- analizar_transiciones_periodos(periodos)
  # Punto de quiebre "real": cambio de tendencia o de pendiente con al menos un período significativo
  pq <- c()
  if (length(periodos) > 1) for (i in 2:length(periodos)) {
    a <- periodos[[i - 1]]; b <- periodos[[i]]
    if ((!is.na(a$p_value) && a$p_value < 0.05) || (!is.na(b$p_value) && b$p_value < 0.05)) pq <- c(pq, b$inicio_real)
  }
  list(periodos = periodos, periodos_no_significativos = no_sig, transiciones = trans, puntos_quiebre = pq, quiebres_detectados = quiebres)
}

# 6. Aplicar a todos los métodos y seleccionar el mejor (puntaje del script) ----
# Como en el script original, los MISMOS períodos se analizan con cada método
# (serie original y suavizadas); los quiebres se identifican sobre la serie original.
analizar_todos_metodos <- function(tabla_completa, h = 5) {
  orig <- data.frame(AÑO = tabla_completa$AÑO, TASA = tabla_completa$TASA_ORIGINAL)
  quiebres <- identificar_quiebres(orig, h)
  res <- list()
  for (m in c("original", "loess", "spline", "media_movil")) {
    dm <- data.frame(AÑO = tabla_completa$AÑO, TASA = tabla_completa[[paste0("TASA_", toupper(m))]])
    res[[m]] <- analisis_completo(dm, quiebres = quiebres)
  }
  res
}

seleccionar_mejor_metodo <- function(res) {
  scores <- sapply(res, function(r) {
    s <- (length(r$periodos) - length(r$periodos_no_significativos)) * 10 + length(r$puntos_quiebre) * 5
    pa <- r$periodos[[length(r$periodos)]]
    if (!is.na(pa$p_value) && pa$p_value < 0.05) s <- s + 20
    s })
  list(mejor = names(which.max(scores)), scores = scores)
}

tabla_periodos <- function(r, serie = "", metodo = "") {
  do.call(rbind, lapply(r$periodos, function(p) data.frame(serie = serie, metodo = metodo, desde = p$inicio_real, hasta = p$fin_real,
    n = p$n, VAP = p$vpa, LI = p$vpa_li, LS = p$vpa_ls, p = p$p_value, tendencia = p$tendencia)))
}

# Flujo completo para una serie: tabla de suavizados, análisis por método, elección
tendencia_serie <- function(datos, nombre, h = 5) {
  tc <- generar_tabla_suavizados_completa(datos)
  res <- analizar_todos_metodos(tc, h = h)
  sel <- seleccionar_mejor_metodo(res)
  tabla <- do.call(rbind, lapply(names(res), function(m) tabla_periodos(res[[m]], nombre, m)))
  tabla$elegido <- tabla$metodo == sel$mejor; rownames(tabla) <- NULL
  list(tabla_suavizados = tc, resultados = res, mejor = sel$mejor, scores = sel$scores, tabla = tabla)
}
