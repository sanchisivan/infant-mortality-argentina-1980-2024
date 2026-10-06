# =============================================================================
# Índice de Gini: procedimiento de la app "Calculador del Coeficiente de Gini"
# (ShinyApp/Gini_Calculator/Gini_Calculator_Project/app.R)
#  - unidades ordenadas por tasa DESCENDENTE
#  - curva de Lorenz (prop. acumulada de población vs de casos) suavizada con
#    regresión no paramétrica (sm::sm.regression, h = sm::h.select)
#  - área bajo la curva por trapecios (pracma::trapz); Gini = |(AUC - 0,5) x 2|
#  - IC95%: bootstrap (remuestreo con reemplazo de los individuos de cada unidad,
#    equivalente a CASOS* ~ Binomial(POBLACION, CASOS/POBLACION)), 100 réplicas,
#    método de los cuantiles 2,5 y 97,5
# =============================================================================
suppressPackageStartupMessages({library(sm); library(pracma)})

igini2 <- function(a, b) {               # a = casos, b = población (como en la app)
  df <- data.frame(a, b); df$tasa <- a / b
  dfo <- df[order(df$tasa, decreasing = TRUE), ]
  acasos <- cumsum(dfo$a / sum(dfo$a)); apob <- cumsum(dfo$b / sum(dfo$b))
  h1 <- h.select(apob, acasos)
  giniorig <- sm.regression(apob, acasos, h = h1, display = "none")
  AUCorig <- trapz(giniorig$eval.points, giniorig$estimate)
  abs((AUCorig - 0.5) * 2)
}

gini_app <- function(casos, poblacion, B = 100, seed = 2026) {
  set.seed(seed)
  g <- igini2(casos, poblacion)
  boot <- replicate(B, {
    cb <- rbinom(length(casos), size = poblacion, prob = casos / poblacion)
    if (all(cb == 0)) return(NA)
    igini2(pmax(cb, 0), poblacion)
  })
  q <- quantile(boot, c(0.025, 0.975), na.rm = TRUE)
  c(Gini = g, LI = unname(q[1]), LS = unname(q[2]))
}

# Índice de concentración (Kakwani): unidades ordenadas de PEOR a MEJOR condición
# social (mayor a menor NBI). IC < 0: defunciones concentradas en las jurisdicciones
# con peores condiciones. IC95% por el mismo remuestreo binomial (B réplicas).
conc_index <- function(casos, poblacion, social) {
  o <- order(-social)
  P <- c(0, cumsum(poblacion[o]) / sum(poblacion)); D <- c(0, cumsum(casos[o]) / sum(casos))
  1 - 2 * sum((P[-1] - P[-length(P)]) * (D[-1] + D[-length(D)]) / 2)
}
conc_boot <- function(casos, poblacion, social, B = 1000, seed = 2026) {
  set.seed(seed); c0 <- conc_index(casos, poblacion, social)
  bb <- replicate(B, conc_index(rbinom(length(casos), poblacion, casos / poblacion), poblacion, social))
  c(IC = c0, IC_LI = unname(quantile(bb, .025)), IC_LS = unname(quantile(bb, .975)))
}

# Índices de desigualdad de la pendiente (absoluto, SII) y relativo (RII), OPS:
# regresión sobre el rango relativo (ridit) de NBI ponderado por nacidos vivos
sii_rii <- function(casos, poblacion, social) {
  o <- order(social); cum <- cumsum(poblacion[o]) / sum(poblacion); mid <- cum - poblacion[o] / sum(poblacion) / 2
  r <- numeric(length(mid)); r[o] <- mid
  tasa <- casos / poblacion * 1000
  ml <- lm(tasa ~ r, weights = poblacion); ci <- confint(ml)
  mp <- glm(casos ~ r + offset(log(poblacion)), family = quasipoisson); cp <- confint.default(mp)
  c(SII = unname(coef(ml)[2]), SII_LI = ci[2, 1], SII_LS = ci[2, 2],
    RII = unname(exp(coef(mp)[2])), RII_LI = exp(cp[2, 1]), RII_LS = exp(cp[2, 2]))
}
