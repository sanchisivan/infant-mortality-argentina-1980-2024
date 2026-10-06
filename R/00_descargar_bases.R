# =============================================================================
# 00_descargar_bases.R — Descarga los archivos originales a data_raw/originales/
# (correr en una PC con acceso a internet; no es necesario para reproducir el
#  análisis, que parte de los agregados ya guardados en data_raw/)
# =============================================================================
B <- "https://www.argentina.gob.ar/sites/default/files/2021/03/"
def <- c(`2024` = "datos_sobre_defunciones_2024.csv", `2023` = "defweb23.csv", `2022` = "defweb22_0.csv", `2021` = "defweb21_0.csv", `2020` = "defweb20_0.csv")
nac <- c(`2024` = "datos_sobre_nacidos_vivos_2024.csv", `2023` = "nacweb23.csv", `2022` = "nacweb22_0.csv", `2021` = "nacweb21_0.csv", `2020` = "nacweb20_0.csv")
for (y in 2005:2019) { def[as.character(y)] <- sprintf("defweb%02d.csv", y %% 100); nac[as.character(y)] <- sprintf("nacweb%02d.csv", y %% 100) }
anuarios <- c(`2005`="serie5nro49.pdf",`2006`="serie5nro50.pdf",`2007`="serie5nro51.pdf",`2008`="serie5nro52.pdf",`2009`="serie5nro53.pdf",
  `2010`="serie5nro54.pdf",`2011`="serie5nro55.pdf",`2012`="serie5nro56.pdf",`2013`="serie5nro57.pdf",`2014`="serie5nro58.pdf",`2015`="serie5numero59.pdf",
  `2016`="serie5nro60.pdf",`2017`="serie5nro61.pdf",`2018`="serie5nro62.pdf",`2019`="serie5numero63.pdf",`2020`="serie5numero64_web.pdf",
  `2021`="serie_5_nro_65_anuario_vitales_2021_-_web.pdf",`2022`="serie_5_nro_66_anuario_vitales_2022_3.pdf",
  `2023`="serie_5_nro_67_anuario_vitales_2023-version_final.pdf",`2024`="serie_5_nro_68_anuario_vitales_2024_v2.pdf")
urls <- c(setNames(paste0(B, def), paste0("defunciones_", names(def), ".csv")),
          setNames(paste0(B, nac), paste0("nacidosvivos_", names(nac), ".csv")),
          setNames(paste0("https://www.argentina.gob.ar/sites/default/files/", anuarios), paste0("anuario_DEIS_", names(anuarios), ".pdf")),
          descdef1.xlsx = paste0(B, "descdef1.xlsx"), descnac.xlsx = paste0(B, "descnac.xlsx"),
          indec_serie_nbi_2022.xlsx = "https://www.indec.gob.ar/ftp/cuadros/sociedad/serie_nbi_2022.xlsx",
          censo2022_cobertura_salud_c1.xlsx = "https://censo.gob.ar/wp-content/uploads/2023/11/c2022_tp_salud_c1.xlsx")
dir.create("data_raw/originales", recursive = TRUE, showWarnings = FALSE)
options(timeout = 300)
for (n in names(urls)) { dest <- file.path("data_raw/originales", n); if (file.exists(dest)) next
  ok <- try(download.file(urls[[n]], dest, mode = "wb", quiet = TRUE), silent = TRUE); cat(if (inherits(ok, "try-error")) "ERROR" else "ok", n, "\n") }
