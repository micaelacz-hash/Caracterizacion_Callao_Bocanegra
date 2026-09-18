# ==============================================================================
# 05_LimaMetropolitana.R
# Caracterizacion Callao - Bocanegra
#
# Objetivo: incorporar el reporte de REDATAM a nivel de DISTRITO para la
# Provincia de Lima (43 distritos, sin Callao) y calcular su % de alquiler
# para compararlo con Bocanegra, el distrito Callao y la provincia de Callao.
#
# Fuente: datos/crudos/tenencia_vivienda_provincia_lima.xlsx (REDATAM, INEI
# Censos 2017), hoja "Output". Area Geografica del reporte: "Provincia de
# Lima" (43 distritos + bloque RESUMEN con el total de la provincia).
#
# IMPORTANTE -- Lima Metropolitana (Provincia de Lima) y la provincia de
# Callao son ambitos SEPARADOS, cada uno con su propio numerador/denominador.
# Este script NO los suma en un solo total: los deja como categorias
# independientes para poder verlas comparadas, una junto a otra, en la misma
# tabla y grafico.
# ==============================================================================

library(readxl)
library(tidyverse)
library(flextable)
library(scales)
library(officer)
library(webshot2)

dir_crudos     <- file.path("datos", "crudos")
dir_procesados <- file.path("datos", "procesados")
ruta_salida    <- file.path("outputs", "outputs_exploracion_inicial")

archivo_entrada <- file.path(dir_crudos, "tenencia_vivienda_provincia_lima.xlsx")

# ------------------------------------------------------------------------------
# 1. Parsear el reporte por distrito (Provincia de Lima) ----------------------
# ------------------------------------------------------------------------------
hoja <- read_excel(archivo_entrada, sheet = "Output", col_names = FALSE, .name_repair = "minimal")
m <- as.matrix(hoja)
n <- nrow(m)

resultados <- vector("list", 500)
k <- 0
i <- 1

while (i <= n) {
  celda_b <- m[i, 2]

  if (!is.na(celda_b) && grepl("^AREA #\\s*[0-9]+$|^RESUMEN$", trimws(celda_b))) {

    if (trimws(celda_b) == "RESUMEN") {
      distrito <- "Provincia de Lima (total)"
    } else {
      # celda C trae "Lima, Lima, distrito: <nombre>"
      distrito <- trimws(sub(".*distrito:\\s*", "", m[i, 3]))
    }

    idx <- i + 2  # fila en blanco en i+1, cabecera "Casos" en i+2
    if (idx > n || is.na(m[idx, 3]) || trimws(m[idx, 3]) != "Casos") {
      i <- i + 1
      next
    }

    j <- idx + 1
    while (j <= n) {
      rb <- m[j, 2]
      if (is.na(rb)) { j <- j + 1; break }
      if (trimws(rb) == "Total") { j <- j + 1; break }

      resultados[[k <- k + 1]] <- data.frame(
        distrito  = distrito, categoria = trimws(rb),
        casos     = as.numeric(m[j, 3]), stringsAsFactors = FALSE
      )
      j <- j + 1
    }
    i <- j

  } else {
    i <- i + 1
  }
}

tenencia_provincia_lima_larga <- do.call(rbind, resultados[seq_len(k)])

write.csv(tenencia_provincia_lima_larga, file.path(dir_procesados, "tenencia_provincia_lima.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("Distritos leidos (Provincia de Lima):",
    length(unique(tenencia_provincia_lima_larga$distrito)) - 1, "+ RESUMEN\n")

# ------------------------------------------------------------------------------
# 2. % de alquiler por distrito y total de la Provincia de Lima ---------------
# ------------------------------------------------------------------------------
pct_alquiler_provincia_lima <- tenencia_provincia_lima_larga %>%
  group_by(distrito) %>%
  mutate(viviendas_con_tenencia = sum(casos)) %>%
  ungroup() %>%
  filter(categoria == "Alquilada") %>%
  transmute(distrito,
            viviendas_alquiladas   = casos,
            viviendas_con_tenencia = viviendas_con_tenencia,
            pct_alquiler           = round(100 * viviendas_alquiladas / viviendas_con_tenencia, 1))

write.csv(pct_alquiler_provincia_lima, file.path(dir_procesados, "pct_alquiler_provincia_lima.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\n% de alquiler por distrito (Provincia de Lima):\n")
print(pct_alquiler_provincia_lima, n = Inf)

lima_total <- pct_alquiler_provincia_lima %>% filter(distrito == "Provincia de Lima (total)")
viviendas_alquiladas_lima   <- lima_total$viviendas_alquiladas
viviendas_con_tenencia_lima <- lima_total$viviendas_con_tenencia
pct_alquiler_lima <- lima_total$pct_alquiler

cat("\n% de alquiler, Provincia de Lima (Lima Metropolitana, sin Callao):", pct_alquiler_lima, "%\n")

# ------------------------------------------------------------------------------
# 3. Bocanegra, distrito Callao y provincia Callao (recalculados) -------------
# ------------------------------------------------------------------------------
base_limpia <- read.csv(file.path(dir_procesados, "base_acondicionada_final.csv"),
                         encoding = "UTF-8", stringsAsFactors = FALSE)
base_callao <- read.csv(file.path(dir_procesados, "resumen_manzanas_callao.csv"),
                         encoding = "UTF-8", stringsAsFactors = FALSE)
callao_total <- read.csv(file.path(dir_procesados, "pct_alquiler_distritos_callao.csv"),
                          encoding = "UTF-8", stringsAsFactors = FALSE) %>%
  filter(distrito == "Provincia Callao (total)")

viviendas_alquiladas_bocanegra   <- sum(base_limpia$tenencia_alquilada)
viviendas_con_tenencia_bocanegra <- sum(base_limpia$viviendas_con_tenencia)

viviendas_alquiladas_distrito   <- sum(base_callao$tenencia_alquilada, na.rm = TRUE)
viviendas_con_tenencia_distrito <- sum(base_callao$viviendas_con_tenencia, na.rm = TRUE)

# ==============================================================================
# 4. TABLA Y GRAFICO -- 4 ambitos SEPARADOS, uno junto al otro (sin sumarlos) --
# ==============================================================================
formato_flextable <- function(tabla, titulo) {
  flextable(tabla) %>%
    add_header_lines(values = titulo) %>%
    add_footer_lines(values = "Fuente: INEI - Censos Nacionales 2017 (REDATAM).") %>%
    autofit() %>%
    theme_vanilla() %>%
    border_inner_h(part = "body", border = officer::fp_border(width = 0)) %>%
    align(align = "center", part = "all") %>%
    align(j = 1, align = "left", part = "body") %>%
    bold(part = "header") %>%
    align(align = "left", part = "footer") %>%
    fontsize(size = 9, part = "footer") %>%
    hline_bottom(part = "body", border = officer::fp_border(width = 1)) %>%
    hline_bottom(part = "footer", border = officer::fp_border(width = 0))
}

# Cada fila es un ambito independiente: NO se suman viviendas entre ambitos,
# cada % se calcula solo con el numerador/denominador de su propio ambito.
tabla_comparacion_lima <- tibble(
  Ambito = c("A.H. Bocanegra", "Distrito de Callao", "Provincia de Callao", "Provincia de Lima (Lima Metropolitana)"),
  `Viviendas alquiladas (N)` = c(viviendas_alquiladas_bocanegra, viviendas_alquiladas_distrito,
                                  callao_total$viviendas_alquiladas, viviendas_alquiladas_lima),
  `Viviendas con dato de tenencia (N)` = c(viviendas_con_tenencia_bocanegra, viviendas_con_tenencia_distrito,
                                            callao_total$viviendas_con_tenencia, viviendas_con_tenencia_lima)
) %>%
  mutate(`% en alquiler` = paste0(round(100 * `Viviendas alquiladas (N)` /
                                           `Viviendas con dato de tenencia (N)`, 1), "%"),
         `Viviendas alquiladas (N)` = scales::comma(`Viviendas alquiladas (N)`),
         `Viviendas con dato de tenencia (N)` = scales::comma(`Viviendas con dato de tenencia (N)`))

ft_comparacion_lima <- formato_flextable(tabla_comparacion_lima,
    "Tabla 10. % de viviendas en alquiler por ambito (cifras independientes, no acumuladas), Censo 2017") %>%
  add_footer_lines(values = "Nota: cada ambito es independiente -- Provincia de Callao y Provincia de Lima NO se suman entre si. Provincia de Lima = 43 distritos (sin Callao). % calculado como suma de viviendas alquiladas / suma de viviendas con dato de tenencia, dentro de cada ambito.") %>%
  align(align = "justify", part = "footer") %>%
  fontsize(size = 8, part = "footer")
print(ft_comparacion_lima)

tema_graficos <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    plot.caption = element_text(hjust = 0, size = 8, color = "grey50")
  )

fuente_caption <- "Fuente: INEI - Censos Nacionales 2017 (REDATAM). Ambitos independientes (no acumulados)."

plot_comparacion_lima <- ggplot(tabla_comparacion_lima %>%
                                   mutate(pct_num = parse_number(`% en alquiler`)),
                                 aes(x = factor(Ambito, levels = Ambito), y = pct_num, fill = Ambito)) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_text(aes(label = `% en alquiler`), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("A.H. Bocanegra" = "#D73027", "Distrito de Callao" = "#8C96A8",
                                "Provincia de Callao" = "#4A7C59",
                                "Provincia de Lima (Lima Metropolitana)" = "#2E5B88")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Grafico 11. % de viviendas en alquiler por ambito geografico",
       subtitle = "Cifras independientes -- Callao y Lima NO estan sumadas entre si",
       x = "", y = "% de viviendas en alquiler", caption = fuente_caption) +
  tema_graficos + theme(legend.position = "none", axis.text.x = element_text(angle = 15, hjust = 1))
print(plot_comparacion_lima)

# ==============================================================================
# 5. EXPORTACION
# ==============================================================================
if (!dir.exists(ruta_salida)) dir.create(ruta_salida, recursive = TRUE)

save_as_image(ft_comparacion_lima, path = file.path(ruta_salida, "Tabla10_ComparacionAlquilerPorAmbito.png"))
ggsave(file.path(ruta_salida, "Grafico11_ComparacionAlquilerPorAmbito.jpg"),
       plot = plot_comparacion_lima, width = 8, height = 5, bg = "white")

cat("\nListo.\n")
cat("- Tabla larga (tidy, Provincia de Lima):", file.path(dir_procesados, "tenencia_provincia_lima.csv"), "\n")
cat("- % de alquiler por distrito (Provincia de Lima):", file.path(dir_procesados, "pct_alquiler_provincia_lima.csv"), "\n")
cat("- Tabla y grafico exportados a:", ruta_salida, "\n")
