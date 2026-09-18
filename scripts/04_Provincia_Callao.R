# ==============================================================================
# 04_Provincia_Callao.R
# Caracterizacion Callao - Bocanegra
#
# Objetivo: incorporar un segundo reporte de REDATAM, esta vez a nivel de
# DISTRITO (no de manzana), con la tenencia de la vivienda para los 7
# distritos de la Provincia Constitucional del Callao, y usarlo para comparar
# el % de alquiler de Bocanegra y del distrito Callao contra el total
# provincial.
#
# Fuente: datos/crudos/tenencia_vivienda_distritos_callao.xlsx (REDATAM, INEI
# Censos 2017), hoja "Output". Estructura distinta a los exports por manzana:
# un bloque "V: Tenencia de la vivienda" por cada uno de los 7 distritos
# (marcado "AREA # <codigo>") + un bloque final "RESUMEN" con el total de la
# provincia.
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

archivo_entrada <- file.path(dir_crudos, "tenencia_vivienda_distritos_callao.xlsx")

# ------------------------------------------------------------------------------
# 1. Parsear el reporte por distrito -------------------------------------------
# ------------------------------------------------------------------------------
hoja <- read_excel(archivo_entrada, sheet = "Output", col_names = FALSE, .name_repair = "minimal")
m <- as.matrix(hoja)
n <- nrow(m)

resultados <- vector("list", 200)
k <- 0
i <- 1

while (i <= n) {
  celda_b <- m[i, 2]

  if (!is.na(celda_b) && grepl("^AREA #\\s*[0-9]+$|^RESUMEN$", trimws(celda_b))) {

    if (trimws(celda_b) == "RESUMEN") {
      distrito <- "Provincia Callao (total)"
    } else {
      # celda C trae "Prov. Constitucional del Callao, distrito: <nombre>"
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
        distrito  = distrito,
        categoria = trimws(rb),
        casos     = as.numeric(m[j, 3]),
        stringsAsFactors = FALSE
      )
      j <- j + 1
    }
    i <- j

  } else {
    i <- i + 1
  }
}

tenencia_distritos_larga <- do.call(rbind, resultados[seq_len(k)])

write.csv(tenencia_distritos_larga, file.path(dir_procesados, "tenencia_distritos_callao.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("Distritos leidos:", paste(unique(tenencia_distritos_larga$distrito), collapse = ", "), "\n")

# ------------------------------------------------------------------------------
# 2. Tabla ancha: % de alquiler por distrito (y total provincia) --------------
# ------------------------------------------------------------------------------
pct_alquiler_distritos <- tenencia_distritos_larga %>%
  group_by(distrito) %>%
  mutate(viviendas_con_tenencia = sum(casos)) %>%
  ungroup() %>%
  filter(categoria == "Alquilada") %>%
  transmute(distrito,
            viviendas_alquiladas   = casos,
            viviendas_con_tenencia = viviendas_con_tenencia,
            pct_alquiler           = round(100 * viviendas_alquiladas / viviendas_con_tenencia, 1))

write.csv(pct_alquiler_distritos, file.path(dir_procesados, "pct_alquiler_distritos_callao.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\n% de alquiler por distrito (Provincia Constitucional del Callao):\n")
print(pct_alquiler_distritos, n = Inf)

pct_alquiler_provincia <- pct_alquiler_distritos$pct_alquiler[
  pct_alquiler_distritos$distrito == "Provincia Callao (total)"
]
viviendas_alquiladas_provincia   <- pct_alquiler_distritos$viviendas_alquiladas[pct_alquiler_distritos$distrito == "Provincia Callao (total)"]
viviendas_con_tenencia_provincia <- pct_alquiler_distritos$viviendas_con_tenencia[pct_alquiler_distritos$distrito == "Provincia Callao (total)"]
cat("\n% de alquiler (Provincia Callao, total):", pct_alquiler_provincia, "%\n")

# ------------------------------------------------------------------------------
# 3. Bocanegra y distrito Callao (recalculados desde la base ya acondicionada) -
# ------------------------------------------------------------------------------
base_limpia <- read.csv(file.path(dir_procesados, "base_acondicionada_final.csv"),
                         encoding = "UTF-8", stringsAsFactors = FALSE)
base_callao <- read.csv(file.path(dir_procesados, "resumen_manzanas_callao.csv"),
                         encoding = "UTF-8", stringsAsFactors = FALSE)

viviendas_alquiladas_bocanegra   <- sum(base_limpia$tenencia_alquilada)
viviendas_con_tenencia_bocanegra <- sum(base_limpia$viviendas_con_tenencia)

viviendas_alquiladas_distrito   <- sum(base_callao$tenencia_alquilada, na.rm = TRUE)
viviendas_con_tenencia_distrito <- sum(base_callao$viviendas_con_tenencia, na.rm = TRUE)

# ==============================================================================
# 4. TABLAS DESCRIPTIVAS
# ==============================================================================
formato_flextable <- function(tabla, titulo) {
  flextable(tabla) %>%
    add_header_lines(values = titulo) %>%
    add_footer_lines(values = "Fuente: INEI - Censos Nacionales 2017 (REDATAM). Provincia Constitucional del Callao.") %>%
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

# ------------------------------------------------------------------------------
# 4.1 Tabla 8: % de alquiler -- Bocanegra vs. distrito Callao vs. provincia ----
# ------------------------------------------------------------------------------
tabla_comparacion_provincia <- tibble(
  Ambito = c("A.H. Bocanegra", "Distrito de Callao", "Provincia de Callao"),
  `Viviendas alquiladas (N)` = c(viviendas_alquiladas_bocanegra, viviendas_alquiladas_distrito, viviendas_alquiladas_provincia),
  `Viviendas con dato de tenencia (N)` = c(viviendas_con_tenencia_bocanegra, viviendas_con_tenencia_distrito, viviendas_con_tenencia_provincia)
) %>%
  mutate(`% en alquiler` = paste0(round(100 * `Viviendas alquiladas (N)` /
                                           `Viviendas con dato de tenencia (N)`, 1), "%"),
         `Viviendas alquiladas (N)` = scales::comma(`Viviendas alquiladas (N)`),
         `Viviendas con dato de tenencia (N)` = scales::comma(`Viviendas con dato de tenencia (N)`))

ft_comparacion_provincia <- formato_flextable(tabla_comparacion_provincia,
    "Tabla 8. % de viviendas en alquiler: Bocanegra, distrito y provincia de Callao, Censo 2017") %>%
  add_footer_lines(values = "Nota: provincia de Callao = 7 distritos (Bellavista, Callao, Carmen de la Legua Reynoso, La Perla, La Punta, Mi Peru, Ventanilla). % calculado como suma de viviendas alquiladas / suma de viviendas con dato de tenencia.") %>%
  align(align = "justify", part = "footer") %>%
  fontsize(size = 8, part = "footer")
print(ft_comparacion_provincia)

# ------------------------------------------------------------------------------
# 4.2 Tabla 9: % de alquiler por distrito (los 7 de la provincia) -------------
# ------------------------------------------------------------------------------
tabla_distritos <- pct_alquiler_distritos %>%
  filter(distrito != "Provincia Callao (total)") %>%
  arrange(desc(pct_alquiler)) %>%
  transmute(Distrito = distrito,
            `Viviendas alquiladas (N)` = scales::comma(viviendas_alquiladas),
            `Viviendas con dato de tenencia (N)` = scales::comma(viviendas_con_tenencia),
            `% en alquiler` = paste0(pct_alquiler, "%"))

ft_distritos <- formato_flextable(tabla_distritos,
    "Tabla 9. % de viviendas en alquiler por distrito, Provincia de Callao, Censo 2017") %>%
  add_footer_lines(values = "Nota: distritos de la Provincia Constitucional del Callao, ordenados de mayor a menor % de alquiler.") %>%
  align(align = "justify", part = "footer") %>%
  fontsize(size = 8, part = "footer")
print(ft_distritos)

# ==============================================================================
# 5. GRAFICOS
# ==============================================================================
tema_graficos <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    plot.caption = element_text(hjust = 0, size = 8, color = "grey50")
  )

fuente_caption <- "Fuente: INEI - Censos Nacionales 2017 (REDATAM). Provincia Constitucional del Callao."

# 5.1 Grafico 9: Bocanegra vs. distrito vs. provincia
plot_comparacion_provincia <- ggplot(tabla_comparacion_provincia %>%
                                        mutate(pct_num = parse_number(`% en alquiler`)),
                                      aes(x = factor(Ambito, levels = Ambito), y = pct_num, fill = Ambito)) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_text(aes(label = `% en alquiler`), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("A.H. Bocanegra" = "#D73027", "Distrito de Callao" = "#8C96A8",
                                "Provincia de Callao" = "#4A7C59")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Grafico 9. % de viviendas en alquiler: Bocanegra, distrito y provincia de Callao",
       x = "", y = "% de viviendas en alquiler", caption = fuente_caption) +
  tema_graficos + theme(legend.position = "none")
print(plot_comparacion_provincia)

# 5.2 Grafico 10: % de alquiler por distrito (resaltando Callao)
plot_distritos <- ggplot(tabla_distritos %>%
                            mutate(pct_num = parse_number(`% en alquiler`),
                                   es_callao = Distrito == "Callao"),
                          aes(x = reorder(Distrito, pct_num), y = pct_num, fill = es_callao)) +
  geom_col(alpha = 0.9) +
  geom_text(aes(label = `% en alquiler`), hjust = -0.15, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#D73027", "FALSE" = "#8C96A8")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Grafico 10. % de viviendas en alquiler por distrito (Provincia de Callao)",
       x = "", y = "% de viviendas en alquiler", caption = fuente_caption) +
  tema_graficos + theme(legend.position = "none")
print(plot_distritos)

# ==============================================================================
# 6. EXPORTACION
# ==============================================================================
if (!dir.exists(ruta_salida)) dir.create(ruta_salida, recursive = TRUE)

save_as_image(ft_comparacion_provincia, path = file.path(ruta_salida, "Tabla8_ComparacionAlquilerProvincia.png"))
save_as_image(ft_distritos,             path = file.path(ruta_salida, "Tabla9_AlquilerPorDistrito.png"))

ggsave(file.path(ruta_salida, "Grafico9_ComparacionAlquilerProvincia.jpg"), plot = plot_comparacion_provincia, width = 8, height = 5, bg = "white")
ggsave(file.path(ruta_salida, "Grafico10_AlquilerPorDistrito.jpg"),         plot = plot_distritos,             width = 8, height = 5, bg = "white")

cat("\nListo.\n")
cat("- Tabla larga (tidy):", file.path(dir_procesados, "tenencia_distritos_callao.csv"), "\n")
cat("- % de alquiler por distrito:", file.path(dir_procesados, "pct_alquiler_distritos_callao.csv"), "\n")
cat("- Tablas y graficos exportados a:", ruta_salida, "\n")
