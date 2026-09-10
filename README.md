# Caracterización A.H. Bocanegra, distrito de Callao

Proyecto de caracterización a nivel de manzana del Asentamiento Humano Bocanegra,
distrito de Callao, a partir de datos del Censo Nacional 2017 (INEI, vía REDATAM)
y cartografía censal. Este documento describe las fuentes de datos, el flujo de
procesamiento y las decisiones metodológicas tomadas a lo largo del proyecto.
El proyecto se está desarrollando tanto en R como QGIS. 

## 1. Fuentes de datos

**REDATAM (INEI — Censos Nacionales 2017).** Se extrajeron tablas de frecuencia a
nivel de manzana para el distrito de Callao, para 6 variables: sexo, tipo de
vivienda, condición de ocupación de la vivienda, tenencia de la vivienda, número
de habitaciones y personas por hogar. Descargadas como archivos Excel
individuales (`datos/crudos/`).

**Cartografía de manzanas - QGIS (GEO GPS PERÚ, producto "Manzanas_Poblacion").** Shapefile
con los polígonos de manzana del distrito de Callao y su población asociada
(campo `T_TOTAL`), derivado de la misma cartografía censal del INEI. Provee el
campo `LLAVE_MZS`, que es la llave que se usó para unir la cartografía con los
datos de REDATAM. **Importante:** este producto solo incluye manzanas con
población registrada (`T_TOTAL >= 1`); manzanas sin población (parques, zonas
industriales, terrenos baldíos) no tienen polígono en esta capa.

**Zonas de Bocanegra (`ZONAS.shp`).** Shapefile propio, previo a este proyecto,
con la división informal del A.H. Bocanegra en 5 zonas (campo `nom_local`: "ZONA
1" a "ZONA 5"). Se usó como referencia espacial para delimitar el área de estudio
dentro del distrito de Callao y para la desagregación por zona.

## 2. Flujo de procesamiento (scripts en `scripts/`)

### 2.1 `01_importar_datos.R` — Importación y consolidación

Lee los 6 Excel de REDATAM (hoja "Output"), consultamos la estructura de bloques
(`AREA # <código>`, categorías, casos, porcentajes) y generamos:

- Una tabla larga (tidy): una fila por manzana × categoría (`datos_largos_manzanas.csv`).
- Una tabla ancha, una fila por manzana, lista para unir con la cartografía
  (`resumen_manzanas_callao.csv`).

**Decisión metodológica clave — llave de unión.** El código que aparece junto al
marcador `AREA #` en el export de REDATAM **no** coincide con `LLAVE_MZS` de la
cartografía (tiene 2 ceros de más al final). El código que sí coincide es el que
aparece antes de la primera coma en la descripción de la manzana, dentro de la
misma fila. Verificado contra la cartografía real: **97.5% de coincidencia exacta**
para el distrito de Callao (2,620 de 2,686 manzanas); la mayoría de las no
coincidencias corresponden al código genérico "8888" que usa REDATAM para
viviendas dispersas sin manzana asignada.

**Promedios de habitaciones y personas por hogar.** Se calculan como promedio
ponderado por manzana (usando el punto medio de cada categoría censal) y se
redondean a números enteros.

**Indicadores derivados por manzana:** `poblacion_total`, `pct_hombres`,
`pct_alquiler` (viviendas alquiladas ÷ viviendas con dato de tenencia, de esa
misma manzana), y `hacinamiento_proxy` (personas por hogar promedio ÷ habitaciones
promedio de la manzana).

### 2.2 `02_Acondicionar.R` — Diagnóstico de valores faltantes y base final

Diagnostica valores `NA` en `resumen_manzanas_bocanegra.csv` (215 manzanas × 38
columnas) y genera un reporte (`outputs/reporte_NA_bocanegra.csv/.txt`).

**Resultado:** 213 manzanas (99.1%) completas; 2 manzanas (0.9%) sin ningún dato
de REDATAM en ninguna de las 36 columnas derivadas — es decir, no es un patrón de
NA disperso por variable, sino de manzanas que nunca tuvieron un bloque de
resultados en REDATAM.

**Investigación de la causa (ver sección 3).** Se decidió **eliminar estas 2
manzanas** de la base para el análisis cuantitativo (no se pueden imputar sin
inventar datos), conservando su geometría en los mapas con la categoría "Sin
dato". El resultado es la base limpia final:
`datos/procesados/base_acondicionada_final.csv` (213 manzanas, 0 valores NA).

### 2.3 `03_Explorar.R` — Exploración descriptiva

A partir de `base_acondicionada_final.csv`, genera tablas y gráficos (guardados
en `outputs/outputs_exploracion_inicial/`) para: población y sexo, habitaciones
promedio, personas por hogar promedio, hacinamiento, tipo de vivienda y tenencia
de la vivienda.

A diferencia de un análisis de encuesta por muestreo (que usaría diseño muestral
con factor de expansión, conglomerado y estrato), esta base proviene del Censo
(universo completo de manzanas, no una muestra), por lo que los conteos se suman
directamente. Los "promedios de promedios" (habitaciones, personas por hogar) se
ponderan por el número de viviendas/hogares con dato en cada manzana, no por un
factor muestral.

## 3. Aclaraciones metodológicas importantes

### 3.1 Las 2 manzanas sin datos de REDATAM

Códigos `070101000102500047` (9 habitantes según GEO GPS PERÚ) y
`070101000102600016` (4 habitantes según GEO GPS PERÚ). Se verificó que:

- Ambos códigos **no aparecen en ninguno** de los 6 archivos crudos de REDATAM
  (ni como bloque con datos, ni como "Tabla vacía").
- En la numeración de manzanas de su misma zona censal, ambos códigos son un
  **gap exacto de una unidad** entre manzanas vecinas que sí tienen datos (existen
  ...046 y ...048, pero no ...047; existen ...015 y ...017, pero no ...016).
- Ambas manzanas tienen una población muy pequeña según la cartografía de
  referencia (4 y 9 habitantes).

**Conclusión:** Observamos que no hay evidencia de que el censo no se haya aplicado en estas
manzanas (la cartografía de GEO_GPS sí les asigna población). La explicación más probable es
que REDATAM consolidó estas manzanas de muy baja población dentro de una manzana
vecina al momento de tabular (práctica común en sistemas censales para evitar
unidades estadísticamente muy pequeñas), en vez de generar un bloque de resultados
independiente para cada una. No se puede confirmar la razón administrativa exacta
sin consultar directamente al INEI. Por este motivo se excluyeron del análisis
cuantitativo (no se pueden imputar) y se mantienen en los mapas como "Sin dato".

### 3.3 Indicador de hacinamiento (proxy) [en evaluación si realmente usarlo]

`hacinamiento_proxy = personas por hogar promedio / habitaciones promedio` usa el
total de habitaciones de la vivienda (incluye sala, cocina, etc.), no solo
dormitorios. El indicador oficial de hacinamiento del INEI divide entre número de
dormitorios, que normalmente es menor al total de habitaciones. Este proxy,
entonces, probablemente **subestima** el hacinamiento real frente al indicador
oficial, y debe interpretarse como una aproximación, no como el indicador censal
oficial.

## 4. Resultados descriptivos (213 manzanas, base acondicionada final)
```text
| Indicador | Valor |
|---|---|
| Población total | 43,708 habitantes |
| % hombres / % mujeres | 48.6% / 51.4% |
| Habitaciones promedio (ponderado) | 3.1 |
| Personas por hogar promedio (ponderado) | 3.83 |
| Hacinamiento (proxy, promedio) | 1.24 personas/habitación |
| Tipo de vivienda predominante | Casa independiente (86.7%) |
| Tenencia — vivienda propia con título | 55.3% |
| Tenencia — vivienda alquilada | 27.2% |
```

## 5. Uso posterior de esta base

La base de datos actualizada (Sin NAs) generada en el script 02_Acondicionar será el insumo de entrada para los mapas temáticos elaborados en QGIS
(por ejemplo, % de alquiler por manzana, diferenciado por las 5 zonas de
Bocanegra). El trabajo de mapeo y sus decisiones de diseño se documentan en un
repositorio aparte, dedicado al proyecto de QGIS.

## 6. Estructura del repositorio
├── datos/
│ ├── crudos/ Exports originales de REDATAM (.xlsx)
│ └── procesados/ Tablas intermedias y base_acondicionada_final.csv
├── scripts/
│ ├── 01_importar_datos.R
│ ├── Acondicionar.R
│ └── 03_Explorar.R
├── outputs/
│ ├── reporte_NA_bocanegra.csv / .txt
│ └── outputs_exploracion_inicial/ Tablas y gráficos exportados
└── docs/

## 7. Fuentes
- Instituto Nacional de Estadística e Informática (INEI) — Censos Nacionales
  2017: XII de Población, VII de Vivienda y III de Comunidades Indígenas, vía
  REDATAM.
- GEO GPS PERÚ — cartografía de manzanas con población ("Manzanas_Poblacion"),
  distrito de Callao.
