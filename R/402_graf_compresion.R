library( resmush )

#Parámetro------------------------------------------------------------------------------------------
calidad <- 5

#Comprimir gráficas---------------------------------------------------------------------------------

resmush_dir(
  paste0( parametros$resultado_graficos ),
  ext = "\\.(png|jpe?g|bmp|gif|tif)$",
  suffix = "",
  overwrite = FALSE,
  progress = TRUE,
  report = TRUE,
  recursive = FALSE,
  qlty = calidad
)
