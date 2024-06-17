message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tLectura de las inversiones' )

#Cargando información financiera--------------------------------------------------------------------
file <- paste0( parametros$Data_seg, 'IESS_IVM_inversiones.xlsx' )

# Parámetros----------------------------------------------------------------------------------------
anio_max <- 2020
anio_min <- 2012
fecha_max <- as.Date( '2020/12/31' )

#Carga de recursos administrados por el BIESS-------------------------------------------------------
recurs_adm_biess <- readxl::read_excel( 
  file,
  sheet = 'recur_adm_biess',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  ) %>%
  dplyr::select( -inflacion ) %>% 
  filter( ano <= anio_max & ano >= anio_min )

inver_corte <- readxl::read_excel( 
  file,
  sheet = 'corte_20',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  )

rendimientos_netos <- readxl::read_excel( 
  file,
  sheet = 'rend_netos',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  ) %>%
  mutate( corte_a = as.Date( corte_a, "%Y-%m-%d" ) ) %>%
  filter( corte_a <= as.Date( fecha_max ) )

rendimiento_neto_hist <- readxl::read_excel( 
  file,
  sheet = 'rendimiento_neto_hist',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  ) %>%
  mutate( periodo = as.Date( periodo, "%d/%m/%Y" ) ) %>%
  filter( periodo <= as.Date( fecha_max ) )

ingresos <- readxl::read_excel( 
  file,
  sheet = 'ingresos',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  ) %>%
  dplyr::select( -x2021,-x2022 ) %>%
  mutate_if( is.numeric, replace_na, 0 )

gastos_opera <- readxl::read_excel( 
  file,
  sheet = 'egresos',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  ) %>%
  dplyr::select( -x2021,-x2022 ) %>%
  mutate_if( is.numeric, replace_na, 0 )

inv_instrumento <- readxl::read_excel( 
  file,
  sheet = 'instrumento',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  ) %>%
  filter( ano <= anio_max & ano >= anio_min )

creditos <- readxl::read_excel( 
  file,
  sheet = 'creditos',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  ) %>%
  filter( ano <= anio_max & ano >= anio_min )


detalle_bonos <- readxl::read_excel( 
  file,
  sheet = 'bonos_20',
  col_names = TRUE,
  col_types = c( "text",
                 "text",
                 "numeric",
                 "numeric",
                 "numeric",
                 "numeric",
                 "text",
                 "text" ),
  na = "",
  skip = 0
) %>% clean_names(  ) %>%
  mutate( fecha_vcmt = fecha_max + plazo_x_vencer )


recuperacion_bonos <- readxl::read_excel( 
  file,
  sheet = 'recuperacion_20',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  ) %>%
  mutate( fecha_cupon = as.Date( fecha_cupon, "%d/%m/%Y" ) ) %>%
  mutate( fecha_vcmto = as.Date( fecha_vcmto, "%d/%m/%Y" ) )

detalle_bonos_40 <- readxl::read_excel( 
  file,
  sheet = 'bonos40',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  )

detalle_obligaciones <- readxl::read_excel( 
  file,
  sheet = 'obligacion_20',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  )

detalle_titularizaciones <- readxl::read_excel( 
  file,
  sheet = 'titularizacion_20',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  )

detalle_fidecomisos <- readxl::read_excel( 
  file,
  sheet = 'fidecomisos_20',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  )

detalle_renta_variable <- readxl::read_excel( 
  file,
  sheet = 'renta_variable_20',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando inversiones en un solo data.frame' )

save( recurs_adm_biess,
      inver_corte,
      rendimientos_netos,
      rendimiento_neto_hist,
      ingresos,
      gastos_opera,
      inv_instrumento,
      creditos,
      detalle_bonos,
      recuperacion_bonos,
      detalle_bonos_40,
      detalle_obligaciones,
      detalle_titularizaciones,
      detalle_fidecomisos,
      detalle_renta_variable,
      file = paste0( parametros$RData_seg, 'BIESS_IVM_inversiones.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()