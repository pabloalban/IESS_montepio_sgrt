message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\t Activo Fondo' )

#Parámetros de filtrado-----------------------------------------------------------------------------

anio_max <- 2022

anio_min <- 2014

#Path del archivo-----------------------------------------------------------------------------------

file <-
  paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )

#1. Activo------------------------------------------------------------------------------------------
##Análisis Componentes Activo-----------------------------------------------------------------------
message( '\t Activo' )

activo_del_fondo <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'activo',
    skip = 0,
    col_names = T
  )   %>%
  clean_names(  ) %>%
  filter( ano <= anio_max, ano >= anio_min )

##Análisis Componentes Activo-----------------------------------------------------------------------
message( '\t Análisis Componentes Activo' )

analisis_componentes_activo <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'comp_activo',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  dplyr::select( -x2010,-x2011, -x2012, -x2013 )

##Análisis Horizontal del Activo--------------------------------------------------------------------
message( '\t Análisis Horizontal del Activo' )
analisis_horizontal_activo <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'ana_horiz_activo',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  dplyr::select( -x2011_2010,-x2012_2011, -x2013_2012 )

##Análisis Vertical del Activo----------------------------------------------------------------------
message( '\t Análisis Vertical del Activo' )
analisis_vertical_activo <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'ana_ver_activo',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  dplyr::select( -x2010,-x2011, -x2012, -x2013 )

##Cuentas por Cobrar del Fondo RT-------------------------------------------------------------------
message( '\t Cuentas por Cobrar del Activo' )
cuentas_cobrar_fondo <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'cc_activo',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  filter( ano <= anio_max, ano >= anio_min )

##Componentes de las Cuentas por Cobrar del Fondo RT------------------------------------------------
message( '\t Componentes de las Cuentas por Cobrar del Activo' )
analisis_componentes_cobrar_fondo <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'comp_cc_activo',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  dplyr::select( -x2012, -x2013 )

#2. Pasivo------------------------------------------------------------------------------------------
##Pasivos del Fondo---------------------------------------------------------------------------------
message( '\t Pasivos del Fondo' )
pasivos_fondo <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'pasivo',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  filter( ano <= anio_max, ano >= anio_min )


##Componentes del Pasivo del Fondo------------------------------------------------------------------
message( '\t Componentes del Pasivo del Fondo' )
componentes_pasivos_fondo <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'comp_pasivo',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  dplyr::select( -x2010,-x2011, -x2012, -x2013 ) %>% 
  mutate_if( is.numeric, replace_na, 0 )


##Análisis Horizontal del Pasivo--------------------------------------------------------------------
message( '\t Análisis Horizontal del Pasivo' )
analisis_horizontal_pasivos <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'ana_horiz_pasivo',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  dplyr::select( -x2011_2010,-x2012_2011, -x2013_2012 )

##Análisis Vertical del Pasivo----------------------------------------------------------------------
message( '\t Análisis Vertical del Pasivo' )
analisis_vertical_pasivos <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'ana_ver_pasivo',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  dplyr::select( -x2010,-x2011, -x2012, -x2013 ) %>%
  mutate_if( is.numeric, replace_na, 0 )

##Cuentas por Pagar del Fondo-----------------------------------------------------------------------
message( '\t Cuentas por Pagar del Fondo' )
cuentas_pagar_fondo <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'cp_pasivo',
    skip = 0,
    col_names = T
  )

#3. Patrimonio--------------------------------------------------------------------------------------
##Patrimonio del Fondo------------------------------------------------------------------------------
message( '\t Patrimonio del Fondo' )
patrimonio_fondo <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'patrimonio',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  filter( ano <= anio_max, ano >= anio_min )


##Componentes del Patrimonio------------------------------------------------------------------------
message( '\t Componentes del Patrimonio' )
componentes_patrimonio_fondo <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'comp_patrimonio',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  dplyr::select( -x2010,-x2011, -x2012, -x2013 ) %>%
  mutate_if( is.numeric, replace_na, 0 )

##Análisis Horizontal del Patrimonio----------------------------------------------------------------
message( '\t Análisis Horizontal del Patrimonio' )
analisis_horizontal_patrimonio <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'ana_horz_patrimonio',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  dplyr::select( -x2011_2010,-x2012_2011, -x2013_2012 )

##Análisis Vertical del Patrimonio------------------------------------------------------------------
message( '\t Análisis Vertical del Patrimonio' )
analisis_vertical_patrimonio <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'ana_ver_patrimonio',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  dplyr::select( -x2010,-x2011, -x2012, -x2013 ) %>%
  mutate_if( is.numeric, replace_na, 0 )

#4. Ingresos----------------------------------------------------------------------------------------
##Ingresos del Fondo--------------------------------------------------------------------------------
message( '\t Ingresos del Fondo' )
ingresos_fondo <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'ingresos',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  filter( ano <= anio_max, ano >= anio_min )

##Componentes de los Ingresos-----------------------------------------------------------------------
message( '\t Componentes de los Ingresos' )
componentes_ingresos <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'comp_ingre',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  dplyr::select( -x2010,-x2011, -x2012, -x2013 ) %>%
  mutate_if( is.numeric, replace_na, 0 )

##Análisis Horizontal de los Ingresos---------------------------------------------------------------
message( '\t Análisis Horizontal de los Ingresos' )
analisis_horizontal_ingresos <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'ana_horz_ingreso',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  dplyr::select( -x2011_2010,-x2012_2011, -x2013_2012 )

##Análisis Vertical de los Ingresos-----------------------------------------------------------------
message( '\t Análisis Vertical de los Ingresos' )
analisis_vertical_ingresos <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'ana_ver_ingreso',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  dplyr::select( -x2010,-x2011, -x2012, -x2013 ) %>%
  mutate_if( is.numeric, replace_na, 0 )

##Ingresos por Aportes------------------------------------------------------------------------------
message( '\t Ingresos por Aportes' )
ingresos_aportes <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'aportes',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  filter( ano <= anio_max, ano >= anio_min )

#5. Gastos------------------------------------------------------------------------------------------
##Gastos--------------------------------------------------------------------------------------------
message( '\t Gastos' )
gastos <- readxl::read_xlsx( 
  path = file,
  sheet = 'gasto',
  skip = 0,
  col_names = T
) %>%
  clean_names(  ) %>%
  filter( ano <= anio_max, ano >= anio_min )

##Componentes del Gastos----------------------------------------------------------------------------
message( '\t Componentes del Gastos' )
componentes_gastos <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'compo_gasto',
    skip = 0,
    col_names = T
  )  %>%
  clean_names(  ) %>%
  dplyr::select( -x2010,-x2011, -x2012, -x2013 ) %>%
  mutate_if( is.numeric, replace_na, 0 )

##Análisis Horizontal del Gastos--------------------------------------------------------------------
message( '\t Análisis Horizontal del Gastos' )
analisis_horizontal_gastos <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'ana_horiz_gastos',
    skip = 0,
    col_names = T
  )  %>%
  clean_names(  ) %>%
  dplyr::select( -x2011_2010,-x2012_2011, -x2013_2012 )

##Análisis Vertical del Gastos----------------------------------------------------------------------
message( '\t Análisis Vertical del Gastos' )
analisis_vertical_gastos <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'ana_ver_gastos',
    skip = 0,
    col_names = T
  )  %>%
  clean_names(  ) %>%
  dplyr::select( -x2010,-x2011, -x2012, -x2013 ) %>%
  mutate_if( is.numeric, replace_na, 0 )

##Gastos Prestacionales-----------------------------------------------------------------------------
message( '\t Gastos Prestacionales' )
gastos_prestacionales <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'gastos_pensiones_rt',
    skip = 0,
    col_names = T
  )  %>%
  clean_names(  ) %>%
  dplyr::select( -x2010, -x2011, -x2012, -x2013 ) %>%
  mutate_if( is.numeric, replace_na, 0 )

##Lectura de ingresos vs gastos totales-------------------------------------------------------------
message( '\t ingresos vs gastos totales' )
ingresos_vs_gastos <-
  readxl::read_xlsx( 
    path = file,
    sheet = 'resultado_ejecicio',
    skip = 0,
    col_names = T
  ) %>%
  clean_names(  ) %>%
  filter( ano <= anio_max, ano >= anio_min )

#Guardar RData--------------------------------------------------------------------------------------
lista <- c( 
  "activo_del_fondo",
  "analisis_componentes_activo",
  "analisis_horizontal_activo",
  "analisis_vertical_activo",
  "cuentas_cobrar_fondo",
  "analisis_componentes_cobrar_fondo",
  "pasivos_fondo",
  "componentes_pasivos_fondo",
  "analisis_horizontal_pasivos",
  "analisis_vertical_pasivos",
  "cuentas_pagar_fondo",
  "patrimonio_fondo",
  "componentes_patrimonio_fondo",
  "analisis_horizontal_patrimonio",
  "analisis_vertical_patrimonio",
  "ingresos_fondo",
  "componentes_ingresos",
  "analisis_horizontal_ingresos",
  "analisis_vertical_ingresos",
  "ingresos_aportes",
  "gastos",
  "componentes_gastos",
  "analisis_horizontal_gastos",
  "analisis_vertical_gastos",
  "gastos_prestacionales",
  "ingresos_vs_gastos"
)

save( 
  list = lista,
  file = paste0( parametros$RData_seg, 'IESS_RTR_analisis_financiero.RData' )
)

#Limpiar RAM----------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% c( 'parametros' ) )] )
gc(  )
