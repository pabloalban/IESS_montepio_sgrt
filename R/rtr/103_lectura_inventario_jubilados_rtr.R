message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo tabla beneficiarios de DSP, con la información de cálculo de pensiones de DSP y DSGRT' )

#Carga del Registro Civil a enero de 2023-----------------------------------------------------------
message( '\tLeyendo información del RC' )

file <- paste0( parametros$Data_seg, 'RC_01_2023.dsv' ) 

rc <- read.table( file,
                  dec = ".",
                  header = TRUE,
                  sep = "\t",
                  na.strings = NA,
                  colClasses = c( "character",
                                  "NULL",
                                  "factor",
                                  "character",
                                  rep( "NULL", 2 ),
                                  "character",
                                  rep( "NULL", 13 ) ), #20 var
                  stringsAsFactors = FALSE ) %>% clean_names( ) %>%  clean_names( ) %>% 
  mutate( sexo = if_else( genper == '2',
                          'F',
                          'M' ) ) %>% 
  mutate( fecha_nacimiento = as.Date( fecnacper, "%d/%m/%Y", tz = "America/Guayaquil" ),
          fecha_defuncion = as.Date( fecdefper, "%d/%m/%Y", tz = "America/Guayaquil" ) ) %>% 
  mutate( fecha_nacimiento = if_else( fecha_nacimiento < as.Date( "1830-12-31") | 
                                        fecha_nacimiento > as.Date( "2023-2-1"),
                                      NA,
                                      fecha_nacimiento ) ) %>% 
  dplyr::select( cedula := cedideusu,
                 sexo,
                 fecha_nacimiento,
                 fecha_defuncion ) 


#Carga de la tabla beneficiarios a enero de 2023----------------------------------------------------

file <- paste0( parametros$Data_seg, 'IESS_inventario_pensionistas_01_2023.dsv' ) 

inventario_jubilados <- read.table(file,
                                   dec = ".",
                                   header = TRUE,
                                   sep = "\t",
                                   na.strings = NA,
                                   stringsAsFactors = FALSE) %>% clean_names( ) %>% 
  mutate( cedula = asegurado ) %>% 
  dplyr::select( -asegurado ) %>% 
  mutate( fecha_derecho = as.Date( fecha_derecho, "%d/%m/%Y", tz = "America/Guayaquil" ),
          fecha_incapacidad = as.Date( fecha_incapacidad, "%d/%m/%Y", tz = "America/Guayaquil" ) ) %>% 
  left_join( ., rc, by = "cedula" ) %>% 
  dplyr::select( cedula,
                 sexo,
                 fecha_nacimiento,
                 fecha_defuncion,
                 tipo_seguro,
                 tipo_prestacion,
                 estado_prestacion,
                 numero_acuerdo,
                 fecha_derecho,
                 promedio_sueldo_teorico,
                 promedio_sueldo_real,
                 coeficiente_teorico,
                 coeficiente_real,
                 valor_pension_teorica_ajustada,
                 valor_pension_concedida,
                 numero_imposiciones,
                 numero_imposiciones_totales,
                 numero_expediente,
                 grado_incapacidad,
                 causa_incapacidad,
                 fecha_incapacidad,
                 act_fecha_fallecimiento
  )

#Tabla con los porcentajes de incapacidad del SGRT--------------------------------------------------

porcentaje_incapacidad <- inventario_jubilados %>% 
  filter( tipo_seguro == 'RT',
          estado_prestacion %in% c( 'A', 'I' ) ) %>% 
  dplyr::select( cedula,
                 tipo_seguro,
                 tipo_prestacion,
                 coeficiente_teorico,
                 coeficiente_real
  )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando porcentajes de incapacidad de SGRT en un solo data.frame' )

save( 
  porcentaje_incapacidad,
  file = paste0( parametros$RData_seg, 'IESS_RTR_porcentaje_incapacidad.RData' )
)

save( 
  inventario_jubilados,
  file = paste0( parametros$RData_seg, 'IESS_RTR_inventario_pensionistas.RData' )
)

save( 
  rc,
  file = paste0( parametros$RData_seg, 'RC_informacion.RData' )
)

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% 'parametros' )] )
gc(  )