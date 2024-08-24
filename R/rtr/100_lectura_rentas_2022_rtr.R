message( paste( rep( '-', 100 ), collapse = '' ) )

#Parámetros de filtrado-----------------------------------------------------------------------------

anio_max <- 2022

anio_min <- 2014

#Lectura de nomina----------------------------------------------------------------------------------

message( '\tLeyendo nomina de pensiones de SGRT desde 2012 a 2022' )

file_nomina <-
  paste0( parametros$Data_seg, 'IESS_RTR_nominas_mensuales_2022.csv' )

nomina_sgrt <- ( 
  read.table( 
    file_nomina,
    skip = 0,
    dec = ".",
    header = TRUE,
    sep = ",",
    na.strings = "NA",
    encoding = "UTF-8",
    #nrows = 10000,
    row.names = NULL
  )
) %>% clean_names(  ) %>%
  mutate( descripcion_rubro = NA ) %>% 
  dplyr::select( 
    anio := ano,
    mes,
    cedula := asegurado,
    sexo := cod_sexo,
    fecha_nacimiento,
    provincia := ubicacion_geografica,
    cod_tipo_prestacion := tipo_prestacion,
    tipo_beneficiario,
    rubro,
    descripcion := descripcion_rubro,
    valor
  ) %>%
  mutate( fecha_nacimiento = as.Date( fecha_nacimiento, "%m/%d/%Y" ) ) %>%
  mutate( sexo = if_else( sexo == "1" | is.na( sexo ), "M", "F" ) ) %>%
  filter( anio <= anio_max, anio >= anio_min ) %>% 
  filter( !is.na( fecha_nacimiento ) )

# Filtrar prestación permanente parcial-------------------------------------------------------------
prestaciones_pp <- nomina_sgrt %>%
  filter( cod_tipo_prestacion == "PP" )

# Filtrar prestación permanente total---------------------------------------------------------------
prestaciones_pt <- nomina_sgrt %>%
  filter( cod_tipo_prestacion == "PT" )

# Filtrar prestaciones permanente absoluta----------------------------------------------------------
prestaciones_pa <- nomina_sgrt %>%
  filter( cod_tipo_prestacion == "PA" )

# Filtrar prestaciones de montepío viudez-----------------------------------------------------------
prestaciones_viudez <- nomina_sgrt %>%
  filter( cod_tipo_prestacion == 'VO' ) %>%
  filter( 
    tipo_beneficiario %in% c( 
      "CONVIF",
      "CONVIM",
      "CONVIV",
      "HERMSO",
      "MADNOR",
      "MADRE",
      "MAFM",
      "PADINV",
      "PADRE",
      "VIUDA",
      "VIUDO"
    )
  )

#Cargando base del RC-------------------------------------------------------------------------------
load( file = paste0( parametros$RData, "IESS_Reg_Civil.RData" ) )

rc <- rc %>% dplyr::select( cedula,
                            sexo,
                            fecha_nacimiento )

load( paste0( parametros$RData_seg, 'IESS_RTR_inventario_pensionistas.RData' ) )

inventario_jubilados_sgrt <- inventario_jubilados %>% 
  filter( tipo_seguro %in% c( 'RT' ),
          tipo_prestacion %in% c( 'PA', 'PT' ) ) %>% 
  dplyr::select( causante := cedula,
                 sexo_causante := sexo,
                 fecha_nacimiento_causante := fecha_nacimiento ) %>% 
  mutate( circunstancia = 'defunción jubilado' )


inventario_jubilados_sgrt <- rbind( prestaciones_pt,
                                    prestaciones_pa ) %>% 
  distinct( cedula, .keep_all = TRUE ) %>% 
  dplyr::select( causante := cedula,
                 sexo_causante := sexo,
                 fecha_nacimiento_causante := fecha_nacimiento ) %>% 
  mutate( circunstancia = 'defunción jubilado' )


rc <- rc %>% 
  dplyr::select( cedula,
                 sexo,
                 fecha_nacimiento )

#Lectura de montepío--------------------------------------------------------------------------------

message( '\tLeyendo nomina de montepío de SGRT desde 2012 a 2023' )

file_nomina <-
  paste0( parametros$Data_seg, 'IESS_RTR_nomina_montepio.txt' )

nomina_montepio_sgrt <- ( 
  read.table( 
    file_nomina,
    skip = 0,
    dec = ".",
    header = TRUE,
    sep = "\t",
    na.strings = "NA",
    encoding = "UTF-8",
    #nrows = 10000,
    row.names = NULL
  )
) %>% clean_names(  ) %>%
  #mutate( descripcion_rubro = NA ) %>% 
  dplyr::select( 
    anio := ano,
    mes,
    cedula := cedula_beneficiario,
    #sexo := sexo,
    #fecha_nacimiento,
    provincia := provincia,
    cod_tipo_prestacion := tipo_prestacion,
    tipo_beneficiario,
    rubro,
    descripcion := descripcion_rubro,
    valor,
    #circunstancia,
    causante
  ) %>%
  #mutate( fecha_nacimiento = as.Date( fecha_nacimiento, "%m/%d/%Y" ) ) %>%
  #mutate( sexo = if_else( sexo == "HOMBRE" | is.na( sexo ), "M", "F" ) ) %>%
  filter( anio <= anio_max, anio >= anio_min ) %>% 
  mutate( causante = as.character( causante ) ) %>% 
  mutate( causante = if_else( nchar( causante ) == 9,
                              paste0( '0', causante ),
                              causante ) )


nomina_montepio_sgrt <- nomina_montepio_sgrt %>% 
  left_join( ., rc, by = 'cedula') %>% 
  left_join( ., inventario_jubilados_sgrt, by = c( 'causante' = 'causante' ) ) %>% 
  filter( !is.na( fecha_nacimiento ) ) %>% 
  mutate( circunstancia = ifelse( ( circunstancia %in% c( 'defunción jubilado' ) ),
                                  circunstancia,
                                  'defunción afiliado' ) ) %>% 
  mutate( circunstancia = ifelse( is.na( causante ),
                                  NA,
                                  circunstancia ) )


# Filtrar prestaciones de montepío orfandad---------------------------------------------------------
prestaciones_orfandad <- nomina_montepio_sgrt %>%
  filter( cod_tipo_prestacion == 'VO' ) %>%
  filter( tipo_beneficiario %in% c( "HIJINV",
                                    "HIJNOR",
                                    "HIJO" ) )

# Filtrar prestaciones de montepío viudez-----------------------------------------------------------
prestaciones_viudez <- nomina_montepio_sgrt %>%
  filter( cod_tipo_prestacion == 'VO' ) %>%
  filter( 
    tipo_beneficiario %in% c( 
      "CONVIF",
      "CONVIM",
      "CONVIV",
      "HERMSO",
      "MADNOR",
      "MADRE",
      "MAFM",
      "PADINV",
      "PADRE",
      "VIUDA",
      "VIUDO"
    )
  )
# Guardando en un Rdata-----------------------------------------------------------------------------
message( "\tGuardando Rdatas" )

save( 
  prestaciones_pp,
  prestaciones_pt,
  prestaciones_pa,
  prestaciones_viudez,
  prestaciones_orfandad,
  #prestaciones_padres,
  nomina_montepio_sgrt,
  file = paste0( parametros$RData_seg, "IESS_RTR_rentas.RData" )
)

# Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep( "-", 100 ), collapse = "" ) )
rm( list = ls(  )[!( ls(  ) %in% "parametros" )] )
gc( )
