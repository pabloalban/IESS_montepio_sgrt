message( paste( rep( '-', 100 ), collapse = '' ) )

#Parámetros de filtrado-----------------------------------------------------------------------------

anio_max <- parametros$fec_fin

anio_min <- 2012

#Lectura de nomina----------------------------------------------------------------------------------

message( '\tLeyendo nomina de pensiones de SGRT desde 2012 a 2022' )

file_nomina <-
  paste0( parametros$Data_seg, 'IESS_RTR_nominas_mensuales.tsv' )

nomina_sgrt <- ( 
  read.table( 
    file_nomina,
    skip = 0,
    dec = ".",
    header = TRUE,
    sep = "\t",
    na.strings = "NA",
    encoding = "UTF-8",
    row.names = NULL
  )
) %>% clean_names(  ) %>%
  dplyr::select( 
    anio := ano,
    mes,
    cedula := asegurado,
    sexo,
    fecha_nacimiento,
    provincia := prov_beneficiario,
    cod_tipo_prestacion := tipo_prestacion,
    tipo_beneficiario,
    rubro,
    descripcion := descripcion_rubro,
    valor
  ) %>%
  mutate( fecha_nacimiento = as.Date( fecha_nacimiento, "%Y-%m-%d" ) ) %>%
  mutate( sexo = if_else( sexo == "HOMBRE" | is.na( sexo ), "M", "F" ) ) %>%
  filter( anio <= anio_max, anio >= anio_min )

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

# Filtrar prestaciones de montepío orfandad---------------------------------------------------------
prestaciones_orfandad <- nomina_sgrt %>%
  filter( cod_tipo_prestacion == 'VO' ) %>%
  filter( tipo_beneficiario %in% c( "HIJINV",
                                    "HIJNOR",
                                    "HIJO" ) )

# Filtrar prestaciones de montepío padres-----------------------------------------------------------
prestaciones_padres <- nomina_sgrt %>%
  filter( cod_tipo_prestacion == 'VO' ) %>%
  filter( tipo_beneficiario %in% c( "MADNOR",
                                    "MADRE",
                                    "MAFM",
                                    "PADINV" ) )

# Guardando en un Rdata-----------------------------------------------------------------------------
message( "\tGuardando Rdatas" )

save( 
  prestaciones_pp,
  prestaciones_pt,
  prestaciones_pa,
  prestaciones_viudez,
  prestaciones_orfandad,
  prestaciones_padres,
  file = paste0( parametros$RData_seg, "IESS_RTR_rentas.RData" )
)

# Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep( "-", 100 ), collapse = "" ) )
rm( list = ls(  )[!( ls(  ) %in% "parametros" )] )
gc(  )
