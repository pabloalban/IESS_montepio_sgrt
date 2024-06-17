message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tLectura de pagos de subsidios de SGRT' )

#Cargando pagos de subsidios -----------------------------------------------------------------------

message( '\tLeyendo subsidios de SGRT' )

file_subsidios <-
  paste0( parametros$Data_seg, 'IESS_RTR_subsidios.txt' )

colclass <- c( 
  "character",
  "character",
  "character",
  "character",
  "character",
  "character",
  "character",
  "character",
  "integer",
  "integer",
  "numeric",
  "logical",
  "numeric",
  "character",
  "character",
  "character",
  "character",
  "character",
  "numeric",
  "character",
  "character",
  "character",
  "numeric",
  "character",
  "character",
  "character",
  "numeric",
  "numeric",
  "character",
  "character",
  "numeric",
  "character"
)

subsidios_rtr <- ( 
  read.table( 
    file_subsidios,
    skip = 0,
    dec = ".",
    header = TRUE,
    sep = "\t",
    na.strings = "NA",
    encoding = "UTF-8",
    colClasses = colclass,
    row.names = NULL
  )
) %>% clean_names(  ) %>%
  mutate( 
    cedula = cedula_afi,
    sexo = genero,
    fecha_nacimiento = substr( fecnac, 1, 10 ),
    fecha_transferencia = substr( fecha_transferencia, 1, 10 )
  ) %>%
  mutate( 
    fecha_nacimiento = as.Date( fecha_nacimiento, "%Y/%m/%d" ),
    fecha_transferencia  = as.Date( fecha_transferencia, "%Y/%m/%d" )
  ) %>%
  mutate( sexo = if_else( sexo == "Femenino                        ", "F", "M" ) ) %>%
  mutate( anio = year( fecha_transferencia ),
          mes = month( fecha_transferencia ) ) %>%
  filter( anio <= 2020, anio >= 2012 ) %>%
  dplyr::select( 
    cedula,
    sexo,
    fecha_nacimiento,
    anio,
    mes,
    estado_pago,
    porc_sub := sr_porcentaje_cobro,
    dias_reposo,
    valor,
    fecha_transferencia
  ) %>% 
  filter( estado_pago == 'PAGADO   ' )

colclass <- data.frame( col = sapply( subsidios_rtr, class ) )
#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando data.frame de subsidios' )

save( subsidios_rtr,
      file = paste0( parametros$RData_seg, 'IESS_RTR_subsidios.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% 'parametros' )] )
gc(  )