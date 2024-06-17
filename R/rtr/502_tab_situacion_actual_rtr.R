message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura tablas información finaciera ' )
# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_situacion_actual.RData' ) )



###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()