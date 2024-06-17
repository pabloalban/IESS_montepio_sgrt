# Estadística para IVM -----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )

# Carga de información -----------------------------------------------------------------------------
load( file = parametros$demo_rdata_sgo_est_dem )

save_list <- NULL

# Guardando resultados -----------------------------------------------------------------------------
save( list = save_list, file = parametros$ivm_rdata_est_demo )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
