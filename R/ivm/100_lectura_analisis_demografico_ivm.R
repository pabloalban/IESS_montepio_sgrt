message( paste( rep('-', 100 ), collapse = '' ) )

# Parámetros ---------------------------------------------------------------------------------------
message( '\tLectura de los coeficientes de las pensiones' )
s_max <- 70
s_lst <- seq( 0, s_max )

# Inclusión de coeficientes para ajustar las pensiones en función del tiempo de servicio
coef_pen <- read_xlsx(
  path = paste0( parametros$Data_seg, 'coeficientes_pension.xlsx' ), shee = 1, range = 'A2:B37',
  col_names = FALSE )
coef_pen <- as.data.table( coef_pen )
setnames( coef_pen, c( 's', 'coef' ) )
coef_pen <- merge.data.table( 
  data.table( s = s_lst ),
  coef_pen, 
  by = 's',
  all.x = TRUE )
coef_pen[ s < 5, coef := 0 ]
coef_pen[ s > 40, coef := 1 ]
coef_pen[ , coef_4 := coef ]
coef_pen[ , coef_5 := coef ]

save( coef_pen, file = parametros$rdata_sgo_coef_pen )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
