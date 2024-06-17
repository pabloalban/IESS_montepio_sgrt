message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tGenerando proyección de beneficios' )
load( file = parametros$demo_rdata_sgo_tran_prep )
load( file = parametros$rdata_sgo_coef_pen )
load( file = esc$rdata_sal_proy )

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% c( parametros_lista ) ) ] )

# Procesando tasa ----------------------------------------------------------------------------------

# Horizonte de proyección
t_max <- parametros$ivm_horizonte

anio_ini <- max( parametros$anio_ini )

if ( parametros$seguro == 'IVM' ) {
  tas_macro_sex_edad <- esc$apo_act[ , list( 
    t, anio, i_f, i_p, i_r, sbu, ben_aux_fun, 
    cal_pen_vej, cal_pen_inv, cal_pen_viu, cal_pen_orf ) ]
} else {
  tas_macro_sex_edad <- copy( esc$apo_act[ , list( 
    t, anio, i_f, i_p, i_r, sbu, 
    cal_pen_vej, cal_pen_inv, cal_pen_viu, cal_pen_orf ) ] )
}
setorder( tas_macro_sex_edad, anio )
tas_macro_sex_edad[ anio <= anio_ini, i_f := 0 ]
tas_macro_sex_edad[ anio <= anio_ini, i_p := 0 ]
tas_macro_sex_edad[ anio <= anio_ini, i_r := 0 ]
tas_macro_sex_edad[ , u_f := cumprod( 1 + i_f ) ]
tas_macro_sex_edad[ , u_p := cumprod( 1 + i_p ) ]
tas_macro_sex_edad[ , u_r := cumprod( 1 + i_r ) ]

t_max <- 40
s_max <- 70
x_max <- 105

sexo_lst <- factor( c( 'H', 'M' ), levels = c( 'H', 'M' ) )
t_lst <- seq( 0, t_max )
s_lst <- seq( 0, s_max )
x_lst <- seq( 0, x_max )

aux <- data.table( expand_grid( 
  t = t_lst,
  sexo = sexo_lst,
  tip = c( 4, 5, 7, 8 ),
  x = x_lst,
  s = s_lst ) )
aux[ , anio := t + parametros$anio_ini ]
aux <- merge.data.table(
  aux, 
  tas_macro_sex_edad[ , list( anio, u_p, sbu, cal_pen_vej, cal_pen_inv, cal_pen_viu, cal_pen_orf ) ], 
  by = c( 'anio' ), 
  all.x = TRUE )

# Proyección pensiones versión 1 -------------------------------------------------------------------
# Esta versión fue utilizada para las proyecciones del 2018, con las nuevas fuentes de información
# a partir del estudio del 2020, este código no es completamente funcional.
# message( '\tGenerando proyección de pensiones' )
# 
# pen_afi[ , log_pen := log( pen ) ]
# 
# pen_int_f <- data.table( x = 0:105 )
# pen_smooth_vej_f <- lm( log_pen ~ bs( x, df = 6, degree = 3 ),
#                         weights = ER_pen,
#                         data = pen_afi[ sexo == 'F' & tip == 'JV' & x >= 48 & x <= 110 ] )
# 
# pen_smooth_inv_f <- lm( log_pen ~ bs( x, df = 16, degree = 3 ),
#                         weights = ER_pen,
#                         data = pen_afi[ sexo == 'F' & tip == 'JI' & x >= 20 & x <= 110 ] )
# 
# pen_int_f[ , log_pen_vej := predict( object = pen_smooth_vej_f, newdata = pen_int_f ) ]
# pen_int_f[ , pen_vej := exp( log_pen_vej ) ]
# pen_int_f[ x < 48, pen_vej := 0 ]
# pen_int_f[ , log_pen_inv := predict( object = pen_smooth_inv_f, newdata = pen_int_f ) ]
# pen_int_f[ , pen_inv := exp( log_pen_inv ) ]
# pen_int_f[ x < 25, pen_inv := 0 ]
# pen_int_f[ , sexo := 'F' ]
# 
# pen_int_m <- data.table( x = 0:105 )
# pen_smooth_vej_m <- lm( log_pen ~ bs( x, df = 5, degree = 3 ),
#                         weights = ER_pen,
#                         data = pen_afi[ sexo == 'M' & tip == 'JV' & x >= 48 & x <= 110 ] )
# 
# pen_smooth_inv_m <- lm( log_pen ~ bs( x, df = 11, degree = 3 ),
#                         weights = ER_pen,
#                         data = pen_afi[ sexo == 'M' & tip == 'JI' & x >= 20 & x <= 110 ] )
# 
# pen_int_m[ , log_pen_vej := predict( object = pen_smooth_vej_m, newdata = pen_int_m ) ]
# pen_int_m[ , pen_vej := exp( log_pen_vej ) ]
# pen_int_m[ x < 48, pen_vej := 0 ]
# pen_int_m[ , log_pen_inv := predict( object = pen_smooth_inv_m, newdata = pen_int_m ) ]
# pen_int_m[ , pen_inv := exp( log_pen_inv ) ]
# pen_int_m[ x < 25, pen_inv := 0 ]
# pen_int_m[ , sexo := 'M' ]
# 
# pen_int <- rbind( pen_int_f, pen_int_m )
# 
# # aux_f <- pen_int[ sexo == 'F' ]
# # aux_ini_f <- pen_afi[ sexo == 'F' & tip == 'JV' ]
# # plot( aux_f$x, aux_f$pen_vej, type = 'l', col = parametros$iess_blue, ylim = c( 0, 2000 ) )
# # points( aux_ini_f$x, aux_ini_f$pen, type = 'p', col = parametros$iess_green, cex = 0.7, pch = 16 )
# # 
# # aux_f <- pen_int[ sexo == 'F' ]
# # aux_ini_f <- pen_afi[ sexo == 'F' & tip == 'JI' ]
# # plot( aux_f$x, aux_f$pen_inv, type = 'l', col = parametros$iess_blue, ylim = c( 0, 1500 ) )
# # points( aux_ini_f$x, aux_ini_f$pen, type = 'p', col = parametros$iess_green, cex = 0.7, pch = 16 )
# # 
# # aux_m <- pen_int[ sexo == 'M' ]
# # aux_ini_m <- pen_afi[ sexo == 'M' & tip == 'JV' ]
# # plot( aux_m$x, aux_m$pen_vej, type = 'l', col = parametros$iess_blue, ylim = c( 0, 2000 ) )
# # points( aux_ini_m$x, aux_ini_m$pen, type = 'p', col = parametros$iess_green, cex = 0.7, pch = 16 )
# #
# # aux_m <- pen_int[ sexo == 'M' ]
# # aux_ini_m <- pen_afi[ sexo == 'M' & tip == 'JI' ]
# # plot( aux_m$x, aux_m$pen_inv, type = 'l', col = parametros$iess_blue, ylim = c( 0, 1500 ) )
# # points( aux_ini_m$x, aux_ini_m$pen, type = 'p', col = parametros$iess_green, cex = 0.7, pch = 16 )

# Proyección pensiones versión 2 -------------------------------------------------------------------
message( '\tGenerando proyección de pensiones' )

load( file = paste0( parametros$demo_rdata_sgo_incom_tran_pen_anio, parametros$anio_ini, '.RData' ) )

## Proyección de las pensiones existentes ----------------------------------------------------------
pen_proy <- sgo_comp_tran[ , list( anio, tipo, sexo, x, imp, P, ER_pen ) ]
pen_proy[ , s := round( imp, 0 ) ]
pen_proy[ is.na( s ), s := 0 ]

# Numerando tipos de pensionistas
pen_proy[ tipo == 'DISCAPACIDAD', tip := as.integer( 5 ) ]
pen_proy[ tipo == 'INVALIDEZ', tip := as.integer( 5 ) ]
pen_proy[ tipo == 'ORFANDAD', tip := as.integer( 8 ) ]
pen_proy[ tipo == 'VEJEZ', tip := as.integer( 4 ) ]
pen_proy[ tipo == 'VIUDEDAD', tip := as.integer( 7 ) ]
pen_proy[ , tipo := NULL ]

pen_proy <- pen_proy[ !( tip == 7 & x < 18 ) ]
pen_proy[ x > x_max, x := x_max ]
pen_proy[ s > s_max, s := s_max ]

pen_proy_mont <- pen_proy[ 
  tip %in% c( 7, 8 ), 
  list( 
    P = sum( P, na.rm = TRUE ),
    ER = sum( ER_pen, na.rm = TRUE ) ),
  by = list( anio, tip, sexo, x ) ]

pen_proy_mont[ , id := 1]

pen_proy_mont <- merge.data.table( 
  data.table( id = 1, s = s_lst ),
  pen_proy_mont,
  by = 'id', 
  allow.cartesian = TRUE )

pen_proy_mont[ , id := NULL ]

pen_proy <- pen_proy[ 
  !( tip %in% c( 7, 8 ) ), 
  list( 
    P = sum( P, na.rm = TRUE ),
    ER = sum( ER_pen, na.rm = TRUE ) ),
  by = list( anio, tip, sexo, x, s ) ]

pen_proy <- rbindlist( list( pen_proy, pen_proy_mont ), use.names = TRUE )

pen_proy[ , pen := 0 ]
pen_proy[ ER > 0, pen := P / ER ]

pen_proy[ , pen_per := quantile( pen, probs = 0.9995 ), list( sexo, tip, s ) ]
pen_proy[ pen > pen_per, pen := pen_per ]
pen_proy[ , pen_per := NULL ]

pen_proy <- merge.data.table(
  pen_proy[ , list( tip, sexo, x, s, P, ER, pen ) ], 
  aux, 
  by = c( 'tip', 'sexo', 'x', 's' ), 
  all.y = TRUE )

pen_proy[ , pen := u_p * pen ]
setorder( pen_proy, sexo, x, s )

pen_proy[ , tip := paste0( 'pen_ant_', tip ) ]
pen_proy <- dcast.data.table( 
  data = pen_proy,
  formula = anio + t + sexo + x + s + sbu ~ tip, value.var = 'pen' )

# Acotaciones de proyecciones proyectadas según la ley
pen_proy[ is.na( pen_ant_4 ), pen_ant_4 := 0 ]
pen_proy[ x < 55, pen_ant_4 := 0 ]

pen_proy[ is.na( pen_ant_5 ), pen_ant_5 := 0 ]
pen_proy[ x < 25, pen_ant_5 := 0 ]

pen_proy[ is.na( pen_ant_7 ), pen_ant_7 := 0 ]
pen_proy[ x < 18, pen_ant_7 := 0 ]

pen_proy[ is.na( pen_ant_8 ), pen_ant_8 := 0 ]

# Proyección de nuevas pensiones -------------------------------------------------------------------
pen_new_proy <- copy( sal_proy )
pen_new_proy[ , r := x - s ]
pen_new_proy[ , u := s - t ]
setorder( pen_new_proy, sexo, r, u )

if ( esc$use_arit_mean ) {
  
  pen_new_proy[ , PY2 := roll_mean( sal, 2, fill = 0, align = 'right', na.rm = FALSE ), by = list( sexo, r, u ) ]
  pen_new_proy[ , PY3 := roll_mean( sal, 3, fill = 0, align = 'right', na.rm = FALSE ), by = list( sexo, r, u ) ]
  pen_new_proy[ , PY4 := roll_mean( sal, 4, fill = 0, align = 'right', na.rm = FALSE ), by = list( sexo, r, u ) ]
  pen_new_proy[ , PY  := roll_mean( sal, 5, fill = 0, align = 'right', na.rm = FALSE ), by = list( sexo, r, u ) ]
  
} else {
  
  pen_new_proy[ , PY2 := roll_prod( sal, 2, fill = 0, align = 'right', na.rm = FALSE )^(1/2), by = list( sexo, r, u ) ]
  pen_new_proy[ , PY3 := roll_prod( sal, 3, fill = 0, align = 'right', na.rm = FALSE )^(1/3), by = list( sexo, r, u ) ]
  pen_new_proy[ , PY4 := roll_prod( sal, 4, fill = 0, align = 'right', na.rm = FALSE )^(1/4), by = list( sexo, r, u ) ]
  pen_new_proy[ , PY  := roll_prod( sal, 5, fill = 0, align = 'right', na.rm = FALSE )^(1/5), by = list( sexo, r, u ) ]
  
}
pen_new_proy[ is.na( PY2 ), PY2 := 0 ]
pen_new_proy[ is.na( PY3 ), PY3 := 0 ]
pen_new_proy[ is.na( PY4 ), PY4 := 0 ]
pen_new_proy[ PY2 == 0 & PY3 == 0 & PY4 == 0 & PY == 0, PY := sal ]
pen_new_proy[ PY2 > 0 & PY3 == 0 & PY4 == 0 & PY == 0, PY := PY2 ]
pen_new_proy[ PY2 > 0 & PY3 > 0 & PY4 == 0 & PY == 0, PY := PY3 ]
pen_new_proy[ PY2 > 0 & PY3 > 0 & PY4 > 0 & PY == 0, PY := PY4 ]

pen_new_proy[ , PY2 := NULL ]
pen_new_proy[ , PY3 := NULL ]
pen_new_proy[ , PY4 := NULL ]
pen_new_proy[ , r := NULL ]
pen_new_proy[ , u := NULL ]

pen_proy <- merge.data.table( pen_proy, pen_new_proy, by = c( 't', 'anio', 'sexo', 'x', 's' ) )

pen_proy <- merge.data.table( 
  pen_proy, 
  coef_pen[ , list( s, coef_4, coef_5 ) ],
  by = c( 's' ), 
  all.x = TRUE )

pen_proy <- merge.data.table(
  pen_proy, 
  tas_macro_sex_edad[ , list( t, cal_pen_vej, cal_pen_inv, cal_pen_viu, cal_pen_orf ) ], 
  by = c( 't' ), 
  all.x = TRUE )

pen_proy[ is.na( coef_4 ), coef_4 := 0 ]
pen_proy[ is.na( coef_5 ), coef_5 := 0 ]

pen_proy[ , pen_4 := coef_4 * cal_pen_vej * PY ]
pen_proy[ , pen_4 := pmax( pen_4, 0.5 * 12 * sbu ) ]
pen_proy[ , pen_4 := pmin( pen_4, 5.5 * 12 * sbu ) ]
pen_proy[ , pen_4 := pen_4 + pen_4 / 12 + sbu ]
pen_proy[ x < 55, pen_4 := 0  ]

pen_proy[ , pen_5 := coef_5 * cal_pen_inv * PY ]
pen_proy[ , pen_5 := pmax( pen_5, 0.5 * 12 * sbu ) ]
pen_proy[ , pen_5 := pmin( pen_5, 5.5 * 12 * sbu ) ]
pen_proy[ , pen_5 := pen_5 + pen_5 / 12 + sbu ]
pen_proy[ x < 25, pen_5 := 0 ]

pen_proy[ , pen_7_4 := 0.6 * pen_4 ]
pen_proy[ , pen_7_4 := pmax( pen_7_4, 0.5 * 12 * sbu ) ]
pen_proy[ , pen_7_4 := pmin( pen_7_4, 5.5 * 12 * sbu ) ]
pen_proy[ , pen_7_4 := pen_7_4 + pen_7_4 / 12 + sbu ]
pen_proy[ x < 18, pen_7_4 := 0 ]

pen_proy[ , pen_7_5 := 0.6 * pen_5 ]
pen_proy[ , pen_7_5 := pmax( pen_7_5, 0.5 * 12 * sbu ) ]
pen_proy[ , pen_7_5 := pmin( pen_7_5, 5.5 * 12 * sbu ) ]
pen_proy[ , pen_7_5 := pen_7_5 + pen_7_5 / 12 + sbu ]
pen_proy[ x < 18, pen_7_5 := 0 ]

pen_proy[ , pen_8_4 := 0.4 * pen_4 ]
pen_proy[ , pen_8_4 := pmax( pen_8_4, 0.5 * 12 * sbu ) ]
pen_proy[ , pen_8_4 := pmin( pen_8_4, 4.5 * 12 * sbu ) ]
pen_proy[ , pen_8_4 := pen_8_4 + pen_8_4 / 12 + sbu ]

pen_proy[ , pen_8_5 := 0.4 * pen_5 ]
pen_proy[ , pen_8_5 := pmax( pen_8_5, 0.5 * 12 * sbu ) ]
pen_proy[ , pen_8_5 := pmin( pen_8_5, 4.5 * 12 * sbu ) ]
pen_proy[ , pen_8_5 := pen_8_5 + pen_8_5 / 12 + sbu ]

setorder( pen_proy, t, sexo, x, s )
pen_proy <- pen_proy[ , list( 
  t, anio, sexo, x, s, sbu, 
  pen_ant_4, pen_4, 
  pen_ant_5, pen_5, 
  pen_ant_7, pen_7_4, pen_7_5, 
  pen_ant_8, pen_8_4, pen_8_5 ) ]

# Evitando que las pensiones nuevas sean muy pequeñas con respecto a las pensiones existentes
# pen_proy[ pen_7_4 / pen_ant_7 < 0.95, pen_7_4 := pen_ant_7 ]
# pen_proy[ pen_7_5 / pen_ant_7 < 0.95, pen_7_4 := pen_ant_7 ]
# pen_proy[ pen_8_4 / pen_ant_8 < 0.95, pen_8_4 := pen_ant_8 ]
# pen_proy[ pen_8_5 / pen_ant_8 < 0.95, pen_8_5 := pen_ant_8 ]

# Proyección auxilio de funerales ------------------------------------------------------------------
if ( parametros$seguro == 'IVM' ) {
  message( '\tGenerando proyección de auxilio de funerales' )
  aux_fun_proy <- data.table( expand.grid(
    t = 0:t_max, 
    sexo = factor( c( 'H', 'M' ), levels = c( 'H', 'M' ) ), 
    x = 0:105 ) )
  aux_fun_proy <- merge.data.table( aux_fun_proy, tas_macro_sex_edad[ , list( t, ben_aux_fun, u_f ) ], by = 't', all.x = TRUE )
  setorder( aux_fun_proy, sexo, x, t )
  aux_fun_proy[ , ben_2_6 := ben_aux_fun * u_f ]
  aux_fun_proy[ , ben_3_6 := 0 ]
  aux_fun_proy[ , ben_4_6 := ben_aux_fun * u_f ]
  aux_fun_proy[ , ben_5_6 := ben_aux_fun * u_f ]
  aux_fun_proy[ , ben_7_6 := ben_aux_fun * u_f ]
  aux_fun_proy[ , ben_8_6 := ben_aux_fun * u_f ]
  aux_fun_proy <- aux_fun_proy[ , list( t, sexo, x, ben_2_6, ben_3_6, ben_4_6, ben_5_6, ben_7_6, ben_8_6 ) ]
}

# Proyección de pensiones tomando en cuenta la proyección población --------------------------------
message( '\tProyección dinámica de pensiones futuras' )

# # Tiempo
# t_min <- 0
# t_max <- parametros$ivm_horizonte # horizonte de proyección
# t_lst <- seq( t_min, t_max, 1 )
# 
# # Lista de sexos
# sx_lst <- c( 'H', 'M')
# 
# # Año inicial de proyección
# anio_ini <- parametros$anio_ini
# 
# # Año final de proyección
# anio_fin <- anio_ini + t_max
# 
# # Tiempos de servicio
# s_min <- 0
# s_max <- 70
# s_lst <- seq( s_min, s_max, 1 )
# 
# # Edades
# x_min <- 0
# x_max <- parametros$edad_max
# x_lst <- seq( x_min, x_max, 1 )
# 
# nd <- 6
# nt <- length( t_lst )
# ns <- length( s_lst )
# nx <- length( x_lst )
# nsx <- length( sx_lst )

# Inclusión pensiones ------------------------------------------------------------------------------
# message( '\tPreparando flujos' )
# load( parametros$demo_rdata_sgo_pob_proy )
# 
# cols <- c( 'pen_ant_4', 'pen_4', 'pen_ant_5', 'pen_5', 'pen_ant_7', 'pen_7_4', 
#            'pen_7_5', 'pen_ant_8', 'pen_8_4', 'pen_8_5' )
# 
# # Pensiones ajustadas por población
# pen_din <- array( 0.0, dim = c( nsx, nt, ns, nx, length( cols ) ) )
# 
# for ( i in 1:nsx ) {
#   for ( j in 1:nt ) {
#     for ( k in 1:length( cols ) ) {
#       aux <- pen_proy[ sexo == sx_lst[ i ] & t == t_lst[ j ], c( 'x', 's', cols[ k ] ), with = FALSE ]
#       aux <- dcast.data.table( data = aux, formula = s ~ x, value.var = cols[ k ], fill = 0 )  
#       aux[ , s := NULL ]
#       aux <- as.matrix( aux )
#       pen_din[ i, j, , , k ] <- aux
#     }
#   }
# }

# Proyección pensiones -----------------------------------------------------------------------------
# message( '\tProyección ajustada' )
# r_p <- c( 1, 1 + esc$apo_act$i_p[ 2:N ] )
# act_pen_pot <- 1.0
# for ( t in 2:nt ) {
#   for ( x in 1:nx ) {
#     for ( s in 1:ns ) {
#       
#       # Proyectando pensiones de vejez
#       u <- 4
#       v <- 4
#       I <- nd * ( v - 1 ) + u
#       u <- 4
#       v <- 2
#       J <- nd * ( v - 1 ) + u
#       u <- 4
#       v <- 3
#       K <- nd * ( v - 1 ) + u
#       
#       plt <- lth[ t, s, x, I ] + lth[ t, s, x, J ]
#       Plt <- pen_din[ 1, t - 1, x, s, 1 ] * lth[ t, s, x, I ] + pen_din[ 1, t, x, s, 2 ] * ( lth[ t, s, x, J ] + lth[ t, s, x, K ] )
#       pen_din[ 1, t, x, s, 1 ] = ifelse( plt > 0, Plt / plt, 0 )
#       
#       plt <- ltm[ t, s, x, I ] + ltm[ t, s, x, J ]
#       Plt <- pen_din[ 2, t - 1, x, s, 1 ] * ltm[ t, s, x, I ] + pen_din[ 2, t, x, s, 2 ] * ( ltm[ t, s, x, J ] + ltm[ t, s, x, K ] )
#       pen_din[ 2, t, x, s, 1 ] = ifelse( plt > 0, Plt / plt, 0 )
#       
#       # Proyectando pensiones de invalidez
#       u <- 5
#       v <- 5
#       I <- nd * ( v - 1 ) + u
#       u <- 5
#       v <- 2
#       J <- nd * ( v - 1 ) + u
#       u <- 5
#       v <- 3
#       K <- nd * ( v - 1 ) + u
#       
#       plt <- lth[ t, s, x, I ] + lth[ t, s, x, J ]
#       Plt <- pen_din[ 1, t - 1, x, s, 3 ] * lth[ t, s, x, I ] + pen_din[ 1, t, x, s, 4 ] * ( lth[ t, s, x, J ] + lth[ t, s, x, K ] )
#       pen_din[ 1, t, x, s, 3 ] = ifelse( plt > 0, Plt / plt, 0 )
#       
#       plt <- ltm[ t, s, x, I ] + ltm[ t, s, x, J ]
#       Plt <- pen_din[ 2, t - 1, x, s, 3 ] * ltm[ t, s, x, I ] + pen_din[ 2, t, x, s, 4 ] * ( ltm[ t, s, x, J ] + ltm[ t, s, x, K ] )
#       pen_din[ 2, t, x, s, 3 ] = ifelse( plt > 0, Plt / plt, 0 )
#       
#     }
#   }
# }

# Transformación proyección a data.table -----------------------------------------------------------
# message( '\tTransformando resultado a data.table' )
# pen_proy <- NULL
# for ( n in 1:N ) {
#   penx <- rbind( pen_f[ , n, ], pen_m[ , n, ] )
#   
#   penx <- data.table( t = t[ n ],
#                       sexo = rep( c( 'F', 'M' ), each = M ), 
#                       x = rep( x, 2 ), 
#                       pen = penx )
#   
#   setnames( penx, c( 't', 'sexo', 'x', 
#                      paste0( 'pen', c( '_ant_3', '_nue_3', '_ant_4', '_nue_4' ) ) ) )
#   
#   pen_proy <- rbind( pen_proy, penx )
#   
# }

# Unión beneficios ---------------------------------------------------------------------------------
if ( parametros$seguro == 'IVM' ) {
  ben_proy <- merge.data.table( pen_proy, aux_fun_proy, by = c( 't', 'sexo', 'x' ) )
  
} else {
  ben_proy <- copy( pen_proy )
}
setorder( ben_proy, t, sexo, x, s )

save( ben_proy, file = esc$rdata_ben_proy )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% parametros_lista ) ] )
gc()
