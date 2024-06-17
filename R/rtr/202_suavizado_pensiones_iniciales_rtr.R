message( paste( rep('-', 100 ), collapse = '' ) )

# Descripción de campos ----------------------------------------------------------------------------
# 12 = pensionistas de riesgos del trabajo;
# 13 = indemnizaciones por incapacidad permanente parcial;
# 14 = subsidios por incapacidad temporal;
# 15 = montepíos de orfandad de riesgos del trabajo;
# 16 = montepíos de viudedad de riesgos del trabajo.

#0.Cargando datos-----------------------------------------------------------------------------------
message( '\tCargando transiciones de los pensionistas del SGRT' )
load( parametros$demo_rdata_rtr_tran_prep )
load( paste0( parametros$RData_seg, 'IESS_RTR_pensionistas_iniciales.RData' ) )

#Eliminando data frames no usado
rm( list = ls()[ !(ls() %in% c( "parametros",
                                "sgrt_pen_tran_pa_pt_pp",
                                "sgrt_pen_tran_orf",
                                "sgrt_pen_tran_viu",
                                "sgrt_pen_tran_pa_pt_pp_anio",
                                "sgrt_pen_tran_orf_anio",
                                "sgrt_pen_tran_viu_anio",
                                "l_12_ini",
                                "l_15_ini",
                                "l_16_ini" ) ) ] )

#1. Suavizado de pensiones iniciales de PA, PT y PP, sin decimos-----------------------------------

aux <- sgrt_pen_tran_pa_pt_pp %>% 
  filter( anio == '2020' ) %>% 
  mutate( coef_decima_tercera = mean( coef_decima_tercera, na.rm = TRUE ) ) %>%
  mutate( coef_decima_cuarta = mean( coef_decima_cuarta, na.rm = TRUE ) ) %>%
  distinct( anio, .keep_all = TRUE ) %>% 
  dplyr::select( coef_decima_tercera, coef_decima_cuarta )

coef_decima_tercera_b_12 <- aux$coef_decima_tercera 
coef_decima_cuarta_b_12 <- aux$coef_decima_cuarta

b_12_ini <- sgrt_pen_tran_pa_pt_pp_anio %>% 
  filter( anio == '2020' ) %>% 
  group_by( sexo, x ) %>% 
  mutate( b_12 = P / ERx_incap ) %>% 
  ungroup( ) %>% 
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, x, b_12 ) %>% 
  arrange( sexo, x ) %>% 
  full_join( ., l_12_ini %>% dplyr::select( sexo, x, l_12, l_12_int ), by = c( 'sexo', 'x' ) ) %>% 
  arrange( sexo, x )

##1.1. Hombres--------------------------------------------------------------------------------------
b_12_ini_h <- b_12_ini %>% 
  filter( sexo == 'H' )

aux <- b_12_ini_h %>% 
  na.omit( . ) %>% 
  filter( !(x %in% c( '97', '100' ) ) )

plot( aux$x, aux$b_12 )


mod<-smooth.spline( aux$x,
                    aux$b_12, df = 6 ) 

pred <- data.frame( x = seq( 15, 105, 1 ), 
                    b_12_int = predict( mod, seq( 15, 105, 1 ), deriv = 0)[["y"]] ) %>%
  mutate( b_12_int = if_else( b_12_int < 0,
                                0,
                                b_12_int ) )

b_12_ini_h <- expand.grid( t = 0,
                             sexo = c( 'H' ),
                             x = c( 15:105 ) )%>% 
  full_join( ., b_12_ini_h, by = c( 'x', 'sexo' ) ) %>%
  full_join( ., pred, by = 'x' ) %>% 
  mutate( b_12_int = b_12_int * sum( b_12 * l_12, na.rm = TRUE  ) / sum( b_12_int * l_12_int, na.rm = TRUE  ) )

#Gráfico de Control
plot( b_12_ini_h$x, b_12_ini_h$b_12 )
lines( b_12_ini_h$x, b_12_ini_h$b_12_int )

#Comprobación
sum( b_12_ini_h$b_12 * b_12_ini_h$l_12, na.rm = TRUE  )
sum( b_12_ini_h$b_12_int * b_12_ini_h$l_12_int, na.rm = TRUE  )


##1.2. Mujeres--------------------------------------------------------------------------------------
b_12_ini_m <- b_12_ini %>% 
  filter( sexo == 'M' )

aux <- b_12_ini_m %>% 
  na.omit( . ) %>% 
  filter( !(x %in% c( '27' ) ) )

plot( aux$x, aux$b_12 )


mod<-smooth.spline( aux$x,
                    aux$b_12, df = 6 ) 

pred <- data.frame( x = seq( 15, 105, 1 ), 
                    b_12_int = predict( mod, seq( 15, 105, 1 ), deriv = 0)[["y"]] ) %>%
  mutate( b_12_int = if_else( b_12_int < 0,
                                0,
                                b_12_int ) )

b_12_ini_m <- expand.grid( t = 0,
                             sexo = c( 'M' ),
                             x = c( 15:105 ) )%>% 
  full_join( ., b_12_ini_m, by = c( 'x', 'sexo' ) ) %>%
  full_join( ., pred, by = 'x' ) %>% 
  mutate( b_12_int = b_12_int * sum( b_12 * l_12, na.rm = TRUE  ) / sum( b_12_int * l_12_int, na.rm = TRUE  ) )

#Gráfico de Control
plot( b_12_ini_m$x, b_12_ini_m$b_12 )
lines( b_12_ini_m$x, b_12_ini_m$b_12_int )

#Comprobación
sum( b_12_ini_m$b_12 * b_12_ini_m$l_12, na.rm = TRUE  )
sum( b_12_ini_m$b_12_int * b_12_ini_m$l_12_int, na.rm = TRUE  )


#2. Suavizado de pensiones iniciales de orfandad, sin decimos--------------------------------------

aux <- sgrt_pen_tran_orf %>% 
  filter( anio == '2020' ) %>% 
  mutate( coef_decima_tercera = mean( coef_decima_tercera, na.rm = TRUE ) ) %>%
  mutate( coef_decima_cuarta = mean( coef_decima_cuarta, na.rm = TRUE ) ) %>%
  distinct( anio, .keep_all = TRUE ) %>% 
  dplyr::select( coef_decima_tercera, coef_decima_cuarta )

coef_decima_tercera_b_15 <- aux$coef_decima_tercera 
coef_decima_cuarta_b_15 <- aux$coef_decima_cuarta

b_15_ini <- sgrt_pen_tran_orf_anio %>% 
  filter( anio == '2020' ) %>% 
  group_by( sexo, x ) %>% 
  mutate( b_15 = P / ERx_incap ) %>% 
  ungroup( ) %>% 
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, x, b_15 ) %>% 
  arrange( sexo, x ) %>% 
  full_join( ., l_15_ini %>% dplyr::select( sexo, x, l_15, l_15_int ), by = c( 'sexo', 'x' ) ) %>% 
  arrange( sexo, x )

##2.1. Hombres--------------------------------------------------------------------------------------
b_15_ini_h <- b_15_ini %>% 
  filter( sexo == 'H' )

aux <- b_15_ini_h %>% 
  na.omit( . ) %>% 
  filter( !(x %in% c( '84' ) ) )

plot( aux$x, aux$b_15 )


mod<-smooth.spline( aux$x,
                    aux$b_15, df = 6 ) 

pred <- data.frame( x = seq( 0, 105, 1 ), 
                    b_15_int = predict( mod, seq( 0, 105, 1 ), deriv = 0)[["y"]] ) %>%
  mutate( b_15_int = if_else( b_15_int < 0,
                                0,
                                b_15_int ) )

b_15_ini_h <- expand.grid( t = 0,
                             sexo = c( 'H' ),
                             x = c( 0:105 ) )%>% 
  full_join( ., b_15_ini_h, by = c( 'x', 'sexo' ) ) %>%
  full_join( ., pred, by = 'x' ) %>% 
  mutate( b_15_int = b_15_int * sum( b_15 * l_15, na.rm = TRUE  ) / sum( b_15_int * l_15_int, na.rm = TRUE  ) )

#Gráfico de Control
plot( b_15_ini_h$x, b_15_ini_h$b_15 )
lines( b_15_ini_h$x, b_15_ini_h$b_15_int )

#Comprobación
sum( b_15_ini_h$b_15 * b_15_ini_h$l_15, na.rm = TRUE  )
sum( b_15_ini_h$b_15_int * b_15_ini_h$l_15_int, na.rm = TRUE  )


##2.2. Mujeres--------------------------------------------------------------------------------------
b_15_ini_m <- b_15_ini %>% 
  filter( sexo == 'M' )

aux <- b_15_ini_m %>% 
  na.omit( . ) %>% 
  filter( !(x %in% c( '92', '85' ) ) )

plot( aux$x, aux$b_15 )


mod<-smooth.spline( aux$x,
                    aux$b_15, df = 6 ) 

pred <- data.frame( x = seq( 0, 105, 1 ), 
                    b_15_int = predict( mod, seq( 0, 105, 1 ), deriv = 0)[["y"]] ) %>%
  mutate( b_15_int = if_else( b_15_int < 0,
                                0,
                                b_15_int ) )

b_15_ini_m <- expand.grid( t = 0,
                             sexo = c( 'M' ),
                             x = c( 0:105 ) )%>% 
  full_join( ., b_15_ini_m, by = c( 'x', 'sexo' ) ) %>%
  full_join( ., pred, by = 'x' ) %>% 
  mutate( b_15_int = b_15_int * sum( b_15 * l_15, na.rm = TRUE  ) / sum( b_15_int * l_15_int, na.rm = TRUE  ) )

#Gráfico de Control
plot( b_15_ini_m$x, b_15_ini_m$b_15 )
lines( b_15_ini_m$x, b_15_ini_m$b_15_int )

#Comprobación
sum( b_15_ini_m$b_15 * b_15_ini_m$l_15, na.rm = TRUE  )
sum( b_15_ini_m$b_15_int * b_15_ini_m$l_15_int, na.rm = TRUE  )

#3. Suavizado de pensiones iniciales de viudedad, sin decimos--------------------------------------
aux <- sgrt_pen_tran_viu %>% 
  filter( anio == '2020' ) %>% 
  mutate( coef_decima_tercera = mean( coef_decima_tercera, na.rm = TRUE ) ) %>%
  mutate( coef_decima_cuarta = mean( coef_decima_cuarta, na.rm = TRUE ) ) %>%
  distinct( anio, .keep_all = TRUE ) %>% 
  dplyr::select( coef_decima_tercera, coef_decima_cuarta )

coef_decima_tercera_b_16 <- aux$coef_decima_tercera 
coef_decima_cuarta_b_16 <- aux$coef_decima_cuarta

b_16_ini <- sgrt_pen_tran_viu_anio %>% 
  filter( anio == '2020' ) %>% 
  group_by( sexo, x ) %>% 
  mutate( b_16 = P / ERx_incap ) %>% 
  ungroup( ) %>% 
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, x, b_16 ) %>% 
  arrange( sexo, x ) %>% 
  full_join( ., l_16_ini %>% dplyr::select( sexo, x, l_16, l_16_int ), by = c( 'sexo', 'x' ) ) %>% 
  arrange( sexo, x )

##3.1. Hombres--------------------------------------------------------------------------------------
b_16_ini_h <- b_16_ini %>% 
  filter( sexo == 'H' )

aux <- b_16_ini_h %>% 
  na.omit( . ) %>% 
  filter( !(x %in% c( '106' ) ) )

plot( aux$x, aux$b_16 )


mod<-smooth.spline( aux$x,
                    aux$b_16, df = 6 ) 

pred <- data.frame( x = seq( 15, 105, 1 ), 
                    b_16_int = predict( mod, seq( 15, 105, 1 ), deriv = 0)[["y"]] ) %>%
  mutate( b_16_int = if_else( b_16_int < 0,
                                0,
                                b_16_int ) )

b_16_ini_h <- expand.grid( t = 0,
                             sexo = c( 'H' ),
                             x = c( 15:105 ) )%>% 
  full_join( ., b_16_ini_h, by = c( 'x', 'sexo' ) ) %>%
  full_join( ., pred, by = 'x' ) %>% 
  mutate( b_16_int = b_16_int * sum( b_16 * l_16, na.rm = TRUE  ) / sum( b_16_int * l_16_int, na.rm = TRUE  ) )

#Gráfico de Control
plot( b_16_ini_h$x, b_16_ini_h$b_16 )
lines( b_16_ini_h$x, b_16_ini_h$b_16_int )

#Comprobación
sum( b_16_ini_h$b_16 * b_16_ini_h$l_16, na.rm = TRUE  )
sum( b_16_ini_h$b_16_int * b_16_ini_h$l_16_int, na.rm = TRUE  )


##3.2. Mujeres--------------------------------------------------------------------------------------
b_16_ini_m <- b_16_ini %>% 
  filter( sexo == 'M' )

aux <- b_16_ini_m %>% 
  na.omit( . ) %>% 
  filter( !(x %in% c( '92', '85' ) ) )

plot( aux$x, aux$b_16 )


mod<-smooth.spline( aux$x,
                    aux$b_16, df = 6 ) 

pred <- data.frame( x = seq( 15, 105, 1 ), 
                    b_16_int = predict( mod, seq( 15, 105, 1 ), deriv = 0)[["y"]] ) %>%
  mutate( b_16_int = if_else( b_16_int < 0,
                                0,
                                b_16_int ) )

b_16_ini_m <- expand.grid( t = 0,
                             sexo = c( 'M' ),
                             x = c( 15:105 ) )%>% 
  full_join( ., b_16_ini_m, by = c( 'x', 'sexo' ) ) %>%
  full_join( ., pred, by = 'x' ) %>% 
  mutate( b_16_int = b_16_int * sum( b_16 * l_16, na.rm = TRUE  ) / sum( b_16_int * l_16_int, na.rm = TRUE  ) )

#Gráfico de Control
plot( b_16_ini_m$x, b_16_ini_m$b_16 )
lines( b_16_ini_m$x, b_16_ini_m$b_16_int )

#Comprobación
sum( b_16_ini_m$b_16 * b_16_ini_m$l_16, na.rm = TRUE  )
sum( b_16_ini_m$b_16_int * b_16_ini_m$l_16_int, na.rm = TRUE  )


#4. Consolidación-----------------------------------------------------------------------------------

b_12_ini <- rbind( b_12_ini_h, b_12_ini_m )
b_15_ini <- rbind( b_15_ini_h, b_15_ini_m )
b_16_ini <- rbind( b_16_ini_h, b_16_ini_m )

# Guarda resultados --------------------------------------------------------------------------------
message( '\tGuardando suavizamiento de tasas' )
save( coef_decima_tercera_b_12,
      coef_decima_cuarta_b_12,
      coef_decima_tercera_b_15,
      coef_decima_cuarta_b_15,
      coef_decima_tercera_b_16,
      coef_decima_cuarta_b_16,
      b_12_ini,
      b_15_ini, 
      b_16_ini,
      file = paste0( parametros$RData_seg, 'IESS_RTR_pensiones_iniciales.RData' ) )

# Limpiar Ram---------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% 'parametros' ) ] )
gc( )