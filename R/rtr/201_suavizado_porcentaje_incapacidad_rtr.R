message( paste( rep( '-', 100 ), collapse = '' ) )

# Descripción de campos ----------------------------------------------------------------------------
# 12 = Pensionistas de riesgos del trabajo;
# 13 = Indemnizaciones por incapacidad permanente parcial;
# 14 = subsidios por incapacidad temporal;
# 15 = Montepíos de orfandad de riesgos del trabajo;
# 16 = Montepíos de viudedad de riesgos del trabajo.

#0.Cargando datos-----------------------------------------------------------------------------------
message( '\tCargando transiciones de los pensionistas del SGRT' )
load( file = parametros$demo_rdata_rtr_tran_prep )
load( file = parametros$demo_rdata_sgo_tran_prep )
load( file = parametros$demo_rdata_rtr_tasas_tran )

#Eliminando data frames no usado
rm( list = ls()[ !(ls() %in% c( "parametros",
                                "sgrt_pen_tran_pa_pt_pp_anio",
                                "sgrt_pen_tran_orf_anio",
                                "sgrt_pen_tran_viu_anio",
                                "sgrt_pen_tran_indem_anio",
                                "sgrt_pen_tran_sub_anio",
                                "sgo_act_tran_anio",
                                "tas_2_12",
                                "tas_2_13",
                                "tas_2_14",
                                "tas_0_15",
                                "tas_0_16" ) ) ] )

sal_2020 <- sgo_act_tran_anio %>% 
  filter( anio == 2020 ) %>% 
  mutate( sal_prom = S / ERx_act ) %>% 
  dplyr::select( sexo, x, l2x,  ERx_act, sal_prom )

sum(sal_2020$ERx_act, na.rm = TRUE )
sum(sal_2020$l2x, na.rm = TRUE )

tas_2_12 <- tas_2_12 %>% 
  dplyr::select( sexo, x, t_2_12_int )

tas_2_13 <- tas_2_13 %>% 
  dplyr::select( sexo, x, t_2_13_int )

tas_2_14 <- tas_2_14 %>%  
  dplyr::select( sexo, x, t_2_14_int )

age_grid <- c( 0: 105 )

# 1. Coeficientes de incapacidad total, absoluta y parcial------------------------------------------

coef_incap_12 <- sgrt_pen_tran_pa_pt_pp_anio %>%
  filter( coef_incap >= 0.8 ) %>% #No hay más ingresos por incapacidad permanente parcial
  group_by( sexo ) %>% 
  mutate( P_2020 = sum( if_else( anio == 2020,
                                 P,
                                 0 ), na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( sexo, x ) %>% 
  mutate( coef_incap_12 = weighted.mean( coef_incap,  Nx_ing, na.rm = TRUE  ) ) %>% 
  ungroup( ) %>% 
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, x, coef_incap_12, P, P_2020 ) %>% 
  full_join( ., sal_2020, by = c( 'sexo', 'x' ) ) %>% 
  full_join( ., tas_2_12, by = c( 'sexo', 'x' ) ) %>% 
  filter( x >= 15, x <= 105 ) %>% 
  arrange( sexo, x )

#Hombres
coef_incap_12_h <- coef_incap_12 %>% 
  filter( sexo == 'H' )

aux <- coef_incap_12_h %>% 
  filter( !is.na( coef_incap_12 ) ) %>% 
  filter( !( x %in% c( '20', '71', '72', '74', '78' ) ) )

plot( aux$x, aux$coef_incap_12 ) 


mod<-smooth.spline( aux$x,
                    aux$coef_incap_12, df = 3 ) 

pred <- data.frame( x = age_grid, 
                    coef_incap_12_int = predict( mod, age_grid, deriv = 0)[["y"]] )


coef_incap_12_h <- expand.grid( sexo = c( 'H' ),
                                x = age_grid ) %>% 
  full_join( ., coef_incap_12_h, by = c('x', 'sexo') ) %>% 
  full_join( ., pred, by = c('x') ) 

#Comprobación
sum( coef_incap_12_h$coef_incap_12_int * coef_incap_12_h$sal_prom * coef_incap_12_h$t_2_12_int * coef_incap_12_h$ERx_act, na.rm = TRUE )

sum( coef_incap_12_h$coef_incap_12 * coef_incap_12_h$sal_prom * coef_incap_12_h$t_2_12_int * coef_incap_12_h$ERx_act, na.rm = TRUE )

#Gráfico de control
plot( coef_incap_12_h$x, coef_incap_12_h$coef_incap_12 )
lines( coef_incap_12_h$x, coef_incap_12_h$coef_incap_12_int )

#Mujeres
coef_incap_12_m <- coef_incap_12 %>% 
  filter( sexo == 'M' )

aux <- coef_incap_12_m %>% 
  filter( !is.na( coef_incap_12 ) ) %>% 
  filter( !( x %in% c( '23', '29', '45', '55', '33' ) ) )

plot( aux$x, aux$coef_incap_12)


mod<-smooth.spline( aux$x,
                    aux$coef_incap_12, df = 4 ) 

pred <- data.frame( x = age_grid, 
                    coef_incap_12_int = predict( mod, age_grid, deriv = 0)[["y"]] )

coef_incap_12_m <- expand.grid( sexo = c( 'M' ),
                                x = age_grid ) %>% 
  full_join( ., coef_incap_12_m, by = c('x', 'sexo') ) %>% 
  full_join( ., pred, by = c('x') )

#Comprobación
sum( coef_incap_12_m$coef_incap_12_int * coef_incap_12_m$sal_prom * coef_incap_12_m$t_2_12_int * coef_incap_12_m$ERx_act, na.rm = TRUE )

#Gráfico de control
plot( coef_incap_12_m$x, coef_incap_12_m$coef_incap_12 )
lines( coef_incap_12_m$x, coef_incap_12_m$coef_incap_12_int )

coef_incap_12 <- rbind( coef_incap_12_h, coef_incap_12_m )

# 2. Tasas Indemnizaciones--------------------------------------------------------------------------
##2.1. Indemnizaciones porcentaje de incapacidad promedio-------------------------------------------

coef_incap_13 <- sgrt_pen_tran_indem_anio %>%
  group_by( sexo ) %>% 
  mutate( P_2020 = sum( if_else( anio == 2020,
                                 P,
                                 0 ), na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( sexo, x ) %>% 
  mutate( coef_incap_13 = weighted.mean(coef_incap,  Nx_ing, na.rm = TRUE  ) ) %>% 
  ungroup( ) %>% 
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, x, coef_incap_13, P, P_2020 ) %>% 
  full_join( ., sal_2020, by = c( 'sexo', 'x' ) ) %>% 
  full_join( ., tas_2_13, by = c( 'sexo', 'x' ) ) %>% 
  filter( x >= 15, x <= 105 ) %>% 
  arrange( sexo, x )

#Hombres
coef_incap_13_h <- coef_incap_13 %>% 
  filter( sexo == 'H' )

aux <- coef_incap_13_h %>% 
  filter( !is.na( coef_incap_13 ) ) %>% 
  filter( !( x %in% c( '18', '88', '72', '79' ) ) )

plot( aux$x, aux$coef_incap_13)


mod<-smooth.spline( aux$x,
                    aux$coef_incap_13, df = 4 ) 

pred <- data.frame( x = age_grid, 
                    coef_incap_13_int = predict( mod, age_grid, deriv = 0)[["y"]] )


coef_incap_13_h <- expand.grid( sexo = c( 'H' ),
                                x = age_grid ) %>% 
  full_join( ., coef_incap_13_h, by = c('x', 'sexo') ) %>% 
  full_join( ., pred, by = c('x') ) %>%
  # mutate( lx_2_13 =  t_2_13_int * ERx_act ) %>% 
  # mutate( P_int = 5 * coef_incap_13_int * sal_prom * lx_2_13 ) %>% 
  mutate( coef_incap_13 = ( coef_incap_13 * mean( P_2020, na.rm = TRUE ) /  sum ( 5 * coef_incap_13 * sal_prom * t_2_13_int * ERx_act, na.rm = TRUE ) ) ) %>% 
  mutate( coef_incap_13_int = ( coef_incap_13_int * mean( P_2020, na.rm = TRUE ) /  sum ( 5 * coef_incap_13_int * sal_prom * t_2_13_int * ERx_act, na.rm = TRUE ) ) ) %>% 
  dplyr::select( sexo, x, coef_incap_13, coef_incap_13_int, sal_prom, t_2_13_int, ERx_act )

sum( 5 * coef_incap_13_h$coef_incap_13_int * coef_incap_13_h$sal_prom * coef_incap_13_h$t_2_13_int * coef_incap_13_h$ERx_act, na.rm = TRUE )

plot( coef_incap_13_h$x, coef_incap_13_h$coef_incap_13 )
lines( coef_incap_13_h$x, coef_incap_13_h$coef_incap_13_int )

#Mujeres

coef_incap_13_m <- coef_incap_13 %>% 
  filter( sexo == 'M' )

aux <- coef_incap_13_m %>% 
  filter( !is.na( coef_incap_13 ) ) %>% 
  filter( !( x %in% c( '18', '88', '72', '79' ) ) )

plot( aux$x, aux$coef_incap_13)


mod<-smooth.spline( aux$x,
                    aux$coef_incap_13, df = 4 ) 

pred <- data.frame( x = age_grid, 
                    coef_incap_13_int = predict( mod, age_grid, deriv = 0)[["y"]] )


coef_incap_13_m <- expand.grid( sexo = c( 'M' ),
                                x = age_grid ) %>% 
  full_join( ., coef_incap_13_m, by = c('x', 'sexo') ) %>% 
  full_join( ., pred, by = c('x') ) %>%
  # mutate( lx_2_13 =  t_2_13_int * ERx_act ) %>% 
  # mutate( P_int = 5 * coef_incap_13_int * sal_prom * lx_2_13 ) %>% 
  mutate( coef_incap_13 = ( coef_incap_13 * mean( P_2020, na.rm = TRUE ) /  sum ( 5 * coef_incap_13 * sal_prom * t_2_13_int * ERx_act, na.rm = TRUE ) ) ) %>% 
  mutate( coef_incap_13_int = ( coef_incap_13_int * mean( P_2020, na.rm = TRUE ) /  sum ( 5 * coef_incap_13_int * sal_prom * t_2_13_int * ERx_act, na.rm = TRUE ) ) ) %>% 
  dplyr::select( sexo, x, coef_incap_13, coef_incap_13_int, sal_prom, t_2_13_int, ERx_act )


sum( 5 * coef_incap_13_m$coef_incap_13_int * coef_incap_13_m$sal_prom * coef_incap_13_m$t_2_13_int * coef_incap_13_m$ERx_act, na.rm = TRUE )

plot( coef_incap_13_h$x, coef_incap_13_h$coef_incap_13 )
lines( coef_incap_13_h$x, coef_incap_13_h$coef_incap_13_int )

coef_incap_13 <- rbind( coef_incap_13_h, coef_incap_13_m )
#3. Tasas de Subsidios------------------------------------------------------------------------------

## 3.1. Subsidios duración en días------------------------------------------------------------------

dias_sub_14 <- sgrt_pen_tran_sub_anio %>%
  group_by( sexo, x ) %>% 
  mutate( dias_sub_14 = weighted.mean( dias_reposo,  Nx_ing, na.rm = TRUE  ) ) %>% 
  ungroup( ) %>% 
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, x, dias_sub_14 ) %>% 
  filter( x >= 15, x <= 105 ) %>% 
  arrange( sexo, x )


#Hombres
dias_sub_14_h <- dias_sub_14 %>% 
  filter( sexo == 'H' )

aux <- dias_sub_14_h %>% 
  filter( !is.na( dias_sub_14 ) ) %>% 
  filter( !( x %in% c( '80', '81', '82', '84', '85', '87' ) ) )

plot( aux$x, aux$dias_sub_14)


mod<-smooth.spline( aux$x,
                    aux$dias_sub_14, df = 11 ) 

pred <- data.frame( x = age_grid, 
                    dias_sub_14_int = predict( mod, age_grid, deriv = 0)[["y"]] )

dias_sub_14_h <- expand.grid( sexo = c( 'H' ),
                              x = age_grid ) %>% 
  full_join( ., dias_sub_14_h, by = c('x', 'sexo') ) %>% 
  full_join( ., pred, by = c('x') ) %>%
  dplyr::select( sexo, x, dias_sub_14, dias_sub_14_int )


plot( dias_sub_14_h$x, dias_sub_14_h$dias_sub_14 )
lines( dias_sub_14_h$x, dias_sub_14_h$dias_sub_14_int )

#Mujeres
dias_sub_14_m <- dias_sub_14 %>% 
  filter( sexo == 'M' )

aux <- dias_sub_14_m %>% 
  filter( !is.na( dias_sub_14 ) ) %>% 
  filter( !( x %in% c( '76', '77', '75', '72', '73' ) ) )

plot( aux$x, aux$dias_sub_14)


mod<-smooth.spline( aux$x,
                    aux$dias_sub_14, df = 4 ) 

pred <- data.frame( x = age_grid, 
                    dias_sub_14_int = predict( mod, age_grid, deriv = 0)[["y"]] )


dias_sub_14_m <- expand.grid( sexo = c( 'M' ),
                              x = age_grid ) %>% 
  full_join( ., dias_sub_14_m, by = c('x', 'sexo') ) %>% 
  full_join( ., pred, by = c('x') ) %>%
  dplyr::select( sexo, x, dias_sub_14, dias_sub_14_int )


plot( dias_sub_14_m$x, dias_sub_14_m$dias_sub_14 )
lines( dias_sub_14_m$x, dias_sub_14_m$dias_sub_14_int )

dias_sub_14 <- rbind( dias_sub_14_m, dias_sub_14_h )

## 3.2. Subsidios porcentaje de incapacidad promedio------------------------------------------------

coef_incap_14 <- sgrt_pen_tran_sub_anio %>%
  group_by( sexo ) %>% 
  mutate( P_2020 = sum( if_else( anio == 2020,
                                 P,
                                 0 ), na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( sexo, x ) %>% 
  mutate( coef_incap_14 = weighted.mean( coef_incap,  Nx_ing, na.rm = TRUE  ) ) %>% 
  ungroup( ) %>% 
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, x, coef_incap_14, P, P_2020 ) %>% 
  full_join( ., sal_2020, by = c( 'sexo', 'x' ) ) %>% 
  full_join( ., tas_2_14, by = c( 'sexo', 'x' ) ) %>% 
  full_join( ., dias_sub_14, by = c( 'sexo', 'x' ) ) %>% 
  filter( x >= 15, x <= 105 ) %>% 
  arrange( sexo, x )


#Hombres
coef_incap_14_h <- coef_incap_14 %>% 
  filter( sexo == 'H' )

aux <- coef_incap_14_h %>% 
  filter( !is.na( coef_incap_14 ) ) %>% 
  filter( !( x %in% c( '17', '77', '78', '80', '81', '82', '83', '84', '85', '86', '87' ) ) )

plot( aux$x, aux$coef_incap_14)


mod<-smooth.spline( aux$x,
                    aux$coef_incap_14, df = 4 ) 

pred <- data.frame( x = age_grid, 
                    coef_incap_14_int = predict( mod, age_grid, deriv = 0)[["y"]] )


coef_incap_14_h <- expand.grid( sexo = c( 'H' ),
                                x = age_grid ) %>% 
  full_join( ., coef_incap_14_h, by = c('x', 'sexo') ) %>% 
  full_join( ., pred, by = c('x') )  %>%
  mutate( coef_incap_14 = ( coef_incap_14 * mean( P_2020, na.rm = TRUE ) /  sum ( dias_sub_14_int * coef_incap_14 * sal_prom * t_2_14_int * ERx_act/360, na.rm = TRUE ) ) ) %>%
  mutate( coef_incap_14_int = ( coef_incap_14_int * mean( P_2020, na.rm = TRUE ) /  sum ( dias_sub_14_int * coef_incap_14_int * sal_prom * t_2_14_int * ERx_act/360, na.rm = TRUE ) ) ) %>%
  dplyr::select( sexo, x, coef_incap_14, dias_sub_14_int, coef_incap_14_int, sal_prom, t_2_14_int, ERx_act )


sum( coef_incap_14_h$dias_sub_14_int * coef_incap_14_h$coef_incap_14_int * coef_incap_14_h$sal_prom * coef_incap_14_h$t_2_14_int * coef_incap_14_h$ERx_act/360, na.rm = TRUE )


plot( coef_incap_14_h$x, coef_incap_14_h$coef_incap_14 )
lines( coef_incap_14_h$x, coef_incap_14_h$coef_incap_14_int )

#Mujeres
coef_incap_14_m <- coef_incap_14 %>% 
  filter( sexo == 'M' )

aux <- coef_incap_14_m %>% 
  filter( !is.na( coef_incap_14 ) ) %>% 
  filter( !( x %in% c( '74', '75', '76', '77' ) ) )

plot( aux$x, aux$coef_incap_14)


mod<-smooth.spline( aux$x,
                    aux$coef_incap_14, df = 4 ) 

pred <- data.frame( x = age_grid, 
                    coef_incap_14_int = predict( mod, age_grid, deriv = 0)[["y"]] )


coef_incap_14_m <- expand.grid( sexo = c( 'M' ),
                                x = age_grid ) %>% 
  full_join( ., coef_incap_14_m, by = c('x', 'sexo') ) %>% 
  full_join( ., pred, by = c('x') )  %>%
  mutate( coef_incap_14 = ( coef_incap_14 * mean( P_2020, na.rm = TRUE ) /  sum ( dias_sub_14_int * coef_incap_14 * sal_prom * t_2_14_int * ERx_act/360, na.rm = TRUE ) ) ) %>%
  mutate( coef_incap_14_int = ( coef_incap_14_int * mean( P_2020, na.rm = TRUE ) /  sum ( dias_sub_14_int * coef_incap_14_int * sal_prom * t_2_14_int * ERx_act/360, na.rm = TRUE ) ) ) %>%
  dplyr::select( sexo, x, coef_incap_14, dias_sub_14_int, coef_incap_14_int, sal_prom, t_2_14_int, ERx_act )


sum( coef_incap_14_m$dias_sub_14_int * coef_incap_14_m$coef_incap_14_int * coef_incap_14_m$sal_prom * coef_incap_14_m$t_2_14_int * coef_incap_14_m$ERx_act/360, na.rm = TRUE )


plot( coef_incap_14_m$x, coef_incap_14_m$coef_incap_14 )
lines( coef_incap_14_m$x, coef_incap_14_m$coef_incap_14_int )

coef_incap_14 <- rbind( coef_incap_14_m, coef_incap_14_h )

# 4. Coeficientes de orfandad-----------------------------------------------------------------------

coef_incap_15 <- sgrt_pen_tran_orf_anio %>%
  filter( coef_incap > 0 ) %>%
  group_by( sexo ) %>% 
  mutate( P_2020 = sum( if_else( anio == 2020,
                                 P,
                                 0 ), na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( sexo, x ) %>% 
  mutate( coef_incap_15 = weighted.mean( coef_incap,  Nx_ing, na.rm = TRUE  ) ) %>% 
  ungroup( ) %>% 
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, x, coef_incap_15, P, P_2020 ) %>% 
  full_join( ., sal_2020, by = c( 'sexo', 'x' ) ) %>% 
  full_join( ., tas_0_15, by = c( 'sexo', 'x' ) ) %>% 
  arrange( sexo, x ) %>% 
  filter( x < 18 )

#Hombres
coef_incap_15_h <- coef_incap_15 %>% 
  filter( sexo == 'H' )

aux <- coef_incap_15_h %>% 
  filter( !is.na( coef_incap_15 ) )

plot( aux$x, aux$coef_incap_15 ) 


mod<-smooth.spline( aux$x,
                    aux$coef_incap_15, df = 3 ) 

pred <- data.frame( x = c( 0 : 17), 
                    coef_incap_15_int = predict( mod, c( 0 : 17), deriv = 0)[["y"]] )


coef_incap_15_h <- expand.grid( sexo = c( 'H' ),
                                x = c( 0 : 17) ) %>% 
  full_join( ., coef_incap_15_h, by = c('x', 'sexo') ) %>% 
  full_join( ., pred, by = c('x') ) 

#Gráfico de control
plot( coef_incap_15_h$x, coef_incap_15_h$coef_incap_15 )
lines( coef_incap_15_h$x, coef_incap_15_h$coef_incap_15_int )

#Mujeres
coef_incap_15_m <- coef_incap_15 %>% 
  filter( sexo == 'M' )

aux <- coef_incap_15_m %>% 
  filter( !is.na( coef_incap_15 ) )

plot( aux$x, aux$coef_incap_15)


mod<-smooth.spline( aux$x,
                    aux$coef_incap_15, df = 4 ) 

pred <- data.frame( x = c( 0 : 17), 
                    coef_incap_15_int = predict( mod, c( 0 : 17), deriv = 0)[["y"]] )


coef_incap_15_m <- expand.grid( sexo = c( 'M' ),
                                x = c( 0 : 17) ) %>% 
  full_join( ., coef_incap_15_m, by = c('x', 'sexo') ) %>% 
  full_join( ., pred, by = c('x') )

#Gráfico de control
plot( coef_incap_15_m$x, coef_incap_15_m$coef_incap_15 )
lines( coef_incap_15_m$x, coef_incap_15_m$coef_incap_15_int )

coef_incap_15 <- rbind( coef_incap_15_h, coef_incap_15_m ) %>% 
  full_join( expand.grid( x = age_grid,
                          sexo = c( 'H', 'M' ) ), ., by = c( 'x', 'sexo' ) ) %>%
  mutate_if( is.numeric, replace_na, 0 )

# 5. Coeficientes de viudedad-----------------------------------------------------------------------

coef_incap_16 <- sgrt_pen_tran_viu_anio %>%
  filter( coef_incap > 0 ) %>%
  group_by( sexo ) %>% 
  mutate( P_2020 = sum( if_else( anio == 2020,
                                 P,
                                 0 ), na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( sexo, x ) %>% 
  mutate( coef_incap_16 = mean( coef_incap, na.rm = TRUE  ) ) %>% 
  ungroup( ) %>% 
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, x, coef_incap_16, P, P_2020 ) %>% 
  full_join( ., sal_2020, by = c( 'sexo', 'x' ) ) %>% 
  full_join( ., tas_0_16, by = c( 'sexo', 'x' ) ) %>% 
  arrange( sexo, x ) %>% 
  filter( x >= 15, x <= 105 )

#Hombres
coef_incap_16_h <- coef_incap_16 %>% 
  filter( sexo == 'H' )

aux <- coef_incap_16_h %>% 
  filter( !is.na( coef_incap_16 ) )

plot( aux$x, aux$coef_incap_16 ) 


mod<-smooth.spline( aux$x,
                    aux$coef_incap_16, df = 3 ) 

pred <- data.frame( x = c( 15 : 105), 
                    coef_incap_16_int = predict( mod, c( 15 : 105), deriv = 0)[["y"]] )


coef_incap_16_h <- expand.grid( sexo = c( 'H' ),
                                x = c( 15 : 105) ) %>% 
  full_join( ., coef_incap_16_h, by = c('x', 'sexo') ) %>% 
  full_join( ., pred, by = c('x') ) 


#Gráfico de control
plot( coef_incap_16_h$x, coef_incap_16_h$coef_incap_16 )
lines( coef_incap_16_h$x, coef_incap_16_h$coef_incap_16_int )

#Mujeres
coef_incap_16_m <- coef_incap_16 %>% 
  filter( sexo == 'M' )

aux <- coef_incap_16_m %>% 
  filter( !is.na( coef_incap_16 ) )

plot( aux$x, aux$coef_incap_16)


mod<-smooth.spline( aux$x,
                    aux$coef_incap_16, df = 4 ) 

pred <- data.frame( x = c( 15 : 105), 
                    coef_incap_16_int = predict( mod, c( 15 : 105), deriv = 0)[["y"]] )


coef_incap_16_m <- expand.grid( sexo = c( 'M' ),
                                x = c( 15 : 105) ) %>% 
  full_join( ., coef_incap_16_m, by = c('x', 'sexo') ) %>% 
  full_join( ., pred, by = c('x') )


#Gráfico de control
plot( coef_incap_16_m$x, coef_incap_16_m$coef_incap_16 )
lines( coef_incap_16_m$x, coef_incap_16_m$coef_incap_16_int )

coef_incap_16 <- rbind( coef_incap_16_h, coef_incap_16_m ) %>% 
  full_join( expand.grid( x = age_grid,
                          sexo = c( 'H', 'M' ) ), ., by = c( 'x', 'sexo' ) ) %>%
  mutate_if( is.numeric, replace_na, 0 )


# Guarda resultados --------------------------------------------------------------------------------
message( '\tGuardando suavizamiento de tasas' )
save( dias_sub_14,
      coef_incap_14,
      coef_incap_12,
      coef_incap_13,
      coef_incap_15,
      coef_incap_16,
      file = paste0( parametros$RData_seg, 'IESS_RTR_porcentaje_incapacidad_ajustado.RData' ) )

# Limpiar Ram---------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% 'parametros' ) ] )
gc( )

