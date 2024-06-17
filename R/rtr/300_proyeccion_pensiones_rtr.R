message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tBeneficios de ', parametros$seguro, ' del escenario: ', esc$nombre )

# Descripción de campos ----------------------------------------------------------------------------
# 12 = pensionistas de riesgos del trabajo;
# 13 = indemnizaciones por incapacidad permanente parcial;
# 14 = subsidios por incapacidad temporal;
# 15 = montepíos de orfandad de riesgos del trabajo;
# 16 = montepíos de viudedad de riesgos del trabajo.

# Carga información --------------------------------------------------------------------------------
#0.Cargando datos-----------------------------------------------------------------------------------
message( '\tCargando pensiones iniciales a 2020 del SGRT' )
load( paste0( parametros$RData_seg, 'IESS_RTR_pensiones_iniciales.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_pensionistas_iniciales.RData' ) )
load( esc$rdata_sal_proy )
load( paste0( parametros$RData_seg, 'IESS_RTR_porcentaje_incapacidad_ajustado.RData' ) )
load( parametros$demo_rdata_rtr_pob_proy  )
load( parametros$demo_rdata_sgo_pob_proy )
load( parametros$demo_rdata_inec_fert_model )

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% c( parametros_lista,
                                 "b_12_ini",
                                 "b_15_ini",
                                 "b_16_ini",
                                 "l_12_ini",
                                 "l_15_ini",
                                 "l_16_ini",
                                 "pob_proy_rtr",
                                 "pob_proy",
                                 "pob_proy_ts",
                                 "sal_pro",
                                 "coef_incap_12",
                                 "coef_incap_13",
                                 "coef_incap_14",
                                 "dias_sub_14",
                                 "coef_incap_15",
                                 "coef_incap_16",
                                 "tasas_macro_anuales",
                                 "coef_decima_tercera_b_12",
                                 "coef_decima_cuarta_b_12",
                                 "coef_decima_tercera_b_15",
                                 "coef_decima_cuarta_b_15",
                                 "coef_decima_tercera_b_16",
                                 "coef_decima_cuarta_b_16",
                                 "fer_dat",
                                 "nup_dat",
                                 "cen_iess_hij_alis",
                                 "cen_iess_cony_alis"  ) ) ] )

#Parámetros

coef_orfandad <- 0.4
coef_viudedad <- 0.6
hijos_prom_por_mujer <- 2.05

#1. Preparando tablas auxiliares--------------------------------------------------------------------

cen_iess_hij_alis <- fer_dat %>% 
  #arrange( x, y ) %>% 
  dplyr::select( -sexo, -sexo_dep ) %>% 
  mutate( sexo = c( rep( 'H' , nrow( . )/4 ),
                    rep( 'H' , nrow( . )/4 ),
                    rep( 'M' , nrow( . )/4 ),
                    rep( 'M' , nrow( . )/4 ) ) ) %>% 
  mutate( sexo_dep = c( rep( 'H' , nrow( . )/4 ),
                    rep( 'M' , nrow( . )/4 ),
                    rep( 'H' , nrow( . )/4 ),
                    rep( 'M' , nrow( . )/4 ) ) )


cen_iess_cony_alis <- nup_dat

pob_proy_rtr <- pob_proy_rtr %>% 
  dplyr::select( t, sexo, x, l_12, l_12_6, l_2_12, l_15, l_15_0, l_0_15, l_16, l_16_0, l_0_16 )

tas_macro_sex_edad <- esc$apo_act %>%  dplyr::select( t, i_p, sbu )

dias_sub_14 <- dias_sub_14 %>% 
  dplyr::select( sexo, x, dias_sub_14 := dias_sub_14_int)

coef_incap_12 <- coef_incap_12 %>% 
  dplyr::select( sexo, x, coef_incap_12 := coef_incap_12_int ) %>% 
  mutate_if( is.numeric, replace_na, 0 )

coef_incap_13 <- coef_incap_13 %>% 
  dplyr::select( sexo, x, coef_incap_13 := coef_incap_13_int ) %>% 
  mutate_if( is.numeric, replace_na, 0 )

coef_incap_14 <- coef_incap_14 %>% 
  dplyr::select( sexo, x, coef_incap_14 := coef_incap_14_int ) %>% 
  mutate_if( is.numeric, replace_na, 0 )

coef_incap_15 <- coef_incap_15 %>% 
  dplyr::select( sexo, x, coef_incap_15 := coef_incap_15_int ) %>% 
  mutate_if( is.numeric, replace_na, 0 )

coef_incap_16 <- coef_incap_16 %>% 
  dplyr::select( sexo, x, coef_incap_16 := coef_incap_16_int )%>% 
  mutate_if( is.numeric, replace_na, 0 )

sal_proy <- sal_proy  %>% 
  left_join( . , pob_proy_ts %>%  dplyr::select( t, s, sexo, x, l2 ), by = c( 't', 's', 'sexo', 'x' ) ) %>% 
  mutate( M = l2 * sal ) %>% 
  group_by( t, sexo, x ) %>% 
  mutate( M = sum( M, na.rm = TRUE  ) ) %>% 
  mutate( l2 = sum( l2, na.rm = TRUE  ) ) %>% 
  ungroup( ) %>% 
  distinct( ., t, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( t, sexo, x, l2, M ) %>% 
  mutate( sal_prom = M / l2 ) %>% 
  replace( is.na( . ), 0 )

sal_proy_padres <- sal_proy %>% 
  left_join( ., cen_iess_hij_alis, by = c( 'x' = 'x', 'sexo' = 'sexo' ), relationship = "many-to-many" ) %>%
  mutate( hij = q * l2 ) %>% 
  group_by( t, sexo_dep, y ) %>% 
  mutate( l2 = sum( l2, na.rm = TRUE ),
          M = sum( M, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( t, sexo_dep, y, .keep_all = TRUE ) %>% 
  mutate( sal_prom_padres = M / l2 ) %>% 
  dplyr::select( t,
                 sexo := sexo_dep,
                 x:=y,
                 sal_prom_padres ) %>% 
  na.omit( x, sexo ) %>% 
  full_join( expand.grid( t = c( 0: parametros$horizonte ),
               sexo = c( 'H', 'M' ),
               x = seq( 0, 105, 1 ) ), ., by = c( 't', 'x', 'sexo' ) ) %>% 
  mutate( sal_prom_padres = if_else( x >= 18,
                                     0,
                                     sal_prom_padres ) )

#2. Proyección de pensiones con un vector de tasas de crecimiento de entrada------------------------
message( '\tProyectando beneficios de SGRT' )

ben_proy <- expand.grid( t = 0:40,
                    sexo = c( 'H', 'M' ),
                    x = seq( 0, 105, 1 ) ) %>%
  left_join( ., tas_macro_sex_edad, by = c( 't' ) ) %>% 
  full_join( ., pob_proy_rtr, by = c( 't', 'sexo', 'x' ) ) %>% 
  full_join( ., b_12_ini %>%  dplyr::select( t, sexo, x, b_12 := b_12_int ), by = c( 't', 'sexo', 'x' ) ) %>% 
  full_join( ., b_15_ini %>%  dplyr::select( t, sexo, x, b_15 := b_15_int ), by = c( 't', 'sexo', 'x' ) ) %>% 
  full_join( ., b_16_ini %>%  dplyr::select( t, sexo, x, b_16 := b_16_int ), by = c( 't', 'sexo', 'x' ) ) %>% 
  arrange( t, sexo, x )  %>% 
  mutate_if( is.numeric, replace_na, 0 )

sal_prom_padres <- xtabs(  sal_prom_padres ~ t + x + sexo, sal_proy_padres )

sal_prom <- xtabs(  sal_prom ~ t + x + sexo, sal_proy )
  
l12 <- xtabs(  l_12 ~ t + x + sexo, ben_proy )

l2_12 <- xtabs(  l_2_12 ~ t + x + sexo, ben_proy )

l15 <- xtabs(  l_15 ~ t + x + sexo, ben_proy )

l0_15 <- xtabs(  l_0_15 ~ t + x + sexo, ben_proy )

l16 <- xtabs(  l_16 ~ t + x + sexo, ben_proy )

l0_16 <- xtabs(  l_0_16 ~ t + x + sexo, ben_proy )

b12 <- xtabs(  b_12 ~ t + x + sexo, ben_proy )

b15 <- xtabs(  b_15 ~ t + x + sexo, ben_proy )

b16 <- xtabs(  b_16 ~ t + x + sexo, ben_proy )

b16 <- xtabs(  b_16 ~ t + x + sexo, ben_proy )

i_p <-  xtabs(  i_p ~ t + x + sexo, ben_proy )

coef_incap_12 <- xtabs(  coef_incap_12 ~  x + sexo, coef_incap_12 )

coef_incap_15 <- xtabs(  coef_incap_15 ~  x + sexo, coef_incap_15 )

coef_incap_16 <- xtabs(  coef_incap_16 ~  x + sexo, coef_incap_16 )

for ( g in c( 1, 2 ) ) {
  for ( t in c( 1: parametros$horizonte ) ) {
    for ( x in c( 1: 105 ) ) { 
      b12[ t + 1, x + 1, g ] <- ( l12[ t, x, g ] * b12[ t, x, g ] * ( 1 + i_p[ t + 1, x + 1, g ] ) + l2_12[ t + 1, x + 1, g ] * sal_prom[ t + 1, x + 1, g ] * coef_incap_12[ x + 1, g ] ) / (  l12[ t, x, g ] + l2_12[ t + 1, x + 1, g ] + 0.00001 )
     
       b15[ t + 1, x + 1, g ] <- ( l15[ t, x, g ] * b15[ t, x, g ] * ( 1 + i_p[ t + 1, x + 1, g ] ) + coef_orfandad * l0_15[ t + 1, x + 1, g ] * sal_prom_padres[ t + 1, x + 1, g ] * coef_incap_15[ x + 1, g ] / hijos_prom_por_mujer ) / ( l15[ t, x, g ]  + l0_15[ t + 1, x + 1, g ] + 0.00001 )
      
       b16[ t + 1, x + 1, g ] <- ( l16[ t, x, g ] * b16[ t, x, g ] * ( 1 + i_p[ t + 1, x + 1, g ] ) + coef_viudedad * l0_16[ t + 1, x + 1, g ] * sal_prom[ t + 1, x + 1, g ] * coef_incap_16[ x + 1, g ] ) / (  l16[ t, x, g ] + l0_16[ t + 1, x + 1, g ] + 0.00001 )

    }
  }
}

b12 <- data.frame( b12 ) %>% 
  dplyr::select(  t, x, sexo, b_12 := Freq ) %>% 
  mutate( t = as.integer( t ),
          x = as.integer( x ),
          sexo = as.character( sexo ) ) %>% 
  mutate( t = t - 1,
          x = x - 1 )

b15 <- data.frame( b15 ) %>% 
  dplyr::select(  t, x, sexo, b_15 := Freq ) %>% 
  mutate( t = as.integer( t ),
          x = as.integer( x ),
          sexo = as.character( sexo ) ) %>% 
  mutate( t = t - 1,
          x = x - 1 )

b16 <- data.frame( b16 ) %>% 
  dplyr::select(  t, x, sexo, b_16 := Freq ) %>% 
  mutate( t = as.integer( t ),
          x = as.integer( x ),
          sexo = as.character( sexo ) ) %>% 
  mutate( t = t - 1,
          x = x - 1 )

ben_proy <- ben_proy %>% 
  dplyr::select( -b_12, -b_15, -b_16 ) %>% 
  full_join( ., b12, by = c( 't', 'x', 'sexo' ) ) %>% 
  full_join( ., b15, by = c( 't', 'x', 'sexo' ) ) %>% 
  full_join( ., b16, by = c( 't', 'x', 'sexo' ) ) %>% 
  dplyr::select( t, sexo, x, b_12, b_15, b_16 )


#3. Proyección de subsidios e Indemnizaciones-------------------------------------------------------

ben_proy <- ben_proy %>% 
  left_join( ., coef_incap_13, by = c( 'sexo', 'x' ) ) %>% 
  left_join( ., dias_sub_14, by = c( 'sexo', 'x' ) ) %>% 
  left_join( ., coef_incap_14, by = c( 'sexo', 'x' ) ) %>% 
  left_join( ., sal_proy, by =  c( 'sexo', 'x', 't' ) ) %>% 
  mutate( b_13 =  5 * coef_incap_13 * sal_prom,
          b_14 = dias_sub_14 * coef_incap_14 * sal_prom / 360 ) %>% 
  dplyr::select( t, sexo, x, b_12, b_13, b_14, b_15, b_16 ) %>% 
  replace( is.na( . ), 0 )


# 4. Añadir el porcentaje que representan las décimas-----------------------------------------------

ben_proy <- ben_proy %>% 
  mutate( coef_decima_tercera_b_12 = coef_decima_tercera_b_12,
          coef_decima_cuarta_b_12 = coef_decima_cuarta_b_12,
          coef_decima_tercera_b_15 = coef_decima_tercera_b_15,
          coef_decima_cuarta_b_15 = coef_decima_cuarta_b_15,
          coef_decima_tercera_b_16 = coef_decima_tercera_b_16,
          coef_decima_cuarta_b_16 = coef_decima_cuarta_b_16 )
  
# 4. Guardando resultados --------------------------------------------------------------------------
message( '\tGuardando resultados de proyección de pensiones de SGRT' )
save( coef_decima_tercera_b_12,
      coef_decima_cuarta_b_12,
      coef_decima_tercera_b_15,
      coef_decima_cuarta_b_15,
      coef_decima_tercera_b_16,
      coef_decima_cuarta_b_16,
      ben_proy,
      file = esc$rtr_rdata_icomp_proy_benef )

#Limpiar Ram----------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( parametros_lista ) ) ] )
gc( )
