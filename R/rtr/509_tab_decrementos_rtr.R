message( paste( rep( '-', 100 ), collapse = '' ) )

# Cargando datos -----------------------------------------------------------------------------------
load( parametros$demo_rdata_rtr_tasas_tran )
load( paste0( parametros$RData_seg, 'IESS_RTR_porcentaje_incapacidad_ajustado.RData' ) )

#1. Seleccionando variables-------------------------------------------------------------------------
tas_2_12 <- tas_2_12 %>% 
  dplyr::select( x, sexo, t_2_12 := t_2_12_int)

tas_12_6 <- tas_12_6 %>% 
  dplyr::select( x, sexo, t_12_6 := t_12_6_int)

tas_2_13 <- tas_2_13 %>% 
  dplyr::select( x, sexo, t_2_13 := t_2_13_int )

tas_2_14 <- tas_2_14 %>% 
  dplyr::select( x, sexo, t_2_14 := t_2_14_int )

tas_0_15 <- tas_0_15 %>% 
  dplyr::select( x, sexo, t_0_15 := t_0_15_int )

tas_15_0 <- tas_15_0 %>% 
  dplyr::select( x, sexo, t_15_0 := t_15_0_int )

tas_0_16 <- tas_0_16 %>% 
  dplyr::select( x, sexo, t_0_16 := t_0_16_int )

tas_16_0 <- tas_16_0 %>% 
  dplyr::select( x, sexo, t_16_0 := t_16_0_int)

coef_incap_13 <- coef_incap_13 %>% 
  dplyr::select( x, sexo, coef_incap_13 := coef_incap_13_int ) 

coef_incap_14 <- coef_incap_14 %>% 
  dplyr::select( x, sexo, coef_incap_14 := coef_incap_14_int ) 

dias_sub_14 <- dias_sub_14 %>% 
  dplyr::select( x, sexo, dias_sub_14 := dias_sub_14_int ) 

#2. Generando tabla de mortalidad pensionistas de SGRT----------------------------------------------
message( 
  '\tGenerando tabla de mortalidad pensionistas de incapacidad permanete absoluta, total y parcial'
 )

tas_2_12_m <- tas_2_12 %>% 
  filter( sexo == 'M' ) %>% 
  dplyr::select( -sexo )

tas_2_12_h <- tas_2_12 %>% 
  filter( sexo == 'H' ) %>% 
  dplyr::select( -sexo )

tas_12_6_m <- tas_12_6 %>% 
  filter( sexo == 'M' ) %>% 
  dplyr::select( -sexo )

tas_12_6_h <- tas_12_6 %>% 
  filter( sexo == 'H' ) %>% 
  dplyr::select( -sexo )

tas_2_13_m <- tas_2_13 %>% 
  filter( sexo == 'M' ) %>% 
  dplyr::select( -sexo )

tas_2_13_h <- tas_2_13 %>% 
  filter( sexo == 'H' ) %>% 
  dplyr::select( -sexo )

tas_2_14_m <- tas_2_14 %>% 
  filter( sexo == 'M' ) %>% 
  dplyr::select( -sexo )

tas_2_14_h <- tas_2_14 %>% 
  filter( sexo == 'H' ) %>% 
  dplyr::select( -sexo )

tas_0_15_m <- tas_0_15 %>% 
  filter( sexo == 'M' ) %>% 
  dplyr::select( -sexo )

tas_0_15_h <- tas_0_15 %>% 
  filter( sexo == 'H' ) %>% 
  dplyr::select( -sexo )

tas_15_0_m <- tas_15_0 %>% 
  filter( sexo == 'M' ) %>% 
  dplyr::select( -sexo )

tas_15_0_h <- tas_15_0 %>% 
  filter( sexo == 'H' ) %>% 
  dplyr::select( -sexo )

tas_0_16_m <- tas_0_16 %>% 
  filter( sexo == 'M' ) %>% 
  dplyr::select( -sexo )

tas_0_16_h <- tas_0_16 %>% 
  filter( sexo == 'H' ) %>% 
  dplyr::select( -sexo )

tas_16_0_m <- tas_16_0 %>% 
  filter( sexo == 'M' ) %>% 
  dplyr::select( -sexo )

tas_16_0_h <- tas_16_0 %>% 
  filter( sexo == 'H' ) %>% 
  dplyr::select( -sexo )

##2.1. Generando tabla de entradas de pensionistas de SGRT------------------------------------------

aux <- expand.grid( x = seq( 1, 105, 1 ) ) %>%  
  left_join( ., tas_2_12_m, by = 'x' ) %>% 
  left_join( ., tas_0_15_m, by = 'x' ) %>% 
  left_join( ., tas_0_16_m, by = 'x' ) %>% 
  cbind( ., y = seq( 1, 105, 1 ) ) %>% 
  left_join( ., tas_2_12_h, by = 'x' ) %>% 
  left_join( ., tas_0_15_h, by = 'x' ) %>% 
  left_join( ., tas_0_16_h, by = 'x' )

xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 6, 3 ), 0, rep( 6, 3 ) ) )

print( 
  xtb_aux,
  file = paste0( parametros$resultado_tablas, 'iess_pen_incap_ent_rtr.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)

##2.2. Generando tabla de mortalidad pensionistas de SGRT-------------------------------------------

aux <- expand.grid( x = seq( 1, 105, 1 ) ) %>%  
  left_join( ., tas_12_6_m, by = 'x' ) %>% 
  left_join( ., tas_15_0_m, by = 'x' ) %>% 
  left_join( ., tas_16_0_m, by = 'x' ) %>% 
  cbind( ., y = seq( 1, 105, 1 ) ) %>% 
  left_join( ., tas_12_6_m, by = 'x' ) %>% 
  left_join( ., tas_15_0_m, by = 'x' ) %>% 
  left_join( ., tas_16_0_m, by = 'x' )

xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 6, 3 ), 0, rep( 6, 3 ) ) )

print( 
  xtb_aux,
  file = paste0( parametros$resultado_tablas, 'iess_pen_incap_mort_rtr.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

#3. Tabla de siniestralidad de Indemnizaciones------------------------------------------------------
message( '\tTabla de siniestralidad de Indemnizaciones' )

tas_2_13_m <- tas_2_13 %>% 
  filter( sexo == 'M' ) %>% 
  dplyr::select( -sexo )

tas_2_13_h <- tas_2_13 %>% 
  filter( sexo == 'H' ) %>% 
  dplyr::select( -sexo )

coef_incap_13_m <- coef_incap_13 %>% 
  filter( sexo == 'M' ) %>% 
  dplyr::select( -sexo )

coef_incap_13_h <- coef_incap_13 %>% 
  filter( sexo == 'H' ) %>% 
  dplyr::select( -sexo )

aux <- expand.grid( x = seq( 15, 105, 1 ) ) %>%
  left_join( ., tas_2_13_m, by = 'x' ) %>% 
  left_join( ., coef_incap_13_m, by = 'x' ) %>%
  cbind( ., y = seq( 15, 105, 1 ) ) %>% 
  left_join( ., tas_2_13_h, by = 'x' ) %>% 
  left_join( ., coef_incap_13_h, by = 'x' )

xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 9, 2 ), 0, rep( 9, 2 ) ) )

print( 
  xtb_aux,
  file = paste0( parametros$resultado_tablas, 'iess_indem_sinies_rtr.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

#4. Tabla siniestralidad de subsidios---------------------------------------------------------------
message( '\tTabla de siniestralidad de los subsidios otorgados por incapacidad temporal' )

tas_2_14_m <- tas_2_14 %>% 
  filter( sexo == 'M' ) %>% 
  dplyr::select( -sexo )

tas_2_14_h <- tas_2_14 %>% 
  filter( sexo == 'H' ) %>% 
  dplyr::select( -sexo )

coef_incap_14_m <- coef_incap_14 %>% 
  filter( sexo == 'M' ) %>% 
  dplyr::select( -sexo )

coef_incap_14_h <- coef_incap_14 %>% 
  filter( sexo == 'H' ) %>% 
  dplyr::select( -sexo )

dias_sub_14_h <- dias_sub_14 %>% 
  filter( sexo == 'H' ) %>% 
  dplyr::select( -sexo )

dias_sub_14_m <- dias_sub_14 %>% 
  filter( sexo == 'M' ) %>% 
  dplyr::select( -sexo )

aux <- expand.grid( x = seq( 15, 105, 1 ) ) %>%
  left_join( ., tas_2_14_m, by = 'x' ) %>% 
  left_join( ., coef_incap_14_m, by = 'x' ) %>%
  left_join( ., dias_sub_14_m, by = 'x' ) %>%
  cbind( ., y = seq( 15, 105, 1 ) ) %>% 
  left_join( ., tas_2_14_h, by = 'x' ) %>% 
  left_join( ., coef_incap_14_h, by = 'x' ) %>% 
  left_join( ., dias_sub_14_h, by = 'x' )

xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 8, 3 ), 0,  rep( 8, 3 ) ) )

print( 
  xtb_aux,
  file = paste0( parametros$resultado_tablas, 'iess_subs_sinies_rtr.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

# Tabla de entrada de nuevos pensionistas-----------------------------------------------------------
message( 
  '\tGenerando tabla de entrada de pensionistas de incapacidad y montepío'
)

tas_2_12_m <- tas_2_12 %>% 
  filter( sexo == 'M' ) %>% 
  dplyr::select( -sexo )

tas_2_12_h <- tas_2_12 %>% 
  filter( sexo == 'H' ) %>% 
  dplyr::select( -sexo )

tas_0_15_m <- tas_0_15 %>% 
  filter( sexo == 'M' ) %>% 
  dplyr::select( -sexo )

tas_0_15_h <- tas_0_15 %>% 
  filter( sexo == 'H' ) %>% 
  dplyr::select( -sexo )

tas_0_16_m <- tas_0_16 %>% 
  filter( sexo == 'M' ) %>% 
  dplyr::select( -sexo )

tas_0_16_h <- tas_0_16 %>% 
  filter( sexo == 'H' ) %>% 
  dplyr::select( -sexo )

aux <- expand.grid( x = seq( 1, 105, 1 ) ) %>%  
  left_join( ., tas_2_12_m, by = 'x' ) %>% 
  left_join( ., tas_0_15_m, by = 'x' ) %>% 
  left_join( ., tas_0_16_m, by = 'x' ) %>% 
  cbind( ., y = seq( 1, 105, 1 ) ) %>% 
  left_join( ., tas_2_12_m, by = 'x' ) %>% 
  left_join( ., tas_0_15_m, by = 'x' ) %>% 
  left_join( ., tas_0_16_m, by = 'x' )

xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 6, 3 ), 0, rep( 6, 3 ) ) )

print( 
  xtb_aux,
  file = paste0( parametros$resultado_tablas, 'iess_pen_ent_pen_rtr.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)
# Limpieza de RAM-----------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% c( 'parametros' ) )] )
gc(  )
