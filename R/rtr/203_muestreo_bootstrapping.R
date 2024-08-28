message( paste( rep( "-", 100 ), collapse = "" ) )
#Cargando Rdatas------------------------------------------------------------------------------------
message( "\tCargando pagos del SGRT" )
load( paste0( parametros$RData_seg, "IESS_RTR_rentas.RData" ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_inventario_pensionistas.RData' ) )
load( paste0( parametros$RData_macro, 'IESS_contexto_economico.RData' ) )

#0. Preparando inventario de jubilados--------------------------------------------------------------
inventario_jubilados <- inventario_jubilados %>% 
  filter( tipo_seguro == 'RT',
          estado_prestacion %in% c( 'A', 'I' ) ) %>%
  group_by( cedula, tipo_prestacion ) %>% 
  mutate( n = n( ) ) %>% 
  mutate( coef_incap = max( coeficiente_real, na.rm = TRUE ),
          Sx_prom = max( promedio_sueldo_real, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( cedula, tipo_prestacion, .keep_all = TRUE ) %>% 
  dplyr::select( cedula,
                 #sexo,
                 #fecha_nacimiento,
                 fecha_derecho,
                 fecha_defuncion,
                 tipo_prestacion,
                 coef_incap,
                 Sx_prom )

## 0.1. Parámetro-----------------------------------------------------------------------------------

n = 2000

#1. Transiciones de activo a pensionistas de montepío-----------------------------------------------

##1.1. ( 0 ---> 15 ) Ingreso de pensionistas de orfandad---------------------------------------------- 

sgrt_pen_tran_orf <- prestaciones_orfandad %>%
  left_join( ., inventario_jubilados, by = c( 'cedula'='cedula', 'cod_tipo_prestacion'='tipo_prestacion' ) ) %>% 
  mutate( planilla = as.Date( paste0( anio, '-', mes,'-01' ), "%Y-%m-%d" ) ) %>%
  group_by( cedula, anio, mes ) %>% 
  mutate( tot_ingr = sum( ifelse( rubro %in% c( 1,
                                                2,
                                                3,
                                                5,
                                                9,
                                                10,
                                                22,
                                                55,
                                                60,
                                                69,
                                                89,
                                                101,
                                                102,
                                                103,
                                                345,
                                                374,
                                                375,
                                                376 ),
                                  valor,
                                  0 ), na.rm = TRUE ),
          decimo_tercero = sum( ifelse( rubro %in% c( 345, 374, 376, 10 ),
                                        valor,
                                        0 ), na.rm = TRUE ),
          decimo_cuarto = sum( ifelse( rubro %in% c( 9, 375 ),
                                       valor,
                                       0 ), na.rm = TRUE ),
          renta_mensual = tot_ingr - decimo_tercero - decimo_cuarto ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, mes, cedula, .keep_all = TRUE ) %>% 
  group_by( cedula ) %>% 
  mutate( fecha_ingreso = min( planilla, na.rm = TRUE ) ) %>%
  mutate( fecha_salida = max( planilla, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  mutate( fecha_ingreso = if_else( fecha_ingreso == as.Date( '2014-01-01' ),
                                   NA,
                                   fecha_ingreso ) ) %>% 
  mutate( fecha_salida = if_else( fecha_salida == as.Date( '2022-12-01' ),
                                  NA,
                                  fecha_salida ) ) %>% 
  group_by( anio, cedula ) %>% 
  mutate( P = sum( renta_mensual, na.rm = TRUE ),
          Sx_prom = Sx_prom,
          coef_incap = coef_incap,
          P_13 = sum( decimo_tercero, na.rm = TRUE ),
          P_14 = sum( decimo_cuarto, na.rm = TRUE ),
          P_Tot = sum( tot_ingr, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( cedula ) %>% 
  mutate( max_planilla = max( planilla, na.rm = TRUE ) ) %>%
  ungroup( ) %>% 
  distinct( ., anio, cedula, .keep_all = TRUE ) %>% 
  left_join( ., sbu %>% dplyr::select( anio, sbu ), by = 'anio' ) %>% 
  mutate( coef_decima_tercera = P_13 / P,
          coef_decima_cuarta = P_14 / sbu ) %>% 
  filter( tot_ingr > 0 ) %>% 
  dplyr::select( -rubro, -descripcion ) %>% 
  mutate( i = 1:nrow( . ) )



aux_a <- sgrt_pen_tran_orf %>% 
  mutate( x = round( as.numeric( difftime( as.Date( paste0( anio, '-06-30' ) ), fecha_nacimiento, units = "days" ) ) / 365 ), 0 ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( ERx_incap = n( ),
          P = sum( P, na.rm = TRUE ),
          P_13= sum( P_13, na.rm = TRUE ),
          P_14 = sum( P_14, na.rm = TRUE ),
          P_Tot = sum( P_Tot, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, x, coef_incap, ERx_incap, P, P_13, P_14, P_Tot )

aux_b <- sgrt_pen_tran_orf %>% 
  mutate( x = round( as.numeric( difftime( fecha_ingreso, fecha_nacimiento, units = "days" ) ) / 365 ), 0 ) %>% 
  distinct( ., cedula, .keep_all = TRUE ) %>% 
  filter( fecha_ingreso > as.Date( "2014-01-01" ) ) %>% 
  mutate( anio = year( fecha_ingreso ) ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( Nx_ing = n( ) ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, x, Nx_ing ) 

aux_ing <- full_join( aux_a, aux_b, by = c( 'anio', 'sexo', 'x' ) )

ingresos_anuales_huerfanos <- aux_ing %>% 
  group_by( anio, sexo ) %>% 
  mutate( N = n( ) ) %>% 
  distinct( anio, sexo, .keep_all = TRUE ) %>%
  dplyr::select( anio, sexo, N ) %>% 
  pivot_wider( names_from = sexo, values_from = N ) %>% 
  mutate( total = F + M ) %>% 
  mutate( porc_F = 100 * F / total,
          porc_M = 100 * M / total ) %>% 
  dplyr::select( anio, M, porc_M, F, porc_F, total )


ingresos_anuales_huerfanos_circunstancia <- sgrt_pen_tran_orf %>% 
  mutate( circunstancia = if_else( is.na( circunstancia ),
                                   'datos perdido',
                                   circunstancia ) ) %>% 
  distinct( ., cedula, .keep_all = TRUE ) %>% 
  filter( fecha_ingreso > as.Date( "2014-01-01" ) ) %>% 
  group_by( anio, circunstancia ) %>% 
  mutate( N = n( ) ) %>% 
  distinct( anio, circunstancia, .keep_all = TRUE ) %>%
  dplyr::select( anio, circunstancia, N ) %>% 
  pivot_wider( names_from = circunstancia, values_from = N ) %>% 
  replace( ., is.na( . ), 0 ) %>% 
  mutate( total = `defunción jubilado` + `defunción afiliado` + `datos perdido` ) %>% 
  mutate( porc_j = 100 * `defunción jubilado` / total,
          porc_a = 100 * `defunción afiliado` / total,
          porc_na = 100 * `datos perdido` / total ) %>% 
  dplyr::select( anio, `defunción afiliado`, porc_a, `defunción jubilado`, porc_j, `datos perdido`, porc_na, total ) %>% 
  arrange( anio ) %>% 
  clean_names( )


tab_ing_of <- as.data.frame( ingresos_anuales_huerfanos_circunstancia ) %>% 
  rbind( ., c( "Total", as.character( colSums( .[ ,2:ncol( . )], na.rm =TRUE ) ) ) ) %>% 
  mutate_at( c( 2:ncol( . ) ), as.numeric ) %>% 
  mutate( porc_j = 100 * defuncion_jubilado / total,
          porc_a = 100 * defuncion_afiliado / total,
          porc_na = 100 * datos_perdido / total )

## 1.2. ( 0 ---> 16 ) Ingreso de pensionistas de viudedad---------------------------------------------

sgrt_pen_tran_vo <- prestaciones_viudez %>%
  left_join( ., inventario_jubilados, by = c( 'cedula'='cedula', 'cod_tipo_prestacion'='tipo_prestacion' ) ) %>% 
  mutate( planilla = as.Date( paste0( anio, '-', mes,'-01' ), "%Y-%m-%d" ) ) %>%
  group_by( cedula, anio, mes ) %>% 
  mutate( tot_ingr = sum( ifelse( rubro %in% c( 1,
                                                2,
                                                3,
                                                5,
                                                9,
                                                10,
                                                22,
                                                55,
                                                60,
                                                69,
                                                89,
                                                101,
                                                102,
                                                103,
                                                345,
                                                374,
                                                375,
                                                376 ),
                                  valor,
                                  0 ), na.rm = TRUE ),
          decimo_tercero = sum( ifelse( rubro %in% c( 345, 374, 376, 10 ),
                                        valor,
                                        0 ), na.rm = TRUE ),
          decimo_cuarto = sum( ifelse( rubro %in% c( 9, 375 ),
                                       valor,
                                       0 ), na.rm = TRUE ),
          renta_mensual = tot_ingr - decimo_tercero - decimo_cuarto ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, mes, cedula, .keep_all = TRUE ) %>% 
  group_by( cedula ) %>% 
  mutate( fecha_ingreso = min( planilla, na.rm = TRUE ) ) %>%
  mutate( fecha_salida = max( planilla, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( anio, cedula ) %>% 
  mutate( P = sum( renta_mensual, na.rm = TRUE ),
          coef_incap = coef_incap,
          P_13 = sum( decimo_tercero, na.rm = TRUE ),
          P_14 = sum( decimo_cuarto, na.rm = TRUE ),
          P_Tot = sum( tot_ingr, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( cedula ) %>% 
  mutate( max_planilla = max( planilla, na.rm = TRUE ) ) %>%
  ungroup( ) %>% 
  distinct( ., anio, cedula, .keep_all = TRUE ) %>% 
  left_join( ., sbu %>% dplyr::select( anio, sbu ), by = 'anio' ) %>% 
  mutate( coef_decima_tercera = P_13 / P,
          coef_decima_cuarta = P_14 / sbu ) %>% 
  filter( tot_ingr > 0 ) %>% 
  dplyr::select( -rubro, -descripcion ) %>% 
  mutate( i = 1:nrow( . ) )


aux_a <- sgrt_pen_tran_vo %>% 
  mutate( x = round( as.numeric( difftime( as.Date( paste0( anio, '-06-30' ) ), fecha_nacimiento, units = "days" ) ) / 365 ), 0 ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( ERx_incap = n( ),
          P = sum( P, na.rm = TRUE ),
          P_13= sum( P_13, na.rm = TRUE ),
          P_14 = sum( P_14, na.rm = TRUE ),
          P_Tot = sum( P_Tot, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, x, coef_incap, ERx_incap, P, P_13, P_14, P_Tot )

aux_b <- sgrt_pen_tran_vo %>% 
  mutate( x = round( as.numeric( difftime( fecha_ingreso, fecha_nacimiento, units = "days" ) ) / 365 ), 0 ) %>% 
  distinct( ., cedula, .keep_all = TRUE ) %>% 
  filter( fecha_ingreso > as.Date( "2014-01-01" ) ) %>% 
  mutate( anio = year( fecha_ingreso ) ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( Nx_ing = n( ) ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, x, Nx_ing ) 

aux_ing <- full_join( aux_a, aux_b, by = c( 'anio', 'sexo', 'x' ) )


ingresos_anuales_viudas <- aux_ing %>% 
  group_by( anio, sexo ) %>% 
  mutate( N = n( ) ) %>% 
  distinct( anio, sexo, .keep_all = TRUE ) %>%
  dplyr::select( anio, sexo, N ) %>% 
  pivot_wider( names_from = sexo, values_from = N ) %>% 
  mutate( total = F + M ) %>% 
  mutate( porc_F = 100 * F / total,
          porc_M = 100 * M / total ) %>% 
  dplyr::select( anio, M, porc_M, F, porc_F, total )


ingresos_anuales_viudas_circunstancia <- sgrt_pen_tran_vo %>% 
  mutate( circunstancia = if_else( is.na( circunstancia ),
                                   'datos perdido',
                                   circunstancia ) ) %>% 
  distinct( ., cedula, .keep_all = TRUE ) %>% 
  filter( fecha_ingreso > as.Date( "2014-01-01" ) ) %>% 
  group_by( anio, circunstancia ) %>% 
  mutate( N = n( ) ) %>% 
  distinct( anio, circunstancia, .keep_all = TRUE ) %>%
  dplyr::select( anio, circunstancia, N ) %>% 
  pivot_wider( names_from = circunstancia, values_from = N ) %>% 
  replace( ., is.na( . ), 0 ) %>% 
  mutate( total = `defunción jubilado` + `defunción afiliado` + `datos perdido` ) %>% 
  mutate( porc_j = 100 * `defunción jubilado` / total,
          porc_a = 100 * `defunción afiliado` / total,
          porc_na = 100 * `datos perdido` / total ) %>% 
  dplyr::select( anio, `defunción afiliado`, porc_a, `defunción jubilado`, porc_j, `datos perdido`, porc_na, total ) %>% 
  arrange( anio ) %>% 
  clean_names( )

tab_ing_vo <- as.data.frame( ingresos_anuales_viudas_circunstancia ) %>% 
  rbind( ., c( "Total", as.character( colSums( .[ ,2:ncol( . )], na.rm =TRUE ) ) ) ) %>% 
  mutate_at( c( 2:ncol( . ) ), as.numeric ) %>% 
  mutate( porc_j = 100 * defuncion_jubilado / total,
          porc_a = 100 * defuncion_afiliado / total,
          porc_na = 100 * datos_perdido / total )

#2 Bootstrapping ingresos de pensionistas-----------------------------------------------------------

##2.0. Declaración función--------------------------------------------------------------------------

estadistico <- function( data ){
  #set.seed( 12345 )
  
  base <- data %>% filter( !is.na( causante ) )
  n = nrow( data )
  
  i <- data.frame( i = sample( base$i, size = n, replace = TRUE, prob = NULL ) )
  
  base <- left_join( i, base, by = 'i' )
  
  aux_b <- base %>% 
    mutate( x = round( as.numeric( difftime( fecha_ingreso, fecha_nacimiento, units = "days" ) ) / 365 ), 0 ) %>% 
    distinct( ., cedula, .keep_all = TRUE ) %>% 
    filter( fecha_ingreso > as.Date( "2014-01-01" ) ) %>% 
    mutate( anio = year( fecha_ingreso ) ) %>% 
    group_by( anio, circunstancia, x ) %>% 
    mutate( Nx_ing = n( ) ) %>% 
    ungroup( ) %>% 
    distinct( ., anio, circunstancia, x, .keep_all = TRUE ) %>% 
    dplyr::select( anio, circunstancia, x, Nx_ing ) %>% 
    group_by( anio, circunstancia ) %>% 
    mutate( N = n( ) ) %>% 
    distinct( anio, circunstancia, .keep_all = TRUE ) %>%
    dplyr::select( anio, circunstancia, N ) %>% 
    pivot_wider( names_from = circunstancia, values_from = N ) %>% 
    replace( ., is.na( . ), 0 ) %>% 
    mutate( total = `defunción jubilado` + `defunción afiliado` ) %>% 
    mutate( porc_j = 100 * `defunción jubilado` / total,
            porc_a = 100 * `defunción afiliado` / total ) %>% 
    dplyr::select( anio, `defunción afiliado`, porc_a, `defunción jubilado`, porc_j, total ) %>% 
    arrange( anio )
  
  
  porc_a <- mean( aux_b$porc_a )
  porc_j <- mean( aux_b$porc_j )
  
  return( list( porc_a = porc_a, porc_j = porc_j ) )
}



bootstrap <- function( data, n ){
  
  porc_a <- c( rep( 0, n ) )
  porc_j <- c( rep( 0, n ) )
  
  for ( i in 1:n ) {
    aux <- estadistico( data )
    
    porc_a[ i ] <- aux$porc_a
    porc_j[ i ] <- aux$porc_j
  }
  
  return( list( porc_a = porc_a, porc_j = porc_j ) )
}


## 2.1. Bootstrapping de huerfanos------------------------------------------------------------------

aux = bootstrap( sgrt_pen_tran_orf, n )

muestreo_of <- data.frame( porc_a = aux$porc_a,
                           porc_j = aux$porc_j )

tab_est_porcentaje_of <- data.frame( causante = c( 'Fallecido recientemente', 'Fallecido Jubilado' ),
                                     tamano_muestra = c( n, n ),
                                     media = c( mean( muestreo_of$porc_a ), 100 - mean( muestreo_of$porc_a ) ),
                                     sd = c( std( muestreo_of$porc_a ), std( muestreo_of$porc_j ) ),
                                     ic_inf = c( as.numeric( quantile( muestreo_of$porc_a, probs = 0.025 ) ), 
                                                 as.numeric( quantile( muestreo_of$porc_j, probs = 0.025 ) ) ), 
                                     ic_sup = c( as.numeric( quantile( muestreo_of$porc_a, probs = 0.975 ) ),
                                                 as.numeric( quantile( muestreo_of$porc_j, probs = 0.975 ) ) )
)

tab_est_beneficarios_of <- data.frame( causante = c( 'Fallecido recientemente', 'Fallecido Jubilado' ),
                                       tamano_muestra = c( n, n ),
                                       media = c( nrow( sgrt_pen_tran_orf ) * mean( muestreo_of$porc_a ) / 100,
                                                  nrow( sgrt_pen_tran_orf ) * ( 100 - mean( muestreo_of$porc_a ) ) / 100 ),
                                       sd = c( nrow( sgrt_pen_tran_orf ) * std( muestreo_of$porc_a ) / 100, nrow( sgrt_pen_tran_orf ) * std( muestreo_of$porc_j ) / 100 ),
                                       ic_inf = c( nrow( sgrt_pen_tran_orf ) * as.numeric( quantile( muestreo_of$porc_a, probs = 0.025 ) ) / 100, 
                                                   nrow( sgrt_pen_tran_orf ) * as.numeric( quantile( muestreo_of$porc_j, probs = 0.025 ) ) / 100 ), 
                                       ic_sup = c( nrow( sgrt_pen_tran_orf ) * as.numeric( quantile( muestreo_of$porc_a, probs = 0.975 ) ) / 100,
                                                   nrow( sgrt_pen_tran_orf ) * as.numeric( quantile( muestreo_of$porc_j, probs = 0.975 ) ) / 100 ),
                                       beneficiarios = c( nrow( sgrt_pen_tran_orf ) * mean( muestreo_of$porc_a ) / 100,
                                                          nrow( sgrt_pen_tran_orf ) * ( 100 - mean( muestreo_of$porc_a ) ) / 100 )
) 


porc_a_of <- mean( muestreo_of$porc_a ) / 100
porc_j_of <- ( 100 - mean( muestreo_of$porc_a ) ) / 100

## 2.2. Bootstrapping de viudas---------------------------------------------------------------------

aux = bootstrap( sgrt_pen_tran_vo, n )

muestreo_vo <- data.frame( porc_a = aux$porc_a,
                           porc_j = aux$porc_j )

tab_est_porcentaje_vo <- data.frame( causante = c( 'Fallecido recientemente', 'Fallecido Jubilado' ),
                                     tamano_muestra = c( n, n ),
                                     media = c( mean( muestreo_vo$porc_a ), 100 - mean( muestreo_vo$porc_a ) ),
                                     sd = c( std( muestreo_vo$porc_a ), std( muestreo_vo$porc_j ) ),
                                     ic_inf = c( as.numeric( quantile( muestreo_vo$porc_a, probs = 0.025 ) ), 
                                                 as.numeric( quantile( muestreo_vo$porc_j, probs = 0.025 ) ) ), 
                                     ic_sup = c( as.numeric( quantile( muestreo_vo$porc_a, probs = 0.975 ) ),
                                                 as.numeric( quantile( muestreo_vo$porc_j, probs = 0.975 ) ) )
)

tab_est_beneficarios_vo <- data.frame( causante = c( 'Fallecido recientemente', 'Fallecido Jubilado' ),
                                       tamano_muestra = c( n, n ),
                                       media = c( nrow( sgrt_pen_tran_orf ) * mean( muestreo_vo$porc_a ) / 100,
                                                  nrow( sgrt_pen_tran_orf ) * ( 100 - mean( muestreo_vo$porc_a ) ) / 100 ),
                                       sd = c( nrow( sgrt_pen_tran_orf ) * std( muestreo_vo$porc_a ) / 100, nrow( sgrt_pen_tran_orf ) * std( muestreo_vo$porc_j ) / 100 ),
                                       ic_inf = c( nrow( sgrt_pen_tran_orf ) * as.numeric( quantile( muestreo_vo$porc_a, probs = 0.025 ) ) / 100, 
                                                   nrow( sgrt_pen_tran_orf ) * as.numeric( quantile( muestreo_vo$porc_j, probs = 0.025 ) ) / 100 ), 
                                       ic_sup = c( nrow( sgrt_pen_tran_orf ) * as.numeric( quantile( muestreo_vo$porc_a, probs = 0.975 ) ) / 100,
                                                   nrow( sgrt_pen_tran_orf ) * as.numeric( quantile( muestreo_vo$porc_j, probs = 0.975 ) ) / 100 ),
                                       beneficiarios = c( nrow( sgrt_pen_tran_orf ) * mean( muestreo_vo$porc_a ) / 100,
                                                          nrow( sgrt_pen_tran_orf ) * ( 100 - mean( muestreo_vo$porc_a ) ) / 100 )
) 


porc_a_vo <- mean( muestreo_vo$porc_a ) / 100
porc_j_vo <- ( 100 - mean( muestreo_vo$porc_a ) ) / 100
n_bootstrap <- n
#Guardar en Rdatas----------------------------------------------------------------------------------
message( "\tGuardando Rdatas" )
save( muestreo_of,
      tab_est_porcentaje_of,
      tab_est_beneficarios_of,
      porc_a_of,
      porc_j_of,
      tab_ing_of,
      muestreo_vo,
      tab_est_porcentaje_vo,
      tab_est_beneficarios_vo,
      porc_a_vo,
      porc_j_vo,
      tab_ing_vo,
      n_bootstrap,
      file = paste0( parametros$RData_seg, 'IESS_RTR_bootstrap.RData' ) )
#Limpiar Ram----------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% c( 'parametros' ) ) ] )
gc( )