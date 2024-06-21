message( paste( rep( "-", 100 ), collapse = "" ) )
#Cargando Rdatas------------------------------------------------------------------------------------
message( "\tCargando pagos del SGRT" )
load( paste0( parametros$RData_seg, "IESS_RTR_rentas.RData" ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_inventario_pensionistas.RData' ) )
load( paste0( parametros$RData_macro, 'IESS_contexto_economico.RData' ) )
#1. Funciones---------------------------------------------------------------------------------------
message( "\tCargando funciones" )

# 0. Coeficientes de Incapacidad--------------------------------------------------------------------
inventario_jubilados <- inventario_jubilados %>% 
  filter( tipo_seguro == 'RT',
          estado_prestacion %in% c( 'A', 'I' ) ) %>%
  group_by( cedula, tipo_prestacion ) %>% 
  mutate( n = n() ) %>% 
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

#1. Transiciones de activo a pensionistas de orfandad-----------------------------------------------
  
sgrt_pen_tran_orf <- prestaciones_orfandad %>%
  left_join( ., inventario_jubilados, by = c('cedula'='cedula',  'cod_tipo_prestacion'='tipo_prestacion' ) ) %>% 
  mutate( planilla = as.Date( paste0(anio, '-', mes,'-01'), "%Y-%m-%d" ) ) %>%
  group_by( cedula, anio, mes ) %>% 
  mutate(  tot_ingr =  sum(  ifelse(  rubro %in% c( 1,
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
                                                    376  ),
                                      valor,
                                      0 ), na.rm = TRUE  ),
           decimo_tercero = sum(  ifelse(  rubro %in% c( 345, 374, 376, 10  ),
                                           valor,
                                           0 ), na.rm = TRUE  ),
           decimo_cuarto = sum(  ifelse(  rubro %in% c( 9, 375  ),
                                          valor,
                                          0 ), na.rm = TRUE  ),
           renta_mensual = tot_ingr - decimo_tercero - decimo_cuarto ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, mes, cedula, .keep_all = TRUE ) %>% 
  group_by( cedula ) %>% 
  mutate( fecha_ingreso = min( planilla, na.rm = TRUE )  ) %>%
  mutate( fecha_salida = max( planilla, na.rm = TRUE )  ) %>% 
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

##1.1. (0 ---> 15) Ingreso de pensionistas de orfandad-----------------------------------------

aux_a <- sgrt_pen_tran_orf %>% 
  mutate( x = round( as.numeric( difftime( as.Date( paste0( anio, '-06-30') ), fecha_nacimiento, units = "days") ) / 365 ), 0 ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( ERx_incap = n( ),
          P = sum( P, na.rm = TRUE ),
          P_13= sum( P_13, na.rm = TRUE ),
          P_14 = sum( P_14, na.rm = TRUE ),
          P_Tot = sum( P_Tot, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select(  anio, sexo, x, coef_incap, ERx_incap, P, P_13, P_14, P_Tot )

aux_b <- sgrt_pen_tran_orf %>% 
  mutate( x = round( as.numeric( difftime( fecha_ingreso, fecha_nacimiento, units = "days") ) / 365 ), 0 ) %>% 
  distinct( ., cedula, .keep_all = TRUE ) %>% 
  filter( fecha_ingreso > as.Date( "2014-01-01" ) ) %>% 
  mutate( anio = year( fecha_ingreso ) ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( Nx_ing = n( ) ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, x, Nx_ing ) 

aux_ing <- full_join( aux_a, aux_b, by = c('anio', 'sexo', 'x') )

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
  replace( ., is.na(.), 0 ) %>% 
  mutate( total = `defunción jubilado` + `defunción afiliado` + `datos perdido` ) %>% 
  mutate( porc_J = 100 * `defunción jubilado` / total,
          porc_A = 100 * `defunción afiliado` / total,
          porc_na = 100 * `datos perdido` / total ) %>% 
  dplyr::select( anio, `defunción afiliado`, porc_A, `defunción jubilado`, porc_J, `datos perdido`, porc_na,  total ) %>% 
  arrange( anio )


##1.2 Bootsrapping imgresos de huerfanos------------------------------------------------------------


base <- sgrt_pen_tran_orf %>%  filter( !is.na( causante ) )
n = 20000


i <- data.frame( i = sample( base$i, size = n, replace = FALSE, prob = NULL) )

base <- left_join( i, base, by = 'i' )

aux_b <- base %>% 
  mutate( x = round( as.numeric( difftime( fecha_ingreso, fecha_nacimiento, units = "days") ) / 365 ), 0 ) %>% 
  distinct( ., cedula, .keep_all = TRUE ) %>% 
  filter( fecha_ingreso > as.Date( "2014-01-01" ) ) %>% 
  mutate( anio = year( fecha_ingreso ) ) %>% 
  group_by( anio, circunstancia, x ) %>% 
  mutate( Nx_ing = n( ) ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, circunstancia, x, .keep_all = TRUE ) %>% 
  dplyr::select( anio, circunstancia, x, Nx_ing )  %>% 
  group_by( anio, circunstancia ) %>%  
  mutate( N = n( ) ) %>% 
  distinct( anio, circunstancia, .keep_all = TRUE ) %>%
  dplyr::select( anio, circunstancia, N ) %>% 
  pivot_wider( names_from = circunstancia, values_from = N ) %>% 
  replace(., is.na(.), 0 ) %>% 
  mutate( total = `defunción jubilado` + `defunción afiliado` ) %>% 
  mutate( porc_J = 100 * `defunción jubilado` / total,
          porc_A = 100 * `defunción afiliado` / total ) %>% 
  dplyr::select( anio, `defunción afiliado`, porc_A, `defunción jubilado`, porc_J, total ) %>% 
  arrange( anio )

#Bootsrapping---------------------------------------------------------------------------------------

set.seed(12345)

foo <- function(data, indices, cor.type){
  dt<-data[indices,]
  c(
    cor(dt[,1], dt[,2], method=cor.type),
    median(dt[,1]),
    median(dt[,2])
  )
}

myBootstrap <- boot::boot( iris, foo, R = 1000, cor.type = 's' )


set.seed( 12345 )
sample( iris$Sepal.Length, size = 10, replace = FALSE, prob = NULL)

#Guardar en Rdatas----------------------------------------------------------------------------------
message( "\tGuardando Rdatas" )
save(  tab_evo_ben_pt,
       tab_evo_ben_pp,
       tab_evo_ben_pa,
       tab_evo_ben_vo,
       tab_evo_ben_of,
       tab_evo_monto_pp,
       tab_evo_monto_pt,
       tab_evo_monto_pa,
       tab_evo_monto_vo,
       tab_evo_monto_of,
       pir_ben_pp,
       pir_ben_pt,
       pir_ben_pa,
       pir_ben_vo,
       pir_ben_of,
       pir_pensiones_pp,
       pir_pensiones_pt,
       pir_pensiones_pa,
       pir_pensiones_vo,
       pir_pensiones_of,
       tab_rango_monto_pp,
       tab_rango_monto_pt,
       tab_rango_monto_pa,
       tab_rango_monto_vo,
       tab_rango_monto_of,
       tab_evo_ben_subsidios,
       tab_evo_monto_subsidios,
       pir_ben_subsidios,
       pir_montos_subsidios,
       tab_rango_monto_subsidios,
       tab_evo_ben_indemnizaciones,
       tab_evo_monto_indemnizaciones,
       pir_ben_indemnizaciones,
       pir_montos_indemnizaciones,
       tab_rango_monto_indemnizaciones,
       evo_er_sgo,
       evo_masa_sgo,
       dist_sal_edad_sexo,
       file = paste0(  parametros$RData_seg, 'IESS_RTR_tablas_demografia.RData'  )  )
#Limpiar Ram----------------------------------------------------------------------------------------
message(  paste(  rep( '-', 100  ), collapse = ''  )  )
rm(  list = ls(  )[ !(  ls(  ) %in% c(  'parametros'  )  ) ]  )
gc(  )