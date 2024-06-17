message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tGenerando tablas de parámetros escenarios' )
# --------------------------------------------------------------------------------------------------
escenario <- paste0( 'escenario_', 1:5 )
nombres <- c( 'Base', 'Pesimista', 'Alternativo', 'Propuesto 2', 'Propuesto' )
# --------------------------------------------------------------------------------------------------
if ( parametros$seguro == 'IVM_CONST' ) {
  escenario <- paste0( 'escenario_', 7:10 )
  nombres <- c( 'Comparativo', 'Corte' )
}
# --------------------------------------------------------------------------------------------------
if ( parametros$seguro == 'PRI' ) {
  escenario <- paste0( 'escenario_', 4 )
  nombres <- c( 'Propuesto' )
}
# --------------------------------------------------------------------------------------------------
var_nom <- c( 'Tasa actuarial $i_a$',
              'Tasa crecimiento salarios $i_r$',
              'Tasa crecimiento salario b\\\'{a}sico unificado $i_s$',
              'Tasa crecimiento pensiones $i_p$',
              'Tasa crecimiento auxilios de funerales $i_f$',
              'Porcentaje aporte estatal $\\alpha_{est}$',
              'Porcentaje gasto administrativo' )
# --------------------------------------------------------------------------------------------------
aux_tot <- NULL
aux_geom_mean <-
  function( x, t = TRUE ) { 
    if ( t ) {
      n <- 1:length(x)
    } else {
      n <- 0:( length(x) - 1 )
    }
    y <- ( cumprod( x + 1 ) )^( 1 / n ) - 1
    return( last( y ) ) }

for( i in 1:length( escenario ) ){ # i <- 5
  load( file = paste0( parametros$RData_seg, 'IESS_IVM_configuracion_2020_', escenario[i], '.RData' ) )
  
  if ( parametros$seguro == 'IVM_CONST' ) {
    aux <- data.table( 
      nom = var_nom,
      val = c( aux_geom_mean( esc$apo_act$i_a ), 
               aux_geom_mean( esc$apo_act$i_r ), 
               aux_geom_mean( esc$apo_act$i_sbu ), 
               aux_geom_mean( esc$apo_act$i_p ), 
               aux_geom_mean( esc$apo_act$i_f ), 
               aux_geom_mean( esc$apo_act$por_apo_est ), 
               aux_geom_mean( esc$apo_act$por_gast ) ) )
  }else if ( parametros$seguro == 'PRI' ) { # sin uso al momento
    aux <- data.table( 
      nom = var_nom,
      val = c( esc$i_a, esc$i_r, esc$i_sbu, esc$i_p, esc$i_f, esc$aporte_estado, 
               esc$porcentaje_gasto ) )
  }else{
    aux <- data.table( 
      nom = var_nom,
      val = c( aux_geom_mean( esc$apo_act$i_a[-c(1)] ), 
               aux_geom_mean( esc$apo_act$i_r[-c(1)] ),
               aux_geom_mean( esc$apo_act$i_sbu[-c(1)] ), 
               aux_geom_mean( esc$apo_act$i_p[-c(1)] ),
               aux_geom_mean( esc$apo_act$i_f[-c(1)] ), 
               aux_geom_mean( esc$apo_act$por_apo_est ), 
               aux_geom_mean( esc$apo_act$por_gast ) ) )
  }
  aux[ , val := 100 * val ]
  aux_tot <- cbind( aux_tot, aux[ , list( val ) ] )
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_tab_conf_', escenario[i], '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
}

setnames( aux_tot, escenario )
aux_tot <- aux_tot[ , 1:3, with = FALSE ]
aux_tot[ , nom := var_nom ]
aux_tot <- aux_tot[ , c( ncol( aux_tot ), 1:( ncol( aux_tot ) - 1 ) ), with = FALSE ]
xtb_aux_tot <- xtable( aux_tot, digits = c( 0, 0, rep( 2, ncol( aux_tot ) - 1 ) ) )
# --------------------------------------------------------------------------------------------------
print( xtb_aux_tot,
       file = paste0( parametros$resultado_tablas, 'iess_tab_conf_escenarios.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )
# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()
