message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tablas de primas por escenario' )

escenarios <- paste0( 'escenario_', 1:9 )
nom_esc <- c( 'Legal',
              'Intermedio',
              'Pesimista',
              'Escenario 4',
              'Escenario 5',
              'Escenario 6',
              'Escenario 7',
              'Escenario 8',
              'Escenario 9' )

pri <- NULL
for ( escenario in escenarios ) {
  load( paste0( parametros$rtr_rdata_icomp_conf_esc, escenario, '.RData' ) )
  load( paste0( parametros$rtr_rdata_icomp_prima, escenario, '.RData' ) )
  load( paste0( parametros$rtr_rdata_icomp_balance, escenario, '.RData' ) )
  
  prima_med_niv <- prima[ t == parametros$horizonte]$pri_med_niv_apo_est_pen
  bal <- balance_anual[ t == max( t ),  ]$V
  
  pri <- rbind( pri, 
                c( 100 * mean( esc$apo_act$i_a[-1]), 
                   100 * mean( esc$apo_act$i_r[-1] ), 
                   100 * mean( esc$apo_act$i_sbu[-1] ), 
                   100 * mean( esc$apo_act$i_p[-1] ),
                   100 * esc$apo_act$por_apo_est[2],
                   100 * prima_med_niv,
                   bal ) )
  
}
pri <- as.data.frame( cbind( nom_esc, pri ) ) %>% 
  mutate_at( c( 2:ncol( . ) ), as.numeric )

xtb_pri <- xtable( pri, digits = c( 0, 0, rep( 4, 6 ), 2 ) )

print( xtb_pri,
       file = paste0( parametros$resultado_tablas, 'iess_tab_primas.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = nrow( pri ),
       sanitize.text.function = identity )

# Limpiando memoria RAM-----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
