message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tGenerando tablas de primas por escenario' )

escenario <- paste0( 'escenario_', 1:3 )
nom_esc <- c( 'Legal', 'Base','Pesimista')

pri <- NULL
for( i in 1:length( escenario ) ){ # i <- 1
  load( paste0( parametros$ivm_rdata_icomp_primas, escenario[i], '.RData' ) )
  load( paste0( parametros$ivm_rdata_icomp_conf_esc, escenario[i], '.RData' ) )
  
  pri <- rbind( pri, 
                prima[ t == parametros$ivm_horizonte, 
                       list( nombre = nom_esc[ i ],
                             tasa_actuarial = 100 * esc$apo_act[t == parametros$ivm_horizonte]$i_a,
                             prima = 100 * pri_med_niv_apo_est_pen,
                             porc_apo_est = 100 * esc$apo_act[t == parametros$ivm_horizonte]$por_apo_est ) ] )
}

xtb_pri <- xtable( pri, digits = c( 0, 0, 2, 2, 2 ) )
print( xtb_pri,
       file = paste0( parametros$resultado_tablas, 'iess_tab_primas.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()
