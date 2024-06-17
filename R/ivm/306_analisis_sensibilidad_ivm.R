message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tAgregando resultados de los análisis de sensibilidad' )

esc_sens_lst <- c( 'sensibilidad_1', 'sensibilidad_2' )

balance_anual_sens <- NULL
esc_sens_par <- list()
for ( i in 1:length( esc_sens_lst ) ) { # i <- 1
  
  load( paste0( parametros$RData_seg, 'IESS_IVM_configuracion_analisis_', esc_sens_lst[ i ], '.RData' ) )
  load( paste0( parametros$ivm_rdata_icomp_balance, esc_sens$escenario, '.RData' ) )
  
  balance_anual[ , sens := i ]
  balance_anual[ , esc := esc_sens$escenario ]
  balance_anual[ , sim := 0 ]
  balance_anual_sens <- rbind( balance_anual_sens, balance_anual )
  
  par <- list()
  if ( length( esc_sens$escenario_conf_list ) > 0 ) { 
    
    for ( j in 1:length( esc_sens$escenario_conf_list ) ) { # j <- 1
      
      load( esc_sens$escenario_conf_list[ j ] )
      load( esc$rdata_balance )
      
      aux <- copy( balance_anual )
      aux[ , sens := i ]
      aux[ , esc := esc_sens$escenario ]
      aux[ , sim := j ]
      
      if ( i == 1 ) {
        par[[ j ]] <- esc$apo_act$i_a
      } else if ( i == 2 ) {
        par[[ j ]] <- esc$apo_act$i_r
      }
      esc_sens_par[[ i ]] <- par
      
      balance_anual_sens <- rbind( balance_anual_sens, aux )
      
      rm( esc )
      
    }
  }
  rm( esc_sens )
}

balance_anual_base <- balance_anual_sens[ sim == 0 ]

balance_anual_sens <- merge.data.table( 
  balance_anual_sens, 
  balance_anual_base[ , list( sens, esc, t, V_esc = V ) ], 
  by = c( 'sens', 'esc', 't' ), 
  all.x = TRUE )

balance_anual_sens[ , r := V / V_esc - 1.0 ]

save( balance_anual_sens, esc_sens_par,
      file = parametros$ivm_rdata_ana_sen )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
