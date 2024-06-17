message( paste( rep('-', 100 ), collapse = '' ) )

load( paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% c( 'parametros', 'balance', 'balance_anual', 'Crecimiento_PIB_Anual',
                                 'sbu_proy') ) ] )

message( '\tAnálsis de ratios para RTR' )

pib <- as.data.table( Crecimiento_PIB_Anual )
pib <- pib[ , list( t = Fecha, pib_tasa = Crecimiento_Pib_Observado ) ]
pib <- pib[ t > parametros$anio_ini ]
pib[ , t := t - parametros$anio_ini ]
setorder( pib, t )
pib[ , pib := cumprod( 1 + pib_tasa ) * 108398e6 ]


escenarios <- paste0( 'escenario_', 1:4 )

for ( escenario in escenarios ) {
  message( '\tAnálisis ratios para el ', escenario )
  
  load( paste0( parametros$RData_seg, 'IESS_RTR_balances_', escenario, '.RData' ) )
  load( paste0( parametros$RData, 'IESS_proyeccion_salarios_', escenario, '.RData' ) )
  
  ratios <- balance[ t > 0, list( l2_cot = sum( l2_cot ), 
                                  l_pen = sum( l5 + l7 + l8 + l9 ),
                                  l5 = sum( l5 ),
                                  l7 = sum( l7 ),
                                  l8 = sum( l8 ),
                                  l9 = sum( l9 ),
                                  l_10 = sum( l_10 ),
                                  l_11 = sum( l_11 )), by = list( t ) ]
  ratios <- merge( ratios, 
                   balance_anual[ t > 0, list( t, M, A5, A7, A9, A10, A_est, B_pen, B5, B7, B8, B9, B10, B11) ],
                   by = c( 't' ) )
  
  ratios <- merge( ratios, 
                   sbu_proy[ , list( t, sbu )],
                   by = c( 't' ) )
  
  ratios <- merge( ratios, pib, by = c( 't' ) )
  ratios[ , sal_mean := M / ( l2_cot * 12 )  ]
  ratios[ , pen_mean := ( ( B5 + B7 + B8 + B9 + B10 + B11 ) / ( l5 + l7 + l8 + l9 + l_10 + l_11 ) - sbu ) / 13 ]
  ratios[ , dep_tasa := l2_cot / ( l5 + l7 + l8 + l9 + l_10 + l_11 ) ]
  ratios[ , rem_tasa := pen_mean / sal_mean ]
  ratios[ , pen_tasa := shift( pen_mean, 1, type = 'lag', fill = 0 ) ]
  ratios[ , pen_tasa := ( pen_mean - pen_tasa ) / pen_tasa ]
  ratios[ t == 1, pen_tasa := 0 ]
  ratios[ , sal_tasa := shift( sal_mean, 1, type = 'lag', fill = 0 ) ]
  ratios[ , sal_tasa := ( sal_mean - sal_tasa ) / sal_tasa ]
  ratios[ t == 1, sal_tasa := 0 ]
  ratios[ , mas_tasa := shift( M, 1, type = 'lag', fill = 0 ) ]
  ratios[ , mas_tasa := ( M - mas_tasa ) / mas_tasa ]
  ratios[ t == 1, mas_tasa := 0 ]
  ratios[ , ae_pib_tasa := A_est / pib ]
  
  
  save( ratios, 
        file = paste0( parametros$RData_seg, 'IESS_RTR_analisis_ratios_', escenario, '.RData' ) )
}

#5. Limpiando memoria RAM-----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
