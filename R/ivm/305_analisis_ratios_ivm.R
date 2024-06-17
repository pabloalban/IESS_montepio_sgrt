message( paste( rep('-', 100 ), collapse = '' ) )

# Carga de información macroeconómica --------------------------------------------------------------
load( parametros$macro_rdata_macro_est )

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% c( 'parametros', 'tasas_macro_anuales', 'predicciones_anuales' ) ) ] )

# Análisis de ratios -------------------------------------------------------------------------------
message( '\tAnálsis de ratios para IVM' )

pib <- as.data.table( predicciones_anuales )
pib <- pib[ anio >= parametros$anio_ini, list( t = anio - parametros$anio_ini, anio, pib = 1000 * pib_anual, sbu = sbu_anual ) ]
setorder( pib, t )

N <- 1:6
if ( parametros$ivm_cal_add_esc ) {
  N <- 1:11
}
escenarios <- paste0( 'escenario_', N )

for ( escenario in escenarios ) {
  message( '\tAnálisis ratios para el ', escenario )
  
  load( paste0( parametros$ivm_rdata_icomp_balance, escenario, '.RData' ) )
  
  ratios <- balance[ t > 0, list( l1 = sum( l1 ), 
                                  l2 = sum( l2 ), 
                                  l3 = sum( l3 ),
                                  l4 = sum( l4 ),
                                  l5 = sum( l5 ),
                                  l7 = sum( l7 ),
                                  l8 = sum( l8 ),
                                  lpen = sum( l4 + l5 + l7 + l8 ),
                                  lpea = sum( l1 + l2 + l3 ) ), 
                     by = list( t ) ]
  ratios <- merge( ratios, 
                   balance_anual[ t > 0, list( t, M, A, A2, A4, A5, A7, A8, A_est, B_pen, B4, B5, B7, B8 ) ],
                   by = c( 't' ) )
  
  ratios <- merge.data.table( ratios, pib, by = c( 't' ) )
  
  # Salario mensual promedio
  ratios[ , sal_mean := M / l2 / 12  ]
  
  # Aportes promedio anuales
  ratios[ , apo_mean := A / l2  ]
  
  # Pensión promedio
  ratios[ , pen_mean := ( B_pen / lpen - sbu ) / 13 ]
  
  # Beneficios pensiones promedio anuales
  ratios[ , ben_pen_mean := B_pen / lpen ]
  
  # Tasa de dependencia = razón de activos para pensionistas
  ratios[ , dep_tasa := l2 / lpen ]
  
  # Tasa de activos respecto a la PEA
  ratios[ , act_pea := l2 / lpea ]
  
  # Tasa de beneficios de pensiones para masa salarial
  ratios[ , pen_sal := B_pen / M ]
  
  # Tasa de remplazo = razón entre pensiones promedio para salarios promedio
  # EP / EM = ( Activos / Pensionistas ) * ( B / M )
  # ratios[ , rem_tasa := dep_tasa * pen_sal ]
  ratios[ , rem_tasa := pen_mean / sal_mean ]
  
  # Tasa de evolución de las pensiones promedio
  ratios[ , pen_tasa := shift( pen_mean, 1, type = 'lag', fill = 0 ) ]
  ratios[ , pen_tasa := ( pen_mean - pen_tasa ) / pen_tasa ]
  ratios[ t == 1, pen_tasa := 0 ]
  
  # Tasa de evolución de los salarios promedio
  ratios[ , sal_tasa := shift( sal_mean, 1, type = 'lag', fill = 0 ) ]
  ratios[ , sal_tasa := ( sal_mean - sal_tasa ) / sal_tasa ]
  ratios[ t == 1, sal_tasa := 0 ]
  
  # Tasa de evolución de la masa salarial
  ratios[ , mas_tasa := shift( M, 1, type = 'lag', fill = 0 ) ]
  ratios[ , mas_tasa := ( M - mas_tasa ) / mas_tasa ]
  ratios[ t == 1, mas_tasa := 0 ]
  
  # Tasa de evolución de la contribución del estado respecto al PIB
  ratios[ , ae_pib_tasa := A_est / pib ]

  save( ratios, file = paste0( parametros$ivm_rdata_icomp_ratios, escenario, '.RData' ) )
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
