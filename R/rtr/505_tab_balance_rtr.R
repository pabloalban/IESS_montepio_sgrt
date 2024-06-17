message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tGenerando tabla de balance total' )
# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )
# --------------------------------------------------------------------------------------------------
escenarios_lista <- paste0( 'escenario_', 1:3 )

for ( i in 1:length( escenarios_lista ) ) {
  escenario <- escenarios_lista[i]
  # escenario <- escenarios_lista[1]
  load( file = paste0( parametros$rtr_rdata_icomp_balance, escenario, '.RData' ) )
  
  # Masa salarial ----------------------------------------------------------------------------------
  if ( escenario == 'escenario_1' ) {
    aux <- balance_anual[ , list( t = t + parametros$anio_ini, M ) ]
    aux <- aux[ t > 2020 ]
    aux[ , t := as.character( t ) ]
    xtb_aux <- xtable( aux, digits = c( 0, 0, 2 ) )
    print( xtb_aux,
           file = paste0( parametros$resultado_tablas, 'iess_balance_ms.tex' ),
           type = 'latex',
           include.colnames = FALSE, include.rownames = FALSE, 
           format.args = list( decimal.mark = ',', big.mark = '.' ), 
           only.contents = TRUE, 
           hline.after = nrow(aux),
           sanitize.text.function = identity )
  }
  # Balance corriente ------------------------------------------------------------------------------
  aux <- balance_anual[ , list( t = t + parametros$anio_ini, A, B, G, V_cor, V_cap ) ]
  aux <- aux[ t > 2020 ]
  aux[ , t := as.character( t ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_corriente_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = nrow(aux),
         sanitize.text.function = identity )
  
  # Aportes ----------------------------------------------------------------------------------------
  aux <- balance_anual[ , list( t = t + parametros$anio_ini, A2, A12, A15, A16, A_est, 
                                A ) ]
  aux <- aux[ t > 2020 ]
  aux[ , t := as.character( t ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 6 ) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_aportes_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = nrow(aux),
         sanitize.text.function = identity )
  
  # Beneficios -------------------------------------------------------------------------------------
  aux <- balance_anual[ , list( t = t + parametros$anio_ini, B12, B13, B13, B15, B16, B_sal, B ) ]
  aux <- aux[ t > 2020 ]
  aux[ , t := as.character( t ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 7 ) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_beneficios_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = nrow( aux ), 
         sanitize.text.function = identity )
  
  # Balance dinámico (actuarial) -------------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio_ini, t, A2_vap, A_est_vap, B_vap, G_vap, 
                                V0, V ) ]
  aux <- aux[ anio > 2020 ]
  aux[ , anio := as.character( anio ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_actuarial_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = nrow( aux ), 
         sanitize.text.function = identity )
  
  # Balance dinámico (aportes) ---------------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio_ini, t, A2_vap, A12_vap, A15_vap, A16_vap, 
                                A_est_vap, A_vap ) ]
  aux <- aux[ anio > 2020 ]
  aux[ , anio := as.character( anio ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, rep( 2, 6 ) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_aportes_vap_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = nrow(aux), 
         sanitize.text.function = identity )
  
  # Balance dinámico (beneficios) ------------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio_ini, t, B12_vap, B13_vap, B14_vap,
                                B15_vap, B16_vap, B_sal_vap, B_vap ) ]
  aux <- aux[ anio > 2020 ]
  aux[ , anio := as.character( anio ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, rep( 2, 7 ) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_beneficios_vap_',
                        escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = nrow(aux), 
         sanitize.text.function = identity )
  
  # Balance dinámico (resumen) ---------------------------------------------------------------------
  aux <- balance_anual[ t == max(t), 
                        list( V0, A2_vap, A12_vap, A15_vap, A16_vap, A_est_vap,
                              A_vap, Act_vap,
                              B12_vap, B13_vap, B14_vap, B15_vap, B16_vap, B_sal_vap,
                              G_vap, B_vap, Pas_vap, 
                              V ) ]
  aux1 <- melt.data.table( aux, measure.vars = 1:ncol(aux) )
  aux2 <- data.table( V0        = '\\quad Reserva inicial', 
                      A2_vap    = '\\quad \\quad Aportes activos', 
                      A12_vap   = '\\quad \\quad Aportes pensionistas de incapacidad permanente parcial, absoluta y total',
                      A15_vap   = '\\quad \\quad Aportes pensionistas montep\\\'{i}o de orfandad',
                      A16_vap   = '\\quad \\quad Aportes pensionistas montep\\\'{i}o de viudedad',
                      A_est_vap = '\\quad \\quad Contribución estatal para financiar las pensiones', 
                      A_vap     = '\\quad Total de aportes y contribuciones', 
                      Act_vap   = 'Total activo actuarial', 
                      B12_vap   = '\\quad \\quad Beneficios por incapacidad permanente parcial, absoluta y total', 
                      B13_vap   = '\\quad \\quad Beneficios por incapacidad permanente parcial (indemnizaciones)',
                      B14_vap   = '\\quad \\quad Beneficios por incapacidad temporal (subsidios)',
                      B15_vap   = '\\quad \\quad Beneficios pensionistas montep\\\'{i}o de orfandad',
                      B16_vap   = '\\quad \\quad Beneficios pensionistas montep\\\'{i}o de viudedad',
                      B_sal_vap = '\\quad \\quad Prestaciones m\\\'{e}dico-asistenciales',
                      B_vap     = '\\quad Total beneficios', 
                      G_vap     = '\\quad Gastos administrativos',
                      Pas_vap   = 'Total pasivo actuarial',
                      V         = 'Balance actuarial' )
  aux2 <- melt.data.table( aux2, measure.vars = 1:ncol(aux2) )
  aux <- merge( aux2, aux1, by = 'variable', all.x = TRUE )
  setnames( aux, c('item', 'descripcion', 'valor') )
  xtb_aux <- xtable( aux[ , list(descripcion, valor) ], digits = c( 0, 0, 2 ) )
  xtb_aux <- tildes_a_latex(xtb_aux)
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_bal_act_vap_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = c( 1, 6, 7, 8, 14, 15, 16, 17, 18 ),
         sanitize.text.function = identity,
         add.to.row = 
           list( pos = list( 0, 8, 17 ), # posición después de 0, 9 y 19
                 command = c(paste(" \n \\multicolumn{2}{c}{\\textbf{Activo actuarial}} \\\\ \n \\hline \n"), 
                             paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Pasivo actuarial}} \\\\ \n"),
                             paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Balance actuarial}} \\\\ \n") ) 
           ) )
  
  rm( balance, balance_anual ) #limpiar #remove
}

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
