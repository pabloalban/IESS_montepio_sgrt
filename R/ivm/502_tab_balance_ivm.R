message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tGenerando tabla de balance total' )

escenarios_lista <- paste0( 'escenario_', 1:5 )

# --------------------------------------------------------------------------------------------------
for ( i in 1:length( escenarios_lista ) ) { # i <- 1
  escenario <- escenarios_lista[i]
  load( file = paste0( parametros$ivm_rdata_icomp_balance, escenario, '.RData' ) )
  
  # Masa salarial ----------------------------------------------------------------------------------
  if( i == 1 ){
    aux <- balance_anual[ , list( t = t + parametros$anio_ini, M ) ]
    aux[, t := as.character( t )]
    xtb_aux <- xtable( aux, digits = c( 0, 0, 2 ) )
    print( xtb_aux,
           file = paste0( parametros$resultado_tablas, 'iess_balance_ms', '.tex' ),
           type = 'latex', 
           include.colnames = FALSE, include.rownames = FALSE, 
           format.args = list( decimal.mark = ',', big.mark = '.' ), 
           only.contents = TRUE, 
           hline.after = NULL, sanitize.text.function = identity )
  }
  # Balance corriente ------------------------------------------------------------------------------
  aux <- balance_anual[ , list( t = t + parametros$anio_ini, A, A_est, B, G, V_cor, V_cap ) ]
  aux[, t := as.character( t )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_corriente_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  aux <- balance_anual[ , list( t = t + parametros$anio_ini, A2, A4, A5, A7, A8, A, A_est, A_tot = A + A_est ) ]
  aux[, t := as.character( t )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_aportes_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  aux <- balance_anual[ , list( t = t + parametros$anio_ini, B4, B5, B7, B8, B_pen, B_aux, B ) ]
  aux[, t := as.character( t )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_beneficios_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico  ------------------------------------------------------------------------------
  # Balance dinámico (actuarial) -------------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio_ini, t, A_vap, A_est_vap, B_vap, G_vap, V0, V ) ]
  aux[, anio := as.character( anio )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_actuarial_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico (aportes) ---------------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio_ini, t, A2_vap, A4_vap, A5_vap,
                                A7_vap, A8_vap, A_vap, A_est_vap, A_tot = A_vap + A_est_vap ) ]
  aux[, anio := as.character( anio )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_aportes_vap_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico (beneficios) ------------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio_ini, t, B4_vap, B5_vap, B7_vap, B8_vap,
                                B_pen_vap, B_aux_vap, B_vap ) ]
  aux[, anio := as.character( anio )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_beneficios_vap_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico (resumen) ---------------------------------------------------------------------
  aux <- balance_anual[ t == max(t), 
                        list( V0, A2_vap, A4_vap, A5_vap, A7_vap, A8_vap, A_est_vap,
                              A_tot = A_vap + A_est_vap, 
                              activo = V0 + A_vap + A_est_vap,
                              B4_vap, B5_vap, B7_vap, B8_vap, B_aux_vap, B_vap, G_vap, 
                              pasivo = B_vap + G_vap, 
                              V ) ]
  aux1 <- melt.data.table( aux, measure.vars = 1:ncol(aux) )
  aux2 <- data.table( V0        = '\\quad Reserva inicial', # 1
                      A2_vap    = '\\quad \\quad Aportes activos', # 2
                      A4_vap    = '\\quad \\quad Aportes pensionistas vejez', # 3
                      A5_vap    = '\\quad \\quad Aportes pensionistas invalidez', # 4
                      A7_vap    = '\\quad \\quad Aportes pensionistas montep\\\'{i}o viudas', # 5
                      A8_vap    = '\\quad \\quad Aportes pensionistas montep\\\'{i}o hu\\\'{e}rfanos', # 6
                      A_est_vap = '\\quad \\quad Contribución estatal', # 7
                      A_tot     = '\\quad Total aportes', # 8
                      activo    = 'Activo actuarial', # 9
                      B4_vap    = '\\quad \\quad Beneficios pensionistas vejez', # 10
                      B5_vap    = '\\quad \\quad Beneficios pensionistas invalidez', # 11
                      B7_vap    = '\\quad \\quad Beneficios pensionistas montep\\\'{i}o viudas', # 12
                      B8_vap    = '\\quad \\quad Beneficios pensionistas montep\\\'{i}o hu\\\'{e}rfanos', # 13
                      B_aux_vap = '\\quad \\quad Beneficios auxilio funerales', # 14
                      B_vap     = '\\quad Total beneficios', # 15
                      G_vap     = '\\quad Gastos administrativos', # 16
                      pasivo    = 'Pasivo actuarial', # 17
                      V         = 'Balance actuarial' ) # 18
  aux2 <- melt.data.table( aux2, measure.vars = 1:ncol(aux2) )
  aux <- merge( aux2, aux1, by = 'variable', all.x = TRUE )
  setnames( aux, c( 'item', 'descripcion', 'valor' ) )
  xtb_aux <- xtable( aux[ , list(descripcion, valor)], digits = c( 0, 0, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_bal_act_vap_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = c( 1, 7, 8, 9, 14, 15, 16, 17 ), sanitize.text.function = identity )
  
  rm( balance, balance_anual )
}
# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()
