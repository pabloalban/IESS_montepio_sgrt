message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando la tabla de resumen de resultados de la valuación' )

#parametrización de los escenarios -----------------------------------------------------------------
escenarios <- paste0( 'escenario_', 1:3 )
nom_esc <- c( 'Legal', 'Intermedio', 'Pesimista')

#Creación data.frame -------------------------------------------------------------------------------
result_list <- vector(mode = "list", length = length( escenarios ) )

result <- data.table( variable = c( 'Act_vap', 'V0', 'A2_vap', 'A12_vap', 'A_est_vap', 'Pas_vap', 'B_pen_vap', 
                                    'B_sal_vap', 'B_vap', 'G_vap', 'V', 'pri_med_niv_apo' ),
                      desc = c( 
                        # 'Tasa actuarial (\\%)',
                        # 'Tasa de crecimiento del SBU (\\%)',
                        # 'Tasa de crecimiento del salario promedio (\\%)',
                        # 'Tasa de crecimiento de las pensiones (\\%)',
                        'Activo actuarial(USD)', 
                        '\\quad Reserva inicial (USD)', 
                        '\\quad Aportes patronales (USD)', 
                        '\\quad Aportes de pensionistas (USD)', 
                        '\\quad Aportes del Estado (USD)', 
                        'Pasivo actuarial (USD)',
                        '\\quad Beneficios por pensiones',
                        '\\quad Prestaciones m\\\'{e}dico-asistenciales ',
                        '\\quad Beneficios totales (USD)', 
                        '\\quad Gastos administrativos (USD)',
                        'Balance actuarial (USD)',
                        'Prima suficiente (\\%)' ),
                      orden = 1:12
                      )

for ( escenario in escenarios ) {
  load( paste0( parametros$rtr_rdata_icomp_conf_esc, escenario, '.RData' ) )
  load( paste0( parametros$rtr_rdata_icomp_prima, escenario, '.RData' ) )
  load( paste0( parametros$rtr_rdata_icomp_balance, escenario, '.RData' ) )

  # i_a = formatC( esc$i_a*100, decimal.mark = ",", format = 'f', digits = 2),
  # i_sbu = formatC( esc$i_sbu*100, decimal.mark = ",", format = 'f', digits = 5),
  # i_sal = formatC( esc$i_r*100, decimal.mark = ",", format = 'f', digits = 5),
  # i_p = formatC( esc$i_p*100, decimal.mark = ",", format = 'f', digits = 5),
  
  pri_med_niv_apo <- 100 * prima[ t ==  parametros$horizonte ]$pri_med_niv_apo_est_pen
  
  aux1 <- balance_anual[ t == max( t ), 
                         list( V0, A2_vap, A_pen_vap = A12_vap + A15_vap + A16_vap, A_est_vap, Act_vap,
                               BP_vap = B12_vap + B15_vap + B16_vap + B13_vap + B14_vap, 
                               B_sal_vap, B_vap,
                               G_vap,
                               Pas_vap, 
                               V, 
                               pri_med_niv_apo ) ]
  
  aux1 <- melt.data.table( aux1, measure.vars = 1:ncol( aux1 ), 
                           variable.name = 'variable', value.name = paste0( 'val_esc_', escenario ) )
  
  result <- merge( result, aux1, by = 'variable' )
  
}

result <- result[,-1, with = FALSE ]
setorder(result, orden)
result[, orden := NULL]

#Guardar en latex-----------------------------------------------------------------------------------
xtb_result <- xtable( result, digits = c( 0, 0, rep( 3, 3 ) ) )
print( xtb_result,
       file = paste0( parametros$resultado_tablas, 'iess_resultados.tex' ),
       type = 'latex', 
       include.colnames = FALSE, 
       include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       sanitize.text.function = identity,
       hline.after = c( 4, 8, 9, 10 ) )

# Limpieza -----------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()
