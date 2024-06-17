message( paste( rep('-', 100 ), collapse = '' ) )
#Diccionario de variables --------------------------------------------------------------------------
message( '\tCreando diccionarios' )

diccionario_ms <- data.table( item = c('t','M', 'M_vap' ),
                              descripcion = c('Año del Horizonte de Proyección',
                                              'Masa Salarial de Afiliados al SGO',
                                              'Masa Salarial VAP' ) )

# diccionario_ms <- 
#   data.table( item = c('t','M', 'MS','MD', 'M_vap', 'MS_vap','MD_vap' ),
#               descripcion = c('Año del Horizonte de Proyección',
#                               'Masa Salarial de Afiliados al SGO',
#                               'Masa Salarial del SSC',
#                               'Masa Salarial de Afiliados al SGO bajo Relación de Dependientes',
#                               'Masa Salarial VAP',
#                               'Masa Salarial del SSC VAP',
#                               'Masa Salarial de Afiliados al SGO bajo Relación de Dependientes VAP') )

diccionario_bal_corriente <- data.table( item = c( 'A', 'A_est', 'B', 'G', 'V_cor', 'V_cap' ), 
                                         descripcion = c('Aportes de afiliados',
                                                         'Aporte estatal',
                                                         'Beneficios',
                                                         'Gasto administrativo',
                                                         'Balance corriente',
                                                         'Balance capitalizado' ) )

diccionario_apo_corriente <- data.table( item = c( 'A2', 'A4', 'A5', 'A7', 'A8', 'A_est', 'A'), 
                             descripcion = c( 'Aporte activos',
                                              'Aporte pensionistas por vejez',
                                              'Aporte pensionistas por invalidez',
                                              'Aporte pensionistas de montepio viudedad',
                                              'Aporte pensionistas de montepío orfandad',
                                              'Aporte estatal',
                                              'Aporte total' ) )

diccionario_ben_corriente <- data.table( item = c('B4', 'B5', 'B7', 'B8', 'B_pen', 'B_aux', 'B' ), 
                             descripcion = c( 'Beneficios pensionistas por vejez',
                                              'Beneficios pensionistas por invalidez',
                                              'Beneficios pensionistas de montepío viudedad',
                                              'Beneficios pensionistas de montepío orfandadad',
                                              'Beneficios por pensiones',
                                              'Beneficios por auxilio de funerales',
                                              'Beneficios totales' ) )

diccionario_bal_dinamico <- data.table( item = c( 'A', 'A_est', 'B', 'G', 'V0', 'V'), 
                                        descripcion = c( 'Aportes', 
                                                         'Aporte estatal',
                                                         'Beneficios',
                                                         'Gasto administrativo',
                                                         'Reserva inicial',
                                                         'Balance actuarial' ) )

diccionario_apo_dinamico <- data.table( item = c( 'A2_vap', 'A4_vap', 'A5_vap', 'A7_vap', 'A8_vap',
                                                  'A_vap', 'A_est_vap', 'A_vap'), 
                            descripcion = c( 'Aporte activos vap',
                                             'Aporte pensionistas por vejez vap',
                                             'Aporte pensionistas por invalidez vap',
                                             'Aporte pensionistas de montepío viudedad vap',
                                             'Aporte pensionistas de montepío orfandad vap',
                                             'Aporte afiliados vap',
                                             'Aporte estatal vap',
                                             'Aporte total') )

diccionario_ben_dinamico <- data.table( item = c( 'B4_vap', 'B5_vap', 'B7_vap', 'B8_vap',
                                                  'B_pen_vap', 'B_aux_vap', 'B' ), 
                            descripcion = c( 'Beneficios pensionistas por vejez vap',
                                             'Beneficios pensionistas por invalidez vap',
                                             'Beneficios pensionistas de montepío viudedad vap',
                                             'Beneficios pensionistas de montepío orfandad vap',
                                             'Beneficios por pensiones vap',
                                             'Beneficios por auxilio de funerales vap', 
                                             'Beneficios totales') )

diccionario_tot_corriente <- do.call('rbind', list(diccionario_ms, 
                                         diccionario_bal_corriente, 
                                         diccionario_apo_corriente,
                                         diccionario_ben_corriente ) )

diccionario_tot_dinamico <- do.call('rbind', list( diccionario_bal_dinamico, 
                                                  diccionario_apo_dinamico, 
                                                  diccionario_ben_dinamico ) )