message( '\tEstableciendo información para la configuración del reporte' )

# PROYECCIONES DEMOGRAFICAS Y FINANCIERAS ----------------------------------------------------------
load( paste0( parametros$RData_seg ,'IESS_IVM_salidas_demograficos.RData' ) )

REP$media_crecimiento_afi_act <- format( media,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$max_tasa_cob <- format( 100 * tasa_cob[ year == 2060 ]$tcv
                            ,digits = 2
                            , nsmall = 2
                            , big.mark = '.'
                            , decimal.mark = ','
                            , format = 'f' )

REP$cap_2021 <- format( tasa_cob[ year == 2021 ]$cap
                            ,digits = 2
                            , nsmall = 2
                            , big.mark = '.'
                            , decimal.mark = ','
                            , format = 'f' )

REP$cap_2060 <- format( tasa_cob[ year == 2060 ]$cap
                        ,digits = 2
                        , nsmall = 2
                        , big.mark = '.'
                        , decimal.mark = ','
                        , format = 'f' )

REP$pcjact_2020 <- format( 100 * pctjact_2020
                        ,digits = 2
                        , nsmall = 2
                        , big.mark = '.'
                        , decimal.mark = ','
                        , format = 'f' )

REP$pcjinact_2020 <- format( 100 * pctjinac_2020
                           ,digits = 2
                           , nsmall = 2
                           , big.mark = '.'
                           , decimal.mark = ','
                           , format = 'f' )

REP$pcjact_2060 <- format( 100 * pctjact_2060
                           ,digits = 2
                           , nsmall = 2
                           , big.mark = '.'
                           , decimal.mark = ','
                           , format = 'f' )

REP$pcjinact_2060 <- format( 100 * pctjinac_2060
                             ,digits = 2
                             , nsmall = 2
                             , big.mark = '.'
                             , decimal.mark = ','
                             , format = 'f' )

REP$FL_2020 <- format( tasa_cob[year=='2021']$fuerza_la_f_m
                             ,digits = 2
                             , nsmall = 2
                             , big.mark = '.'
                             , decimal.mark = ','
                             , format = 'f' )

REP$AFI_2020 <- format( tasa_cob[year=='2021']$activos_f_m
                       ,digits = 2
                       , nsmall = 2
                       , big.mark = '.'
                       , decimal.mark = ','
                       , format = 'f' )

REP$FL_AFI_2020 <- format( 100 * ( tasa_cob[year=='2021']$activos_f_m / tasa_cob[year=='2021']$fuerza_la_f_m  )
                        ,digits = 2
                        , nsmall = 2
                        , big.mark = '.'
                        , decimal.mark = ','
                        , format = 'f' )

REP$FL_2060 <- format( tasa_cob[year=='2060']$fuerza_la_f_m
                       ,digits = 2
                       , nsmall = 2
                       , big.mark = '.'
                       , decimal.mark = ','
                       , format = 'f' )

REP$AFI_2060 <- format( tasa_cob[year=='2060']$activos_f_m
                        ,digits = 2
                        , nsmall = 2
                        , big.mark = '.'
                        , decimal.mark = ','
                        , format = 'f' )

REP$FL_AFI_2060 <- format( 100 * ( tasa_cob[year=='2060']$activos_f_m / tasa_cob[year=='2060']$fuerza_la_f_m  )
                           ,digits = 2
                           , nsmall = 2
                           , big.mark = '.'
                           , decimal.mark = ','
                           , format = 'f' )

REP$jv_f <- format( tasa_cob[ year=='2021']$jv_female
                       ,digits = 2
                       , nsmall = 2
                       , big.mark = '.'
                       , decimal.mark = ','
                       , format = 'f' )

REP$jv_m <- format( tasa_cob[ year=='2021']$jv_male
                    , digits = 2
                    , nsmall = 2
                    , big.mark = '.'
                    , decimal.mark = ','
                    , format = 'f' )

REP$jv_f_2060 <- format( tasa_cob[ year=='2060']$jv_female
                    ,digits = 2
                    , nsmall = 2
                    , big.mark = '.'
                    , decimal.mark = ','
                    , format = 'f' )

REP$jv_m_2060 <- format( tasa_cob[ year=='2060']$jv_male
                    ,digits = 2
                    , nsmall = 2
                    , big.mark = '.'
                    , decimal.mark = ','
                    , format = 'f' )


REP$dis_f_2010 <- format( pen_dis[ anio==2010]$mujeres
                          , digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$dis_f_2020 <- format( pen_dis[ anio==2020]$mujeres
                          , digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$dis_f_2060 <- format( pen_dis[ anio==2060]$mujeres
                          ,digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$dis_m_2010 <- format( pen_dis[ anio==2010]$hombres
                          , digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$dis_m_2020 <- format( pen_dis[ anio==2020]$hombres
                          , digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$dis_m_2060 <- format( pen_dis[ anio==2060]$hombres
                         , digits = 2
                         , nsmall = 2
                         , big.mark = '.'
                         , decimal.mark = ','
                         , format = 'f' )


REP$viu_f_2012 <- format( pen_viu[ anio=='2012']$mujeres
                          , digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$viu_f_2020 <- format( pen_viu[ anio=='2020']$mujeres
                          , digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$viu_f_2060 <- format( pen_viu[ anio=='2060']$mujeres
                          , digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$viu_m_2012 <- format( pen_viu[ anio=='2012']$hombres
                          , digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$viu_m_2020 <- format( pen_viu[ anio=='2020']$hombres
                          , digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$viu_m_2060 <- format( pen_viu[ anio=='2060']$hombres
                          , digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$orf_f_2012 <- format( pen_orf[ anio==2012]$mujeres
                          , digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$orf_m_2012 <- format( pen_orf[ anio==2012]$hombres
                          , digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$orf_m_2020 <- format( pen_orf[ anio==2020]$hombres
                          , digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$orf_f_2020 <- format( pen_orf[ anio==2020]$mujeres
                          ,digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$orf_m_2060 <- format( pen_orf[ anio==2060]$hombres
                          ,digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$orf_f_2060 <- format( pen_orf[ anio==2060]$mujeres
                          ,digits = 2
                          , nsmall = 2
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )


# # Escenario 1 --------------------------------------------------------------------------------------
#source( 'R/IVM/303_agregado_financiero_ivm.R', encoding = 'UTF-8', echo = FALSE )
escenario <- 'escenario_1'
load( paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_IVM_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_IVM_balances_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', esc$nombre, '.RData' ) )
# 
 REP$bal_act_esc_1 <- format( agregado_financiero[ t == parametros$horizonte ]$V, 
                              digits = 2, nsmall = 2, big.mark = '.', 
                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 

#ingresos por contribuciones no solventan los gasto (momento crítico 1)
REP$moment_crit1_esc_1 <-min(which(agregado_financiero$contribuciones+agregado_financiero$Otros < agregado_financiero$total2 )) + parametros$anio_ini -1

#ingresos+intereses por contribuciones no solventan los gasto (momento crítico 2)
REP$moment_crit2_esc_1 <-min(which(agregado_financiero$total1 < agregado_financiero$total3 )) + parametros$anio_ini -1
 
#último año reserva positiva (momento crítico 3)
REP$duracion_esc_1 <- max( which( agregado_financiero$reserva > 0 ) ) + parametros$anio_ini -1

REP$balance_esc_1 <- format( agregado_financiero$V[41],digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )
# 
# REP$sup_apo_esc_1 <- min( balance_anual[ A_est >= A & t > 0 ]$t ) + parametros$anio_ini
# 
REP$cap_ini <- format( esc$V0, 
                        digits = 2, nsmall = 2, big.mark = '.', 
                        decimal.mark = ',', format = 'f' )
 
# REP$dep_tas_ini_esc_1 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_1 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_1 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_1 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
 REP$pri_med_niv_esc_1 <- format( 100*prima[ t == parametros$horizonte ]$pri_med_niv,
                                  digits = 2, nsmall = 2, decimal.mark = ',', format = 'f' )
 
 REP$apo_est_esc_1<-format( 100 * esc$apo_act$por_apo_est[1],
                             digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )

 REP$tasa_act_esc_1<-format( 100 * esc$apo_act$i_a[1],
                             digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )
# 
# # Escenario 2 --------------------------------------------------------------------------------------
 escenario <- 'escenario_2'
load( paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_IVM_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_IVM_balances_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_2 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_2 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
#ingresos por contribuciones no solventan los gasto (momento crítico 1)
REP$moment_crit1_esc_2 <-min(which(agregado_financiero$contribuciones+agregado_financiero$Otros < agregado_financiero$total2 )) + parametros$anio_ini -1

#ingresos+intereses por contribuciones no solventan los gasto (momento crítico 2)
REP$moment_crit2_esc_2 <-min(which(agregado_financiero$total1 < agregado_financiero$total3 )) + parametros$anio_ini -1



REP$duracion_esc_2 <- max( which(agregado_financiero$reserva > 0 ) ) + parametros$anio_ini -1
REP$balance_esc_2 <- format( agregado_financiero$V[41],digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_ini_esc_2 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_2 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_2 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_2 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
REP$pri_med_niv_esc_2 <- format( 100*prima[ t == parametros$horizonte ]$pri_med_niv,
                                 digits = 2, nsmall = 2, decimal.mark = ',', format = 'f' )

REP$apo_est_esc_2<-format( 100 * esc$apo_act$por_apo_est[1],
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )

REP$tasa_act_esc_2<-format( 100 * esc$apo_act$i_a[1],
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )
# # Escenario 3 --------------------------------------------------------------------------------------
 escenario <- 'escenario_3'
load( paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', escenario, '.RData' ) )
 load( paste0( parametros$RData_seg, 'IESS_IVM_primas_', esc$nombre, '.RData' ) )
 load( paste0( parametros$RData_seg, 'IESS_IVM_balances_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 

#ingresos por contribuciones no solventan los gasto (momento crítico 1)
 REP$moment_crit1_esc_3 <-min(which(agregado_financiero$contribuciones+agregado_financiero$Otros < agregado_financiero$total2 )) + parametros$anio_ini -1
 
#ingresos+intereses por contribuciones no solventan los gasto (momento crítico 2)
 REP$moment_crit2_esc_3 <- min(which(agregado_financiero$total1 < agregado_financiero$total3 )) + parametros$anio_ini -1
 
 
REP$duracion_esc_3 <- max( which( agregado_financiero$reserva > 0 ) ) + parametros$anio_ini - 1
REP$balance_esc_3 <- format( agregado_financiero$V[41],digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )
# 
# 
# REP$dep_tas_ini_esc_3 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_3 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_3 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_3 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )

REP$pri_med_niv_esc_3<-format( 100*prima[ t == parametros$horizonte ]$pri_med_niv,
                               digits = 2, nsmall = 2, decimal.mark = ',', format = 'f' )

REP$apo_est_esc_3<-format( 100 * esc$apo_act$por_apo_est[1],
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )

REP$tasa_act_esc_3<-format( 100 * esc$apo_act$i_a[1],
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )


# # Escenario 4 --------------------------------------------------------------------------------------
escenario <- 'escenario_4'
load( paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_IVM_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_IVM_balances_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 

#ingresos por contribuciones no solventan los gasto (momento crítico 1)
REP$moment_crit1_esc_4 <-min(which(agregado_financiero$contribuciones+agregado_financiero$Otros < agregado_financiero$total2 )) + parametros$anio_ini -1

#ingresos+intereses por contribuciones no solventan los gasto (momento crítico 2)
REP$moment_crit2_esc_4 <-min(which(agregado_financiero$total1 < agregado_financiero$total3 )) + parametros$anio_ini -1


REP$duracion_esc_4 <- max( which( agregado_financiero$reserva > 0 ) ) + parametros$anio_ini - 1
REP$balance_esc_4 <- format( agregado_financiero$V[41],digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )
# 
# 
# REP$dep_tas_ini_esc_3 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_3 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_3 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_3 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )

REP$pri_med_niv_esc_4<-format( 100*prima[ t == parametros$horizonte ]$pri_med_niv,
                               digits = 2, nsmall = 2, decimal.mark = ',', format = 'f' )

REP$apo_est_esc_4<-format( 100 * esc$apo_act$por_apo_est[1],
                           digits = 2, nsmall = 2, big.mark = '.',
                           decimal.mark = ',', format = 'f' )

REP$tasa_act_esc_4<-format( 100 * esc$apo_act$i_a[1],
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )

# # Escenario 5 --------------------------------------------------------------------------------------
escenario <- 'escenario_5'
load( paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_IVM_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_IVM_balances_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )

#ingresos por contribuciones no solventan los gasto (momento crítico 1)
REP$moment_crit1_esc_5 <-min(which(agregado_financiero$contribuciones+agregado_financiero$Otros < agregado_financiero$total2 )) + parametros$anio_ini -1

#ingresos+intereses por contribuciones no solventan los gasto (momento crítico 2)
REP$moment_crit2_esc_5 <-min(which(agregado_financiero$total1 < agregado_financiero$total3 )) + parametros$anio_ini -1

REP$duracion_esc_5 <- max( which( agregado_financiero$reserva > 0 ) ) + parametros$anio_ini - 1
REP$balance_esc_5 <- format( agregado_financiero$V[41],digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )
# 
# 
# REP$dep_tas_ini_esc_3 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_3 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_3 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_3 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )

REP$pri_med_niv_esc_5<-format( 100*prima[ t == parametros$horizonte ]$pri_med_niv,
                               digits = 2, nsmall = 2, decimal.mark = ',', format = 'f' )

REP$apo_est_esc_5<-format( 100 * esc$apo_act$por_apo_est[1],
                           digits = 2, nsmall = 2, big.mark = '.',
                           decimal.mark = ',', format = 'f' )

REP$tasa_act_esc_5<-format( 100 * esc$apo_act$i_a[1],
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )

# # Escenario 6 --------------------------------------------------------------------------------------
escenario <- 'escenario_6'
load( paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_IVM_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_IVM_balances_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 

#ingresos por contribuciones no solventan los gasto (momento crítico 1)
REP$moment_crit1_esc_6 <-min(which(agregado_financiero$contribuciones+agregado_financiero$Otros < agregado_financiero$total2 )) + parametros$anio_ini -1

#ingresos+intereses por contribuciones no solventan los gasto (momento crítico 2)
REP$moment_crit2_esc_6 <-min(which(agregado_financiero$total1 < agregado_financiero$total2 )) + parametros$anio_ini -1


REP$duracion_esc_6 <- max( which( agregado_financiero$reserva > 0 ) ) + parametros$anio_ini - 1
REP$balance_esc_6<- format( agregado_financiero$V[41],digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )
# 
# 
# REP$dep_tas_ini_esc_3 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_3 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_3 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_3 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )

REP$pri_med_niv_esc_6<-format( 100*prima[ t == parametros$horizonte ]$pri_med_niv,
                               digits = 2, nsmall = 2, decimal.mark = ',', format = 'f' )

REP$apo_est_esc_6<-format( 100 * esc$apo_act$por_apo_est[1],
                           digits = 2, nsmall = 2, big.mark = '.',
                           decimal.mark = ',', format = 'f' )

REP$tasa_act_esc_6<-format( 100 * esc$apo_act$i_a[1],
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )


# # Escenario 7 --------------------------------------------------------------------------------------
escenario <- 'escenario_7'
load( paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_IVM_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_IVM_balances_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 

#ingresos por contribuciones no solventan los gasto (momento crítico 1)
REP$moment_crit1_esc_7 <-min(which(agregado_financiero$contribuciones+agregado_financiero$Otros < agregado_financiero$total2 )) + parametros$anio_ini -1

#ingresos+intereses por contribuciones no solventan los gasto (momento crítico 2)
REP$moment_crit2_esc_7 <-min(which(agregado_financiero$total1 < agregado_financiero$total2 )) + parametros$anio_ini -1


REP$duracion_esc_7 <- max( which( agregado_financiero$reserva > 0 ) ) + parametros$anio_ini - 1
REP$balance_esc_7 <- format( agregado_financiero$V[41],digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )
#  
# REP$dep_tas_ini_esc_3 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_3 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_3 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_3 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )

REP$pri_med_niv_esc_7<-format( 100*prima[ t == parametros$horizonte ]$pri_med_niv,
                               digits = 2, nsmall = 2, decimal.mark = ',', format = 'f' )

REP$apo_est_esc_7<-format( 100 * esc$apo_act$por_apo_est[1],
                           digits = 2, nsmall = 2, big.mark = '.',
                           decimal.mark = ',', format = 'f' )

REP$tasa_act_esc_7<-format( 100 * esc$apo_act$i_a[1],
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )


# # Escenario 8 --------------------------------------------------------------------------------------
escenario <- 'escenario_8'
load( paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_IVM_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_IVM_balances_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 

#ingresos por contribuciones no solventan los gasto (momento crítico 1)
REP$moment_crit1_esc_8 <-min(which(agregado_financiero$contribuciones+agregado_financiero$Otros < agregado_financiero$total2 )) + parametros$anio_ini -1

#ingresos+intereses por contribuciones no solventan los gasto (momento crítico 2)
REP$moment_crit2_esc_8 <-max(which(agregado_financiero$total1 > agregado_financiero$total2 )) + parametros$anio_ini -1


REP$duracion_esc_8 <- max( which( agregado_financiero$reserva > 0 ) ) + parametros$anio_ini - 1
REP$balance_esc_8 <- format( agregado_financiero$V[41],digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_ini_esc_3 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_3 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_3 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_3 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )

REP$pri_med_niv_esc_8<-format( 100*prima[ t == parametros$horizonte ]$pri_med_niv,
                               digits = 2, nsmall = 2, decimal.mark = ',', format = 'f' )

REP$apo_est_esc_8<-format( 100 * esc$apo_act$por_apo_est[1],
                           digits = 2, nsmall = 2, big.mark = '.',
                           decimal.mark = ',', format = 'f' )

REP$tasa_act_esc_8<-format( 100 * esc$apo_act$i_a[1],
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )



# # Escenario 1 TNRH --------------------------------------------------------------------------------------
escenario <- 'escenario_1'
load( paste0( parametros$RData_seg, 'IESS_TNRH_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_TNRH_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_TNRH_balances_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
REP$duracion_tnrh_esc_1<- max( which( agregado_financiero$reserva > 0 ) ) + parametros$anio_ini - 1
REP$balance_tnrh_esc_1 <- format( agregado_financiero$V[41],digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_ini_esc_3 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_3 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_3 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_3 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )

REP$pri_med_niv_tnrh_esc_1<-format( 100*prima[ t == parametros$horizonte ]$pri_med_niv,
                               digits = 2, nsmall = 2, decimal.mark = ',', format = 'f' )

REP$apo_est_tnrh_esc_1<-format( 100 * esc$apo_act$por_apo_est[1],
                           digits = 2, nsmall = 2, big.mark = '.',
                           decimal.mark = ',', format = 'f' )

REP$tasa_act_tnrh_esc_1<-format( 100 * esc$apo_act$i_a[1],
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )

# # Escenario 2 TNRH --------------------------------------------------------------------------------------
escenario <- 'escenario_2'
load( paste0( parametros$RData_seg, 'IESS_TNRH_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_TNRH_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_TNRH_balances_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
REP$duracion_tnrh_esc_2<- max( which( agregado_financiero$reserva > 0 ) ) + parametros$anio_ini - 1
REP$balance_tnrh_esc_2 <- format( agregado_financiero$V[41],digits = 2, nsmall = 2, big.mark = '.', 
                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_ini_esc_3 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_3 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_3 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_3 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )

REP$pri_med_niv_tnrh_esc_2<-format( 100*prima[ t == parametros$horizonte ]$pri_med_niv,
                                    digits = 2, nsmall = 2, decimal.mark = ',', format = 'f' )

REP$apo_est_tnrh_esc_2<-format( 100 * esc$apo_act$por_apo_est[1],
                                digits = 2, nsmall = 2, big.mark = '.',
                                decimal.mark = ',', format = 'f' )

REP$tasa_act_tnrh_esc_2<-format( 100 * esc$apo_act$i_a[1],
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

# # Escenario 3 TNRH --------------------------------------------------------------------------------------
escenario <- 'escenario_3'
load( paste0( parametros$RData_seg, 'IESS_TNRH_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_TNRH_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_TNRH_balances_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
REP$duracion_tnrh_esc_3<- max( which( agregado_financiero$reserva > 0 ) ) + parametros$anio_ini - 1
REP$balance_tnrh_esc_3 <- format( agregado_financiero$V[41],digits = 2, nsmall = 2, big.mark = '.', 
                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_ini_esc_3 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_3 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_3 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_3 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )

REP$pri_med_niv_tnrh_esc_3<-format( 100*prima[ t == parametros$horizonte ]$pri_med_niv,
                                    digits = 2, nsmall = 2, decimal.mark = ',', format = 'f' )

REP$apo_est_tnrh_esc_3<-format( 100 * esc$apo_act$por_apo_est[1],
                                digits = 2, nsmall = 2, big.mark = '.',
                                decimal.mark = ',', format = 'f' )

REP$tasa_act_tnrh_esc_3<-format( 100 * esc$apo_act$i_a[1],
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )



# # Escenario 4 --------------------------------------------------------------------------------------
# escenario <- 'escenario_4'
# load( paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_primas_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_balances_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_4 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_4 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$duracion_esc_4 <- max( which( balance_anual$V_cap > 0 ) ) + parametros$anio_ini - 1
# 
# REP$dep_tas_ini_esc_4 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_4 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_4 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_4 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$apo_est_esc_4<-format( 100 * esc$aporte_estado,
#                             digits = 2, nsmall = 2, big.mark = '.', 
#                             decimal.mark = ',', format = 'f' )
# 
# REP$tasa_act_esc_4<-format( 100 * esc$i_a,
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# # Escenario 5 --------------------------------------------------------------------------------------
# escenario <- 'escenario_5'
# load( paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_primas_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_balances_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_5 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_5 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$duracion_esc_5 <- max( which( balance_anual$V_cap > 0 ) ) + parametros$anio_ini - 1
# 
# REP$dep_tas_ini_esc_5 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_5 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_5 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_5 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_5 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$apo_est_esc_5<-format( 100 * esc$aporte_estado,
#                             digits = 2, nsmall = 2, big.mark = '.', 
#                             decimal.mark = ',', format = 'f' )
# 
# REP$tasa_act_esc_5<-format( 100 * esc$i_a,
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )



