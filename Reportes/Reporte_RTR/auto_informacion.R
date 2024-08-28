message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tEstableciendo información para la configuración del reporte' )

#REP <- new.env( )

REP$corte <- parametros$anio_ini

REP$cap_ini <- format( 
  parametros$rtr_reserva_ini, 
  digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

# Carga información --------------------------------------------------------------------------------
load( parametros$demo_rdata_sgo_est_dem )
load( parametros$macro_rdata_info )
load( paste0( parametros$RData_seg, 'IESS_RTR_analisis_financiero.RData' ) )
load( paste0( parametros$RData_seg, 'BIESS_RTR_inversiones.RData' ) )
load( paste0(  parametros$RData_seg, 'IESS_RTR_tablas_demografia.RData' ) )

# Afiliados ----------------------------------------------------------------------------------------

REP$cre_prom_anual <- format( est_sal_anio[ , .(anio, ER_act, st = lag(ER_act) ) ]
                              [ anio <= 2022, var := mean( ( ER_act / st - 1 ) * 100, na.rm = T ) ]
                              [ anio == REP$corte, var ], digits = 3, big.mark = '.',  
                              decimal.mark = ',', format = 'f' )

REP$prom_anual_sgo <- format( 
  mean( est_sal_anio$ER_act, na.rm = TRUE ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$afi_sgo <- format( 
  est_sal_anio[ anio == parametros$anio_ini, ]$ER_act, 
  nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$cre_afi_sgo <- format( 
  100 * ( est_sal_anio[ anio == parametros$anio_ini ]$ER_act / 
            est_sal_anio[ anio == 2012 ]$ER_act - 1 ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$afi_var <- format( 
  100 * ( est_sal_anio[ anio == parametros$anio_ini ]$ER_act / 
            est_sal_anio[ anio == 2019 ]$ER_act - 1 ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$por_sgo_h <- format( 
  100 * est_sal_anio_sexo[ anio == REP$corte & sexo == 'H' ]$ER_act / 
    sum( est_sal_anio_sexo[ anio == REP$corte ]$ER_act ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$por_sgo_m <- format(
  100 * est_sal_anio_sexo[ anio == REP$corte & sexo == 'M' ]$ER_act / 
    sum( est_sal_anio_sexo[ anio == REP$corte ]$ER_act ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

aux <- est_sal_anio_sexo_edad[ 
  anio == REP$corte & sexo == 'H', 
  list( x_mean = sum( x * ER_act, na.rm = TRUE ) / sum( ER_act, na.rm = TRUE ) ) ]
REP$edad_prom_h <- format(
  aux$x_mean,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

aux <- est_sal_anio_sexo_edad[ 
  anio == REP$corte & sexo == 'M', 
  list( x_mean = sum( x * ER_act, na.rm = TRUE ) / sum( ER_act, na.rm = TRUE ) ) ]
REP$edad_prom_m <- format( 
  aux$x_mean,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

# Afiliados TNRH -----------------------------------------------------------------------------------

REP$cre_prom_anua_tnrh <- format( est_sal_anio[ anio > 2015 , .(anio, ER_tnrh_act, st = lag(ER_tnrh_act) ) ]
                                  [ anio <= 2022, var := mean( ( ER_tnrh_act / st - 1 ) * 100, na.rm = T ) ]
                                  [ anio == REP$corte, var ], digits = 3, big.mark = '.',  
                                  decimal.mark = ',', format = 'f' )

REP$prom_anual_tnrh <- format(
  mean( est_sal_anio$ER_tnrh_act, na.rm = TRUE ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$afi_tnrh <-format( 
  est_sal_anio[ anio == parametros$anio_ini, ]$ER_tnrh_act, 
  nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$cre_afi_tnrh <- format(
  100 * ( est_sal_anio[ anio == parametros$anio_ini ]$ER_tnrh_act / 
            est_sal_anio[ anio == 2015 ]$ER_tnrh_act - 1 ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$por_tnrh_2019 <- format( 
  100 * ( est_sal_anio[ anio == 2022 ]$ER_tnrh_act / 
            est_sal_anio[ anio == 2019 ]$ER_tnrh_act - 1 ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$por_tnrh_h <- format( 
  100 * est_sal_anio_sexo[ anio == REP$corte & sexo == 'H' ]$ER_tnrh_act / 
    sum( est_sal_anio_sexo[ anio == REP$corte ]$ER_tnrh_act ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$por_tnrh_m <- format( 
  100 * est_sal_anio_sexo[ anio == REP$corte & sexo == 'M' ]$ER_tnrh_act / 
    sum( est_sal_anio_sexo[ anio == REP$corte ]$ER_tnrh_act ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

aux <- est_sal_anio_sexo_edad[ 
  anio == REP$corte & sexo == 'H', 
  list( x_mean = sum( x * ER_tnrh_act, na.rm = TRUE ) / sum( ER_tnrh_act, na.rm = TRUE ) ) ]
REP$edad_prom_tnrh_h <- format(
  aux$x_mean,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

aux <- est_sal_anio_sexo_edad[ 
  anio == REP$corte & sexo == 'M', 
  list( x_mean = sum( x * ER_tnrh_act, na.rm = TRUE ) / sum( ER_tnrh_act, na.rm = TRUE ) ) ]
REP$edad_prom_tnrh_m <- format( 
  aux$x_mean,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

# Masa salarial ------------------------------------------------------------------------------------

REP$masa_salarial <- format(
  est_sal_anio[ anio == REP$corte ]$S, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$masa_salarial_var <- format(
  100 * ( est_sal_anio[ anio == REP$corte ]$S / est_sal_anio[ anio == 2012 ]$S - 1 ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$masa_sal_prom_2012_2015 <- format( est_sal_anio[ anio < 2016 , .(anio, S, st = lag(S) ) ]
                                       [ , var := mean( ( S / st - 1 ) * 100, na.rm = T ) ]
                                       [ anio == 2015, var ], digits = 3, big.mark = '.',  
                                       decimal.mark = ',', format = 'f' )

REP$masa_salarial_prom <- format( 
  mean( est_sal_anio$S ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$masa_salarial_var_2020 <- format( 
  100 * ( est_sal_anio[ anio == REP$corte ]$S / est_sal_anio[ anio == 2019 ]$S - 1 ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$sal_prom_h <- format( 
  est_sal_anio_sexo[ anio == REP$corte & sexo == 'H' ]$ESm, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$sal_prom_m <- format( 
  est_sal_anio_sexo[ anio == REP$corte & sexo == 'M' ]$ESm, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$sal_prom <- format( 
  est_sal_anio[ anio == REP$corte ]$ESm, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

#Jubilados------------------------------------------------------------------------------------------

## Subsidios----------------------------------------------------------------------------------------
REP$dem_sub_22 <- format( filter( tab_evo_ben_subsidios, anio ==  REP$corte)$ben,
                          nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_sub_22_dic <- format( filter( tab_evo_ben_subsidios, anio ==  REP$corte)$ben12,
                          nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_sub_22_h <- format( filter( tab_evo_ben_subsidios, anio ==  REP$corte)$sexobenM,
                          nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )


REP$dem_sub_22_h_porc <- format( 100 * filter( tab_evo_ben_subsidios, anio ==  REP$corte)$sexobenM / 
                                   filter( tab_evo_ben_subsidios, anio ==  REP$corte)$ben,
                            nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )


REP$dem_sub_22_m <- format( filter( tab_evo_ben_subsidios, anio ==  REP$corte)$sexobenF,
                            nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_sub_22_m_porc <- format( 100 * filter( tab_evo_ben_subsidios, anio ==  REP$corte)$sexobenF / 
                                   filter( tab_evo_ben_subsidios, anio ==  REP$corte)$ben,
                                 nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_sub_monto_22 <- format( filter( tab_evo_monto_subsidios, anio ==  REP$corte)$subsidios,
                          nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_sub_monto_dic_22 <- format( filter( tab_evo_monto_subsidios, anio ==  REP$corte)$subsidios_12,
                                 nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_sub_monto_incr_21_22 <- format(
  filter( tab_evo_monto_subsidios, anio ==  REP$corte)$tasa_crecimiento,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_sub_monto_22_h <- format( filter( tab_evo_monto_subsidios, anio ==  REP$corte)$sexobenM,
                                 nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_sub_monto_22_h_porc <- format( 100 * filter( tab_evo_monto_subsidios, anio ==  REP$corte)$sexobenM  /
                                          filter( tab_evo_monto_subsidios, anio ==  REP$corte)$subsidios,
                                   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_sub_monto_22_m <- format( filter( tab_evo_monto_subsidios, anio ==  REP$corte)$sexobenF,
                                   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_sub_monto_22_m_porc <- format( 100 * filter( tab_evo_monto_subsidios, anio ==  REP$corte)$sexobenF  /
                                          filter( tab_evo_monto_subsidios, anio ==  REP$corte)$subsidios,
                                        nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_sub_edad_h <- format( filter( dem_sub, sexo == 'M' )$edad_prom,
                              nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_sub_edad_m <- format( filter( dem_sub, sexo == 'F' )$edad_prom,
                              nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_sub_prom_h <- format( filter( dem_sub, sexo == 'M' )$prom_sub,
                              nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_sub_prom_m <- format( filter( dem_sub, sexo == 'F' )$prom_sub,
                              nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

##Indemnización-------------------------------------------------------------------------------------

REP$dem_ind_22 <- format( filter( tab_evo_ben_indemnizaciones, anio ==  REP$corte)$ben,
                          nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_ind_22_dic <- format( filter( tab_evo_ben_indemnizaciones, anio ==  REP$corte)$ben12,
                              nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_ind_22_h <- format( filter( tab_evo_ben_indemnizaciones, anio ==  REP$corte)$sexobenM,
                            nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )


REP$dem_ind_22_h_porc <- format( 100 * filter( tab_evo_ben_indemnizaciones, anio ==  REP$corte)$sexobenM / 
                                   filter( tab_evo_ben_indemnizaciones, anio ==  REP$corte)$ben,
                                 nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )


REP$dem_ind_22_m <- format( filter( tab_evo_ben_indemnizaciones, anio ==  REP$corte)$sexobenF,
                            nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_ind_22_m_porc <- format( 100 * filter( tab_evo_ben_indemnizaciones, anio ==  REP$corte)$sexobenF / 
                                   filter( tab_evo_ben_indemnizaciones, anio ==  REP$corte)$ben,
                                 nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_ind_incr_21_22 <- format(
  filter( tab_evo_ben_indemnizaciones, anio ==  REP$corte)$tasa_crecimiento,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_ind_monto_22 <- format( filter( tab_evo_monto_indemnizaciones, anio ==  REP$corte)$subsidios,
                                nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_ind_monto_dic_22 <- format( filter( tab_evo_monto_indemnizaciones, anio ==  REP$corte)$subsidios_12,
                                    nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_ind_monto_incr_21_22 <- format(
  filter( tab_evo_monto_indemnizaciones, anio ==  REP$corte)$tasa_crecimiento,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_ind_monto_22_h <- format( filter( tab_evo_monto_indemnizaciones, anio ==  REP$corte)$sexobenM,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_ind_monto_22_h_porc <- format( 100 * filter( tab_evo_monto_indemnizaciones, anio ==  REP$corte)$sexobenM  /
                                         filter( tab_evo_monto_indemnizaciones, anio ==  REP$corte)$subsidios,
                                       nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_ind_monto_22_m <- format( filter( tab_evo_monto_indemnizaciones, anio ==  REP$corte)$sexobenF,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_ind_monto_22_m_porc <- format( 100 * filter( tab_evo_monto_indemnizaciones, anio ==  REP$corte)$sexobenF  /
                                         filter( tab_evo_monto_indemnizaciones, anio ==  REP$corte)$subsidios,
                                       nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_ind_edad_h <- format( filter( dem_ind, sexo == 'M' )$edad_prom,
                              nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_ind_edad_m <- format( filter( dem_ind, sexo == 'F' )$edad_prom,
                              nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_ind_prom_h <- format( filter( dem_ind, sexo == 'M' )$prom_sub,
                              nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_ind_prom_m <- format( filter( dem_ind, sexo == 'F' )$prom_sub,
                              nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

## Permanente Parcial-------------------------------------------------------------------------------

REP$dem_pp_22 <- format( filter( tab_evo_ben_pp, anio ==  REP$corte)$ben,
                          nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_22_dic <- format( filter( tab_evo_ben_pp, anio ==  REP$corte)$ben12,
                              nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_22_h <- format( filter( tab_evo_ben_pp, anio ==  REP$corte)$sexobenM,
                            nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )


REP$dem_pp_22_h_porc <- format( 100 * filter( tab_evo_ben_pp, anio ==  REP$corte)$sexobenM / 
                                   filter( tab_evo_ben_pp, anio ==  REP$corte)$ben,
                                 nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )


REP$dem_pp_22_m <- format( filter( tab_evo_ben_pp, anio ==  REP$corte)$sexobenF,
                            nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_22_m_porc <- format( 100 * filter( tab_evo_ben_pp, anio ==  REP$corte)$sexobenF / 
                                   filter( tab_evo_ben_pp, anio ==  REP$corte)$ben,
                                 nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_incr_21_22 <- format(
  abs( filter( tab_evo_ben_pp, anio ==  REP$corte)$tasa_crecimiento ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_tot_ingr_22 <- format( filter( tab_evo_monto_pp, anio ==  REP$corte)$tot_ingr,
                                nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_tot_ingr_21 <- format( filter( tab_evo_monto_pp, anio ==  REP$corte - 1 )$tot_ingr,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_tot_ingr_21_22_crec <- format( 
  abs( 100 * filter( tab_evo_monto_pp, anio ==  REP$corte)$tot_ingr / 
         filter( tab_evo_monto_pp, anio ==  REP$corte - 1 )$tot_ingr - 100),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_rentas_22 <- format( filter( tab_evo_monto_pp, anio ==  REP$corte)$renta_mensual,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_rentas_22_porc <- format( 100 * filter( tab_evo_monto_pp, anio ==  REP$corte)$renta_mensual /
                                       filter( tab_evo_monto_pp, anio ==  REP$corte)$tot_ingr,
                                nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_dec_terc_22 <- format( filter( tab_evo_monto_pp, anio ==  REP$corte)$decimo_tercero,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_dec_terc_22_porc <- format( 100 * filter( tab_evo_monto_pp, anio ==  REP$corte)$decimo_tercero /
                                         filter( tab_evo_monto_pp, anio ==  REP$corte)$tot_ingr,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_dec_cuar_22 <- format( filter( tab_evo_monto_pp, anio ==  REP$corte)$decimo_cuarto,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_dec_cuar_22_porc <- format( 100 * filter( tab_evo_monto_pp, anio ==  REP$corte)$decimo_cuarto /
                                         filter( tab_evo_monto_pp, anio ==  REP$corte)$tot_ingr,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_tot_desc_22 <- format( filter( tab_evo_monto_pp, anio ==  REP$corte)$tot_desc,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_tot_desc_21 <- format( filter( tab_evo_monto_pp, anio ==  REP$corte - 1 )$tot_desc,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_tot_desc_21_22_crec <- format( 
  100 * filter( tab_evo_monto_pp, anio ==  REP$corte)$tot_desc / 
    filter( tab_evo_monto_pp, anio ==  REP$corte - 1 )$tot_desc - 100,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_liq_pagar_22 <- format( filter( tab_evo_monto_pp, anio ==  REP$corte)$liq_pagar,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_edad_h <- format( filter( dem_pp, sexo == 'M' )$prom_edad,
                              nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_edad_m <- format( filter( dem_pp, sexo == 'F' )$prom_edad,
                              nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_prom_h <- format( filter( dem_pp, sexo == 'M' )$prom_tot_ingr,
                              nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pp_prom_m <- format( filter( dem_pp, sexo == 'F' )$prom_tot_ingr,
                              nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
## Permanente Total---------------------------------------------------------------------------------

REP$dem_pt_22 <- format( filter( tab_evo_ben_pt, anio ==  REP$corte)$ben,
                         nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_22_dic <- format( filter( tab_evo_ben_pt, anio ==  REP$corte)$ben12,
                             nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_22_h <- format( filter( tab_evo_ben_pt, anio ==  REP$corte)$sexobenM,
                           nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )


REP$dem_pt_22_h_porc <- format( 100 * filter( tab_evo_ben_pt, anio ==  REP$corte)$sexobenM / 
                                  filter( tab_evo_ben_pt, anio ==  REP$corte)$ben,
                                nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )


REP$dem_pt_22_m <- format( filter( tab_evo_ben_pt, anio ==  REP$corte)$sexobenF,
                           nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_22_m_porc <- format( 100 * filter( tab_evo_ben_pt, anio ==  REP$corte)$sexobenF / 
                                  filter( tab_evo_ben_pt, anio ==  REP$corte)$ben,
                                nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_incr_21_22 <- format(
  abs( filter( tab_evo_ben_pt, anio ==  REP$corte)$tasa_crecimiento ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_tot_ingr_22 <- format( filter( tab_evo_monto_pt, anio ==  REP$corte)$tot_ingr,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_tot_ingr_21 <- format( filter( tab_evo_monto_pt, anio ==  REP$corte - 1 )$tot_ingr,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_tot_ingr_21_22_crec <- format( 
  abs( 100 * filter( tab_evo_monto_pt, anio ==  REP$corte)$tot_ingr / 
         filter( tab_evo_monto_pt, anio ==  REP$corte - 1 )$tot_ingr - 100),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_rentas_22 <- format( filter( tab_evo_monto_pt, anio ==  REP$corte)$renta_mensual,
                                nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_rentas_22_porc <- format( 100 * filter( tab_evo_monto_pt, anio ==  REP$corte)$renta_mensual /
                                       filter( tab_evo_monto_pt, anio ==  REP$corte)$tot_ingr,
                                     nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_dec_terc_22 <- format( filter( tab_evo_monto_pt, anio ==  REP$corte)$decimo_tercero,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_dec_terc_22_porc <- format( 100 * filter( tab_evo_monto_pt, anio ==  REP$corte)$decimo_tercero /
                                         filter( tab_evo_monto_pt, anio ==  REP$corte)$tot_ingr,
                                       nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_dec_cuar_22 <- format( filter( tab_evo_monto_pt, anio ==  REP$corte)$decimo_cuarto,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_dec_cuar_22_porc <- format( 100 * filter( tab_evo_monto_pt, anio ==  REP$corte)$decimo_cuarto /
                                         filter( tab_evo_monto_pt, anio ==  REP$corte)$tot_ingr,
                                       nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_tot_desc_22 <- format( filter( tab_evo_monto_pt, anio ==  REP$corte)$tot_desc,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_tot_desc_21 <- format( filter( tab_evo_monto_pt, anio ==  REP$corte - 1 )$tot_desc,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_tot_desc_21_22_crec <- format( 
  100 * filter( tab_evo_monto_pt, anio ==  REP$corte)$tot_desc / 
    filter( tab_evo_monto_pt, anio ==  REP$corte - 1 )$tot_desc - 100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_liq_pagar_22 <- format( filter( tab_evo_monto_pt, anio ==  REP$corte)$liq_pagar,
                                   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_tot_desc_22_rel <- format( 100 * filter( tab_evo_monto_pt, anio ==  REP$corte )$tot_desc / 
                                        filter( tab_evo_monto_pt, anio ==  REP$corte )$liq_pagar,
                                   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_edad_h <- format( filter( dem_pt, sexo == 'M' )$prom_edad,
                             nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_edad_m <- format( filter( dem_pt, sexo == 'F' )$prom_edad,
                             nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_prom_h <- format( filter( dem_pt, sexo == 'M' )$prom_tot_ingr,
                             nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pt_prom_m <- format( filter( dem_pt, sexo == 'F' )$prom_tot_ingr,
                             nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

##Permanente Absoluta-------------------------------------------------------------------------------

REP$dem_pa_22 <- format( filter( tab_evo_ben_pa, anio ==  REP$corte)$ben,
                         nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_22_dic <- format( filter( tab_evo_ben_pa, anio ==  REP$corte)$ben12,
                             nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_22_h <- format( filter( tab_evo_ben_pa, anio ==  REP$corte)$sexobenM,
                           nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )


REP$dem_pa_22_h_porc <- format( 100 * filter( tab_evo_ben_pa, anio ==  REP$corte)$sexobenM / 
                                  filter( tab_evo_ben_pa, anio ==  REP$corte)$ben,
                                nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )


REP$dem_pa_22_m <- format( filter( tab_evo_ben_pa, anio ==  REP$corte)$sexobenF,
                           nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_22_m_porc <- format( 100 * filter( tab_evo_ben_pa, anio ==  REP$corte)$sexobenF / 
                                  filter( tab_evo_ben_pa, anio ==  REP$corte)$ben,
                                nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_incr_21_22 <- format(
  abs( filter( tab_evo_ben_pa, anio ==  REP$corte)$tasa_crecimiento ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_tot_ingr_22 <- format( filter( tab_evo_monto_pa, anio ==  REP$corte)$tot_ingr,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_tot_ingr_21 <- format( filter( tab_evo_monto_pa, anio ==  REP$corte - 1 )$tot_ingr,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_tot_ingr_21_22_crec <- format( 
  abs( 100 * filter( tab_evo_monto_pa, anio ==  REP$corte)$tot_ingr / 
         filter( tab_evo_monto_pa, anio ==  REP$corte - 1 )$tot_ingr - 100),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_rentas_22 <- format( filter( tab_evo_monto_pa, anio ==  REP$corte)$renta_mensual,
                                nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_rentas_22_porc <- format( 100 * filter( tab_evo_monto_pa, anio ==  REP$corte)$renta_mensual /
                                       filter( tab_evo_monto_pa, anio ==  REP$corte)$tot_ingr,
                                     nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_dec_terc_22 <- format( filter( tab_evo_monto_pa, anio ==  REP$corte)$decimo_tercero,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_dec_terc_22_porc <- format( 100 * filter( tab_evo_monto_pa, anio ==  REP$corte)$decimo_tercero /
                                         filter( tab_evo_monto_pa, anio ==  REP$corte)$tot_ingr,
                                       nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_dec_cuar_22 <- format( filter( tab_evo_monto_pa, anio ==  REP$corte)$decimo_cuarto,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_dec_cuar_22_porc <- format( 100 * filter( tab_evo_monto_pa, anio ==  REP$corte)$decimo_cuarto /
                                         filter( tab_evo_monto_pa, anio ==  REP$corte)$tot_ingr,
                                       nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_tot_desc_22 <- format( filter( tab_evo_monto_pa, anio ==  REP$corte)$tot_desc,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_tot_desc_21 <- format( filter( tab_evo_monto_pa, anio ==  REP$corte - 1 )$tot_desc,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_tot_desc_21_22_crec <- format( 
  100 * filter( tab_evo_monto_pa, anio ==  REP$corte)$tot_desc / 
    filter( tab_evo_monto_pa, anio ==  REP$corte - 1 )$tot_desc - 100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_liq_pagar_22 <- format( filter( tab_evo_monto_pa, anio ==  REP$corte)$liq_pagar,
                                   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_tot_desc_22_rel <- format( 100 * filter( tab_evo_monto_pa, anio ==  REP$corte )$tot_desc / 
                                        filter( tab_evo_monto_pa, anio ==  REP$corte )$liq_pagar,
                                      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_edad_h <- format( filter( dem_pa, sexo == 'M' )$prom_edad,
                             nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_edad_m <- format( filter( dem_pa, sexo == 'F' )$prom_edad,
                             nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_prom_h <- format( filter( dem_pa, sexo == 'M' )$prom_tot_ingr,
                             nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_pa_prom_m <- format( filter( dem_pa, sexo == 'F' )$prom_tot_ingr,
                             nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

## Orfandad-----------------------------------------------------------------------------------------

REP$dem_of_22 <- format( filter( tab_evo_ben_of, anio ==  REP$corte)$ben,
                         nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_22_dic <- format( filter( tab_evo_ben_of, anio ==  REP$corte)$ben12,
                             nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_22_h <- format( filter( tab_evo_ben_of, anio ==  REP$corte)$sexobenM,
                           nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )


REP$dem_of_22_h_porc <- format( 100 * filter( tab_evo_ben_of, anio ==  REP$corte)$sexobenM / 
                                  filter( tab_evo_ben_of, anio ==  REP$corte)$ben,
                                nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )


REP$dem_of_22_m <- format( filter( tab_evo_ben_of, anio ==  REP$corte)$sexobenF,
                           nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_22_m_porc <- format( 100 * filter( tab_evo_ben_of, anio ==  REP$corte)$sexobenF / 
                                  filter( tab_evo_ben_of, anio ==  REP$corte)$ben,
                                nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_incr_21_22 <- format(
  abs( filter( tab_evo_ben_of, anio ==  REP$corte)$tasa_crecimiento ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_tot_ingr_22 <- format( filter( tab_evo_monto_of, anio ==  REP$corte)$tot_ingr,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_tot_ingr_21 <- format( filter( tab_evo_monto_of, anio ==  REP$corte - 1 )$tot_ingr,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_tot_ingr_21_22_crec <- format( 
  abs( 100 * filter( tab_evo_monto_of, anio ==  REP$corte)$tot_ingr / 
         filter( tab_evo_monto_of, anio ==  REP$corte - 1 )$tot_ingr - 100),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_rentas_22 <- format( filter( tab_evo_monto_of, anio ==  REP$corte)$renta_mensual,
                                nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_rentas_22_porc <- format( 100 * filter( tab_evo_monto_of, anio ==  REP$corte)$renta_mensual /
                                       filter( tab_evo_monto_of, anio ==  REP$corte)$tot_ingr,
                                     nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_dec_terc_22 <- format( filter( tab_evo_monto_of, anio ==  REP$corte)$decimo_tercero,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_dec_terc_22_porc <- format( 100 * filter( tab_evo_monto_of, anio ==  REP$corte)$decimo_tercero /
                                         filter( tab_evo_monto_of, anio ==  REP$corte)$tot_ingr,
                                       nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_dec_cuar_22 <- format( filter( tab_evo_monto_of, anio ==  REP$corte)$decimo_cuarto,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_dec_cuar_22_porc <- format( 100 * filter( tab_evo_monto_of, anio ==  REP$corte)$decimo_cuarto /
                                         filter( tab_evo_monto_of, anio ==  REP$corte)$tot_ingr,
                                       nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_tot_desc_22 <- format( filter( tab_evo_monto_of, anio ==  REP$corte)$tot_desc,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_tot_desc_21 <- format( filter( tab_evo_monto_of, anio ==  REP$corte - 1 )$tot_desc,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_tot_desc_21_22_crec <- format( 
  100 * filter( tab_evo_monto_of, anio ==  REP$corte)$tot_desc / 
    filter( tab_evo_monto_of, anio ==  REP$corte - 1 )$tot_desc - 100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_liq_pagar_22 <- format( filter( tab_evo_monto_of, anio ==  REP$corte)$liq_pagar,
                                   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_tot_desc_22_rel <- format( 100 * filter( tab_evo_monto_of, anio ==  REP$corte )$tot_desc / 
                                        filter( tab_evo_monto_of, anio ==  REP$corte )$liq_pagar,
                                      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_edad_h <- format( filter( dem_orf, sexo == 'M' )$prom_edad,
                             nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_edad_m <- format( filter( dem_orf, sexo == 'F' )$prom_edad,
                             nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_prom_h <- format( filter( dem_orf, sexo == 'M' )$prom_tot_ingr,
                             nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_of_prom_m <- format( filter( dem_orf, sexo == 'F' )$prom_tot_ingr,
                             nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

## Viudez-------------------------------------------------------------------------------------------

REP$dem_vo_22 <- format( filter( tab_evo_ben_vo, anio ==  REP$corte)$ben,
                         nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_22_dic <- format( filter( tab_evo_ben_vo, anio ==  REP$corte)$ben12,
                             nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_22_h <- format( filter( tab_evo_ben_vo, anio ==  REP$corte)$sexobenM,
                           nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )


REP$dem_vo_22_h_porc <- format( 100 * filter( tab_evo_ben_vo, anio ==  REP$corte)$sexobenM / 
                                  filter( tab_evo_ben_vo, anio ==  REP$corte)$ben,
                                nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )


REP$dem_vo_22_m <- format( filter( tab_evo_ben_vo, anio ==  REP$corte)$sexobenF,
                           nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_22_m_porc <- format( 100 * filter( tab_evo_ben_vo, anio ==  REP$corte)$sexobenF / 
                                  filter( tab_evo_ben_vo, anio ==  REP$corte)$ben,
                                nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_incr_21_22 <- format(
  abs( filter( tab_evo_ben_vo, anio ==  REP$corte)$tasa_crecimiento ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_tot_ingr_22 <- format( filter( tab_evo_monto_vo, anio ==  REP$corte)$tot_ingr,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_tot_ingr_21 <- format( filter( tab_evo_monto_vo, anio ==  REP$corte - 1 )$tot_ingr,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_tot_ingr_21_22_crec <- format( 
  abs( 100 * filter( tab_evo_monto_vo, anio ==  REP$corte)$tot_ingr / 
         filter( tab_evo_monto_vo, anio ==  REP$corte - 1 )$tot_ingr - 100),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_rentas_22 <- format( filter( tab_evo_monto_vo, anio ==  REP$corte)$renta_mensual,
                                nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_rentas_22_porc <- format( 100 * filter( tab_evo_monto_vo, anio ==  REP$corte)$renta_mensual /
                                       filter( tab_evo_monto_vo, anio ==  REP$corte)$tot_ingr,
                                     nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_dec_terc_22 <- format( filter( tab_evo_monto_vo, anio ==  REP$corte)$decimo_tercero,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_dec_terc_22_porc <- format( 100 * filter( tab_evo_monto_vo, anio ==  REP$corte)$decimo_tercero /
                                         filter( tab_evo_monto_vo, anio ==  REP$corte)$tot_ingr,
                                       nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_dec_cuar_22 <- format( filter( tab_evo_monto_vo, anio ==  REP$corte)$decimo_cuarto,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_dec_cuar_22_porc <- format( 100 * filter( tab_evo_monto_vo, anio ==  REP$corte)$decimo_cuarto /
                                         filter( tab_evo_monto_vo, anio ==  REP$corte)$tot_ingr,
                                       nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_tot_desc_22 <- format( filter( tab_evo_monto_vo, anio ==  REP$corte)$tot_desc,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_tot_desc_21 <- format( filter( tab_evo_monto_vo, anio ==  REP$corte - 1 )$tot_desc,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_tot_desc_21_22_crec <- format( 
  100 * filter( tab_evo_monto_vo, anio ==  REP$corte)$tot_desc / 
    filter( tab_evo_monto_vo, anio ==  REP$corte - 1 )$tot_desc - 100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_liq_pagar_22 <- format( filter( tab_evo_monto_vo, anio ==  REP$corte)$liq_pagar,
                                   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_tot_desc_22_rel <- format( 100 * filter( tab_evo_monto_vo, anio ==  REP$corte )$tot_desc / 
                                        filter( tab_evo_monto_vo, anio ==  REP$corte )$liq_pagar,
                                      nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_edad_h <- format( filter( dem_vo, sexo == 'M' )$prom_edad,
                             nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_edad_m <- format( filter( dem_vo, sexo == 'F' )$prom_edad,
                             nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_prom_h <- format( filter( dem_vo, sexo == 'M' )$prom_tot_ingr,
                             nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_vo_prom_m <- format( filter( dem_vo, sexo == 'F' )$prom_tot_ingr,
                             nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

#Información contable-------------------------------------------------------------------------------

REP$inf_cont_activo_inc_max <- format(
  max_incre <- activo_del_fondo %>% 
    dplyr::select( incremento_porcentual_anual ) %>%  unlist() %>%  max(), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_activo_inc_2016 <- format(
  filter( activo_del_fondo, ano == 2016 )$incremento_porcentual_anual, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_activo_2014 <- format(
  filter( activo_del_fondo, ano == 2014 )$activo, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_activo_2022 <- format(
  filter( activo_del_fondo, ano == 2022 )$activo, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_incre_2014_2022 <- format(
  ((filter( activo_del_fondo, ano == 2022 )$activo-
      filter( activo_del_fondo, ano == 2014 )$activo)/
     (filter( activo_del_fondo, ano == 2014 )$activo))*100, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_incr_anual_9anios <- format(
  incre_anuales <- activo_del_fondo %>% 
    dplyr::select(incremento_porcentual_anual) %>% 
    unlist() %>% mean(), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_comp_acti_porcen_biess_2022 <- format(
  (filter( analisis_componentes_activo, descripcion_de_las_cuentas ==
             "Recursos Adm. BIESS" )$x2022/
     filter( analisis_componentes_activo, descripcion_de_las_cuentas ==
               "Total activo")$x2022 )*100, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_comp_acti_cuenta_cobrar_2022 <- format(
  filter( analisis_componentes_activo, descripcion_de_las_cuentas ==
            "Cuentas por cobrar" )$x2022, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_comp_acti_porcen_cuentas_cobrar_2022 <- format(
  (filter( analisis_componentes_activo, descripcion_de_las_cuentas ==
             "Cuentas por cobrar" )$x2022/
     filter( analisis_componentes_activo, descripcion_de_las_cuentas ==
               "Total activo" )$x2022)*100, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_cobrar_fondo_porce_cc_2022 <- format(
  (filter( analisis_componentes_cobrar_fondo, cuenta ==
             "Deuda de Gobierno" )$x2022/
     filter(analisis_componentes_cobrar_fondo, cuenta ==
              "Total")$x2022)*100, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_cobrar_fondo_deu_gobierno_2022 <- format(
  filter( analisis_componentes_cobrar_fondo, cuenta ==
            "Deuda de Gobierno" )$x2022, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_cobrar_fondo_cc_admin_2022 <- format(
  filter( analisis_componentes_cobrar_fondo, cuenta ==
            "CxC a administradoras" )$x2022, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_cobrar_fondo_porce_cc_admin_2022 <- format(
  ( filter( analisis_componentes_cobrar_fondo, cuenta ==
              "CxC a administradoras" )$x2022/
      filter( analisis_componentes_cobrar_fondo, cuenta ==
                "Total" )$x2022)*100, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_cobrar_fondo_neteos_2022 <- format(
  filter( analisis_componentes_cobrar_fondo, cuenta ==
            "Otras CxC/ Neteos/ RP" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_comp_acti_prop_planta_equ_2022 <- format(
  filter( analisis_componentes_activo, descripcion_de_las_cuentas ==
            "Propiedad planta y equipo" )$x2022, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_cobrar_fondo_porce_prop_planta_equ_2022 <- format(
  (filter( analisis_componentes_activo, descripcion_de_las_cuentas ==
             "Propiedad planta y equipo" )$x2022/
     filter(analisis_componentes_activo, descripcion_de_las_cuentas ==
              "Total activo")$x2022)*100, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_comp_acti_fondos_disp_2022 <- format(
  filter( analisis_componentes_activo, descripcion_de_las_cuentas ==
            "Fondos disponibles" )$x2022, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_comp_acti_porce_fondos_disp_2022 <- format(
  (filter( analisis_componentes_activo, descripcion_de_las_cuentas ==
             "Fondos disponibles" )$x2022/
     filter( analisis_componentes_activo, descripcion_de_las_cuentas ==
               "Total activo" )$x2022)*100, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_hori_activo_crecimiento_promedio_rec_adm_2014_2022 <- format(
  crecimiento_promedio <- analisis_horizontal_activo %>% 
    filter( descripcion_de_las_cuentas == "Recursos Adm. BIESS" ) %>% 
    dplyr::select( -descripcion_de_las_cuentas ) %>% 
    unlist() %>%  mean(), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_hori_activo_max_crecimiento_rec_adm <- format(
  max_crecimiento <- analisis_horizontal_activo %>% 
    filter( descripcion_de_las_cuentas == "Recursos Adm. BIESS" ) %>% 
    dplyr::select( -descripcion_de_las_cuentas ) %>% 
    unlist() %>%  max(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_hori_activo_min_crecimiento_rec_adm <- format(
  min_crecimiento <- analisis_horizontal_activo %>% 
    filter( descripcion_de_las_cuentas == "Recursos Adm. BIESS" ) %>% 
    dplyr::select( -descripcion_de_las_cuentas ) %>% 
    unlist() %>%  min(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_hori_activo_crecimiento_promedio_rec_adm_2016_2022 <- format(
  crecimiento_promedio <- analisis_horizontal_activo %>% 
    filter( descripcion_de_las_cuentas == "Recursos Adm. BIESS" ) %>% 
    dplyr::select(x2016_2015,x2022_2021) %>% 
    unlist() %>%  mean(), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_hori_activo_crecimiento_promedio_cxc_2014_2022 <- format(
  crecimiento_promedio <- analisis_horizontal_activo %>% 
    filter( descripcion_de_las_cuentas == "Cuentas por cobrar" ) %>% 
    dplyr::select(-descripcion_de_las_cuentas) %>% 
    unlist() %>%  mean(), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_hori_activo_crec_cxc_2021 <- format(
  filter( analisis_horizontal_activo,
          descripcion_de_las_cuentas =="Cuentas por cobrar" )$x2021_2020, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_hori_activo_crec_cxc_2022 <- format(
  filter( analisis_horizontal_activo,
          descripcion_de_las_cuentas =="Cuentas por cobrar" )$x2022_2021, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_hori_activo_min_crecimiento_fondos <- format(
  min_crecimiento <- analisis_horizontal_activo %>% 
    filter( descripcion_de_las_cuentas == "Fondos disponibles" ) %>% 
    dplyr::select( -descripcion_de_las_cuentas ) %>% 
    unlist() %>%  min(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_hori_activo_max_crecimiento_fondos <- format(
  min_crecimiento <- analisis_horizontal_activo %>% 
    filter( descripcion_de_las_cuentas == "Fondos disponibles") %>% 
    dplyr::select(-descripcion_de_las_cuentas ) %>% 
    unlist() %>%  max(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_hori_activo_crec_fondos_2021 <- format(
  abs( filter( analisis_horizontal_activo,
               descripcion_de_las_cuentas =="Fondos disponibles" )$x2021_2020 ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_vert_activo_crecimiento_promedio_rec_adm_2014_2022 <- format(
  crecimiento_promedio <- analisis_vertical_activo %>% 
    filter( descripcion_de_las_cuentas == "Recursos Adm. BIESS" ) %>% 
    dplyr::select( -descripcion_de_las_cuentas ) %>% 
    unlist() %>%  mean(), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_vert_activo_crecimiento_promedio_cxc_2014_2022 <- format(
  crecimiento_promedio <- analisis_vertical_activo %>% 
    filter( descripcion_de_las_cuentas == "Cuentas por cobrar" ) %>% 
    dplyr::select( -descripcion_de_las_cuentas ) %>% 
    unlist() %>%  mean(), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_vert_activo_crecimiento_promedio_planta_eq_2014_2022 <- format(
  crecimiento_promedio <- analisis_vertical_activo %>% 
    filter( descripcion_de_las_cuentas == "Propiedad planta y equipo" ) %>% 
    dplyr::select( -descripcion_de_las_cuentas ) %>% 
    unlist() %>%  mean(), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_vert_activo_crecimiento_promedio_fondos_2014_2022 <- format(
  crecimiento_promedio <- analisis_vertical_activo %>% 
    filter( descripcion_de_las_cuentas == "Fondos disponibles" ) %>% 
    dplyr::select( -descripcion_de_las_cuentas ) %>% 
    unlist() %>%  mean(), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivos_fondo_2022 <- format(
  filter( pasivos_fondo, ano ==2022 )$pasivo, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivos_fondo_incre_2022 <- format(
  filter( pasivos_fondo, ano ==2022 )$incremento_anual, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivos_fondo_incre_porcen_2022 <- format(
  filter( pasivos_fondo, ano ==2022 )$incremento_porcentual_anual, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivos_fondo_prom_incre_anual_2014_2022 <- format(
  crecimiento_promedio <- pasivos_fondo %>% 
    dplyr::select( incremento_porcentual_anual ) %>% 
    unlist() %>%  mean(), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivos_fondo_max_incre_anual <- format(
  max_crecimiento <- pasivos_fondo %>% 
    dplyr::select( incremento_porcentual_anual ) %>% 
    unlist() %>%  max(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivos_fondo_min_incre_anual <- format(
  min_crecimiento <- pasivos_fondo %>% 
    dplyr::select( incremento_porcentual_anual ) %>% 
    unlist() %>%  min(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivos_fondo_pasivos_2017 <- format(
  filter( pasivos_fondo, ano ==2017 )$pasivo, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivos_fondo_pasivos_2015 <- format(
  filter( pasivos_fondo, ano ==2015 )$pasivo, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_comp_pasivos_corrientes_2022 <- format(
  filter( componentes_pasivos_fondo, 
          descripcion_de_las_cuentas == "Pasivos corrientes" )$x2022, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_comp_pasivos_porcen_pasivos_corr <- format(
  (filter( componentes_pasivos_fondo, 
           descripcion_de_las_cuentas == "Pasivos corrientes" )$x2022/
     filter( componentes_pasivos_fondo, descripcion_de_las_cuentas ==
               "Total pasivo")$x2022)*100, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_comp_pasivos_no_corrientes_2022 <- format(
  filter( componentes_pasivos_fondo, 
          descripcion_de_las_cuentas == "Pasivos no corrientes" )$x2022, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_comp_pasivos_porcen_pas_no_corrientes <- format(
  (filter( componentes_pasivos_fondo, 
           descripcion_de_las_cuentas == "Pasivos no corrientes")$x2022/
     filter( componentes_pasivos_fondo, descripcion_de_las_cuentas ==
               "Total pasivo" )$x2022)*100, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivo_horizontal_corrientes_2022 <- format(
  filter( analisis_horizontal_pasivos, descripcion_de_las_cuentas ==
            "Pasivos corrientes" )$x2022_2021, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_comp_pasivo_horizontal_no_corrientes_2022 <- format(
  abs( filter( analisis_horizontal_pasivos, descripcion_de_las_cuentas ==
                 "Pasivos no corrientes" )$x2022_2021 ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivo_horizontal_prom_pasivos <- format(
  crecimiento_promedio <- analisis_horizontal_pasivos %>%
    filter( descripcion_de_las_cuentas == "Total pasivo" ) %>%
    dplyr::select( x2016_2015:x2022_2021 ) %>% 
    unlist() %>%  mean(na.omit=TRUE),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivo_horizontal_prom_pasivos_corrientes <- format(
  crecimiento_promedio <- analisis_horizontal_pasivos %>%
    filter( descripcion_de_las_cuentas == "Pasivos corrientes" ) %>%
    dplyr::select( x2016_2015:x2022_2021 ) %>% 
    unlist() %>%  mean(na.omit=TRUE),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivo_horizontal_prom_pasivos_no_corrientes <- format(
  crecimiento_promedio <- analisis_horizontal_pasivos %>%
    filter( descripcion_de_las_cuentas == "Pasivos no corrientes" ) %>%
    dplyr::select( x2016_2015:x2022_2021 ) %>% 
    unlist() %>%  mean(na.omit=TRUE),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivo_vert_pasivos_corrientes_2022 <- format(
  filter( analisis_vertical_pasivos, 
          descripcion_de_las_cuentas == "Pasivos corrientes" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivo_vert_pasivos_no_corrientes_2022 <- format(
  filter( analisis_vertical_pasivos, 
          descripcion_de_las_cuentas == "Pasivos no corrientes" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivo_vert_prom_pasivos_corrientes_2015_2022 <- format(
  crecimiento_promedio <- analisis_vertical_pasivos %>%
    filter( descripcion_de_las_cuentas == "Pasivos corrientes" ) %>%
    dplyr::select( x2015:x2022 ) %>% 
    unlist() %>%  mean(na.omit=TRUE),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivo_vert_prom_pasivos_no_corrientes_2015_2022 <- format(
  crecimiento_promedio <- analisis_vertical_pasivos %>%
    filter( descripcion_de_las_cuentas == "Pasivos no corrientes" ) %>%
    dplyr::select( x2015:x2022 ) %>% 
    unlist() %>%  mean(na.omit=TRUE),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_pasivo_vert_max_no_corrientes <- format(
  crecimiento_promedio <- analisis_vertical_pasivos %>%
    filter( descripcion_de_las_cuentas == "Pasivos no corrientes" ) %>%
    dplyr::select( -descripcion_de_las_cuentas ) %>% 
    unlist() %>%  max(na.omit=TRUE),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_2022 <- format(
  filter( patrimonio_fondo, ano == 2022 )$patrimonio,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_incre_2022 <- format(
  filter( patrimonio_fondo, ano == 2022 )$incremento_anual,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_incre_porcen_2022 <- format(
  filter( patrimonio_fondo, ano == 2022 )$incremento_porcentual_anual,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_2014 <- format(
  filter( patrimonio_fondo, ano == 2014 )$patrimonio,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_incre_patromonio_2014_2022 <- format(
  ((filter( patrimonio_fondo, ano == 2022 )$patrimonio-
      filter( patrimonio_fondo, ano == 2014 )$patrimonio)/
     (filter( patrimonio_fondo, ano == 2014 )$patrimonio))*100, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_incre_prom_2014_2022 <- format(
  crecimiento_promedio <- patrimonio_fondo %>%
    dplyr::select( incremento_porcentual_anual ) %>% 
    unlist() %>%  mean(na.omit=TRUE),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_dismin_2015_2016 <- format(
  filter( patrimonio_fondo, ano == 2015 )$incremento_porcentual_anual - 
    filter( patrimonio_fondo, ano == 2016 )$incremento_porcentual_anual,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_min_incre_porcentual <- format(
  crecimiento_min <- patrimonio_fondo %>%
    dplyr::select( incremento_porcentual_anual ) %>% 
    unlist() %>%  min(na.omit=TRUE),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_incre_por_2016 <- format(
  filter( patrimonio_fondo, ano == 2016 )$incremento_porcentual_anual,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_comp_patri_superavit_2022 <- format(
  filter( componentes_patrimonio_fondo, 
          descripcion_de_las_cuentas == "Superávit acumulado")$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_comp_patri_deficit_2022 <- format(
  abs( filter( componentes_patrimonio_fondo, 
               descripcion_de_las_cuentas == "Déficit acumulado")$x2022 ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_horizontal_superavit_2014_2022 <- format(
  crecimiento_promedio <- analisis_horizontal_patrimonio %>%
    filter( descripcion_de_las_cuentas == "Superávit acumulado" ) %>%
    dplyr::select( x2014_2013:x2022_2021 ) %>% 
    unlist() %>%  mean(na.omit=TRUE),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_comp_patri_deficit_2017 <- format(
  abs( filter( componentes_patrimonio_fondo, 
               descripcion_de_las_cuentas == "Déficit acumulado" )$x2017 ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_horizontal_total_min <- format(
  crecimiento_min <- analisis_horizontal_patrimonio %>%
    filter ( descripcion_de_las_cuentas == "Total") %>% 
    dplyr::select( -descripcion_de_las_cuentas ) %>% 
    unlist() %>%  min(na.omit=TRUE),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_horizontal_total_max <- format(
  crecimiento_min <- analisis_horizontal_patrimonio %>%
    filter ( descripcion_de_las_cuentas == "Total") %>% 
    dplyr::select( -descripcion_de_las_cuentas ) %>% 
    unlist() %>%  max(na.omit=TRUE),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_vertical_superavit_prom_2014_2022 <- format(
  crecimiento_min <- analisis_vertical_patrimonio %>%
    filter ( descripcion_de_las_cuentas == "Superávit acumulado") %>% 
    dplyr::select( -descripcion_de_las_cuentas ) %>% 
    unlist() %>%  mean( na.omit=TRUE ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_vertical_deficit_prom_2014_2022 <- format(
  crecimiento_min <- analisis_vertical_patrimonio %>%
    filter ( descripcion_de_las_cuentas == "Déficit acumulado") %>% 
    dplyr::select( -descripcion_de_las_cuentas ) %>% 
    unlist() %>%  mean( na.omit=TRUE ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_patrimonio_vertical_res_ejer_prom_2014_2022 <- format(
  crecimiento_min <- analisis_vertical_patrimonio %>%
    filter ( descripcion_de_las_cuentas == "Resultado del ejercicio") %>% 
    dplyr::select( -descripcion_de_las_cuentas ) %>% 
    unlist() %>%  mean( na.omit=TRUE ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_2022 <- format(
  filter( ingresos_fondo, ano == 2022 )$ingresos,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_incremento_2022 <- format(
  filter( ingresos_fondo, ano == 2022 )$incremento_anual,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_incremento_porc_2022 <- format(
  filter( ingresos_fondo, ano == 2022 )$incremento_porcentual_anual,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_incremento_porc_2014 <- format(
  filter( ingresos_fondo, ano == 2014 )$incremento_porcentual_anual,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_incremento_porc_2016 <- format(
  abs( filter( ingresos_fondo, ano == 2016 )$incremento_porcentual_anual ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_2015 <- format(
  filter( ingresos_fondo, ano == 2015 )$ingresos,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_2016 <- format(
  filter( ingresos_fondo, ano == 2016 )$ingresos,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_com_ingresos_total_2022 <- format(
  filter( componentes_ingresos, ingresos == "Total" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_com_ingresos_financieros_2022 <- format(
  filter( componentes_ingresos, ingresos == "Ingresos financieros" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_com_ingresos_patronales_2022 <- format(
  filter( componentes_ingresos, ingresos == "Aportes de afiliados y patronales" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_com_ingresos_contr_estado_2022 <- format(
  filter( componentes_ingresos, ingresos == "Contribuciones del Estado" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_com_ingresos_complemen_2022 <- format(
  filter( componentes_ingresos, ingresos == "Ing. complementarios oper." )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_com_ingresos_extraor_2022 <- format(
  filter( componentes_ingresos, ingresos == "Ingresos extraordinarios (RP)" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_com_ingresos_arriendo_2022 <- format(
  filter( componentes_ingresos, ingresos == "Ingresos por arriendo y realiz." )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_horizontal_total_2014 <- format(
  filter( analisis_horizontal_ingresos, ingresos == "Total" )$x2014_2013,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_horizontal_total_2015 <- format(
  abs( filter( analisis_horizontal_ingresos, ingresos == "Total" )$x2015_2014 ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_horizontal_total_2016 <- format(
  abs( filter( analisis_horizontal_ingresos, ingresos == "Total" )$x2016_2015 ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_horizontal_total_2017 <- format(
  filter( analisis_horizontal_ingresos, ingresos == "Total" )$x2017_2016,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_horizontal_total_2018 <- format(
  filter( analisis_horizontal_ingresos, ingresos == "Total" )$x2018_2017 ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_horizontal_max_total <- format(
  crec_max <- analisis_horizontal_ingresos %>% 
    filter( ingresos == "Total" ) %>% 
    dplyr::select( -ingresos ) %>% unlist() %>% max(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_horizontal_total_2020 <- format(
  filter( analisis_horizontal_ingresos, ingresos == "Total" )$x2020_2019 ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_horizontal_total_2021 <- format(
  filter( analisis_horizontal_ingresos, ingresos == "Total" )$x2021_2020 ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_horizontal_total_2022 <- format(
  filter( analisis_horizontal_ingresos, ingresos == "Total" )$x2022_2021 ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_vertical_prom_aportes_2014_2015 <- format(
  promedio <- analisis_vertical_ingresos %>% 
    filter( ingresos == "Aportes de afiliados y patronales" ) %>% 
    dplyr::select( x2014:x2015 ) %>% unlist() %>% mean(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_vertical_aporte_2015 <- format(
  filter( analisis_vertical_ingresos, ingresos ==
            "Aportes de afiliados y patronales" )$x2015 ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_vertical_aporte_2016 <- format(
  filter( analisis_vertical_ingresos, ingresos ==
            "Aportes de afiliados y patronales" )$x2016 ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_com_ingresos_aporte_min <- format(
  apor_min <- analisis_vertical_ingresos %>% 
    filter( ingresos == "Aportes de afiliados y patronales" ) %>% 
    dplyr::select( -ingresos ) %>% unlist() %>% min(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_vertical_aporte_2021 <- format(
  filter( analisis_vertical_ingresos, ingresos ==
            "Aportes de afiliados y patronales" )$x2021 ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_vertical_aporte_2022 <- format(
  filter( analisis_vertical_ingresos, ingresos ==
            "Aportes de afiliados y patronales" )$x2022 ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_vertical_aporte_2014 <- format(
  filter( analisis_vertical_ingresos, ingresos ==
            "Aportes de afiliados y patronales" )$x2014 ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_vertical_finan_2014 <- format(
  filter( analisis_vertical_ingresos, ingresos == "Ingresos financieros" )$x2014 ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_vertical_finan_2022 <- format(
  filter( analisis_vertical_ingresos, ingresos == "Ingresos financieros" )$x2022 ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_vertical_contr_estad_2022 <- format(
  filter( analisis_vertical_ingresos, ingresos == "Contribuciones del Estado" )$x2022 ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_vertical_complementarios_2022 <- format(
  filter( analisis_vertical_ingresos, ingresos == "Ing. complementarios oper." )$x2022 ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_ingresos_vertical_estraord_2022 <- format(
  filter( analisis_vertical_ingresos, ingresos == "Ingresos extraordinarios (RP)" )$x2022 ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_2022 <- format(
  filter( gastos, ano == 2022 )$gastos ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_incremento_2022 <- format(
  filter( gastos, ano == 2022 )$incremento_anual ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_incremento_porce_2022 <- format(
  filter( gastos, ano == 2022 )$incremento_porcentual_anual_percent ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_gastos_prom_incre_porcent_2014_2022 <- format(
  crecimiento_min <- gastos %>%
    dplyr::select( incremento_porcentual_anual_percent ) %>% 
    unlist() %>%  mean( na.omit=TRUE ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_comp_rtr_2022 <- format(
  filter( componentes_gastos, gastos == 
            "Pensiones por Riesgos del Trabajo" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_comp_montepio_2022 <- format(
  filter( componentes_gastos, gastos == 
            "Pensiones de Montepío" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_comp_subsidios_2022 <- format(
  filter( componentes_gastos, gastos == 
            "Gastos prestacionales por Subsidios" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_comp_personal_2022 <- format(
  filter( componentes_gastos, gastos == 
            "Gastos Direc. Person. (Serv. Prest)" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_comp_directos_2022 <- format(
  filter( componentes_gastos, gastos == 
            "Otros Gastos Directos" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_comp_administrativos_2022 <- format(
  filter( componentes_gastos, gastos == 
            "Gastos de Administración" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_comp_financieros_2022 <- format(
  filter( componentes_gastos, gastos == 
            "Gastos Financieros" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_horizontal_total_2014 <- format(
  filter( analisis_horizontal_gastos, gastos == 
            "Total gastos" )$x2014_2013,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_horizontal_total_2015 <- format(
  filter( analisis_horizontal_gastos, gastos == 
            "Total gastos" )$x2015_2014,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_horizontal_total_prom_2016_2020 <- format(
  prom <- analisis_horizontal_gastos %>%
    filter( gastos == "Total gastos" ) %>% 
    dplyr::select( x2016_2015:x2020_2019 ) %>% 
    unlist() %>% mean() %>% abs(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_horizontal_total_2021 <- format(
  filter( analisis_horizontal_gastos, gastos == 
            "Total gastos" )$x2021_2020,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_horizontal_total_2022 <- format(
  filter( analisis_horizontal_gastos, gastos == 
            "Total gastos" )$x2022_2021,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_horizontal_total_max <- format(
  prom_min <- analisis_horizontal_gastos %>%
    filter( gastos == "Total gastos" ) %>% 
    dplyr::select( -gastos ) %>% unlist() %>% max(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_horizontal_total_min <- format(
  prom_min <- analisis_horizontal_gastos %>%
    filter( gastos == "Total gastos" ) %>% 
    dplyr::select( -gastos ) %>% unlist() %>% min(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_horizontal_prt_prom <- format(
  prom <- analisis_horizontal_gastos %>%
    filter( gastos == "Pensiones por Riesgos del Trabajo" ) %>% 
    dplyr::select( -gastos ) %>% unlist() %>% mean(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_horizontal_montepio_prom <- format(
  prom <- analisis_horizontal_gastos %>%
    filter( gastos == "Pensiones de Montepío" ) %>% 
    dplyr::select( -gastos ) %>%  unlist() %>% mean(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_horizontal_subsidios_min <- format(
  prom_min <- analisis_horizontal_gastos %>%
    filter( gastos == "Gastos prestacionales por Subsidios" ) %>% 
    dplyr::select( -gastos ) %>% unlist() %>% min(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_horizontal_subsidios_max <- format(
  prom_min <- analisis_horizontal_gastos %>%
    filter( gastos == "Gastos prestacionales por Subsidios" ) %>% 
    dplyr::select( -gastos ) %>% unlist() %>% max(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_horizontal_admin_max <- format(
  prom_min <- analisis_horizontal_gastos %>%
    filter( gastos == "Gastos de Administración" ) %>% 
    dplyr::select( -gastos ) %>% unlist() %>% max(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_vertical_prt_2022 <- format(
  filter( analisis_vertical_gastos, gastos == 
            "Pensiones por Riesgos del Trabajo")$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_vertical_montepio_2022 <- format(
  filter( analisis_vertical_gastos, gastos == 
            "Pensiones de Montepío")$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_vertical_subsidios_2022 <- format(
  filter( analisis_vertical_gastos, gastos == 
            "Gastos prestacionales por Subsidios" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_vertical_personal_2022 <- format(
  filter( analisis_vertical_gastos, gastos == 
            "Gastos Direc. Person. (Serv. Prest)" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_vertical_admin_2022 <- format(
  filter( analisis_vertical_gastos, gastos == 
            "Gastos de Administración" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_gastos_vertical_financieros_2022 <- format(
  filter( analisis_vertical_gastos, gastos == 
            "Gastos Financieros" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_resul_ejer_2022 <- format(
  filter( ingresos_vs_gastos, ano == 2022 )$resultado_del_ejercicio,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_resul_ejer_gastos_2022 <- format(
  filter( ingresos_vs_gastos, ano == 2022 )$gastos,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_resul_ejer_ingresos_2022 <- format(
  filter( ingresos_vs_gastos, ano == 2022 )$ingresos,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_resul_ejer_utilizacion_2022 <- format(
  filter( ingresos_vs_gastos, ano == 2022 )$utilizacion,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_resul_ejer_2016_2022 <- format(
  ((filter( ingresos_vs_gastos, ano == 2022)$resultado_del_ejercicio - 
      filter( ingresos_vs_gastos, ano == 2016)$resultado_del_ejercicio) / 
     filter( ingresos_vs_gastos, ano == 2016)$resultado_del_ejercicio) * 100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_resul_ejer_2016 <- format(
  filter( ingresos_vs_gastos, ano == 2016 )$resultado_del_ejercicio,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_resul_ejer_2022 <- format(
  filter( ingresos_vs_gastos, ano == 2022 )$resultado_del_ejercicio,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_cont_resul_ejer_utilizacion_prom_2014_2022 <- format(
  prom <- ingresos_vs_gastos %>% 
    dplyr::select( utilizacion ) %>% unlist() %>% mean(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

#Inversiones--------------------------------------------------------------------
REP$inver_recurs_adm_bies_total_2022 <- format( 
  filter( recurs_adm_biess, ano == 2022 )$total,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_recurs_adm_bies_inver_2022 <- format( 
  filter( recurs_adm_biess, ano == 2022 )$inversiones,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_recurs_adm_bies_caja_2022 <- format( 
  filter( recurs_adm_biess, ano == 2022 )$caja,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_recurs_adm_bies_rendi_pond_2022 <- format( 
  filter( recurs_adm_biess, ano == 2022 )$rendimiento_ponderado*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_recurs_adm_bies_rendi_neto_2022 <- format( 
  filter( recurs_adm_biess, ano == 2022 )$rendimiento_neto*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_recurs_adm_bies_rendi_real_2022 <- format( 
  filter( recurs_adm_biess, ano == 2022 )$rendimiento_neto_real*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_recurs_adm_bies_plazo_2022 <- format( 
  filter( recurs_adm_biess, ano == 2022 )$plazo_promedio,
  digits = 3 )

REP$inver_rend_historico_min_rendi_neto <- format( 
  minimo_ren <- rendimiento_neto_hist %>% 
    dplyr::select( rendimiento_neto ) %>% unlist() %>% min( na.rm = TRUE)*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_rendi_historico_bies_max_rendi_neto <- format( 
  maximo_ren <- rendimiento_neto_hist %>% 
    dplyr::select( rendimiento_neto ) %>% unlist() %>% max( na.rm = TRUE)*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_recurs_adm_bies_rendi_neto_max <- format( 
  max_ren <- recurs_adm_biess %>% dplyr::select( rendimiento_neto ) %>% 
    unlist() %>% max( na.rm = TRUE )*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_recurs_adm_bies_rendi_neto_min <- format( 
  minimo_ren <- recurs_adm_biess %>% dplyr::select( rendimiento_neto ) %>% 
    unlist() %>% min( na.rm = TRUE )*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_recurs_adm_bies_rendi_pond_max <- format( 
  max_ren <- recurs_adm_biess %>% dplyr::select( rendimiento_ponderado ) %>% 
    unlist() %>% max( na.rm = TRUE )*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_recurs_adm_bies_rendi_pond_min <- format( 
  minimo_ren <- recurs_adm_biess %>% dplyr::select( rendimiento_ponderado ) %>% 
    unlist() %>% min( na.rm = TRUE )*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_participacion_cred_quiro <- format(
  filter( inver_corte, instrumento == "Créditos Quirografarios" )$participacion*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_valor_nominal_cred_quiro <- format(
  filter( inver_corte, instrumento == "Créditos Quirografarios" )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_rend_prom_cred_quiro <- format(
  filter( inver_corte, instrumento == "Créditos Quirografarios" )$rendimiento_promedio*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_plazo_prom_cred_quiro <- format(
  filter( inver_corte, instrumento == "Créditos Quirografarios" )$plazo_promedio_dias,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_participacion_bonos_esta <- format(
  filter( inver_corte, instrumento == "Bonos del Estado" )$participacion*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_valor_nominal_bonos_esta <- format(
  filter( inver_corte, instrumento == "Bonos del Estado" )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_rend_prom_bonos_esta <- format(
  filter( inver_corte, instrumento == "Bonos del Estado" )$rendimiento_promedio*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_plazo_prom_bonos_esta <- format(
  filter( inver_corte, instrumento == "Bonos del Estado" )$plazo_promedio_dias,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_participacion_cetes <- format(
  filter( inver_corte, instrumento == "CETES" )$participacion*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_valor_nominal_cetes <- format(
  filter( inver_corte, instrumento == "CETES" )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_rend_prom_cetes <- format(
  filter( inver_corte, instrumento == "CETES" )$rendimiento_promedio*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_plazo_prom_cetes <- format(
  filter( inver_corte, instrumento == "CETES" )$plazo_promedio_dias,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_participacion_cert_depo <- format(
  filter( inver_corte, instrumento == "Certificados Depósito" )$participacion*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_valor_nominal_cert_depo <- format(
  filter( inver_corte, instrumento == "Certificados Depósito" )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_rend_prom_cert_depo <- format(
  filter( inver_corte, instrumento == "Certificados Depósito" )$rendimiento_promedio*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_plazo_prom_cert_depo <- format(
  filter( inver_corte, instrumento == "Certificados Depósito" )$plazo_promedio_dias,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_participacion_papel_comer <- format(
  filter( inver_corte, instrumento == "Papel Comercial" )$participacion*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_valor_nominal_papel_comer <- format(
  filter( inver_corte, instrumento == "Papel Comercial" )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_rend_prom_papel_comer <- format(
  filter( inver_corte, instrumento == "Papel Comercial" )$rendimiento_promedio*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_plazo_prom_papel_comer <- format(
  filter( inver_corte, instrumento == "Papel Comercial" )$plazo_promedio_dias,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_participacion_cert_inver <- format(
  filter( inver_corte, instrumento == "Certificados de Inversión" )$participacion*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_valor_nominal_cert_inver <- format(
  filter( inver_corte, instrumento == "Certificados de Inversión" )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_rend_prom_cert_inver <- format(
  filter( inver_corte, instrumento == "Certificados de Inversión" )$rendimiento_promedio*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_plazo_prom_cert_inver <- format(
  filter( inver_corte, instrumento == "Certificados de Inversión" )$plazo_promedio_dias,
  digits = 3 )

REP$inver_inver_corte_participacion_obligaciones <- format(
  filter( inver_corte, instrumento == "Obligaciones" )$participacion*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_valor_nominal_obligaciones <- format(
  filter( inver_corte, instrumento == "Obligaciones" )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_rend_prom_obligaciones <- format(
  filter( inver_corte, instrumento == "Obligaciones" )$rendimiento_promedio*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_plazo_prom_obligaciones <- format(
  filter( inver_corte, instrumento == "Obligaciones" )$plazo_promedio_dias,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_participacion_titula <- format(
  filter( inver_corte, instrumento == "Titularizaciones" )$participacion*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_valor_nominal_titula <- format(
  filter( inver_corte, instrumento == "Titularizaciones" )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_rend_prom_titula <- format(
  filter( inver_corte, instrumento == "Titularizaciones" )$rendimiento_promedio*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_plazo_prom_titula <- format(
  filter( inver_corte, instrumento == "Titularizaciones" )$plazo_promedio_dias,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_valor_nominal_caja <- format(
  filter( inver_corte, instrumento == "Caja" )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inver_corte_participacion_caja <- format(
  filter( inver_corte, instrumento == "Caja" )$participacion*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_rendi_netos_prom_rend_neto <- format(
  prom <- rendimientos_netos %>% filter( corte_a >= as.Date( "2014-12-01" )) %>% 
    dplyr::select( rendimiento_neto ) %>% unlist() %>% mean()*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_rendi_netos_prom_rend_bruto <- format(
  prom <- rendimientos_netos %>% filter( corte_a >= as.Date( "2014-12-01" )) %>% 
    dplyr::select( rendimiento_bruto ) %>% unlist() %>% mean()*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

rendimientos_netos <- rendimientos_netos %>% mutate ( anio = year( corte_a ))

REP$inver_rendi_netos_fondo_adm_prom_2022 <- format(
  filter( rendimientos_netos, anio == 2022)$fondo_administrado_promedio,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_rendi_netos_ingresos_2022 <- format(
  filter( rendimientos_netos, anio == 2022 )$ingresos,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_rendi_netos_egresos_2022 <- format(
  filter( rendimientos_netos, anio == 2022 )$gastos_ope,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_rendi_netos_gastos_adm_2022 <- format(
  filter( rendimientos_netos, anio == 2022 )$gastos_administrativos,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_rendi_netos_rendi_bruto_2022 <- format(
  filter( rendimientos_netos, anio == 2022 )$rendimiento_bruto*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_rendi_netos_rendi_neto_2022 <- format(
  filter( rendimientos_netos, anio == 2022 )$rendimiento_neto*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_ingresos_total_2022 <- format(
  filter( ingresos, ingresos == "Total" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_ingresos_cred_quiro_2022 <- format(
  filter( ingresos, ingresos == "De préstamos quirografarios" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_ingresos_cred_quiro_repre_2022 <- format(
  (filter( ingresos, ingresos == "De préstamos quirografarios" )$x2022 /
     filter( ingresos, ingresos == "Total" )$x2022 )*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_ingresos_renta_publ_2022 <- format(
  filter( ingresos, ingresos == "De deuda renta fija sector público" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_ingresos_renta_publ_repre_2022 <- format(
  (filter( ingresos, ingresos == "De deuda renta fija sector público" )$x2022 /
     filter( ingresos, ingresos == "Total" )$x2022 )*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_ingresos_renta_priv_2022 <- format(
  filter( ingresos, ingresos == "De deuda renta fija sector privado" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_ingresos_renta_priv_repre_2022 <- format(
  (filter( ingresos, ingresos == "De deuda renta fija sector privado" )$x2022 /
     filter( ingresos, ingresos == "Total" )$x2022 )*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_ingresos_otros_2022 <- format(
  filter( ingresos, ingresos == "Otros" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_ingresos_otros_repre_2022 <- format(
  (filter( ingresos, ingresos == "Otros" )$x2022 /
     filter( ingresos, ingresos == "Total" )$x2022 )*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_ingresos_valu_inver_2022 <- format(
  filter( ingresos, ingresos == "En valuación de inversiones" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_ingresos_valu_inver_repre_2022 <- format(
  (filter( ingresos, ingresos == "En valuación de inversiones" )$x2022 /
     filter( ingresos, ingresos == "Total" )$x2022 )*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_gastos_ope_total_general_2022 <- format(
  filter( gastos_opera, gastos == "Total general" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_gastos_ope_inte_acum_2022 <- format(
  filter( gastos_opera, gastos == "Intereses acumulados en ejercicios anteriores" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_gastos_ope_inte_acum_repre_2022 <- format(
  (filter( gastos_opera, gastos == "Intereses acumulados en ejercicios anteriores" )$x2022 /
     filter( gastos_opera, gastos == "Total general" )$x2022 )*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_gastos_ope_cred_inco_2022 <- format(
  filter( gastos_opera, gastos == "Provisión para créditos incobrables" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_gastos_ope_cred_inco_repre_2022 <- format(
  (filter( gastos_opera, gastos == "Provisión para créditos incobrables" )$x2022 /
     filter( gastos_opera, gastos == "Total general" )$x2022 )*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_gastos_ope_mora_patro_2022 <- format(
  filter( gastos_opera, gastos == "Provisión mora patronal préstamos" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_gastos_ope_mora_patro_repre_2022 <- format(
  (filter( gastos_opera, gastos == "Provisión mora patronal préstamos" )$x2022 /
     filter( gastos_opera, gastos == "Total general" )$x2022 )*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_gastos_ope_valu_inver_2022 <- format(
  filter( gastos_opera, gastos == "En valuación de inversiones" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_gastos_ope_valu_inver_repre_2022 <- format(
  (filter( gastos_opera, gastos == "En valuación de inversiones" )$x2022 /
     filter( gastos_opera, gastos == "Total general" )$x2022 )*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_gastos_ope_otros_2022 <- format(
  filter( gastos_opera, gastos == "Otros" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_gastos_ope_otros_repre_2022 <- format(
  (filter( gastos_opera, gastos == "Otros" )$x2022 /
     filter( gastos_opera, gastos == "Total general" )$x2022 )*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_gastos_ope_comision_bolsa_val_2022 <- format(
  filter( gastos_opera, gastos == "Comisión bolsa de valores" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_gastos_ope_comision_bolsa_val_repre_2022 <- format(
  (filter( gastos_opera, gastos == "Comisión bolsa de valores" )$x2022 /
     filter( gastos_opera, gastos == "Total general" )$x2022 )*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_gastos_ope_comisiones_2022 <- format(
  filter( gastos_opera, gastos == "Comisiones" )$x2022,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_gastos_ope_comisiones_repre_2022 <- format(
  (filter( gastos_opera, gastos == "Comisiones" )$x2022 /
     filter( gastos_opera, gastos == "Total general" )$x2022 )*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_gastos_ope_rever_inte_2019 <- format(
  filter( gastos_opera, gastos == "Reverso de intereses" )$x2019,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

inv_prest_quirografario <- inv_instrumento %>% 
  filter( instrumento == "Créditos Quirografarios" ) %>% dplyr::select( -instrumento )

REP$inver_inv_pres_quiro_valor_nom_2015 <- format(
  filter( inv_prest_quirografario, ano == 2015 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_pres_quiro_valor_nom_2022 <- format(
  filter( inv_prest_quirografario, ano == 2022 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_pres_quiro_valor_nom_2015_2022 <- format(
  ( filter( inv_prest_quirografario, ano == 2022 )$valor_nominal - 
      filter( inv_prest_quirografario, ano == 2015 )$valor_nominal )*100 / 
    filter( inv_prest_quirografario, ano == 2015 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_pres_quiro_max_rendi_pond <- format(
  max_ren_pon <- inv_prest_quirografario %>% dplyr::select( rdto_prom_pond ) %>% 
    unlist() %>% max()*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_pres_quiro_min_rendi_pond <- format(
  min_ren_pon <- inv_prest_quirografario %>% dplyr::select( rdto_prom_pond ) %>% 
    unlist() %>% min()*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_pres_quiro_prom_rendi_pond <- format(
  pro_ren_pon <- inv_prest_quirografario %>% dplyr::select( rdto_prom_pond ) %>% 
    unlist() %>% mean()*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

inv_bonos_estado <- inv_instrumento %>% 
  filter( instrumento == "Bonos del Estado" ) %>% dplyr::select( -instrumento )

REP$inver_inv_bonos_estado_valor_nominal_2022 <- format(
  filter( inv_bonos_estado, ano == 2022 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_bonos_estado_rend_pond_2022 <- format(
  filter( inv_bonos_estado, ano == 2022 )$rdto_prom_pond*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_bonos_estado_plazo_2022 <- format(
  filter( inv_bonos_estado, ano == 2022 )$plazo_prom_pond,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_bonos_estado_valor_nominal_2014 <- format(
  filter( inv_bonos_estado, ano == 2014 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_bonos_estado_valor_nominal_2015 <- format(
  filter( inv_bonos_estado, ano == 2015 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_bonos_estado_valor_nominal_crec_2014_2015 <- format(
  (filter( inv_bonos_estado, ano == 2015 )$valor_nominal -
     filter( inv_bonos_estado, ano == 2014 )$valor_nominal)*100 / 
    filter( inv_bonos_estado, ano == 2014 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

inv_cetes<- inv_instrumento %>% 
  filter( instrumento == "Certificados de Tesorería - CETES" ) %>% dplyr::select( -instrumento )

REP$inver_inv_cetes_val_nom_2022 <- format(
  filter( inv_cetes, ano == 2022 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_cetes_val_nom_crec_2020_2022 <- format(
  ( filter( inv_cetes, ano == 2022 )$valor_nominal - 
      filter( inv_cetes, ano == 2020 )$valor_nominal )*100 / 
    filter( inv_cetes, ano == 2020 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_cetes_val_nom_2020 <- format(
  filter( inv_cetes, ano == 2020 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_cetes_rend_pond_2022 <- format(
  filter( inv_cetes, ano == 2022 )$rdto_prom_pond*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_cetes_plazo_prom_pond_2022 <- format(
  filter( inv_cetes, ano == 2022 )$plazo_prom_pond,
  digits = 3 )

REP$inver_inv_cetes_rend_pond_2017 <- format(
  filter( inv_cetes, ano == 2017 )$rdto_prom_pond*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_cetes_plazo_prom_pond_2017 <- format(
  filter( inv_cetes, ano == 2017 )$plazo_prom_pond,
  digits = 3 )

REP$inver_inv_cetes_rend_pond_2020 <- format(
  filter( inv_cetes, ano == 2020 )$rdto_prom_pond*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_cetes_plazo_prom_pond_2020 <- format(
  filter( inv_cetes, ano == 2020 )$plazo_prom_pond,
  digits = 3 )

REP$inver_detalle_bonos_cetes_diciem_2022 <- format(
  cetes <- detalle_bonos %>%  filter( tipo_de_papel == "CETES", fecha_de_compra == as.Date("2022-12-21") ) %>% 
    pull( valor_nominal_de_compra ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_detalle_bonos_cetes_nov_2022 <- format(
  cetes <- detalle_bonos %>%  filter( tipo_de_papel == "CETES", fecha_de_compra == as.Date("2022-11-21") ) %>% 
    pull( valor_nominal_de_compra ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_detalle_bonos_cetes_oct_2022 <- format(
  cetes <- detalle_bonos %>%  filter( tipo_de_papel == "CETES", fecha_de_compra == as.Date("2022-10-25") ) %>% 
    pull( valor_nominal_de_compra ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

inv_obli_renta_fija <- inv_instrumento %>% 
  filter( instrumento == "Obligaciones" ) %>% dplyr::select( -instrumento )

REP$inver_inv_obli_crec_val_nom_2021_2022 <- format(
  ( filter( inv_obli_renta_fija, ano == 2022 )$valor_nominal - 
      filter( inv_obli_renta_fija, ano == 2021 )$valor_nominal )*100 /
    filter( inv_obli_renta_fija, ano == 2021 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_obli_max_val_nom <- format(
  max_obli <- inv_obli_renta_fija %>% dplyr::select( valor_nominal) %>% unlist() %>% max(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_obli_min_val_nom <- format(
  min_obli <- inv_obli_renta_fija %>% dplyr::select( valor_nominal) %>% unlist() %>% min(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_obli_rend_pond_2021 <- format(
  filter( inv_obli_renta_fija, ano == 2021 )$rdto_prom_pond*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_obli_rend_pond_2018 <- format(
  filter( inv_obli_renta_fija, ano == 2018 )$rdto_prom_pond*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_obli_val_nomi_2022 <- format(
  filter( inv_obli_renta_fija, ano == 2022 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_obli_rend_pond_2022 <- format(
  filter( inv_obli_renta_fija, ano == 2022 )$rdto_prom_pond*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_obli_plazo_2022 <- format(
  filter( inv_obli_renta_fija, ano == 2022 )$plazo_prom_pond,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_oblig_max_val_nom <- format(
  max_obli <- detalle_obligaciones %>% dplyr::select( saldo_valor_nominal ) %>% unlist() %>% max(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_obli_tasa_audioelec <- format(
  filter( detalle_obligaciones, emisor == "AUDIOELEC S.A." )$tasa_cupon,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_obli_plazo_audioelec <- format(
  filter( detalle_obligaciones, emisor == "AUDIOELEC S.A." )$plazo_x_vencer_dias,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_oblig_max_rend <- format(
  max_obli <- detalle_obligaciones %>% dplyr::select( tasa_cupon ) %>% unlist() %>% max(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_oblig_val_nom_danielsa <- format(
  filter( detalle_obligaciones, emisor == "DANIELCOM S.A." )$saldo_valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_obli_plazo_danielsa <- format(
  filter( detalle_obligaciones, emisor == "DANIELCOM S.A." )$plazo_x_vencer_dias,
  digits = 3 )

inv_titularizaciones <- inv_instrumento %>% 
  filter( instrumento == "Titularizaciones" ) %>% dplyr::select( -instrumento )

REP$inver_titu_valor_nom_max <- format(
  max_titu <- inv_titularizaciones %>% dplyr::select( valor_nominal ) %>%  unlist() %>% max(),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_titu_valor_nom_2019 <- format(
  filter( inv_titularizaciones, ano == 2019 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_titu_valor_nom_2014_2019 <- format(
  ( abs( filter( inv_titularizaciones, ano == 2019 )$valor_nominal - 
           filter( inv_titularizaciones, ano == 2014)$valor_nominal )*100 /
      filter( inv_titularizaciones, ano == 2014)$valor_nominal ),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_titu_valor_nom_2020 <- format(
  filter( inv_titularizaciones, ano == 2020 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_titu_valor_nom_2021 <- format(
  filter( inv_titularizaciones, ano == 2021 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_titu_valor_nom_2022 <- format(
  filter( inv_titularizaciones, ano == 2022 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_titu_valor_nom_2021_2022 <- format(
  ( filter( inv_titularizaciones, ano == 2022 )$valor_nominal - 
      filter( inv_titularizaciones, ano == 2021)$valor_nominal)*100 /
    filter( inv_titularizaciones, ano == 2021)$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_titu_valor_compra_mut_pichi <- format(
  filter( detalle_titularizaciones, emisor == "FIDC CART AUTOMTRZ MUT. PICHINCHA 1" )$valor_nominal_de_compra,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_titu_saldo_nomi_mut_pichi <- format(
  filter( detalle_titularizaciones, emisor == "FIDC CART AUTOMTRZ MUT. PICHINCHA 1" )$saldo_valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_titu_tasa_mut_pichi <- format(
  filter( detalle_titularizaciones, emisor == "FIDC CART AUTOMTRZ MUT. PICHINCHA 1" )$tasa_cupon,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_titu_plazo_mut_pichi <- format(
  filter( detalle_titularizaciones, emisor == "FIDC CART AUTOMTRZ MUT. PICHINCHA 1" )$plazo_x_vencer_dias,
  digits = 3 )

REP$inver_det_titu_valor_compra_ute <- format(
  filter( detalle_titularizaciones, emisor == "UTE" )$valor_nominal_de_compra,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_titu_saldo_nomi_ute <- format(
  filter( detalle_titularizaciones, emisor == "UTE" )$saldo_valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_titu_tasa_ute <- format(
  filter( detalle_titularizaciones, emisor == "UTE" )$tasa_cupon,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_titu_plazo_ute <- format(
  filter( detalle_titularizaciones, emisor == "UTE" )$plazo_x_vencer_dias,
  digits = 3 )

REP$inver_cert_deposi_total_valor_compra <- format(
  total_val_co <- detalle_certificado_deposito %>% dplyr::select( valor_nominal_de_compra ) %>% unlist() %>%  sum() ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_cert_deposi_tasa_min <- format(
  prom_tasa_min <- detalle_certificado_deposito %>% dplyr::select( tasa_cupon ) %>% unlist() %>%  min() ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_cert_deposi_tasa_max <- format(
  prom_tasa_max <- detalle_certificado_deposito %>% dplyr::select( tasa_cupon ) %>% unlist() %>%  max() ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_cert_deposi_plazo_min <- format(
  prom_tasa_min <- detalle_certificado_deposito %>% dplyr::select( plazo_x_vencer_dias ) %>% unlist() %>%  min() ,
  digits = 3 )

REP$inver_cert_deposi_plazo_max <- format(
  prom_tasa_max <- detalle_certificado_deposito %>% dplyr::select( plazo_x_vencer_dias ) %>% unlist() %>%  max() ,
  digits = 3 )

REP$inver_cert_deposi_pacifico_dic_val_nom <- format(
  filter( detalle_certificado_deposito, emisor == "PACIFICO", 
          fecha_de_compra == as.Date("2022-12-01"))$valor_nominal_de_compra,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_cert_deposi_pacifico_dic_tasa <- format(
  filter( detalle_certificado_deposito, emisor == "PACIFICO", 
          fecha_de_compra == as.Date("2022-12-01"))$tasa_cupon,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_cert_deposi_pacifico_dic_plazo <- format(
  filter( detalle_certificado_deposito, emisor == "PACIFICO", 
          fecha_de_compra == as.Date("2022-12-01"))$plazo_x_vencer_dias,
  digits = 3 )

REP$inver_cert_deposi_pacifico_julio_val_nom <- format(
  filter( detalle_certificado_deposito, emisor == "PACIFICO", 
          fecha_de_compra == as.Date("2022-07-19"))$valor_nominal_de_compra,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_cert_deposi_pacifico_julio_tasa <- format(
  filter( detalle_certificado_deposito, emisor == "PACIFICO", 
          fecha_de_compra == as.Date("2022-07-19"))$tasa_cupon,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_cert_deposi_pacifico_julio_plazo <- format(
  filter( detalle_certificado_deposito, emisor == "PACIFICO", 
          fecha_de_compra == as.Date("2022-07-19"))$plazo_x_vencer_dias,
  digits = 3 )

REP$inver_det_cert_inv_total_val_nominal <- format(
  total_val_co <- detalle_certificado_inversiones %>% dplyr::select( valor_nominal_de_compra ) %>% unlist() %>%  sum() ,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_cert_inv_diners_val_nom <- format(
  filter( detalle_certificado_inversiones, emisor == "DINERS CLUB" )$valor_nominal_de_compra,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_cert_inv_diners_tasa <- format(
  filter( detalle_certificado_inversiones, emisor == "DINERS CLUB" )$tasa_cupon,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_cert_inv_diners_plazo <- format(
  filter( detalle_certificado_inversiones, emisor == "DINERS CLUB" )$plazo_x_vencer_dias,
  digits = 3 )

REP$inver_det_cert_inv_bolivariano_val_nom <- format(
  filter( detalle_certificado_inversiones, emisor == "BOLIVARIANO" )$valor_nominal_de_compra,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_cert_inv_bolivariano_tasa <- format(
  filter( detalle_certificado_inversiones, emisor == "BOLIVARIANO" )$tasa_cupon,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_cert_inv_bolivariano_plazo <- format(
  filter( detalle_certificado_inversiones, emisor == "BOLIVARIANO" )$plazo_x_vencer_dias,
  digits = 3 )

inv_papel_comercial <- inv_instrumento %>% 
  filter( instrumento == "Papel Comercial" ) %>% dplyr::select( -instrumento )

REP$inver_inv_papel_comer_val_nom_2022 <- format(
  filter( inv_papel_comercial, ano == 2022 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_papel_comer_val_nom_cre_2020_2022 <- format(
  ( filter( inv_papel_comercial, ano == 2022 )$valor_nominal - 
      filter( inv_papel_comercial, ano == 2020 )$valor_nominal )*100 / 
    filter( inv_papel_comercial, ano == 2020 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_papel_comer_val_nom_2020 <- format(
  filter( inv_papel_comercial, ano == 2020 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_papel_comer_val_nom_2015 <- format(
  filter( inv_papel_comercial, ano == 2015 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_papel_comer_val_nom_2014 <- format(
  filter( inv_papel_comercial, ano == 2014 )$valor_nominal,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_papel_comer_ren_prom_real_2022 <- format(
  filter( inv_papel_comercial, ano == 2022 )$rend_promedio_real*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_papel_comer_ren_prom_real_2020 <- format(
  filter( inv_papel_comercial, ano == 2020 )$rend_promedio_real*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_papel_comer_ren_prom_real_2014 <- format(
  filter( inv_papel_comercial, ano == 2014 )$rend_promedio_real*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_inv_papel_comer_ren_prom_real_2015 <- format(
  filter( inv_papel_comercial, ano == 2015 )$rend_promedio_real*100,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_papel_comer_val_compra_continental <- format(
  filter( detalle_papel_comercial, emisor == "CONTINENTAL TIRE ANDINA S.A." )$valor_nominal_de_compra,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_papel_comer_plazo_continental <- format(
  filter( detalle_papel_comercial, emisor == "CONTINENTAL TIRE ANDINA S.A." )$plazo_x_vencer_dias,
  digits = 3 )

REP$inver_det_papel_comer_val_compra_pronaca_oct <- format(
  filter( detalle_papel_comercial, emisor == "PRONACA" , fecha_de_compra == as.Date( "2022-10-28" ))$valor_nominal_de_compra,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_papel_comer_plazo_pronaca_oct <- format(
  filter( detalle_papel_comercial, emisor == "PRONACA" , fecha_de_compra == as.Date( "2022-10-28" ))$plazo_x_vencer_dias,
  digits = 3 )

REP$inver_det_papel_comer_val_compra_pronaca_may <- format(
  filter( detalle_papel_comercial, emisor == "PRONACA" , fecha_de_compra == as.Date( "2022-05-31" ))$valor_nominal_de_compra,
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inver_det_papel_comer_plazo_pronaca_may <- format(
  filter( detalle_papel_comercial, emisor == "PRONACA" , fecha_de_compra == as.Date( "2022-05-31" ))$plazo_x_vencer_dias,
  digits = 3 )

#Contexto macroeconómico----------------------------------------------------------------------------

REP$macro_inf_acu_2001 <- format(
  filter( inflacion, anio == 2001, mes == 12 )$inflacion_acumulada, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_inf_acu_2020 <- format(
  filter( inflacion, anio == 2020, mes == 12 )$inflacion_acumulada, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_inf_acu_2017 <- format(
  filter( inflacion, anio == 2017, mes == 12 )$inflacion_acumulada, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )


REP$macro_inf_2000 <- format(
  filter( inflacion, anio == 2000, mes == 12 )$inflacion_acumulada, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_inf_acu_2019 <- format(
  filter( inflacion, anio == 2019, mes == 12 )$inflacion_acumulada, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_inf_acu_2022 <- format(
  filter( inflacion, anio == 2022, mes == 12 )$inflacion_acumulada, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_inf_acu_prom_2022 <- format(
  filter( inflacion, anio == 2022, mes == 12 )$inflacion_promedio_acumulada, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

ipa <- inflacion %>% filter(mes ==12, anio > 2012, anio < 2023) %>%
  dplyr::select(inflacion_promedio_acumulada)

REP$macro_inf_acu_prom_13_22 <- format (
  mean(ipa$inflacion_promedio_acumulada),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f')

REP$macro_subempleo_2022 <- format(
  filter( desempleo, anio == 2022, mes == 12 )$subempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_subempleo_m_2022 <- format(
  filter( desempleo, anio == 2022, mes == 12 )$subempleo_m, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_subempleo_h_2022 <- format(
  filter( desempleo, anio == 2022, mes == 12 )$subempleo_h, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_subempleo_2019 <- format(
  filter( desempleo, anio == 2019, mes == 12 )$subempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_subempleo_2020 <- format(
  filter( desempleo, anio == 2020, mes == 12 )$subempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_subempleo_2012 <- format(
  filter( desempleo, anio == 2012, mes == 12 )$subempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_desempleo_2020 <- format(
  filter( desempleo, anio == 2020, mes == 12 )$desempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_desempleo_2021 <- format(
  filter( desempleo, anio == 2021, mes == 12 )$desempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_desempleo_2022 <- format(
  filter( desempleo, anio == 2022, mes == 12 )$desempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_desempleo_2009 <- format(
  filter( desempleo, anio == 2009, mes == 12 )$desempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_desempleo_2019 <- format(
  filter( desempleo, anio == 2019, mes == 12 )$desempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_sbu_2000 <- format(
  filter( sbu, anio == 2000 )$sbu, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_sbu_incremento_2003 <- format(
  filter( sbu, anio == 2003 )$incremento, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_sbu_incremento_2008 <- format(
  filter( sbu, anio == 2008 )$incremento, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_sbu_tasa_cre_2001 <- format(
  filter( sbu, anio == 2001 )$tasa_de_crecimiento, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_sbu_tasa_cre_2021 <- format(
  filter( sbu, anio == 2021 )$tasa_de_crecimiento, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

incremento_sub_7anios <- sbu %>% filter(anio > 2015, anio < 2023) %>%
  dplyr::select(tasa_de_crecimiento)

REP$macro_sub_promedio_7anios <- format (
  mean(incremento_sub_7anios$tasa_de_crecimiento),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f')

REP$macro_sbu_2022 <- format(
  filter( sbu, anio == 2022 )$sbu, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_salarios_prom_2022 <- format(
  filter(salarios, anio == 2022 , mes ==12 )$sal_prom, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

increm <- salarios %>% filter (mes ==12 ) %>% mutate(incremento = sal_prom-lag(sal_prom)) %>% 
  mutate( tasa_crecimiento = (sal_prom- lag(sal_prom))/lag(sal_prom)*100) 

REP$macro_increm_2022 <- format(
  filter(increm, anio == 2022)$incremento, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_increm_tasa_incre_2022 <- format(
  filter(increm, anio == 2022)$tasa_crecimiento, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_increm_tasa_incre_2008 <- format(
  filter(increm, anio == 2008)$tasa_crecimiento, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_increm_tasa_incre_2020 <- format(
  filter(increm, anio == 2020)$tasa_crecimiento, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

incremento_salarios_10_anios <- increm %>% filter(anio > 2012, anio < 2023) %>%
  dplyr::select(tasa_crecimiento)

REP$macro_increm_promedio_10_anios <- format(
  mean(incremento_salarios_10_anios$tasa_crecimiento), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_pib_real_crecimiento_2016 <- format(
  filter(pib_real, anio == 2016)$crecimiento_pib, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

pib_real_7_anios <- pib_real %>% filter(anio > 2015, anio < 2023) %>% 
  dplyr::select(crecimiento_pib)

REP$macro_pib_real_prom_crec_7_anios <- format(
  mean(pib_real_7_anios$crecimiento_pib), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_pib_real_crecimiento_2020 <- format(
  filter(pib_real, anio == 2020)$crecimiento_pib, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_pib_real_crecimiento_2021 <- format(
  filter(pib_real, anio == 2021)$crecimiento_pib, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_pib_real_crecimiento_2022 <- format(
  filter(pib_real, anio == 2022)$crecimiento_pib, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_act_2000_2004 <- format(
  as.numeric(dec_tasa_activa_2000_2004 <- tasas_interes %>% filter(anio == 2000, mes == 12) %>% 
               dplyr::select(tasa_activa) - tasas_interes %>% filter(anio == 2004, mes == 12) %>% 
               dplyr::select(tasa_activa)), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_act_2005_2016 <- format(
  as.numeric(dec_tasa_activa_2005_2016 <- tasas_interes %>% filter(anio == 2005, mes == 12) %>% 
               dplyr::select(tasa_activa) - tasas_interes %>% filter(anio == 2016, mes == 12) %>% 
               dplyr::select(tasa_activa)), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_act_2005 <- format(
  filter(tasas_interes, anio == 2005, mes == 12)$tasa_activa, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_act_2016 <- format(
  filter(tasas_interes, anio == 2016, mes == 12)$tasa_activa, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_act_2020_7 <- format(
  filter(tasas_interes, anio == 2020, mes == 7)$tasa_activa, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_act_2018_5 <- format(
  filter(tasas_interes, anio == 2018, mes == 5)$tasa_activa, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_act_2000_8 <- format(
  filter(tasas_interes, anio == 2000, mes == 8)$tasa_activa, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_pas_2000 <- format(
  filter(tasas_interes, anio == 2000, mes == 12)$tasa_pasiva, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_pas_2004 <- format(
  filter(tasas_interes, anio == 2004, mes == 12)$tasa_pasiva, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_pas_2004_2016 <- format(
  as.numeric(dec_tasa_pas_2000_2004 <- tasas_interes %>% filter(anio == 2016, mes == 12) %>% 
               dplyr::select(tasa_pasiva) - tasas_interes %>% filter(anio == 2004, mes == 12) %>% 
               dplyr::select(tasa_pasiva)), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_pas_min <- format(
  filter(tasas_interes, anio == 2005, mes == 1)$tasa_pasiva, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_pas_max <- format(
  filter(tasas_interes, anio == 2000, mes == 4)$tasa_pasiva, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

spr_prom <- tasas_interes %>% filter(mes == 12, anio > 2015, anio < 2023) %>% 
  dplyr::select(spread)

REP$macro_tasas_interes_spread <- format(
  mean(spr_prom$spread), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_roa_ban_est_2020 <- format(
  as.numeric(a <- (filter(roa, anio == 2020)$banco_estado)*100), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_roa_ban_priv_2020 <- format(
  as.numeric(a <- (filter(roa, anio == 2020)$b_privados)*100), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_roa_cfn_2020 <- format(
  as.numeric(a <- (filter(roa, anio == 2020)$cfn)*100), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_roe_ban_est_2020 <- format(
  as.numeric(a <- (filter(roe, anio == 2020)$banco_estado)*100), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_roe_ban_priv_2020 <- format(
  as.numeric(a <- (filter(roe, anio == 2020)$b_privados)*100), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_roe_banecu_2020 <- format(
  as.numeric(a <- (filter(roe, anio == 2020)$ban_ecuador)*100), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_roe_int_pub_2020 <- format(
  as.numeric(a <- (filter(roe, anio == 2020)$int_publicas)*100), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

# Información por escenarios -----------------------------------------------------------------------
escenarios <- paste0( 'escenario_', 1:3 )
esc_nom <- paste0( 'esc_', 1:3 )

for ( i in 1:length( escenarios ) ) {
  
  escenario <- escenarios[ i ]
  
  message( '\tGenerando auto información para el ', escenario )
  
  load( file = paste0( parametros$rtr_rdata_icomp_balance, escenario, '.RData' ) )
  load( file = paste0( parametros$rtr_rdata_icomp_conf_esc, escenario, '.RData' ) )
  load( file = paste0( parametros$rtr_rdata_icomp_prima, escenario, '.RData' ) )
  
  expr <- expression({
    
    REP$bal_act_ESC <- format( 
      balance_anual[ t == parametros$rtr_horizonte ]$V, 
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$bal_cap_ESC <- format( 
      balance_anual[ t == parametros$rtr_horizonte ]$V_cap, 
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$duracion_ESC <- max( which( balance_anual$V_cap > 0 ) ) + parametros$anio_ini - 1
    
    REP$cap_ini_ESC <- format( 
      esc$V0, 
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$pri_med_niv_ESC <- format( 
      100 * prima[ t == parametros$rtr_horizonte ]$pri_med_niv_apo_est_pen,
      digits = 4, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$apo_est_ESC <- format( 
      100 * mean( esc$apo_act$por_apo_est[-1] ),
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$tasa_act_ESC <- format( 
      100 * mean( esc$apo_act$i_a[-1] ),
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$tasa_sal_ESC <- format( 
      100 * mean( esc$apo_act$i_r[-1] ),
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$tasa_sbu_ESC <- format( 
      100 * mean( esc$apo_act$i_sbu[-1] ),
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$tasa_pen_ESC <- format( 
      100 * mean( esc$apo_act$i_p[-1] ),
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$tasa_apo_jub_ESC <- format( 
      100 * esc$apo_act$por_apo_pen_incap[2],
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$tasa_por_gas_ESC <- format( 
      100 * esc$apo_act$por_gast[2],
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$tasa_aporte_salud_ESC <- format( 
      100 * esc$apo_act$apo_sal[2],
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$tasa_aporte_ESC_2020 <- format( 
      100 * ( esc$apo_act$por_apo[1] + esc$apo_act$apo_sal[2] ),
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$tasa_aporte_ESC_2021 <- format( 
      100 * ( esc$apo_act$por_apo[2] + esc$apo_act$apo_sal[2] ),
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$bal_sum_act_ESC <- format( 
      balance_anual[ t == parametros$rtr_horizonte ]$Act_vap, 
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$bal_sum_pas_ESC <- format( 
      balance_anual[ t == parametros$rtr_horizonte ]$Pas_vap, 
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$ing_jubilados_ESC <- format( 
      balance_anual[ t == parametros$rtr_horizonte ]$A12_vap +
        balance_anual[ t == parametros$rtr_horizonte ]$A15_vap +
        balance_anual[ t == parametros$rtr_horizonte ]$A16_vap,
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
    REP$ing_apo_est_ESC <- format( 
      balance_anual[ t == parametros$rtr_horizonte ]$A_est_vap,
      digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
    
  })
  expr <- gsub( '(ESC)', esc_nom[ i ], deparse( expr ) )
  eval( eval( parse( text = expr ) ), envir = .GlobalEnv )
  
}
