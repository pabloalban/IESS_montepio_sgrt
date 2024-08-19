message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tEstableciendo información para la configuración del reporte' )

#REP <- new.env( )

REP$corte <- parametros$anio_ini

REP$cap_ini <- format( 
  parametros$rtr_reserva_ini, 
  digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

# Carga información --------------------------------------------------------------------------------
load( file = parametros$demo_rdata_sgo_est_dem )
load( file = parametros$macro_rdata_info )
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

#Inversiones----------------------------------------------------------------------------------------

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
