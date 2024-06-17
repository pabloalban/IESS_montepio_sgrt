# Cálculo balances RTR -----------------------------------------------------------------------------
message( '\tCalculando escenarios del balance para SGRT' )

# Carga información --------------------------------------------------------------------------------
load( file = parametros$macro_rdata_macro_est )

# Preparación de información macroeconómica --------------------------------------------------------
predicciones_anuales <- as.data.table( predicciones_anuales )
tasas_macro_anuales <- as.data.table( tasas_macro_anuales )
tasas_macro_anuales <- rbind(
  data.table( anio = 2020, t_pib = 0, t_sal = 0, t_sbu = 0, tp_anual = 0, inf_anual = 0 ),
  tasas_macro_anuales[ , list( 
    anio, t_pib = t_pib / 100, t_sal = t_sal / 100, t_sbu = t_sbu / 100, tp_anual = tp_anual / 100, 
    inf_anual = inf_anual / 100 ) ]
)

# Objetos comunes a todos los cálculos
parametros_lista <- c( 'parametros', 'parametros_lista', 'predicciones_anuales',
                       'sgo_act_tran_anio', 'sgo_pen_tran_anio',
                       'tasas_macro_anuales', 'pob_proy', 'ben_proy', 'sal_proy', 'pen_proy', 
                       'gast_proy', 'aux_fun_proy', 'esc', 'balance', 'balance_anual', 'pob_proy_rtr',
                       'pob_proy_ts', 'coef_decima_tercera_pen_12', 'coef_decima_cuarta_pen_12',
                       'coef_decima_tercera_pen_15', 'coef_decima_cuarta_pen_15',
                       'coef_decima_tercera_pen_16', 'coef_decima_cuarta_pen_16', 'apo_sal' )

# Descripción de factores de calibración------------------------------------------------------------
# Incluimos los factores de calibración siguientes en cada escenario:
# cal_mas = calibra masa salarial
# cal_pen_incap = calibra pensiones de incapacidad permanente absoluta, total y parcial  
# cal_pen_indem = calibra indemnizaciones
# cal_pen_sub = calibra subsidios
# cal_pen_orf = calibra pensiones de orfandad del SGRT
# cal_pen_viu = calibra pensiones de viudedad del SGRT
# cal_gast = calibra gasto administrativo

# Escenario 1 --------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_1'
esc$descripcion <- 'Escenario base, donde se espera se cumplan todas las hipótesis y Ley'
message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$rtr_reserva_ini
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- TRUE

esc$apo_act <- tasas_macro_anuales[ 
  anio >= parametros$anio_ini, 
  list( t = 0:parametros$rtr_horizonte ,
        anio,
        i_a = 0.0625,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        sbu = predicciones_anuales[ anio >= parametros$anio_ini ]$sbu_anual, 
        por_apo = c( 0.002, rep( 0.0038, parametros$rtr_horizonte ) ),
        por_apo_pen_incap = 0,
        por_apo_est = 0.4,
        por_gast = 0.03 / 100,
        por_gast_per = 4125019.20 / 25829157448,
        apo_sal = 0,
        cal_mas = c( 0.8287, seq( 0.83, 0.98, length.out = 13 ), rep( 1, 27 ) ),
        cal_pen_incap = 1.0430098,
        cal_pen_indem = 1.001987,
        cal_pen_sub = 1.001628,
        cal_pen_orf = 1.07853344,
        cal_pen_viu = 1.125673385,
        cal_gast = 1.0 ) ]

esc$rtr_rdata_icomp_proy_benef <- paste0( parametros$rtr_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rtr_rdata_icomp_balance <- paste0( parametros$rtr_rdata_icomp_balance, esc$nombre, '.RData' )
esc$rdata_sal_proy <- paste0( parametros$rtr_rdata_icomp_sal_proy, esc$nombre, '.RData' )
source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/300_proyeccion_pensiones_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/301_calculo_balance_rtr.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$rtr_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

# Escenario 2 --------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_2'
esc$descripcion <- 'Escenario intermedio, donde se espera el Estado cancele solo el 20,93%'
message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$rtr_reserva_ini
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- TRUE

esc$apo_act <- tasas_macro_anuales[ 
  anio >= parametros$anio_ini, 
  list( t = 0:parametros$rtr_horizonte ,
        anio,
        i_a = 0.0625,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        sbu = predicciones_anuales[ anio >= parametros$anio_ini ]$sbu_anual, 
        por_apo = c( 0.002, rep( 0.0038, parametros$rtr_horizonte  ) ),
        por_apo_pen_incap = 0,
        por_apo_est = 0.8284 * 0.4,
        por_gast = 0.03 / 100,
        por_gast_per = 4125019.20 / 25829157448,
        apo_sal = 0,
        cal_mas = c( 0.8287, seq( 0.83, 0.98, length.out = 13 ), rep( 1, 27 ) ),
        cal_pen_incap = 1.0430098,
        cal_pen_indem = 1.001987,
        cal_pen_sub = 1.001628,
        cal_pen_orf = 1.07853344,
        cal_pen_viu = 1.125673385,
        cal_gast = 1.0 ) ]

esc$rtr_rdata_icomp_proy_benef <- paste0( parametros$rtr_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rtr_rdata_icomp_balance <- paste0( parametros$rtr_rdata_icomp_balance, esc$nombre, '.RData' )
esc$rdata_sal_proy <- paste0( parametros$rtr_rdata_icomp_sal_proy, esc$nombre, '.RData' )
source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/300_proyeccion_pensiones_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/301_calculo_balance_rtr.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$rtr_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )


# Escenario 3 --------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_3'
esc$descripcion <- 'Escenario pesimista, donde se espera el Estado cancele 0%'
message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$rtr_reserva_ini
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- TRUE

esc$apo_act <- tasas_macro_anuales[ 
  anio >= parametros$anio_ini, 
  list( t = 0:parametros$rtr_horizonte ,
        anio,
        i_a = 0.0625,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        sbu = predicciones_anuales[ anio >= parametros$anio_ini ]$sbu_anual, 
        por_apo = c( 0.002, rep( 0.0038, parametros$rtr_horizonte  ) ),
        por_apo_pen_incap = 0,
        por_apo_est = 0,
        por_gast = 0.03 / 100,
        por_gast_per = 4125019.20 / 25829157448,
        apo_sal = 0,
        cal_mas = c( 0.8287, seq( 0.83, 0.98, length.out = 13 ), rep( 1, 27 ) ),
        cal_pen_incap = 1.0430098,
        cal_pen_indem = 1.001987,
        cal_pen_sub = 1.001628,
        cal_pen_orf = 1.07853344,
        cal_pen_viu = 1.125673385,
        cal_gast = 1.0 ) ]

esc$rtr_rdata_icomp_proy_benef <- paste0( parametros$rtr_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rtr_rdata_icomp_balance <- paste0( parametros$rtr_rdata_icomp_balance, esc$nombre, '.RData' )
esc$rdata_sal_proy <- paste0( parametros$rtr_rdata_icomp_sal_proy, esc$nombre, '.RData' )
source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/300_proyeccion_pensiones_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/301_calculo_balance_rtr.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$rtr_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

#Escenarios para el análisis de Sensibilidad--------------------------------------------------------

# ia + 1 --------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_4'
esc$descripcion <- 'Escenario sensibilidad'
message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$rtr_reserva_ini
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- TRUE

esc$apo_act <- tasas_macro_anuales[ 
  anio >= parametros$anio_ini, 
  list( t = 0:parametros$rtr_horizonte ,
        anio,
        i_a = 0.0625 + 0.01,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        sbu = predicciones_anuales[ anio >= parametros$anio_ini ]$sbu_anual, 
        por_apo = c( 0.002, rep( 0.0038, parametros$rtr_horizonte  ) ),
        por_apo_pen_incap = 0,
        por_apo_est = 0.8284 * 0.4,
        por_gast = 0.03 / 100,
        por_gast_per = 4125019.20 / 25829157448,
        apo_sal = 0,
        cal_mas = c( 0.8287, seq( 0.83, 0.98, length.out = 13 ), rep( 1, 27 ) ),
        cal_pen_incap = 1.0430098,
        cal_pen_indem = 1.001987,
        cal_pen_sub = 1.001628,
        cal_pen_orf = 1.07853344,
        cal_pen_viu = 1.125673385,
        cal_gast = 1.0 ) ]

esc$rtr_rdata_icomp_proy_benef <- paste0( parametros$rtr_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rtr_rdata_icomp_balance <- paste0( parametros$rtr_rdata_icomp_balance, esc$nombre, '.RData' )
esc$rdata_sal_proy <- paste0( parametros$rtr_rdata_icomp_sal_proy, esc$nombre, '.RData' )
source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/300_proyeccion_pensiones_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/301_calculo_balance_rtr.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$rtr_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )


# ia - 1 -------------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_5'
esc$descripcion <- 'Escenario sensibilidad'
message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$rtr_reserva_ini
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- TRUE

esc$apo_act <- tasas_macro_anuales[ 
  anio >= parametros$anio_ini, 
  list( t = 0:parametros$rtr_horizonte ,
        anio,
        i_a = 0.0625 - 0.01,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        sbu = predicciones_anuales[ anio >= parametros$anio_ini ]$sbu_anual, 
        por_apo = c( 0.002, rep( 0.0038, parametros$rtr_horizonte  ) ),
        por_apo_pen_incap = 0,
        por_apo_est = 0.8284 * 0.4,
        por_gast = 0.03 / 100,
        por_gast_per = 4125019.20 / 25829157448,
        apo_sal = 0,
        cal_mas = c( 0.8287, seq( 0.83, 0.98, length.out = 13 ), rep( 1, 27 ) ),
        cal_pen_incap = 1.0430098,
        cal_pen_indem = 1.001987,
        cal_pen_sub = 1.001628,
        cal_pen_orf = 1.07853344,
        cal_pen_viu = 1.125673385,
        cal_gast = 1.0 ) ]

esc$rtr_rdata_icomp_proy_benef <- paste0( parametros$rtr_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rtr_rdata_icomp_balance <- paste0( parametros$rtr_rdata_icomp_balance, esc$nombre, '.RData' )
esc$rdata_sal_proy <- paste0( parametros$rtr_rdata_icomp_sal_proy, esc$nombre, '.RData' )
source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/300_proyeccion_pensiones_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/301_calculo_balance_rtr.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$rtr_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )


# ir + 1 -------------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_6'
esc$descripcion <- 'Escenario sensibilidad'
message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$rtr_reserva_ini
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- TRUE

esc$apo_act <- tasas_macro_anuales[ 
  anio >= parametros$anio_ini, 
  list( t = 0:parametros$rtr_horizonte ,
        anio,
        i_a = 0.0625,
        i_r = t_sal + 0.01,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        sbu = predicciones_anuales[ anio >= parametros$anio_ini ]$sbu_anual, 
        por_apo = c( 0.002, rep( 0.0038, parametros$rtr_horizonte  ) ),
        por_apo_pen_incap = 0,
        por_apo_est = 0.8284 * 0.4,
        por_gast = 0.03 / 100,
        por_gast_per = 4125019.20 / 25829157448,
        apo_sal = 0,
        cal_mas = c( 0.8287, seq( 0.83, 0.98, length.out = 13 ), rep( 1, 27 ) ),
        cal_pen_incap = 1.0430098,
        cal_pen_indem = 1.001987,
        cal_pen_sub = 1.001628,
        cal_pen_orf = 1.07853344,
        cal_pen_viu = 1.125673385,
        cal_gast = 1.0 ) ]

esc$rtr_rdata_icomp_proy_benef <- paste0( parametros$rtr_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rtr_rdata_icomp_balance <- paste0( parametros$rtr_rdata_icomp_balance, esc$nombre, '.RData' )
esc$rdata_sal_proy <- paste0( parametros$rtr_rdata_icomp_sal_proy, esc$nombre, '.RData' )
source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/300_proyeccion_pensiones_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/301_calculo_balance_rtr.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$rtr_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

# ir - 1 -------------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_7'
esc$descripcion <- 'Escenario sensibilidad'
message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$rtr_reserva_ini
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- TRUE

esc$apo_act <- tasas_macro_anuales[ 
  anio >= parametros$anio_ini, 
  list( t = 0:parametros$rtr_horizonte ,
        anio,
        i_a = 0.0625,
        i_r = t_sal - 0.01,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        sbu = predicciones_anuales[ anio >= parametros$anio_ini ]$sbu_anual, 
        por_apo = c( 0.002, rep( 0.0038, parametros$rtr_horizonte  ) ),
        por_apo_pen_incap = 0,
        por_apo_est = 0.8284 * 0.4,
        por_gast = 0.03 / 100,
        por_gast_per = 4125019.20 / 25829157448,
        apo_sal = 0,
        cal_mas = c( 0.8287, seq( 0.83, 0.98, length.out = 13 ), rep( 1, 27 ) ),
        cal_pen_incap = 1.0430098,
        cal_pen_indem = 1.001987,
        cal_pen_sub = 1.001628,
        cal_pen_orf = 1.07853344,
        cal_pen_viu = 1.125673385,
        cal_gast = 1.0 ) ]

esc$rtr_rdata_icomp_proy_benef <- paste0( parametros$rtr_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rtr_rdata_icomp_balance <- paste0( parametros$rtr_rdata_icomp_balance, esc$nombre, '.RData' )
esc$rdata_sal_proy <- paste0( parametros$rtr_rdata_icomp_sal_proy, esc$nombre, '.RData' )
source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/300_proyeccion_pensiones_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/301_calculo_balance_rtr.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$rtr_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

# ip + 1 -------------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_8'
esc$descripcion <- 'Escenario sensibilidad'
message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$rtr_reserva_ini
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- TRUE

esc$apo_act <- tasas_macro_anuales[ 
  anio >= parametros$anio_ini, 
  list( t = 0:parametros$rtr_horizonte ,
        anio,
        i_a = 0.0625,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual + 0.01, 
        sbu = predicciones_anuales[ anio >= parametros$anio_ini ]$sbu_anual, 
        por_apo = c( 0.002, rep( 0.0038, parametros$rtr_horizonte  ) ),
        por_apo_pen_incap = 0,
        por_apo_est = 0.8284 * 0.4,
        por_gast = 0.03 / 100,
        por_gast_per = 4125019.20 / 25829157448,
        apo_sal = 0,
        cal_mas = c( 0.8287, seq( 0.83, 0.98, length.out = 13 ), rep( 1, 27 ) ),
        cal_pen_incap = 1.0430098,
        cal_pen_indem = 1.001987,
        cal_pen_sub = 1.001628,
        cal_pen_orf = 1.07853344,
        cal_pen_viu = 1.125673385,
        cal_gast = 1.0 ) ]

esc$rtr_rdata_icomp_proy_benef <- paste0( parametros$rtr_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rtr_rdata_icomp_balance <- paste0( parametros$rtr_rdata_icomp_balance, esc$nombre, '.RData' )
esc$rdata_sal_proy <- paste0( parametros$rtr_rdata_icomp_sal_proy, esc$nombre, '.RData' )
source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/300_proyeccion_pensiones_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/301_calculo_balance_rtr.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$rtr_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

# ip - 1 -------------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_9'
esc$descripcion <- 'Escenario sensibilidad'
message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$rtr_reserva_ini
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- TRUE

esc$apo_act <- tasas_macro_anuales[ 
  anio >= parametros$anio_ini, 
  list( t = 0:parametros$rtr_horizonte ,
        anio,
        i_a = 0.0625,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual - 0.01, 
        sbu = predicciones_anuales[ anio >= parametros$anio_ini ]$sbu_anual, 
        por_apo = c( 0.002, rep( 0.0038, parametros$rtr_horizonte  ) ),
        por_apo_pen_incap = 0,
        por_apo_est = 0.8284 * 0.4,
        por_gast = 0.03 / 100,
        por_gast_per = 4125019.20 / 25829157448,
        apo_sal = 0,
        cal_mas = c( 0.8287, seq( 0.83, 0.98, length.out = 13 ), rep( 1, 27 ) ),
        cal_pen_incap = 1.0430098,
        cal_pen_indem = 1.001987,
        cal_pen_sub = 1.001628,
        cal_pen_orf = 1.07853344,
        cal_pen_viu = 1.125673385,
        cal_gast = 1.0 ) ]

esc$rtr_rdata_icomp_proy_benef <- paste0( parametros$rtr_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rtr_rdata_icomp_balance <- paste0( parametros$rtr_rdata_icomp_balance, esc$nombre, '.RData' )
esc$rdata_sal_proy <- paste0( parametros$rtr_rdata_icomp_sal_proy, esc$nombre, '.RData' )
source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/300_proyeccion_pensiones_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/301_calculo_balance_rtr.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$rtr_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

#Limpiar Ram----------------------------------------------------------------------------------------
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

