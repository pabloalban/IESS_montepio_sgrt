# Cálculo balances IVM -----------------------------------------------------------------------------
message( '\tCalculando escenarios del balance para IVM' )

# Carga información --------------------------------------------------------------------------------
load( parametros$macro_rdata_macro_est )

# Preparación de información macroeconómica --------------------------------------------------------
predicciones_anuales <- as.data.table( predicciones_anuales )
val_macro <- as.data.table( tasas_macro_anuales )
val_macro <- rbind(
  data.table( anio = 2020, t_pib = 0, t_sal = 0, t_sbu = 0, tp_anual = 0, inf_anual = 0 ),
  val_macro[ , list( 
    anio, t_pib = t_pib / 100, t_sal = t_sal / 100, t_sbu = t_sbu / 100, tp_anual = tp_anual / 100, 
    inf_anual = inf_anual / 100 ) ]
)
val_macro <- merge.data.table(
  val_macro,
  predicciones_anuales[ anio >= parametros$anio_ini, list( anio, sbu = sbu_anual ) ],
  by = 'anio',
  all.x = TRUE
)

# Objetos comunes a todos los cálculos
parametros_lista <- c( 'parametros', 'parametros_lista', 'val_macro',
                       'sgo_act_tran_anio', 'sgo_pen_tran_anio', 'sgo_comp_tran', 'coef_pen',
                       'pob_proy_ts', 'ben_proy', 'sal_proy', 'pen_proy', 
                       'gast_proy', 'esc', 'balance', 'balance_anual' )

# Factores de calibración --------------------------------------------------------------------------
# Incluimos los factores de calibración siguientes en cada escenario:
# cal_mas = calibra masa salarial
# cal_pen_vej = calibra pensiones de vejez  
# cal_pen_inv = calibra pensiones de invalidez
# cal_aux_fun = calibra auxilio de funerales
# cal_gast = calibra gasto administrativo
# 

# Escenario 1 --------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_1'
esc$descripcion <- 'Escenario *legal*, donde se espera se cumplan todas las hipótesis y reglas aplicables
según la ley vigente a la fecha de corte.'
message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$ivm_reserva_ini
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- TRUE
esc$mas_sal_con_dec <- FALSE

esc$apo_act <- val_macro[ 
  anio >= parametros$anio_ini, 
  list( t = 0:parametros$ivm_horizonte,
        anio,
        i_a = 0.0625,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        sbu = sbu, 
        ben_aux_fun = 1406.67,
        por_apo = rep( 0.1106, parametros$ivm_horizonte + 1 ), # 11.06 = 10.46 + 0.5 + 0.1
        por_apo_pen_vej = 0,
        por_apo_pen_inv = 0,
        por_apo_pen_mon = 0,
        por_apo_est = 0.4,
        por_gast = 0.04,
        cal_mas = c( 0.8287, seq( 0.83, 0.98, length.out = 13 ), rep( 1, 27 ) ),
        cal_pen_vej = c( 1.0, 0.93, 0.94, seq( 0.9425, 1.0, length.out = 38 ) ), # para calzar los flujos de pensiones observados
        cal_pen_inv = 1.00, # para calzar los flujos de pensiones observados
        cal_pen_viu = 1.00,
        cal_pen_orf = 1.056,
        cal_aux_fun = 1.0,
        cal_gast = 1.0 ) ]

tas_list <- list( 'a', 'r', 'sbu', 'f', 'p' )
esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )

esc$rdata_sal_proy <- paste0( parametros$ivm_rdata_icomp_sal_proy, esc$nombre, '.RData' )
esc$rdata_ben_proy <- paste0( parametros$ivm_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rdata_balance <- paste0( parametros$ivm_rdata_icomp_balance, esc$nombre, '.RData' )

source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/302_calculo_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$ivm_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

# Escenario 2 --------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_2'
esc$descripcion <- 'Escenario *base*, donde se espera se cumplan todas las hipótesis y reglas 
aplicables según la ley vigente a la fecha de corte, pero con aporte del estado de 31.33% conforme al 
promedio histórico.'

message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$ivm_reserva_ini
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- TRUE
esc$mas_sal_con_dec <- FALSE

esc$apo_act <- val_macro[ 
  anio >= parametros$anio_ini, 
  list( t = 0:parametros$ivm_horizonte,
        anio,
        i_a = 0.0625,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        sbu = sbu, 
        ben_aux_fun = 1406.67,
        por_apo = rep( 0.1106, parametros$ivm_horizonte + 1 ), # 11.06 = 10.46 + 0.5 + 0.1
        por_apo_pen_vej = 0,
        por_apo_pen_inv = 0,
        por_apo_pen_mon = 0,
        por_apo_est = 0.3133,
        por_gast = 0.04,
        cal_mas = c( 0.8287, seq( 0.83, 0.98, length.out = 13 ), rep( 1, 27 ) ),
        cal_pen_vej = c( 1.0, 0.93, 0.94, seq( 0.9425, 1.0, length.out = 38 ) ), # para calzar los flujos de pensiones observados
        cal_pen_inv = 1.00, # para calzar los flujos de pensiones observados
        cal_pen_viu = 1.00,
        cal_pen_orf = 1.056,
        cal_aux_fun = 1.0,
        cal_gast = 1.0 ) ]

tas_list <- list( 'a', 'r', 'sbu', 'f', 'p' )
esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )

esc$rdata_sal_proy <- paste0( parametros$ivm_rdata_icomp_sal_proy, esc$nombre, '.RData' )
esc$rdata_ben_proy <- paste0( parametros$ivm_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rdata_balance <- paste0( parametros$ivm_rdata_icomp_balance, esc$nombre, '.RData' )

source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/302_calculo_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$ivm_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

# Escenario 3 --------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_3'
esc$descripcion <- 'Escenario *pesimista*, donde se espera se cumplan todas las hipótesis y reglas aplicables
según la ley vigente a la fecha de corte, pero con aporte estatal nulo.'

message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$ivm_reserva_ini
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- TRUE
esc$mas_sal_con_dec <- FALSE

esc$apo_act <- val_macro[ 
  anio >= parametros$anio_ini, 
  list( t = 0:parametros$ivm_horizonte,
        anio,
        i_a = 0.0625,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        sbu = sbu, 
        ben_aux_fun = 1406.67,
        por_apo = rep( 0.1106, parametros$ivm_horizonte + 1 ), # 11.06 = 10.46 + 0.5 + 0.1
        por_apo_pen_vej = 0,
        por_apo_pen_inv = 0,
        por_apo_pen_mon = 0,
        por_apo_est = 0.0,
        por_gast = 0.04,
        cal_mas = c( 0.8287, seq( 0.83, 0.98, length.out = 13 ), rep( 1, 27 ) ),
        cal_pen_vej = c( 1.0, 0.93, 0.94, seq( 0.9425, 1.0, length.out = 38 ) ), # para calzar los flujos de pensiones observados
        cal_pen_inv = 1.00, # para calzar los flujos de pensiones observados
        cal_pen_viu = 1.00,
        cal_pen_orf = 1.056,
        cal_aux_fun = 1.0,
        cal_gast = 1.0 ) ]

tas_list <- list( 'a', 'r', 'sbu', 'f', 'p' )
esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )

esc$rdata_sal_proy <- paste0( parametros$ivm_rdata_icomp_sal_proy, esc$nombre, '.RData' )
esc$rdata_ben_proy <- paste0( parametros$ivm_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rdata_balance <- paste0( parametros$ivm_rdata_icomp_balance, esc$nombre, '.RData' )

source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/302_calculo_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$ivm_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

# Escenario 4 --------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_4'
esc$descripcion <- 'Escenario con reducción del aporte del Estado, incremento de aportes'
message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$ivm_reserva_ini
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- TRUE
esc$mas_sal_con_dec <- FALSE

esc$apo_act <- val_macro[ 
  anio >= parametros$anio_ini, 
  list( t = 0:parametros$ivm_horizonte,
        anio,
        i_a = 0.0625,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        sbu = sbu, 
        ben_aux_fun = 1406.67,
        por_apo = rep( 0.1106, parametros$ivm_horizonte + 1 ), # 11.06 = 10.46 + 0.5 + 0.1
        por_apo_pen_vej = 0,
        por_apo_pen_inv = 0,
        por_apo_pen_mon = 0,
        por_apo_est = 0,
        por_gast = 0.04,
        cal_mas = c( 0.8287, seq( 0.83, 0.98, length.out = 13 ), rep( 1, 27 ) ),
        cal_pen_vej = c( 1.0, 0.93, 0.94, seq( 0.9425, 1.0, length.out = 38 ) ), # para calzar los flujos de pensiones observados
        cal_pen_inv = 1.00, # para calzar los flujos de pensiones observados
        cal_pen_viu = 1.00,
        cal_pen_orf = 1.056,
        cal_aux_fun = 1.0,
        cal_gast = 1.0 ) ]

tas_list <- list( 'a', 'r', 'sbu', 'f', 'p' )
esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )

esc$rdata_sal_proy <- paste0( parametros$ivm_rdata_icomp_sal_proy, esc$nombre, '.RData' )
esc$rdata_ben_proy <- paste0( parametros$ivm_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rdata_balance <- paste0( parametros$ivm_rdata_icomp_balance, esc$nombre, '.RData' )

source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/302_calculo_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$ivm_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

# Escenario 5 --------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_5'
esc$descripcion <- 'Escenario *propuesto*, con la propuesta de sostenibilidad de risko.'
# prima escalonada
# aporte sobre décimos

message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$ivm_reserva_ini
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- TRUE
esc$mas_sal_con_dec <- TRUE

esc$apo_act <- val_macro[ 
  anio >= parametros$anio_ini, 
  list( t = 0:parametros$ivm_horizonte,
        anio,
        i_a = 0.0625,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        sbu = sbu, 
        ben_aux_fun = 1406.67,
        # prima escalonada
        por_apo = c( 0.1106,           # aporte del 2020
                     rep( 0.1106, 5 ), 
                     rep( 0.1141, 5 ),
                     rep( 0.1176, 5 ),
                     rep( 0.1211, 5 ),
                     rep( 0.1246, 5 ),
                     rep( 0.1281, 5 ),
                     rep( 0.1316, 5 ), 
                     rep( 0.1351, 5 ) ), 
        por_apo_pen_vej = 0,
        por_apo_pen_inv = 0,
        por_apo_pen_mon = 0,
        por_apo_est = 0.35,
        por_gast = 0.04,
        cal_mas = c( 0.8287, seq( 0.83, 0.98, length.out = 13 ), rep( 1, 27 ) ),
        cal_pen_vej = c( 1.0, 0.93, 0.94, seq( 0.9425, 1.0, length.out = 38 ) ), # para calzar los flujos de pensiones observados
        cal_pen_inv = 1.00, # para calzar los flujos de pensiones observados
        cal_pen_viu = 1.00,
        cal_pen_orf = 1.056,
        cal_aux_fun = 1.0,
        cal_gast = 1.0 ) ]

tas_list <- list( 'a', 'r', 'sbu', 'f', 'p' )
esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )

esc$rdata_sal_proy <- paste0( parametros$ivm_rdata_icomp_sal_proy, esc$nombre, '.RData' )
esc$rdata_ben_proy <- paste0( parametros$ivm_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rdata_balance <- paste0( parametros$ivm_rdata_icomp_balance, esc$nombre, '.RData' )

source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/302_calculo_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$ivm_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

# Escenario 6 --------------------------------------------------------------------------------------
# Resolución 261
esc <- new.env()
esc$nombre <- 'escenario_6'
esc$descripcion <- 'Escenario según resolución 261'
message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$ivm_reserva_ini
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- TRUE
esc$mas_sal_con_dec <- FALSE

esc$apo_act <- val_macro[ 
  anio >= parametros$anio_ini, 
  list( t = 0:parametros$ivm_horizonte,
        anio,
        i_a = 0.04,
        i_r = t_sal,
        i_sbu = t_sbu,
        i_f = inf_anual,
        i_p = inf_anual, 
        sbu = sbu, 
        ben_aux_fun = 1406.67,
        por_apo = 0.0974 + 0.001,
        por_apo_pen_vej = 0,
        por_apo_pen_inv = 0,
        por_apo_pen_mon = 0,
        por_apo_est = 0.4,
        por_gast = 0.04,
        cal_mas = c( 0.8287, seq( 0.83, 0.98, length.out = 13 ), rep( 1, 27 ) ),
        cal_pen_vej = c( 1.0, 0.93, 0.94, seq( 0.9425, 1.0, length.out = 38 ) ), # para calzar los flujos de pensiones observados
        cal_pen_inv = 1.00, # para calzar los flujos de pensiones observados
        cal_pen_viu = 1.00,
        cal_pen_orf = 1.056,
        cal_aux_fun = 1.0,
        cal_gast = 1.0 ) ]

tas_list <- list( 'a', 'r', 'sbu', 'f', 'p' )
esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )

esc$rdata_sal_proy <- paste0( parametros$ivm_rdata_icomp_sal_proy, esc$nombre, '.RData' )
esc$rdata_ben_proy <- paste0( parametros$ivm_rdata_icomp_proy_benef, esc$nombre, '.RData' )
esc$rdata_balance <- paste0( parametros$ivm_rdata_icomp_balance, esc$nombre, '.RData' )

source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/302_calculo_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$ivm_rdata_icomp_conf_esc, esc$nombre, '.RData' ) )
rm( esc )

# Escenarios modificación corte constitucional -----------------------------------------------------
if ( parametros$ivm_cal_add_esc ) {
  # Escenario 7 --------------------------------------------------------------------------------------
  esc <- new.env()
  esc$nombre <- 'escenario_7'
  message( '\t\t\t', esc$nombre )
  
  esc$V0 <- 6543201759.76
  esc$mont_prop_afi <- parametros$mont_prop_afi
  esc$use_arit_mean <- TRUE
  
  esc$apo_act <- data.table( t = 0:parametros$horizonte,
                             i_a = 0.0625,
                             i_r = Hipotesis[ 4, 2 ],
                             i_sbu = Hipotesis[ 5, 2 ],
                             i_f = Hipotesis[ 7, 2 ],
                             i_p = Hipotesis[ 7, 2 ],
                             por_apo = c( 0.0766, 0.0886, 0.0986, rep( 0.1046, 38 ) ) + 0.001,
                             por_apo_pen_vej = c( 0.0276, 0.0276, rep( 0, 39 ) ),
                             por_apo_pen_inv = c( 0.0276, 0.0276, rep( 0, 39 ) ),
                             por_apo_pen_mon = c( 0.0276, 0.0276, rep( 0, 39 ) ),
                             por_apo_est = 0.4,
                             por_gast = 0.04,
                             cal_aux_fun = 0.22505423,
                             cal_mas = 0.9737615,
                             cal_pen_vej = 1.05145915,
                             cal_pen_inv = 0.91187837,
                             cal_gast = 1.059042 )
  
  tas_list <- list( 'a', 'r', 'sbu', 'f', 'p' )
  esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )
  
  esc$rdata_sal_proy <- paste0( parametros$ivm_rdata_icomp_sal_proy, esc$nombre, '.RData' )
  esc$rdata_ben_proy <- paste0( parametros$ivm_rdata_icomp_proy_benef, esc$nombre, '.RData' )
  esc$rdata_balance <- paste0( parametros$ivm_rdata_icomp_balance, esc$nombre, '.RData' )
  
  source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/302_calculo_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )
  save( esc, file = paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', esc$nombre, '.RData' ) )
  rm( esc )
  
  # Escenario 8 --------------------------------------------------------------------------------------
  load( paste0( parametros$RData, 'IESS_macro_estudio_risko.RData' ) )
  
  esc <- new.env()
  esc$nombre <- 'escenario_8'
  message( '\t\t\t', esc$nombre )
  
  iv_a <- rep( 0.0625, 41 )
  
  iv_sbu <- sbu[ anio >= 2018 ]
  setorder( iv_sbu, anio )
  iv_sbu <- iv_sbu$cre_sbu
  
  iv_r <- salario[ anio >= 2018 ]
  setorder( iv_r, anio )
  iv_r <- iv_r$cre_sal
  
  iv_f <- inflacion[ anio >= 2018 ]
  setorder( iv_f, anio )
  iv_f<- iv_f$inflacion
  
  iv_p <- inflacion[ anio >= 2018 ]
  setorder( iv_p, anio )
  iv_p<- iv_p$inflacion
  
  esc$V0 <- 6543201759.76
  esc$mont_prop_afi <- parametros$mont_prop_afi
  esc$use_arit_mean <- TRUE
  
  esc$apo_act <- data.table( t = 0:parametros$horizonte, 
                             i_a = iv_a,
                             i_r = iv_r,
                             i_sbu = iv_sbu,
                             i_f = iv_f,
                             i_p = iv_p,
                             por_apo = c( 0.0776, 0.0896, 0.0996, 
                                          0.1106, 0.1106, 0.1106, 0.1106, 0.1106, 
                                          0.1150, 0.1150, 0.1150, 0.1150, 0.1150, 
                                          0.1188, 0.1188, 0.1188, 0.1188, 0.1188, 
                                          0.1226, 0.1226, 0.1226, 0.1226, 0.1226, 
                                          0.1263, 0.1263, 0.1263, 0.1263, 0.1263, 
                                          0.1301, 0.1301, 0.1301, 0.1301, 0.1301, 
                                          0.1338, 0.1338, 0.1338, 0.1338, 0.1338, 
                                          0.1353, 0.1353, 0.1353 ),
                             por_apo_pen_vej = c( 0.0276, 0.0276, rep( 0, 39 ) ),
                             por_apo_pen_inv = c( 0.0276, 0.0276, rep( 0, 39 ) ),
                             por_apo_pen_mon = c( 0.0276, 0.0276, rep( 0, 39 ) ),
                             por_apo_est = 0.4,
                             por_gast = 0.04,
                             cal_aux_fun = 0.22607975,
                             cal_mas = 0.9945173,
                             cal_pen_vej = 1.07059476,
                             cal_pen_inv = 0.93001025,
                             cal_gast = 1.059042 )
  
  tas_list <- list( 'a', 'r', 'sbu', 'f', 'p' )
  esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )
  
  esc$rdata_sal_proy <- paste0( parametros$ivm_rdata_icomp_sal_proy, esc$nombre, '.RData' )
  esc$rdata_ben_proy <- paste0( parametros$ivm_rdata_icomp_proy_benef, esc$nombre, '.RData' )
  esc$rdata_balance <- paste0( parametros$ivm_rdata_icomp_balance, esc$nombre, '.RData' )
  
  source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/302_calculo_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )
  save( esc, file = paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', esc$nombre, '.RData' ) )
  rm( esc )
  
  # Escenario 9 --------------------------------------------------------------------------------------
  load( paste0( parametros$RData, 'IESS_macro_estudio_risko.RData' ) )
  
  esc <- new.env()
  esc$nombre <- 'escenario_9'
  message( '\t\t\t', esc$nombre )
  
  iv_a <- rep( 0.0625, 41 )
  
  iv_sbu <- sbu[ anio >= 2018 ]
  setorder( iv_sbu, anio )
  iv_sbu <- iv_sbu$cre_sbu
  
  iv_r <- salario[ anio >= 2018 ]
  setorder( iv_r, anio )
  iv_r <- iv_r$cre_sal
  
  iv_f <- inflacion[ anio >= 2018 ]
  setorder( iv_f, anio )
  iv_f<- iv_f$inflacion
  
  iv_p <- inflacion[ anio >= 2018 ]
  setorder( iv_p, anio )
  iv_p<- iv_p$inflacion
  
  esc$V0 <- 6543201759.76
  esc$mont_prop_afi <- parametros$mont_prop_afi
  esc$use_arit_mean <- TRUE
  
  esc$apo_act <- data.table( t = 0:parametros$horizonte, 
                             i_a = iv_a,
                             i_r = iv_r,
                             i_sbu = iv_sbu,
                             i_f = iv_f,
                             i_p = iv_p,
                             por_apo = c( 0.0776, 0.0896, 0.0996, 
                                          0.1106, 0.1106, 0.1106, 0.1106, 0.1106, 
                                          0.1150, 0.1150, 0.1150, 0.1150, 0.1150, 
                                          0.1188, 0.1188, 0.1188, 0.1188, 0.1188, 
                                          0.1226, 0.1226, 0.1226, 0.1226, 0.1226, 
                                          0.1263, 0.1263, 0.1263, 0.1263, 0.1263, 
                                          0.1301, 0.1301, 0.1301, 0.1301, 0.1301, 
                                          0.1338, 0.1338, 0.1338, 0.1338, 0.1338, 
                                          0.1353, 0.1353, 0.1353 ),
                             por_apo_pen_vej = c( 0.0276, 0.0276, rep( 0, 39 ) ),
                             por_apo_pen_inv = c( 0.0276, 0.0276, rep( 0, 39 ) ),
                             por_apo_pen_mon = c( 0.0276, 0.0276, rep( 0, 39 ) ),
                             por_apo_est = 0.4,
                             por_gast = 0.04,
                             cal_aux_fun = 0.22607975,
                             cal_mas = c( 0.9945173, 0.9945173,
                                          seq( 0.91, 0.97, length.out = 10 ),
                                          seq( 0.97, 0.99, length.out = 29 ) ),
                             cal_pen_vej = 1.07059476,
                             cal_pen_inv = 0.93001025,
                             cal_gast = 1.059042 )
  
  tas_list <- list( 'a', 'r', 'sbu', 'f', 'p' )
  esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )
  
  esc$rdata_sal_proy <- paste0( parametros$ivm_rdata_icomp_sal_proy, esc$nombre, '.RData' )
  esc$rdata_ben_proy <- paste0( parametros$ivm_rdata_icomp_proy_benef, esc$nombre, '.RData' )
  esc$rdata_balance <- paste0( parametros$ivm_rdata_icomp_balance, esc$nombre, '.RData' )
  
  source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/302_calculo_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )
  save( esc, file = paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', esc$nombre, '.RData' ) )
  rm( esc )
  
  # Escenario 10 --------------------------------------------------------------------------------------
  load( paste0( parametros$RData, 'IESS_macro_estudio_risko.RData' ) )
  
  esc <- new.env()
  esc$nombre <- 'escenario_10'
  message( '\t\t\t', esc$nombre )
  
  iv_a <- rep( 0.0625, 41 )
  
  iv_sbu <- sbu[ anio >= 2018 ]
  setorder( iv_sbu, anio )
  iv_sbu <- iv_sbu$cre_sbu
  
  iv_r <- salario[ anio >= 2018 ]
  setorder( iv_r, anio )
  iv_r <- iv_r$cre_sal
  
  iv_f <- inflacion[ anio >= 2018 ]
  setorder( iv_f, anio )
  iv_f<- iv_f$inflacion
  
  iv_p <- inflacion[ anio >= 2018 ]
  setorder( iv_p, anio )
  iv_p<- iv_p$inflacion
  
  esc$V0 <- 6543201759.76
  esc$mont_prop_afi <- parametros$mont_prop_afi
  esc$use_arit_mean <- TRUE
  
  esc$apo_act <- data.table( t = 0:parametros$horizonte, 
                             i_a = iv_a,
                             i_r = iv_r,
                             i_sbu = iv_sbu,
                             i_f = iv_f,
                             i_p = iv_p,
                             por_apo = c( 0.0776, 0.0896, 0.0996, 
                                          0.1106, 0.1106, 0.1106, 0.1106, 0.1106, 
                                          0.1150, 0.1150, 0.1150, 0.1150, 0.1150, 
                                          0.1188, 0.1188, 0.1188, 0.1188, 0.1188, 
                                          0.1226, 0.1226, 0.1226, 0.1226, 0.1226, 
                                          0.1263, 0.1263, 0.1263, 0.1263, 0.1263, 
                                          0.1301, 0.1301, 0.1301, 0.1301, 0.1301, 
                                          0.1338, 0.1338, 0.1338, 0.1338, 0.1338, 
                                          0.1353, 0.1353, 0.1353 ),
                             por_apo_pen_vej = c( 0.0276, 0.0276, rep( 0, 39 ) ),
                             por_apo_pen_inv = c( 0.0276, 0.0276, rep( 0, 39 ) ),
                             por_apo_pen_mon = c( 0.0276, 0.0276, rep( 0, 39 ) ),
                             por_apo_est = 0.28,
                             por_gast = 0.04,
                             cal_aux_fun = 0.22607975,
                             cal_mas = 0.9945173,
                             cal_pen_vej = 1.07059476,
                             cal_pen_inv = 0.93001025,
                             cal_gast = 1.059042 )
  
  tas_list <- list( 'a', 'r', 'sbu', 'f', 'p' )
  esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )
  
  esc$rdata_sal_proy <- paste0( parametros$ivm_rdata_icomp_sal_proy, esc$nombre, '.RData' )
  esc$rdata_ben_proy <- paste0( parametros$ivm_rdata_icomp_proy_benef, esc$nombre, '.RData' )
  esc$rdata_balance <- paste0( parametros$ivm_rdata_icomp_balance, esc$nombre, '.RData' )
  
  source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/302_calculo_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )
  save( esc, file = paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', esc$nombre, '.RData' ) )
  rm( esc )  
  
  # Escenario 11 -------------------------------------------------------------------------------------
  # en base al escenario 10 pero sin aporte del estado
  load( paste0( parametros$RData, 'IESS_macro_estudio_risko.RData' ) )
  
  esc <- new.env()
  esc$nombre <- 'escenario_11'
  message( '\t\t\t', esc$nombre )
  
  iv_a <- rep( 0.0625, 41 )
  
  iv_sbu <- sbu[ anio >= 2018 ]
  setorder( iv_sbu, anio )
  iv_sbu <- iv_sbu$cre_sbu
  
  iv_r <- salario[ anio >= 2018 ]
  setorder( iv_r, anio )
  iv_r <- iv_r$cre_sal
  
  iv_f <- inflacion[ anio >= 2018 ]
  setorder( iv_f, anio )
  iv_f<- iv_f$inflacion
  
  iv_p <- inflacion[ anio >= 2018 ]
  setorder( iv_p, anio )
  iv_p<- iv_p$inflacion
  
  esc$V0 <- 6543201759.76
  esc$mont_prop_afi <- parametros$mont_prop_afi
  esc$use_arit_mean <- TRUE
  
  esc$apo_act <- data.table( t = 0:parametros$horizonte, 
                             i_a = iv_a,
                             i_r = iv_r,
                             i_sbu = iv_sbu,
                             i_f = iv_f,
                             i_p = iv_p,
                             por_apo = c( 0.0776, 0.0896, 0.0996, 
                                          0.1106, 0.1106, 0.1106, 0.1106, 0.1106, 
                                          0.11, 0.1150, 0.1150, 0.1150, 0.1150, 
                                          0.1188, 0.1188, 0.1188, 0.1188, 0.1188, 
                                          0.1226, 0.1226, 0.1226, 0.1226, 0.1226, 
                                          0.1263, 0.1263, 0.1263, 0.1263, 0.1263, 
                                          0.1301, 0.1301, 0.1301, 0.1301, 0.1301, 
                                          0.1338, 0.1338, 0.1338, 0.1338, 0.1338, 
                                          0.1353, 0.1353, 0.1353 ),
                             por_apo_pen_vej = c( 0.0276, 0.0276, rep( 0, 39 ) ),
                             por_apo_pen_inv = c( 0.0276, 0.0276, rep( 0, 39 ) ),
                             por_apo_pen_mon = c( 0.0276, 0.0276, rep( 0, 39 ) ),
                             por_apo_est = 0,
                             por_gast = 0.04,
                             cal_aux_fun = 0.22607975,
                             cal_mas = 0.9945173,
                             cal_pen_vej = 1.07059476,
                             cal_pen_inv = 0.93001025,
                             cal_gast = 1.059042 )
  
  tas_list <- list( 'a', 'r', 'sbu', 'f', 'p' )
  esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )
  
  esc$rdata_sal_proy <- paste0( parametros$ivm_rdata_icomp_sal_proy, esc$nombre, '.RData' )
  esc$rdata_ben_proy <- paste0( parametros$ivm_rdata_icomp_proy_benef, esc$nombre, '.RData' )
  esc$rdata_balance <- paste0( parametros$ivm_rdata_icomp_balance, esc$nombre, '.RData' )
  
  source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/302_calculo_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )
  save( esc, file = paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', esc$nombre, '.RData' ) )
  rm( esc )  
  
}

# Análisis de Sensibilidad -------------------------------------------------------------------------
## Sensibilidad al Escenario 2 ---------------------------------------------------------------------
## Modificando la tasa de descuento actuarial ------------------------------------------------------
esc_sens <- new.env()
esc_sens$nombre <- 'sensibilidad_1'
esc_sens$descripción <- 'sensibilidad ante la tasa actuarial del escenario 2'
esc_sens$escenario <- 'escenario_2'
esc_sens$escenario_pattern <- paste0( esc_sens$escenario, "_sens_tas_act_" )
esc_sens$var_tas <- seq( 0.0525, 0.0725, 0.0025 )

fls_rem <- list.files( parametros$RData_seg, full.names = TRUE, pattern = esc_sens$escenario_pattern )
if ( length( fls_rem ) > 0 ){
  file.remove( fls_rem )  
}

parametros_lista <- unique( c( parametros_lista, 'esc_sens' ) )

esc_sens$escenario_list <- NULL
esc_sens$escenario_conf_list <- NULL
for ( k in 1:length( esc_sens$var_tas ) ) {

  # Escenario sobre el que se construye la sensibilidad
  escenario <- esc_sens$escenario
  
  load( paste0( parametros$ivm_rdata_icomp_conf_esc, escenario, '.RData' ) )
  
  esc$nombre <- paste0( esc_sens$escenario_pattern, k )
  esc$cont_sens <- k
  esc_sens$escenario_list <- c( esc_sens$escenario_list, esc$nombre )
  
  message( '\t\t\t', esc$nombre )
  
  esc$apo_act[ , i_a := esc_sens$var_tas[ k ] ]
  tas_list <- list( 'a', 'r', 'sbu', 'f', 'p' )
  esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )
  
  esc$rdata_sal_proy <- paste0( parametros$ivm_rdata_icomp_sal_proy, esc$nombre, '.RData' )
  esc$rdata_ben_proy <- paste0( parametros$ivm_rdata_icomp_proy_benef, esc$nombre, '.RData' )
  esc$rdata_balance <- paste0( parametros$ivm_rdata_icomp_balance, esc$nombre, '.RData' )
  
  source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/302_calculo_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )
  
  esc_conf_name <- paste0( parametros$ivm_rdata_icomp_conf_esc, esc$nombre, '.RData' )
  save( esc, file = esc_conf_name )
  esc_sens$escenario_conf_list <- c( esc_sens$escenario_conf_list, esc_conf_name )
  rm( esc )  
}

save( esc_sens,
      file = paste0( parametros$RData_seg, 'IESS_IVM_configuracion_analisis_', esc_sens$nombre, '.RData' ) )

## Modificando el crecimiento promedio de los salarios ---------------------------------------------
esc_sens <- new.env()
esc_sens$nombre <- 'sensibilidad_2'
esc_sens$descripción <- 'sensibilidad ante la tasa de crecimiento salarial del escenario 2'
esc_sens$escenario <- 'escenario_2'
esc_sens$escenario_pattern <- paste0( esc_sens$escenario, "_sens_tas_sal_" )
esc_sens$cam_tas <- seq( 0.9, 1.1, 0.025 )

fls_rem <- list.files( parametros$RData_seg, full.names = TRUE, pattern = esc_sens$escenario_pattern )
if ( length( fls_rem ) > 0 ){
  file.remove( fls_rem )  
}

parametros_lista <- unique( c( parametros_lista, 'esc_sens' ) )

esc_sens$escenario_list <- NULL
esc_sens$escenario_conf_list <- NULL
for ( k in 1:length( esc_sens$cam_tas ) ) {

  # Escenario sobre el que se construye la sensibilidad
  escenario <- esc_sens$escenario
  
  load( paste0( parametros$ivm_rdata_icomp_conf_esc, escenario, '.RData' ) )
  
  esc$nombre <- paste0( esc_sens$escenario_pattern, k )
  esc$cont_sens <- k
  esc_sens$escenario_list <- c( esc_sens$escenario_list, esc$nombre )
  
  message( '\t\t\t', esc$nombre )
  
  esc$apo_act[ , i_r := esc_sens$cam_tas[ k ] * i_r ]
  tas_list <- list( 'a', 'r', 'sbu', 'f', 'p' )
  esc$apo_act <- parametros$prepara_tasas( dat = esc$apo_act, tas_list )
  
  esc$rdata_sal_proy <- paste0( parametros$ivm_rdata_icomp_sal_proy, esc$nombre, '.RData' )
  esc$rdata_ben_proy <- paste0( parametros$ivm_rdata_icomp_proy_benef, esc$nombre, '.RData' )
  esc$rdata_balance <- paste0( parametros$ivm_rdata_icomp_balance, esc$nombre, '.RData' )
  
  source( 'R/demografia/305_proyeccion_salarios.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/300_proyeccion_beneficios_ivm.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/ivm/302_calculo_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )
  
  esc_conf_name <- paste0( parametros$ivm_rdata_icomp_conf_esc, esc$nombre, '.RData' )
  save( esc, file = esc_conf_name )
  esc_sens$escenario_conf_list <- c( esc_sens$escenario_conf_list, esc_conf_name )
  rm( esc )
}

save( esc_sens,
      file = paste0( parametros$RData_seg, 'IESS_IVM_configuracion_analisis_', esc_sens$nombre, '.RData' ) )

rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
