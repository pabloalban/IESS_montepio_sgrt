# Configuraciones particulares ---------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tConfiguraciones particulares para el seguro IVM' )

# Controla carga de resultados generados por el software de la ILO
parametros$ivm_cargar_ilo_res <- FALSE

# Controla si los cálculos del balance se realizarán utilizando el software de la ILO
parametros$ivm_calcular_ilo_res <- FALSE

parametros$ivm_hacer_ana_dem <- FALSE
parametros$ivm_calcular_balance <- FALSE

# Calcular escenarios adicionales
parametros$ivm_cal_add_esc <- FALSE

parametros$ivm_horizonte <- 40 # en años  
parametros$ivm_anio_fin <- parametros$anio_ini + parametros$ivm_horizonte
parametros$ivm_fec_fin <- ymd( '2020-12-31' )
parametros$ivm_fec_ini <- ymd( '2012-01-01' ) # fecha inicio del periodo de observación
parametros$ivm_reserva_ini <- 7876664012.99 # reserva inicial incluyendo fondo de TNRH

# Reporte-------------------------------------------------------------------------------------------
parametros$ivm_rep_gen <- paste0( parametros$work_dir, 'R/ivm/600_reporte_latex_ivm.R' )

# Objetos de salida, usualmente .RData -------------------------------------------------------------
parametros$ivm_rdata_est_demo <- paste0( parametros$RData_seg, 'IESS_IVM_estadísticas_demografia.RData' )
parametros$ivm_rdata_bal_fin <- paste0( parametros$RData_seg, 'IESS_IVM_balance_financieros_', parametros$anio_ini, '.RData' )

parametros$ivm_rdata_icomp_balance <- paste0( parametros$RData_seg, 'IESS_IVM_balances_', parametros$anio_ini, '_' )
parametros$ivm_rdata_icomp_conf_esc <- paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', parametros$anio_ini, '_' )
parametros$ivm_rdata_icomp_proy_benef <- paste0( parametros$RData_seg, 'IESS_IVM_proyeccion_beneficios_', parametros$anio_ini, '_' )
parametros$ivm_rdata_icomp_primas <- paste0( parametros$RData_seg, 'IESS_IVM_primas_', parametros$anio_ini, '_' )
parametros$ivm_rdata_icomp_ratios <- paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', parametros$anio_ini, '_' )
parametros$ivm_rdata_icomp_sensibilidad <- paste0( parametros$RData_seg, 'IESS_IVM_analisis', '_' )
parametros$ivm_rdata_icomp_sal_proy <- paste0( parametros$RData_seg, 'IESS_IVM_proyeccion_salarios_', parametros$anio_ini, '_' )
parametros$ivm_rdata_ana_sen <- paste0( parametros$RData_seg, 'IESS_IVM_analisis_sensibilidad_total_', parametros$anio_ini, '.RData' )

# Variables globales que son sobreescritas por cada seguro -----------------------------------------
parametros$horizonte <- parametros$ivm_horizonte

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
