message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tCreación Tablas Estados Financieros IVM (para informe IVM)' ) 

# Nota: se ha cambiado los nombres "incremento" a "variación" pues no siempre crece, a veces baja. 

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_IVM_analisis_financiero.RData' ) )

# Activos ------------------------------------------------------------------------------------------
# "Activo del Fondo de IVM al 31 de diciembre de cada año (En millones de dólares)"
aux <- copy( activos_ivm)
aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux, # set column names
                 c('Año',
                   'Activo',
                   'Incremento Anual',
                   'Incremento Porcentual Anual') )
# View(aux)

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_activo_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


# Componentes de los Activos -----------------------------------------------------------------------
# "Componentes de Activos del Fondo de IVM por años"
aux <- copy( comp_activos_ivm ) 
aux_cuentas <- c( 'ACTIVO',
                  'Fondos Disponibles',
                  'Inversiones',
                  'Cuentas por Cobrar',
                  'Propiedad Planta y Equipo',
                  'Deuda del Gobierno',
                  'Intereses por Cobrar',
                  'Otros Activos')

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_comp_activo_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 1:(nrow(aux)-1),
       sanitize.text.function = identity )

# Analisis horizontal de Activos -------------------------------------------------------------------
# "Analisis horizontal de Activos del Fondo de IVM"
aux <- copy( activos_horizon_ivm ) 
aux_cuentas <- c( 'ACTIVO',
                  'Fondos Disponibles',
                  'Inversiones',
                  'Cuentas por Cobrar',
                  'Propiedad Planta y Equipo',
                  'Deuda del Gobierno',
                  'Intereses por Cobrar',
                  'Otros Activos')

aux_xtable <- xtable( aux, digits = rep( 0, 12 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_activo_horiz_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 1:(nrow(aux)-1),
       sanitize.text.function = identity )

# Análisis vertical de Activos ---------------------------------------------------------------------
# "Análisis vertical de Activos del Fondo de IVM"
aux <- copy( activos_vertical_ivm)
aux_cuentas <- c('ACTIVO',
                 'Fondos Disponibles',
                 'Inversiones',
                 'Cuentas por Cobrar',
                 'Propiedad Planta y Equipo',
                 'Deuda del Gobierno',
                 'Intereses por Cobrar',
                 'Otros Activos')
# View(aux)

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_activo_vert_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 1:(nrow(aux)-1),
       sanitize.text.function = identity )

# Cuentas por cobrar -------------------------------------------------------------------------------
# "Cuentas por cobrar del Fondo de IVM al 31 de diciembre de cada año"
aux <- copy( ctasxcobrar_ivm )
aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux,
                 c( 'Año', 
                    'Cuentas por cobrar', 
                    'Incremento Anual', 
                    'Incremento Porcentual Anual' ) )
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )

print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_cuentas_cobrar_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#  Pasivos ...............--------------------------------------------------------------------------
# "Pasivo del Fondo de IVM al 31 de diciembre de cada año"
# names(pasivos_IVM)
aux <- copy( pasivos_IVM )
aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux,
                 c( 'Año', 
                    'Pasivo', 
                    'Incremento Anual', 
                    'Incremento Porcentual Anual' ) )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) ) # ncol(aux) + 1
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_pasivo_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Componentes de los Pasivos -----------------------------------------------------------------------
# "Componentes de Pasivos del Fondo de IVM por años"
aux <- copy( comp_pasivos_IVM[1:6,])
aux_cuentas <- c( 'PASIVO',
                  'Prestaciones y Beneficios',
                  'Cuentas por pagar',
                  'Pasivo Diferido',
                  'Pasivos Corrientes',
                  'Pasivos No Corrientes' )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_comp_pasivo_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 1:(nrow(aux)-1),
       sanitize.text.function = identity )

# Análisis horizontal de Pasivos -------------------------------------------------------------------
# "Análisis horizontal de Pasivos del Fondo de IVM"
aux <- copy( pasivos_horiz_IVM[1:6,])
aux_cuentas <- c('PASIVO',
                 'Prestaciones y Beneficios',
                 'Cuentas por pagar',
                 'Pasivo Diferido',
                 'Pasivos Corrientes',
                 'Pasivos No Corrientes')
# View(aux)

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_pasivo_horiz_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 1:(nrow(aux)-1),
       sanitize.text.function = identity )

# Análisis vertical de Pasivos ---------------------------------------------------------------------
# "Análisis vertical de Pasivos del Fondo de IVM"
aux <- copy(pasivos_vertical_IVM)
aux_cuentas <- c('PASIVO',
                 'Prestaciones y Beneficios',
                 'Cuentas por pagar',
                 'Pasivo Diferido',
                 'Pasivos Corrientes',
                 'Pasivos No Corrientes')
# View(aux)

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_pasivo_vert_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 1:(nrow(aux)-1),
       sanitize.text.function = identity )

# Cuentas por pagar --------------------------------------------------------------------------------
# "Cuentas por pagar del Fondo de IVM al 31 de diciembre de cada año"
aux <- copy( ctasxpagar_IVM )
aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux, 
                 c( 'Año', 
                    'Cuentas x pagar', 
                    'Incremento Anual',
                    'Incremento Porcentual Anual' ) )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )

print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_cuentas_pagar_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Patrimonio ---------------------------------------------------------------------------------------
# "Patrimonio del Fondo de IVM al 31 de diciembre de cada año."
aux <- copy(patrimonio_IVM )
aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux, 
                 c('Año',
                   'Patrimonio',
                   'Incremento Anual',
                   'Incremento Porcentual Anual') )
# View(aux)
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )

print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_patrimonio_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Componentes del Patrimonio -----------------------------------------------------------------------
# "Componentes del Patrimonio del Fondo de IVM por años"
aux <- copy(comp_patrimonio_IVM)
aux_cuentas <- c('PATRIMONIO',
                 'Fondos \\ Capitalizados',
                 'Resultados',
                 'Superavit \\ Revaluacion',
                 'Reservas',             
                 'Aportes \\ Patrimoniales')

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_comp_patrimonio_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 1:(nrow(aux)-1),
       sanitize.text.function = identity )

# Analisis horizontal de Patrimonio ----------------------------------------------------------------
# "Analisis horizontal de Patrimonio del Fondo de IVM"
aux <- copy(patrimonio_horiz_IVM)
aux_cuentas <- c('PATRIMONIO',
                 'Fondos \\ Capitalizados',
                 'Resultados',
                 'Superavit \\ Revaluacion',
                 'Reservas',             
                 'Aportes \\ Patrimoniales')

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_patrimonio_horiz_ivm','.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 1:(nrow(aux)-1),
       sanitize.text.function = identity )

# Analisis vertical del Patrimonio -----------------------------------------------------------------
# "Analisis vertical del patrimonio del Fondo de IVM"
aux <- copy( patrimonio_vert_IVM)
aux_cuentas <- c('PATRIMONIO',
                 'Fondos \\ Capitalizados',
                 'Resultados',
                 'Superavit \\ Revaluacion',
                 'Reservas',             
                 'Aportes \\ Patrimoniales')

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_patrimonio_vert_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 1:(nrow(aux)-1),
       sanitize.text.function = identity )

# Ingresos -----------------------------------------------------------------------------------------
aux <- copy( ingresos_IVM )
aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux,
                 c('Año',
                   'Ingresos',
                   'Incremento Anual',
                   'Incremento Porcentual Anual') )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) ) 
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_ingresos_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Componentes del Ingreso --------------------------------------------------------------------------
# "Componentes del Ingreso del Fondo de IVM por años"
aux <- copy(comp_ingreso_IVM)
aux_cuentas <- c('INGRESOS',
                 'Ingresos de la \\  Operacion (Aportes IESS)',
                 'Ingresos Financieros',
                 'Ingresos por Arriendo',
                 'Ingresos Extraordinarios',
                 'Otros Resultados Integrales')

aux_xtable <- xtable( aux, digits = c( 0 , 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_comp_ingreso_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 1:(nrow(aux)-1),
       sanitize.text.function = identity )

# Análisis horizontal del Ingreso ------------------------------------------------------------------
# "Análisis horizontal del Ingreso del Fondo de IVM"
aux <- copy( ingreso_horiz_IVM)
aux_cuentas <- c('INGRESOS',
                 'Ingresos de la \\  Operacion (Aportes IESS)',
                 'Ingresos Financieros',
                 'Ingresos por Arriendo',
                 'Ingresos Extraordinarios',
                 'Otros Resultados Integrales')

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_ingreso_horiz_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 1:(nrow(aux)-1),
       sanitize.text.function = identity )

# Analisis vertical del Ingreso --------------------------------------------------------------------
# "Analisis vertical del ingreso del Fondo de IVM"
aux <- copy( ingreso_vert_IVM)
aux_cuentas <- c('INGRESOS',
                 'Ingresos de la \\  Operacion (Aportes IESS)',
                 'Ingresos Financieros',
                 'Ingresos por Arriendo',
                 'Ingresos Extraordinarios',
                 'Otros Resultados Integrales')

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_ingreso_vert_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 1:(nrow(aux)-1),
       sanitize.text.function = identity )

# ingresos por aportes -----------------------------------------------------------------------------
aux <- copy( ingresosxaportes_IVM )
aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux, 
                 c('Año',
                   'Aportes Personales',
                   'Aportes Patronales',
                   'Aportes Jubilados y Pensionistas',
                   'Aportes adicionales magisterio',
                   'Aporte ley Discapacidad',
                   'Aporte personal Amas de Casa',
                   'Aporte Ley Discapacidad ISSPOL',
                   'Aportes_Afiliados') )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2 ) ) 
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_ingresosxaportes_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Otros ingresos -----------------------------------------------------------------------------------
aux <- copy( contriEstadovsPensPag_IVM )
aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux, 
                 c('Año',
                   'Contribución_del_Estado',
                   'Pensiones_Pagadas_de_Invalidez_Vejez_Montepio',
                   'Participación del Estado en el pago de pensiones') )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) ) 
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_contribEstadovsPensPag_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Egresos Prestacionales IVM -----------------------------------------------------------------------
aux <- copy(egresos_prestacionales)
aux[ , Anio := as.character( Anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) ) 
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_egresos_prestacionales_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


# Gastos Administración IVM ------------------------------------------------------------------------
aux <- copy( gastos_administracion)
aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux,
                 c('Año',
                   'Gastos',
                   'Incremento Anual',
                   'variacion anual') )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) ) 
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_gastos_administracion_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )



# Componentes de los Egresos -----------------------------------------------------------------------
# "Componentes de los Egresos del Fondo de IVM por años"
aux <- copy(comp_egresos_IVM)
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_comp_egresos_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 1:(nrow(aux)-1),
       sanitize.text.function = identity )

# Análisis horizontal del los Egresos --------------------------------------------------------------
# "Análisis horizontal de los Egresos del Fondo de IVM"
aux <- copy( egresos_horiz_IVM)
aux_xtable <- xtable( aux, digits = rep(0, 12 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_egresos_horiz_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 1:(nrow(aux)-1),
       sanitize.text.function = identity )

# Análisis vertical de los Egresos -----------------------------------------------------------------
# "Análisis vertical de los egresos del Fondo de IVM"
aux <- copy( egresos_vert_IVM)
aux_xtable <- xtable( aux, digits = rep(0, 13) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_egresos_vert_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 1:(nrow(aux)-1),
       sanitize.text.function = identity )


# Gastos prestacionales por pensiones --------------------------------------------------------------
aux <- copy( gastxprest_IVM )
aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux,
                 c('Año',
                   'Pensiones_Pagadas_de_Invalidez_Vejez_Montepio',
                   'Tasa de variación de las pensiones pagadas') )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2 ) ) 
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_gastxprest_IVM', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


# Otros gastos prestacionales ----------------------------------------------------------------------
aux <- copy( otrosgstospres_IVM  )
aux_cuentas <- c( 'Año',
                  '13ra Pensión',
                  '14ta Pensión',
                  'Auxilios Funerales',
                  'Total Otros Gastos Prestacionales' )

aux_xtable <- xtable( aux, digits = rep( 0, 6) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_otrgastosprest_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Gastos Contribución Administradora IVM------------------------------------------------------------
aux <- copy( gastcontadmin_IVM )
aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux,
                 c( 'Año',
                    'Gasto Contribución Administradora',
                    'Crecimiento de Gasto Contribución Administradora',
                    'Crecimiento Porcentual de Gasto Contribución Administradora' ) )

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) ) 
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_gastcontradmin_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Ingresos vs. gastos ------------------------------------------------------------------------------
# "Evolución de ingresos por aportes vs los gastos pensionales del Fondo de IVM"
aux <- copy(ing_vs_egre_pres_IVM )
aux[ , anio := as.character( anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2 ) ) 

print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_ing_vs_egre_pres_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Ingresos vs Egresos Prestacionales IVM------------------------------------------------------------
aux <- copy( ing_vs_gas_adm_IVM)
aux[ , Anio := as.character( Anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2 ) ) 
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_ing_vs_gas_adm_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Reservas Disponibles -----------------------------------------------------------------------------
aux <- copy(reservas_IVM  )
aux[ , Anio := as.character( Anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2, 2 ) ) 
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_reservas_IVM', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Clean --------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()

