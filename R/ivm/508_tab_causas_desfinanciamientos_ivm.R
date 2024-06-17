message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tTablas de las casusas de desfinanciamiento' )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_IVM_causas_desfinanciamiento.RData' ) )

# Tabla causas totales de desfinanciamiento---------------------------------------------------------
message( '\tTabla total desfinanciamiento' )
aux <- copy( causa_desf_total )
aux <- as.data.table( aux )
aux <- aux[ , print_names := c( 'Ausencia contribuci\\\'{o}n del 40\\% Estado',  
                                'Diferencia aportes C.D. 261 y C.D. 501',
                                'Desinversiones','Total') ]
aux <- aux[ ,list(print_names,capital,lucro,total)]
# aux[, anio := as.character( anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_IVM_total', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux)-1,nrow(aux)),
                         command = c(paste("\\hline \n"),paste("\\hline \n"))))

# Tabla aportes del estado--------------------------------------------------------------------------
message( '\tTabla aportes del estado' )
aux <- as.data.table( causa_desf_estado )
aux[ , tasa_rendimiento_anual := tasa_rendimiento_anual * 100 ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_IVM_Desfi_Aportes_Estado', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux)-1,nrow(aux)),
                         command = c(paste("\\hline \n"),paste("\\hline \n"))))

# Tabla CD 501--------------------------------------------------------------------------------------
message( '\tTabla aportes del estado' )
aux <- as.data.table( causa_desf_CD501 )
aux[, tasa_promedio_de_interes := tasa_promedio_de_interes * 100 ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_IVM_Desfi_CD501', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux)-1,nrow(aux)),
                         command = c(paste("\\hline \n"),paste("\\hline \n"))))

# Tabla desinversiones------------------------------------------------------------------------------
message( '\tTabla desinversiones' )
aux <- ( causa_desf_desinversiones ) %>% filter( capital_desinvertido > 0)
aux$periodo <- as.Date(aux$periodo,"%d/%m/%Y")
aux$periodo <- format(aux$periodo, "%b/%Y")
aux$rentabilidad_neta <- aux$rentabilidad_neta * 100
aux[ nrow(aux), 1 ] <- 'Total'
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 6, 6, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_IVM_desinversiones', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux)-1,nrow(aux)),
                         command = c(paste("\\hline \n"),paste("\\hline \n"))))

# Tabla desinversiones------------------------------------------------------------------------------
message( '\tTabla desinversiones' )
aux <- ( causa_desf_desinversiones_anual )
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2 ,2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_IVM_desinversiones_anual', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux)-1,nrow(aux)),
                         command = c(paste("\\hline \n"),paste("\\hline \n"))))

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()