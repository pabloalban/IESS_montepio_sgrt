message( paste( rep( '-', 100 ), collapse = '' ) )

# Carga de datos -----------------------------------------------------------------------------------
load( parametros$demo_rdata_rtr_pob_proy )

message( '\tGenerando tablas de proyección de la población de beneficiarios de RTR' )

# Generando tabla de estados de RTR- ---------------------------------------------------------------
y_max <- parametros$anio_ini + parametros$horizonte

pob_proy_tot_sex <- as.data.table( pob_proy_tot_sex )

aux_m <- pob_proy_tot_sex[ sexo == 'M', list( t = t + parametros$anio_ini, 
                                              l12_m = l_12, l13_m = l_13, l14_m = l_14, l15_m = l_15, 
                                              l16_m = l_16 ) ]
aux_m <- aux_m[ t <= y_max ]
aux_h <- pob_proy_tot_sex[ sexo == 'H', list( t = t + parametros$anio_ini, 
                                              l12_h = l_12, l13_h = l_13, l14_h = l_14, l15_h = l_15, 
                                              l16_h = l_16 ) ]
aux_h <- aux_h[ t <= y_max ]

aux <- merge( aux_m, aux_h, by = c( 't' ) )

aux[1, ] <- round(aux[1, ],0)

aux[, t := as.character( t ) ]
#aux <- aux[ t > 2020 ]

xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 10 ) ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_rtr.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )


#Limpiando memoria RAM------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
