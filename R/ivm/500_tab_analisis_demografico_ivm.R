message( paste( rep( '-', 100 ), collapse = '' ) )

#Creación tablas capítulo demográfico --------------------------------------------------------------
load( file = parametros$demo_rdata_sgo_est_dem )
# load( file = parametros$ivm_rdata_est_demo )

# Número de Afiliados SGO --------------------------------------------------------------------------
message( '\tNúmero de afiliados SGO activos' )

aux <- copy( est_sal_anio_sexo_edad )
aux_est_sal_anio_sexo_edad <- aux 
aux <- aux[ anio <= parametros$anio_ini,]
aux <- data.table( dcast( aux, anio ~ sexo, value.var = "ER_act", fun.aggregate = sum, na.rm = TRUE ) )
aux[ , total := H + M ]
aux[ , st := shift( total, type = c( "lag" ) ) ]
aux[ , var := ( total / st - 1 ) * 100 ]
aux[ , st := NULL]
aux[, anio := as.character( anio ) ]

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_sgo_act_pob', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

aux <- copy( est_sal_anio_sexo_edad )
aux_est_sal_anio_sexo_edad <- aux 
aux <- aux[ anio <= parametros$anio_ini,]
aux <- data.table( dcast( aux, anio ~ sexo, value.var = "m2", fun.aggregate = sum, na.rm = TRUE ) )
aux[ , total := H + M ]
aux[ , st := shift( total, type = c( "lag" ) ) ]
aux[ , var := ( total / st - 1 ) * 100 ]
aux[ , st := NULL]
aux[, anio := as.character( anio ) ]

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_sgo_act_pob_dic', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Número de Afiliados TNRH -------------------------------------------------------------------------
message( '\tNúmero de afiliados TNRH activos' )
aux <- copy( est_sal_anio_sexo_edad )
aux <- aux[ anio <= parametros$anio_ini,]
aux <- data.table( dcast( aux, anio ~ sexo,value.var = "ER_tnrh_act", fun.aggregate = sum, na.rm = TRUE ) )
aux[ , total := H + M ]
aux <- aux[ anio >= 2015, ]
aux[ , st := shift( total, type = c( "lag" ) ) ]
aux[ , var := ( total / st - 1 ) * 100 ]
aux[ , st := NULL]
aux[, anio := as.character( anio ) ]

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_thnrh_act_pob', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

aux <- copy( est_sal_anio_sexo_edad )
aux <- aux[ anio <= parametros$anio_ini,]
aux <- data.table( dcast( aux, anio ~ sexo,value.var = "m13", fun.aggregate = sum, na.rm = TRUE ) )
aux[ , total := H + M ]
aux <- aux[ anio >= 2015, ]
aux[ , st := shift( total, type = c( "lag" ) ) ]
aux[ , var := ( total / st - 1 ) * 100 ]
aux[ , st := NULL]
aux[, anio := as.character( anio ) ]

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_thnrh_act_pob_dic', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Masa salarial SGO --------------------------------------------------------------------------------
message( '\tMasa salarial de afiliados SGO activos' )
aux <- copy( est_sal_anio_sexo_edad )
aux <- aux[anio <= parametros$anio_ini,]
aux <- data.table( dcast( aux, anio ~ sexo,value.var = "S", fun.aggregate = sum, na.rm = TRUE ) )
aux[ , total := H + M ]
aux[ , st := shift( total, type = c( "lag" ) ) ]
aux[ , dife := total - st ]
aux[ , var := ( total / st - 1 ) * 100 ]
aux[ , st := NULL]
aux[, anio := as.character( anio ) ]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2 ,2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_sgo_mas_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity ) 

# Masa salarial TNRH -------------------------------------------------------------------------------
message( '\tMasa salarial de afiliados TNRH activos' )
aux <- copy( est_sal_anio_sexo_edad )
aux <- aux[ anio <= parametros$anio_ini & ER_tnrh_act > 0, ]
aux <- dcast.data.table( aux, anio ~ sexo,value.var = "ST", fun.aggregate = sum, na.rm = TRUE )
aux[ , total := H + M ]
aux[ , st := shift( total, type = c( "lag" ) ) ]
aux[ , dife := total - st ]
aux[ , var := (total/st-1) * 100 ]
aux[ , st := NULL]
aux[, anio := as.character( anio ) ]

aux_xtable <- xtable( aux, digits = c(0, 0, 2 ,2, 2, 2, 2 ))
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_tnrh_masa_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity ) 

# Imposiciones por edad y sexo del SGO --------------------------------------------------------------
message( '\tImposiciones por edad y sexo de afiliados activos del SGO' )
ff <- function( x ) ifelse( is.na( x ), NA, formatC( x, digits = 2, decimal.mark = ',', big.mark = '.', format = 'f' ) )
gf <- function( x ) ifelse( is.na( x ), NA, paste0( '\\$', x ) )

aux <- copy( est_act_er_sal_xs )
nom <- names( aux )
nom <- nom[ nom %nin% c( 'xg', 'ord' ) ]
aux[ , ( nom ) :=  lapply( .SD, ff ), .SDcols = nom ]
aux[ ord == 2, ( nom ) :=  lapply( .SD, gf ), .SDcols = nom ]
aux[ , xg := gsub( ',', ',\\ ', xg ) ]
aux[ !is.na( xg ), xg := gsub('(Inf)', '\\\\infty', xg ) ]
aux[ !is.na( xg ), xg := paste0( '$', xg, '$' ) ]
aux[ ord == 2, xg := NA ]
aux[ , ord := NULL ]
aux_xtable <- xtable( aux )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_sgo_imp_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

aux <- copy( est_act_er_sal_xs_h )
nom <- names( aux )
nom <- nom[ nom %nin% c( 'xg', 'ord' ) ]
aux[ , ( nom ) :=  lapply( .SD, ff ), .SDcols = nom ]
aux[ ord == 2, ( nom ) :=  lapply( .SD, gf ), .SDcols = nom ]
aux[ , xg := gsub( ',', ',\\ ', xg ) ]
aux[ !is.na( xg ), xg := gsub('(Inf)', '\\\\infty', xg ) ]
aux[ !is.na( xg ), xg := paste0( '$', xg, '$' ) ]
aux[ ord == 2, xg := NA ]
aux[ , ord := NULL ]
aux_xtable <- xtable( aux )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_sgo_imp_sal_h', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

aux <- copy( est_act_er_sal_xs_m )
nom <- names( aux )
nom <- nom[ nom %nin% c( 'xg', 'ord' ) ]
aux[ , ( nom ) :=  lapply( .SD, ff ), .SDcols = nom ]
aux[ ord == 2, ( nom ) :=  lapply( .SD, gf ), .SDcols = nom ]
aux[ , xg := gsub( ',', ',\\ ', xg ) ]
aux[ !is.na( xg ), xg := gsub('(Inf)', '\\\\infty', xg ) ]
aux[ !is.na( xg ), xg := paste0( '$', xg, '$' ) ]
aux[ ord == 2, xg := NA ]
aux[ , ord := NULL ]
aux_xtable <- xtable( aux )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_sgo_imp_sal_m', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

# Imposiciones por edad y sexo del TNRH ------------------------------------------------------------
message( '\tImposiciones por edad y sexo de afiliados activos del TNRH' )

aux <- copy( est_act_tnrh_er_sal_xs )
nom <- names( aux )
nom <- nom[ nom %nin% c( 'xg', 'ord' ) ]
aux[ , ( nom ) :=  lapply( .SD, ff ), .SDcols = nom ]
aux[ ord == 2, ( nom ) :=  lapply( .SD, gf ), .SDcols = nom ]
aux[ , xg := gsub( ',', ',\\ ', xg ) ]
aux[ !is.na( xg ), xg := gsub('(Inf)', '\\\\infty', xg ) ]
aux[ !is.na( xg ), xg := paste0( '$', xg, '$' ) ]
aux[ ord == 2, xg := NA ]
aux[ , ord := NULL ]
aux_xtable <- xtable( aux )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_tnrh_imp_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

aux <- copy( est_act_tnrh_er_sal_xs_h )
nom <- names( aux )
nom <- nom[ nom %nin% c( 'xg', 'ord' ) ]
aux[ , ( nom ) :=  lapply( .SD, ff ), .SDcols = nom ]
aux[ ord == 2, ( nom ) :=  lapply( .SD, gf ), .SDcols = nom ]
aux[ , xg := gsub( ',', ',\\ ', xg ) ]
aux[ !is.na( xg ), xg := gsub('(Inf)', '\\\\infty', xg ) ]
aux[ !is.na( xg ), xg := paste0( '$', xg, '$' ) ]
aux[ ord == 2, xg := NA ]
aux[ , ord := NULL ]
aux_xtable <- xtable( aux )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_tnrh_imp_sal_h', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

aux <- copy( est_act_tnrh_er_sal_xs_m )
nom <- names( aux )
nom <- nom[ nom %nin% c( 'xg', 'ord' ) ]
aux[ , ( nom ) :=  lapply( .SD, ff ), .SDcols = nom ]
aux[ ord == 2, ( nom ) :=  lapply( .SD, gf ), .SDcols = nom ]
aux[ , xg := gsub( ',', ',\\ ', xg ) ]
aux[ !is.na( xg ), xg := gsub('(Inf)', '\\\\infty', xg ) ]
aux[ !is.na( xg ), xg := paste0( '$', xg, '$' ) ]
aux[ ord == 2, xg := NA ]
aux[ , ord := NULL ]
aux_xtable <- xtable( aux )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_tnrh_imp_sal_m', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

# Número de pensionistas por tipo de beneficio -----------------------------------------------------
tipos <- c( 'VEJEZ', 'INVALIDEZ', 'DISCAPACIDAD', 'VIUDEDAD', 'ORFANDAD' )
tab_nom <- c( 'vej', 'inv', 'dis', 'viu', 'orf' )

for ( i in 1:length( tipos ) ) {
  
  message( '\tNúmero de pensionistas de ', tab_nom[ i ], ' del SGO' )
  
  if ( tab_nom[i] == 'dis' ) {
    
    est_pen_anio_tipo[ , td := ifelse( is.infinite( td ), NA , td ) ]
    est_pen_anio_tipo[ , tp := ifelse( is.infinite( tp ), NA , tp ) ]
    est_pen_anio_tipo[ , tpm := ifelse( is.infinite( tpm ), NA , tpm ) ]
    est_pen_anio_tipo_sexo[ , td := ifelse( is.infinite( td ), NA , td ) ]
    est_pen_anio_tipo_sexo[ , tp := ifelse( is.infinite( tp ), NA , tp ) ]
    est_pen_anio_tipo_sexo[ , tpm := ifelse( is.infinite( tpm ), NA , tpm ) ]
    
    aux <- est_pen_anio_tipo[ tipo == tipos[i] & anio <= parametros$anio_ini ]
    aux <- aux[ anio >= 2014, ]
    aux1 <- est_pen_anio_tipo_sexo[ tipo == tipos[ i ] & anio <= parametros$anio_ini ]
    aux1 <- aux1[ anio >= 2014, ]
    aux2 <- est_pen_anio_tipo_sexo[ tipo == tipos[ i ] & anio <= parametros$anio_ini & sexo == 'H' ]
    aux2 <- aux2[ anio >= 2014, ]
    aux3 <- est_pen_anio_tipo_sexo[ tipo == tipos[ i ] & anio <= parametros$anio_ini & sexo == 'M' ]
    aux3 <- aux3[ anio >= 2014, ]
  } else {
    aux <- est_pen_anio_tipo[ tipo == tipos[i] & anio <= parametros$anio_ini ]
    aux <- aux[anio >= 2012, ]
    aux1 <- est_pen_anio_tipo_sexo[ tipo == tipos[ i ] & anio <= parametros$anio_ini ]
    aux1 <- aux1[ anio >= 2012, ]
    aux2 <- est_pen_anio_tipo_sexo[ tipo == tipos[ i ] & anio <= parametros$anio_ini & sexo == 'H' ]
    aux2 <- aux2[ anio >= 2012, ]
    aux3 <- est_pen_anio_tipo_sexo[ tipo == tipos[ i ] & anio <= parametros$anio_ini & sexo == 'M' ]
    aux3 <- aux3[ anio >= 2012, ]
  }

  aux <- aux[ , list( anio, ER, td, EP, tp, EPm, tpm ) ]
  aux[ , anio := as.character( anio ) ]
  
  aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2 ) )
  print( aux_xtable,
         file = paste0( parametros$resultado_tablas, 'iess_pen_', tab_nom[ i ], '.tex' ),
         type = 'latex',
         include.colnames = FALSE,
         include.rownames = FALSE,
         format.args = list( decimal.mark = ',', big.mark = '.' ),
         only.contents = TRUE,
         hline.after = NULL,
         sanitize.text.function = identity )
  
  aux1 <- data.table( dcast( aux1, anio ~ sexo, value.var = "m", fun.aggregate = sum, na.rm = TRUE ) )
  aux1[ , total := H + M ]
  aux1[ , st := shift( total, type = c( "lag" ) ) ]
  aux1[ , var := ( total / st - 1 ) * 100 ]
  aux1[ , st := NULL]
  aux1[, anio := as.character( anio ) ]
  
  aux_xtable <- xtable( aux1, digits = c( 0, 0, 0, 0, 0, 2 ) )
  print( aux_xtable,
         file = paste0( parametros$resultado_tablas, 'iess_pen_', tab_nom[ i ], '_dic.tex' ),
         type = 'latex',
         include.colnames = FALSE,
         include.rownames = FALSE,
         format.args = list( decimal.mark = ',', big.mark = '.' ),
         only.contents = TRUE,
         hline.after = NULL,
         sanitize.text.function = identity )
  
  
  aux2 <- aux2[ , list( anio, ER, td, EP, tp, EPm, tpm ) ]
  aux2[ , anio := as.character( anio ) ]
  
  aux_xtable <- xtable( aux2, digits = c( 0, 0, 0, 2, 2, 2, 2, 2 ) )
  print( aux_xtable,
         file = paste0( parametros$resultado_tablas, 'iess_pen_', tab_nom[ i ], '_h', '.tex' ),
         type = 'latex',
         include.colnames = FALSE,
         include.rownames = FALSE,
         format.args = list( decimal.mark = ',', big.mark = '.' ),
         only.contents = TRUE,
         hline.after = NULL,
         sanitize.text.function = identity )
  
  aux3 <- aux3[ , list( anio, ER, td, EP, tp, EPm, tpm ) ]
  aux3[ , anio := as.character( anio ) ]
  
  aux_xtable <- xtable( aux3, digits = c( 0, 0, 0, 2, 2, 2, 2, 2 ) )
  print( aux_xtable,
         file = paste0( parametros$resultado_tablas, 'iess_pen_', tab_nom[ i ], '_m', '.tex' ),
         type = 'latex',
         include.colnames = FALSE,
         include.rownames = FALSE,
         format.args = list( decimal.mark = ',', big.mark = '.' ),
         only.contents = TRUE,
         hline.after = NULL,
         sanitize.text.function = identity )
}

rm( aux, aux1, aux2, aux3, aux_xtable )

# Edad promedio ------------------------------------------------------------------------------------
aux <- copy( est_pen_anio_tipo_sexo_x[ anio==parametros$anio_ini & !(tipo%in%c('DISCAPACIDAD') ),
                                       list(tipo, sexo, x) ] )
aux1 <- data.table( dcast( aux, tipo ~ sexo, value.var = 'x', fun.aggregate = mean, na.rm = TRUE ) )
aux1[ , tot := c( rowMeans( aux1[, 2:3], na.rm = TRUE ) ) ]
t <- colMeans( aux1[ , 2:4 ] )
aux1 <- rbind( aux1, data.frame( tipo = c('Total') , t(t) ) )

aux <- setnames( aux1, # set column names
                 c('Pensión',
                   'Masculino',
                   'Femenino',
                   'Total') )

aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_edad_promedio_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 4L,
       sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()

