message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tCreación tablas del análisis de sensibilidad' )

# Carga de datos -----------------------------------------------------------------------------------
load( file = parametros$ivm_rdata_ana_sen )

# Sensibilidad de la tasa de descuento--------------------------------------------------------------
message( '\tTabla de sensibilidad de la tasa actuarial' )

aux <- balance_anual_sens[ sens == 1, list( t, sim = as.factor( sim ), V ) ]
aux <- dcast.data.table( data = aux, formula = t ~ sim, value.var = 'V' )

xtb_pri <- xtable( aux, digits = c( 0, 0, rep( 2, ncol( aux ) - 1 ) ) )
print( xtb_pri,
       file = paste0( parametros$resultado_tablas, 'iess_bal_ana_sen_1.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Sensibilidad del crecimiento de salarios----------------------------------------------------------
message( '\tTabla de sensibilidad del crecimiento de salarios' )

aux <- balance_anual_sens[ sens == 2, list( t, sim = as.factor( sim ), V ) ]
aux <- dcast.data.table( data = aux, formula = t ~ sim, value.var = 'V' )

xtb_pri <- xtable( aux, digits = c( 0, 0, rep( 2, ncol( aux ) - 1 ) ) )
print( xtb_pri,
       file = paste0( parametros$resultado_tablas, 'iess_bal_ana_sen_2.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()
