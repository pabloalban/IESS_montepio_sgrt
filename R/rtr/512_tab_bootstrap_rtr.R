message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tCreación de las tablas del análisis bootstrap' )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_bootstrap.RData' ) )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#1.Bootstrap de huerfanos---------------------------------------------------------------------------
message( '\tTablas estimación bootstrap huerfanos' )

## 1.1 Tabla estimación de afiliados fallecidos-----------------------------------------------------

aux <- tab_est_beneficarios_of %>% 
  dplyr::select( -beneficiarios )

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, rep( 2, ncol( aux ) - 2 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_est_ben_bootstrap_of_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

## 1.2 Tabla estimación de porcentaje fallecidos-----------------------------------------------------

aux <- tab_est_porcentaje_of

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, rep( 2, ncol( aux ) - 2 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_porc_ben_bootstrap_of_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

#2.Bootstrap de viudas------------------------------------------------------------------------------
message( '\tTablas estimación bootstrap viudas' )

## 2.1 Tabla estimación de afiliados fallecidos-----------------------------------------------------

aux <- tab_est_beneficarios_of %>% 
  dplyr::select( -beneficiarios )

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, rep( 2, ncol( aux ) - 2 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_est_ben_bootstrap_vo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

## 2.2 Tabla estimación de porcentaje fallecidos-----------------------------------------------------

aux <- tab_est_porcentaje_of

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, rep( 2, ncol( aux ) - 2 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_porc_ben_bootstrap_vo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

#Limpiar RAM----------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% c( 'parametros' ) ) ] )
gc( )