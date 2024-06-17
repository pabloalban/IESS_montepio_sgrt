message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tGenerando tablas de parámetros escenarios' )

aux_geom_mean <-
  function( x, t = TRUE ) { 
    if ( t ) {
      n <- 1:length(x)
    } else {
      n <- 0:( length(x) - 1 )
    }
    y <- ( cumprod( x + 1 ) )^( 1 / n ) - 1
    return( last( y ) ) }

escenarios <- paste0( 'escenario_', 1:3 )
nombres <- c( 'Base' )
var_nom <- c( 'Tasa actuarial ($i_a$)',
              'Tasa crecimiento salarios ($i_r$)',
              'Tasa crecimiento SBU ($i_s$)',
              'Tasa crecimiento pensiones ($i_p$)',
              # 'Tasa de aportaci\\\'{o}n de afiliados ($\\pi^{2}$)',
              'Tasa de aportaci\\\'{o}n de pensionistas',
              'Porcentaje aporte estatal ($\\alpha_{est}$)',
              'Porcentaje gasto administrativo' )

aux_tot <- NULL
for ( escenario in escenarios ) { # escenario <- escenarios[1]
  
  load( file = paste0( parametros$rtr_rdata_icomp_conf_esc, escenario, '.RData' ) )
  
  aux <- data.table( nom = var_nom,
                     val = c( aux_geom_mean( esc$apo_act$i_a ), 
                              aux_geom_mean( esc$apo_act$i_r[c(-1)] ), 
                              aux_geom_mean( esc$apo_act$i_sbu[c(-1)] ), 
                              aux_geom_mean( esc$apo_act$i_p[c(-1)] ),
                              # esc$hip_esc$apo_act[2]+esc$hip_esc$apo_act_salud[2], 
                              esc$apo_act$por_apo_pen_incap[2],
                              esc$apo_act$por_apo_est[2],
                              esc$apo_act$por_gast[2] ) )
  aux[ , val := 100 * as.numeric(val) ]
  aux_tot <- cbind( aux_tot, aux[ , list( val ) ] )
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_tab_conf_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = nrow( aux ), 
         sanitize.text.function = identity )
}

setnames( aux_tot, escenarios )
aux_tot[ , nom := var_nom ]
aux_tot <- aux_tot[ , c( 4, 1, 2, 3 ), with = FALSE ]
xtb_aux_tot <- xtable( aux_tot, digits = c( 0, 0, rep( 2, 3 ) ) )

print( xtb_aux_tot,
       file = paste0( parametros$resultado_tablas, 'iess_tab_conf_escenarios.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

# Tabla de las hipótesis utilizadas ----------------------------------------------------------------
# Carga de datos -----------------------------------------------------------------------------------
load( file = parametros$macro_rdata_macro_est )
message( '\tGenerando tablas de la evolución de las hipótesis macroeconómicas' )
# Tabla resumen de hipótesis macro -----------------------------------------------------------------
var_nom <- c( 'Tasa actuarial',
              'Tasa variaci\\\'{o}n PIB', 
              'Tasa pasiva referencial', 
              'Tasa variaci\\\'{o}n salarial', 
              'Tasa variaci\\\'{o}n SBU',
              'Tasa inflaci\\\'{o}n promedio')
aux <- 
  data.table( nom = var_nom,
              val = c( paste0( formatC(esc$apo_act$i_a[2]*100, decimal.mark = ",", format = 'f',
                                       digits = 2), "\\%" ),
                       paste0( formatC(hipotesis$tasas[1], decimal.mark = ",", format = 'f',
                                       digits = 2), "\\%" ),
                       paste0( formatC(hipotesis$tasas[2], decimal.mark = ",", format = 'f',
                                       digits = 2), "\\%" ),
                       paste0( formatC(hipotesis$tasas[3], decimal.mark = ",", format = 'f',
                                       digits = 2), "\\%" ),
                       paste0( formatC(hipotesis$tasas[4], decimal.mark = ",", format = 'f',
                                       digits = 2), "\\%" ),
                       paste0( formatC(hipotesis$tasas[5], decimal.mark = ",", format = 'f',
                                       digits = 2), "\\%" ) 
              ))
xtb_aux <- xtable( aux, digits = c( 0, 0, 2 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_hipotesis_macro.tex' ),
       type = 'latex', 
       include.colnames = FALSE, 
       include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

# Limpieza -----------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()

