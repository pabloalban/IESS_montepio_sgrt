message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tCálculo de la prima' )

escenarios <- paste0( 'escenario_', 1:9 )

gamma <- 0# porcentaje de V_0 que se incluye en el calculo de la prima media nivelada 

for ( escenario in escenarios ) {
  message( '\tCálculo de la prima para el ', escenario )
  load( paste0( parametros$rtr_rdata_icomp_conf_esc, escenario, '.RData' ) )
  load( paste0( parametros$rtr_rdata_icomp_balance, escenario, '.RData' ) )
  
  prima <- balance_anual[ t > 0,  ]
  
  # Porcentaje de contribución por décimos
  delta <- 0.5
  
  # Prima de reparto puro ----------------------------------------------------------------------------
  prima[ , pri_rep_pur := ( B ) /  M ] # sin aporte estatal
  prima[ , pri_rep_pur_apo_est := ( B + G - A_est ) /  M ] # con aporte estatal AE
  prima[ , pri_rep_pur_apo_est_pen := ( B + G  - A_est - A12 - A15 - A16 ) /  M ] # con aporte estatal AE y aporte de pensionistas
  prima[ , pri_rep_pur_apo_pen := ( B + G  - A12 - A15 - A16 ) / M ] # sin aporte estatal AE y con aporte de pensionistas
  prima[ , pri_rep_pur_delta := ( B + G - delta * B - A_est ) / M ] 
  prima[ , pri_rep_pur_delta_pen := delta * B / M ]
  
  # Prima nivelada en cada horizonte -----------------------------------------------------------------
  prima[ , pri_med_niv := (  B_vap + G_vap - gamma * V0 ) /  M_vap ] # sin aporte estatal
  prima[ , pri_med_niv_apo_est := (  B_vap + G_vap - A_est_vap - gamma * V0 ) /  M_vap ] # con aporte estatal AE
  prima[ , pri_med_niv_apo_est_pen := (  B_vap + G_vap - A_est_vap - A12_vap - A15_vap - A16_vap - gamma * V0 ) / M_vap ] # con aporte estatal AE y aporte de pensionistas
  prima[ , pri_med_niv_apo_pen := (  B_vap + G_vap - A12_vap - A15_vap - A16_vap - gamma * V0 ) / M_vap ] # sin aporte estatal AE y con aporte de pensionistas
  prima[ , pri_med_niv_delta := (  B_vap + G_vap - delta * B_pen_vap - A_est_vap - gamma * V0 ) / M_vap ] 
  prima[ , pri_med_niv_delta_pen := delta * B_pen_vap / M_vap ]
  
  save( prima, 
        file = paste0( parametros$rtr_rdata_icomp_prima, escenario, '.RData' ) )  
}

# Limpiando memoria RAM-----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

