message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tLectura del portafolio de inversiones' )

# Carga de datos -----------------------------------------------------------------------------------
file_inversiones <-
  paste0( parametros$RData_seg, 'BIESS_IVM_inversiones.RData' )
load( file = file_inversiones )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

# Tabla de la evolución del portafolio -------------------------------------------------------------
aux <- recurs_adm_biess %>%
  mutate( 
    ano = as.character( ano ),
    rendimiento_neto = rendimiento_neto * 100,
    rendimiento_ponderado = rendimiento_ponderado * 100,
    rendimiento_neto_real = rendimiento_neto_real * 100,
    rendimiento_ponderado_real  = rendimiento_ponderado_real * 100
  )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 8 ) ) )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'biess_total_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)

# Tabla Resumen situación actual de las inversiones ------------------------------------------------
aux <- inver_corte %>% 
  mutate( rendimiento_promedio = rendimiento_promedio * 100,
          rendimiento_promedio_real = rendimiento_promedio_real * 100,
          participacion = participacion * 100 )

aux_xtab <- xtable( aux, digits = c( 0, 2, 2, 2, 2, 2, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'biess_inv_corte', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtab ) - 1 ),
                     command = c( paste( "\\hline \n" ) ) )
)

# Tabla Rendimientos con ingresos y gastos ---------------------------------------------------------
aux <- rendimientos_netos %>% 
  mutate( rendimiento_bruto = rendimiento_bruto * 100,
          rendimiento_neto = rendimiento_neto * 100,
          corte_a = format( corte_a, "%b/%Y" ) ) %>% 
  dplyr::select( -fondo_administrativos )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 6 ) ) )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'biess_rend_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)

# Tabla del detalle de los ingresos de las inversiones ---------------------------------------------
aux <- ingresos %>%
  replace( is.na( . ), 0 )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 9 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'biess_ingre_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  add.to.row = list( pos = list( nrow( aux_xtab ) - 1 ),
                     command = c( paste( "\\hline \n" ) ) )
)

# Tabla del detalle de los gastos de las inversiones -----------------------------------------------
aux <- gastos_opera %>%
  replace( is.na( . ), 0 )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 9 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'biess_gastos_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity,
  compress = FALSE,
  fileEncoding = "UTF-8",
  add.to.row = list( pos = list( nrow( aux_xtab ) - 1 ),
                     command = c( paste( "\\hline \n" ) ) )
)

# Tabla evolución Inversiones en Créditos ----------------------------------------------------------
aux <- creditos %>%
  mutate( 
    rendimiento = rendimiento * 100,
    rendimiento_ponderado_real = rendimiento_ponderado_real * 100,
    ano = as.character( ano )
  ) %>%
  arrange( ano )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 5), 0 ) )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'biess_creditos_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)

# Tabla evolución Inversiones en Bonos del Estado Ecuatoriano --------------------------------------
aux <- inv_instrumento %>%
  filter( instrumento == 'Bonos del Estado' ) %>%
  na.omit(  ) %>%
  mutate( 
    rendimiento_ponderado = rendimiento_ponderado * 100,
    rendimiento_ponderado_real = rendimiento_ponderado_real * 100,
    ano = as.character( ano )
  ) %>%
  dplyr::select( -inflacion, -instrumento ) %>%
  arrange( ano )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 0 ) )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'biess_bonos_hist_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)


# Tabla detalle Inversiones en Bonos del Estado Ecuatoriano al corte--------------------------------
aux <- detalle_bonos %>%
  filter( tipo_de_papel  == 'BONOS' ) %>%
  dplyr::select( -tipo_de_papel ) %>%
  group_by( plazo_x_vencer, tasa_cupon, amortizacion_capital, observaciones ) %>%
  mutate( 
    valor_nominal_de_compra = sum( valor_nominal_de_compra, na.rm = TRUE ),
    saldo_valor_nominal = sum( saldo_valor_nominal, na.rm =  TRUE )
  ) %>%
  ungroup(  ) %>%
  distinct( plazo_x_vencer, tasa_cupon, amortizacion_capital, observaciones, .keep_all = TRUE ) %>%
  rbind( ., c( "Total",
               '7.91',
               as.character( colSums( .[, 3:4],  na.rm = TRUE ) ),
               '2691.37',
               NA,
               NA,
               NA ) ) %>%
  mutate_at( c( 2:5 ), as.numeric ) %>%
  arrange( fecha_vcmt ) %>%
  mutate( fecha_vcmt = as.character( fecha_vcmt ) ) %>%
  dplyr::select( emisor,
                 saldo_valor_nominal,
                 valor_nominal_de_compra,
                 tasa_cupon,
                 plazo_x_vencer,
                 fecha_vcmt,
                 amortizacion_capital,
                 observaciones )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 3 ), rep( 0, 4 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( 
  aux_xtab,
  file = paste0( 
    parametros$resultado_tablas,
    'biess_bonos_detalle_inv',
    '.tex'
  ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( nrow( aux ) - 1,
                   nrow( aux ) ),
  sanitize.text.function = identity
)

# Tabla recuperación de Bonos del Estado Ecuatoriano al corte---------------------------------------
aux <- recuperacion_bonos %>%
  filter( tipo_bono != 'CTN',
          fecha_cupon > as.Date( "31/12/2020", "%d/%m/%Y" ) ) %>%
  dplyr::select( -tipo_bono ) %>%
  mutate( anio = year( fecha_cupon ) ) %>%
  group_by( anio ) %>%
  mutate( capital = sum( capital, na.rm = TRUE ),
          interes   = sum( interes  , na.rm =  TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( anio, .keep_all = TRUE ) %>%
  mutate( total = capital + interes ) %>%
  dplyr::select( anio,
                 capital,
                 interes,
                 total ) %>%
  rbind( ., c( "Total",
               as.character( colSums( .[, 2:ncol( . )],  na.rm = TRUE ) ) ) ) %>%
  mutate_at( c( 2:ncol( . ) ), as.numeric )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 3 ) ) )

print( 
  aux_xtab,
  file = paste0( 
    parametros$resultado_tablas,
    'biess_bonos_recuperacion_inv',
    '.tex'
  ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( nrow( aux ) - 1,
                   nrow( aux ) ),
  sanitize.text.function = identity
)

# Tabla Recepción de Bonos del Estado por el 40% del pago de las pensiones--------------------------
aux <- detalle_bonos_40 %>% 
  mutate( tasa = tasa * 100 )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 0, 0 ) )

print( 
  aux_xtab,
  file = paste0( 
    parametros$resultado_tablas,
    'biess_repbonos40_hist_inv',
    '.tex'
  ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)

# Tabla evolución Inversiones en CETES--------------------------------------------------------------
aux <- inv_instrumento %>%
  filter( instrumento == 'CETES' ) %>%
  na.omit(  ) %>%
  mutate( 
    rendimiento_ponderado = rendimiento_ponderado * 100,
    rendimiento_ponderado_real = rendimiento_ponderado_real * 100,
    ano = as.character( ano )
  ) %>%
  dplyr::select( -inflacion, -instrumento ) %>%
  arrange( ano ) %>% 
  filter( saldo > 0)


aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 0 ) )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'biess_cetes_hist_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)


# Tabla Evolución Inversiones en Obligaciones-------------------------------------------------------
aux <- inv_instrumento %>%
  filter( instrumento == 'Obligaciones' ) %>%
  na.omit(  ) %>%
  mutate( 
    rendimiento_ponderado = rendimiento_ponderado * 100,
    rendimiento_ponderado_real = rendimiento_ponderado_real * 100,
    ano = as.character( ano )
  ) %>%
  dplyr::select( -inflacion, -instrumento ) %>%
  arrange( ano ) 

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 0 ) )

print( 
  aux_xtab,
  file = paste0( 
    parametros$resultado_tablas,
    'biess_obligaciones_hist_inv',
    '.tex'
  ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)

# Tabla detalle de las inversiones en obligaciones--------------------------------------------------
aux <- detalle_obligaciones %>%
  rbind( ., c( "Total",
               '8.84',
               as.character( colSums( .[, 3:4],  na.rm = TRUE ) ),
               '1325.19',
               NA ) ) %>%
  mutate_at( c( 2:5 ), as.numeric )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 4 ), 0 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( 
  aux_xtab,
  file = paste0( 
    parametros$resultado_tablas,
    'biess_oblig_detalle_inv',
    '.tex'
  ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( nrow( aux ) - 1,
                   nrow( aux ) ),
  sanitize.text.function = identity
)

# Tabla Evolución Inversiones en Titularizaciones---------------------------------------------------
aux <- inv_instrumento %>%
  filter( instrumento == 'Titularizaciones' ) %>%
  na.omit(  ) %>%
  mutate( 
    rendimiento_ponderado = rendimiento_ponderado * 100,
    rendimiento_ponderado_real = rendimiento_ponderado_real * 100,
    ano = as.character( ano )
  ) %>%
  dplyr::select( -inflacion, -instrumento ) %>%
  arrange( ano ) 


aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 0 ) )

print( 
  aux_xtab,
  file = paste0( 
    parametros$resultado_tablas,
    'biess_titularizaciones_hist_inv',
    '.tex'
  ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)
# Tabla detalle de las inversiones en Titularizaciones----------------------------------------------
aux <- detalle_titularizaciones %>%
  rbind( ., c( "Total",
               '8.82',
               as.character( colSums( .[, 3:4],  na.rm = TRUE ) ),
               '1444.64',
               NA ) ) %>%
  mutate_at( c( 2:5 ), as.numeric )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 4 ), 0 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( 
  aux_xtab,
  file = paste0( 
    parametros$resultado_tablas,
    'biess_titul_detalle_inv',
    '.tex'
  ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( nrow( aux ) - 1,
                   nrow( aux ) ),
  sanitize.text.function = identity
)

# Tabla evolución Inversiones en Fideicomisos--------------------------------------------------------
aux <- inv_instrumento %>%
  filter( instrumento == 'Fideicomisos' ) %>%
  #na.omit(  ) %>%
  mutate( 
    rendimiento_ponderado = rendimiento_ponderado * 100,
    rendimiento_ponderado_real = rendimiento_ponderado_real * 100,
    ano = as.character( ano )
  ) %>%
  dplyr::select( -inflacion, -instrumento, -plazo ) %>%
  arrange( ano )


aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'biess_fideicomisos_hist_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)

# Tabla detalle fideicomisos------------------------------------------------------------------------

aux <- detalle_fidecomisos %>%
  mutate( 
    participacion_ivm = participacion_ivm * 100,
  ) %>%
  filter( valoracion_total_al_corte > 0 ) %>% 
  rbind( ., c( "Total",
               as.character( colSums( .[, 2:7],  na.rm = TRUE ) ) ) ) %>%
  mutate_at( c( 2:7 ), as.numeric ) %>% 
  mutate( participacion_ivm = if_else( fideicomisos == 'Total',
                                       100 * valor_mercado_pertenece_ivm / valoracion_total_al_corte,
                                       participacion_ivm ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 6 ) ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'biess_fideicomisos_detalle_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( nrow( aux ) - 1,
                   nrow( aux ) ),
  sanitize.text.function = identity
)

# Tabla evolución Inversiones en Renta variable-----------------------------------------------------
aux <- inv_instrumento %>%
  filter( instrumento == 'Renta' ) %>%
  #na.omit(  ) %>%
  mutate( 
    rendimiento_ponderado = rendimiento_ponderado * 100,
    rendimiento_ponderado_real = rendimiento_ponderado_real * 100,
    ano = as.character( ano )
  ) %>%
  dplyr::select( -inflacion, -instrumento, -plazo ) %>%
  arrange( ano )


aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'biess_renta_variable_hist_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)

# Tabla detalle renta variable----------------------------------------------------------------------
aux <- detalle_renta_variable %>%
  mutate( 
    rend_anual_total = rend_anual_total * 100,
    biess_percent_capital_social = biess_percent_capital_social * 100
  ) %>% 
  dplyr::select( -precio_anterior,
                 -p_u,
                 -p_vl,
                 -rend_dividendo,
                 -patrimonio )

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 0, rep( 2, 6 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'biess_renta_variable_detalle_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( nrow( aux ) - 1,
                   nrow( aux ) ),
  sanitize.text.function = identity
)

# Borrando data.frames------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc(  )

