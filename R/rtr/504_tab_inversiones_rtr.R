message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tLectura del portafolio de inversiones' )

# Carga de datos -----------------------------------------------------------------------------------
file_inversiones <-
  paste0( parametros$RData_seg, 'BIESS_RTR_inversiones.RData' )
load( file = file_inversiones )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

# Tabla de la evolución del portafolio -------------------------------------------------------------
aux <- recurs_adm_biess %>%
  mutate( 
    ano = as.character( ano ),
    rendimiento_neto = rendimiento_neto * 100,
    rendimiento_ponderado = rendimiento_ponderado * 100,
    rendimiento_neto_real = rendimiento_neto_real * 100
  )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 6 ), 0 ) )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'iess_total_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)

#Tabla Resumen situación actual de las inversiones--------------------------------------------------
aux <- inver_corte %>% 
  mutate( rendimiento_promedio = rendimiento_promedio * 100,
          rendimiento_promedio_real = rendimiento_promedio_real * 100,
          participacion = participacion * 100 )

aux_xtab <- xtable( aux, digits = c( 0, 2, 2, 2, 2, 2, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'iess_inv_corte', '.tex' ),
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
  mutate( corte_a = as.character( corte_a ) )

aux_xtab <- xtable( aux, digits = c( 0, 2, 2, 2, 2, 2, 2, 2 ) )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'iess_rend_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)

# Tabla del detalle de los ingresos que producieron las inversiones --------------------------------
aux <- ingresos %>%
  replace( is.na( . ), 0 )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 9 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'iess_ingre_inv', '.tex' ),
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

# Tabla del detalle de los gastos que producieron las inversiones ----------------------------------
aux <- gastos_opera %>%
  replace( is.na( . ), 0 )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 9 ) ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'iess_gastos_inv', '.tex' ),
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
aux <- inv_instrumento %>%
  filter( instrumento == 'Créditos Quirografarios' ) %>%
  na.omit(  ) %>%
  mutate( 
    rdto_prom_pond = rdto_prom_pond * 100,
    rend_promedio_real = rend_promedio_real * 100,
    ano = as.character( ano )
  ) %>%
  dplyr::select( -inflacion, -instrumento ) %>%
  arrange( ano )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 0 ) )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'iess_creditos_inv', '.tex' ),
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
    rdto_prom_pond = rdto_prom_pond * 100,
    rend_promedio_real = rend_promedio_real * 100,
    ano = as.character( ano )
  ) %>%
  dplyr::select( -inflacion, -instrumento ) %>%
  arrange( ano )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 0 ) )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'iess_bonos_hist_inv', '.tex' ),
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
  group_by( plazo_x_vencer, tasa_cupon, amortizacion_capital ) %>%
  mutate( 
    valor_nominal_de_compra = sum( valor_nominal_de_compra, na.rm = TRUE ),
    saldo_valor_nominal = sum( saldo_valor_nominal, na.rm =  TRUE )
  ) %>%
  ungroup(  ) %>%
  distinct( plazo_x_vencer, tasa_cupon, amortizacion_capital, .keep_all = TRUE ) %>%
  rbind( ., c( "Total",
               '6.93',
               as.character( colSums( .[, 3:4],  na.rm = TRUE ) ),
               '1315.19',
               NA,
               NA,
               NA ) ) %>%
  mutate_at( c( 2:5 ), as.numeric ) %>%
  arrange( fecha_vcmt ) %>%
  mutate( fecha_vcmt = as.character( fecha_vcmt ) ) %>%
  dplyr::select( -emisor ) %>% 
  dplyr::select( fecha_vcmt,
                 valor_nominal_de_compra,
                 saldo_valor_nominal,
                 tasa_cupon,
                 plazo_x_vencer,
                 amortizacion_capital,
                 observaciones )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 4 ), 0, 0 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( 
  aux_xtab,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_bonos_detalle_inv',
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
    'iess_bonos_recuperacion_inv',
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

#Tabla Recepción de Bonos del Estado por el 40% del pago de las pensiones---------------------------
aux <- detalle_bonos_40 %>%
  mutate( 
    tasa = tasa * 100,
    fecha_colocacion = as.character( fecha_colocacion ),
    vencimiento = as.character( vencimiento ),
    pago_del_periodo = as.character( format( 
      detalle_bonos_40$pago_del_periodo, "%Y-%B"
    ) )
  )
aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 0, 2, 2, 0 ) )

print( 
  aux_xtab,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_repbonos40_hist_inv',
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
  filter( instrumento == 'Certificados de Tesorería - CETES' ) %>%
  na.omit(  ) %>%
  mutate( 
    rdto_prom_pond = rdto_prom_pond * 100,
    rend_promedio_real = rend_promedio_real * 100,
    ano = as.character( ano )
  ) %>%
  dplyr::select( -inflacion, -instrumento ) %>%
  arrange( ano )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 0 ) )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'iess_cetes_hist_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)

# Tabla detalle Inversiones en CETES al corte-------------------------------------------------------
aux <- detalle_bonos %>%
  filter( tipo_de_papel  == 'CETES' ) %>%
  dplyr::select( -tipo_de_papel ) %>%
  group_by( plazo_x_vencer, tasa_cupon, amortizacion_capital ) %>%
  mutate( 
    valor_nominal_de_compra = sum( valor_nominal_de_compra, na.rm = TRUE ),
    saldo_valor_nominal = sum( saldo_valor_nominal, na.rm =  TRUE )
  ) %>%
  ungroup(  ) %>%
  distinct( plazo_x_vencer, tasa_cupon, amortizacion_capital, .keep_all = TRUE ) %>%
  rbind( ., c( "Total",
               '0',
               as.character( colSums( .[, 3:4],  na.rm = TRUE ) ),
               '22',
               NA,
               NA,
               NA ) ) %>%
  mutate_at( c( 2:5 ), as.numeric ) %>%
  arrange( fecha_vcmt ) %>%
  mutate( fecha_vcmt = as.character( fecha_vcmt ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 4 ), rep( 0, 3 ) ) )

print( 
  aux_xtab,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_cetes_detalle_inv',
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

# Tabla recuperación de CETES al corte--------------------------------------------------------------
aux <- recuperacion_bonos %>%
  filter( tipo_bono == 'CTN' ) %>%
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
    'iess_cetes_recuperacion_inv',
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

#Tabla Evolución Inversiones en Obligaciones--------------------------------------------------------
aux <- inv_instrumento %>%
  filter( instrumento == 'Obligaciones' ) %>%
  na.omit(  ) %>%
  mutate( 
    rdto_prom_pond = rdto_prom_pond * 100,
    rend_promedio_real = rend_promedio_real * 100,
    ano = as.character( ano )
  ) %>%
  dplyr::select( -inflacion,-instrumento ) %>%
  arrange( ano )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 0 ) )

print( 
  aux_xtab,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_obligaciones_hist_inv',
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

#Tabla detalle de las inversiones en obligaciones---------------------------------------------------
aux <- detalle_obligaciones %>%
  rbind( ., c( "Total",
               '8.82',
               as.character( colSums( .[, 3:4],  na.rm = TRUE ) ),
               '1040.97',
               NA ) ) %>%
  mutate_at( c( 2:5 ), as.numeric ) %>%
  dplyr::select( emisor,
                 valor_nominal_de_compra,
                 saldo_valor_nominal,
                 tasa_cupon,
                 plazo_x_vencer,
                 amortizacion_capital )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 4 ), 0 ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( 
  aux_xtab,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_oblig_detalle_inv',
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

#Tabla Evolución Inversiones en Titularizaciones----------------------------------------------------
aux <- inv_instrumento %>%
  filter( instrumento == 'Titularizaciones' ) %>%
  na.omit(  ) %>%
  mutate( 
    rdto_prom_pond = rdto_prom_pond * 100,
    rend_promedio_real = rend_promedio_real * 100,
    ano = as.character( ano )
  ) %>%
  dplyr::select( -inflacion,-instrumento ) %>%
  arrange( ano )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 0 ) )

print( 
  aux_xtab,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_titularizaciones_hist_inv',
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

#Tabla detalle de las inversiones en Titularizaciones-----------------------------------------------
aux <- detalle_titularizaciones %>%
  rbind( ., c( "Total",
               '9.13',
               as.character( colSums( .[, 3:4],  na.rm = TRUE ) ),
               '1156',
               NA ) ) %>%
  mutate_at( c( 2:5 ), as.numeric )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 4 ), 0 ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( 
  aux_xtab,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_titul_detalle_inv',
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

# Tabla evolución inversiones en Papel Comercial----------------------------------------------------
aux <- inv_instrumento %>%
  filter( instrumento == 'Papel Comercial' ) %>%
  na.omit(  ) %>%
  mutate( 
    rdto_prom_pond = rdto_prom_pond * 100,
    rend_promedio_real = rend_promedio_real * 100,
    ano = as.character( ano )
  ) %>%
  dplyr::select( -inflacion,-instrumento ) %>%
  arrange( ano )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 0 ) )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'iess_pc_hist_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)

#Tabla detalle de las inversiones en Papel Comercial------------------------------------------------
aux <- detalle_papel_comercial %>%
  rbind( ., c( "Total",
               '10',
               NA,
               as.character( colSums( .[, 4:5],  na.rm = TRUE ) ),
               '336',
               NA ) ) %>%
  mutate_at( c( 2, 4:6 ), as.numeric )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 0, rep( 2, 3 ), 0 ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'iess_pc_detalle_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( nrow( aux ) - 1,
                   nrow( aux ) ),
  sanitize.text.function = identity
)

# Tabla evolución inversiones en Certificados de Inversión------------------------------------------
aux <- inv_instrumento %>%
  filter( instrumento == 'Certificados de Inversión' ) %>%
  na.omit(  ) %>%
  mutate( 
    rdto_prom_pond = rdto_prom_pond * 100,
    rend_promedio_real = rend_promedio_real * 100,
    ano = as.character( ano )
  ) %>%
  dplyr::select( -inflacion, -instrumento ) %>%
  arrange( ano )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 0 ) )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'iess_cid_hist_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
)

#Tabla detalle de las inversiones en certificados de inversión--------------------------------------
aux <- detalle_certificado_inversiones %>%
  dplyr::select( -tipo_de_papel,-amortizacion_capital ) %>%
  rbind( ., c( "Total",
               '7',
               as.character( colSums( .[, 3:4],  na.rm = TRUE ) ),
               '98' ) ) %>%
  mutate_at( c( 2:5 ), as.numeric )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 4 ) ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'iess_cid_detalle_inv', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( nrow( aux ) - 1,
                   nrow( aux ) ),
  sanitize.text.function = identity
)

# Limpiar Ram---------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% c( 'parametros' ) )] )
gc(  )
