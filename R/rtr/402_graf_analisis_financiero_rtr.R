message( paste( rep( '-', 100 ), collapse = '' ) )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_analisis_financiero.RData' ) )

# Parámetros----------------------------------------------------------------------------------------
anio_max <- 2020
anio_min <- 2012
unidad <- 1e6
x_lim <- c( anio_min, anio_max )
x_brk <- anio_min:anio_max

#1. Activo------------------------------------------------------------------------------------------
##Activo Fondo--------------------------------------------------------------------------------------
message( '\tGraficando activo del fondo' )

aux <- activo_del_fondo %>%
  mutate( activo = activo/unidad )

x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 2000 )
y_brk <- seq( y_lim[1], y_lim[2], 200 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_activo_fondo <- ggplot( data = aux ) + 
  geom_line( aes( x = ano, 
                  y = activo, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5 ) )

ggsave( plot = iess_activo_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_activo_fondo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Cuentas por cobrar del Fondo de SGRT -------------------------------------------------------------
message( '\tGraficando Cuentas por cobrar del Fondo de SGRT ' )

aux <- cuentas_cobrar_fondo %>%
  mutate( cuentas_por_cobrar  = cuentas_por_cobrar /unidad )

x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 90)
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cuentas_cobrar_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = ano, 
                  y = cuentas_por_cobrar , 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)' ) +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5 ) )

ggsave( plot = iess_cuentas_cobrar_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_cuentas_cobrar_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#2. Pasivos-----------------------------------------------------------------------------------------
##Pasivo del Fondo de SGRT--------------------------------------------------------------------------
message( '\tGraficando Pasivo del Fondo de SGRT' )

aux <- pasivos_fondo %>%
  mutate( pasivo = pasivo  /unidad )

x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 50)
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pasivo_fondo <-ggplot( data = aux ) + 
  geom_line( aes( x = ano, 
                  y = pasivo, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)' ) +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5  ) )

ggsave( plot = iess_pasivo_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_pasivo_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Cuentas por pagar del  Fondo de SGRT--------------------------------------------------------------
message( '\tGraficando cuentas por pagar del Fondo de SGRT' )

aux <- cuentas_pagar_fondo %>%
  clean_names( ) %>%
  mutate( cuentas_por_pagar = cuentas_por_pagar / unidad )

x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 50)
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cuentas_pagar_fondo <- ggplot( data = aux ) + 
  geom_line( aes( x = ano, 
                  y = cuentas_por_pagar, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5  ) )

ggsave( plot = iess_cuentas_pagar_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_cuentas_pagar_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#3. Patrimonio--------------------------------------------------------------------------------------
##Patrimonio del  Fondo de SFRT---------------------------------------------------------------------
message( '\tGraficando patrimonio del Fondo de.' )

aux <- patrimonio_fondo %>%
  mutate( patrimonio = patrimonio / unidad )

x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1500)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6  )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_patrimonio_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = ano, 
                  y = patrimonio, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green,
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5  ) )

ggsave( plot = iess_patrimonio_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_patrimonio_fondo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#4. Resultados del ejercicio------------------------------------------------------------------------
##Ingresos del  Fondo de SGRT-----------------------------------------------------------------------
message( '\tGraficando ingrensos del Fondo de SGRT' )

aux <- ingresos_fondo %>%
  mutate( ingresos = ingresos / unidad)

x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 300 )
y_brk <- seq( y_lim[1], y_lim[2], 50 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_ingresos_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = ano, 
                  y = ingresos, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5 ) )

ggsave( plot = iess_ingresos_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_ingresos_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Evolución del gasto del Fondo---------------------------------------------------------------------
message( '\tGraficando evolución del gasto del Fondo de SGRT' )

aux <- gastos %>%
  mutate( gastos = gastos / unidad )

x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 100)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_gastos_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = ano, 
                  y = gastos, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green,
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5) )

ggsave( plot = iess_gastos_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_gastos_fondo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Evolución del resultado del ejercicio Fondo de RT-------------------------------------------------
message( '\tGraficando resultado del ejercicio del Fondo de RT.' )

aux <- ingresos_vs_gastos %>%
  clean_names( )

x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 220000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 12 )
y_lbl <- formatC( y_brk / unidad, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_resultado_del_ejercicio  <-  ggplot( data = aux ) + 
  geom_line( aes( x = ano, 
                  y = resultado_del_ejercicio, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green,
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5) )

ggsave( plot = iess_resultado_del_ejercicio, 
        filename = paste0( parametros$resultado_graficos, 'iess_resultado_del_ejercicio_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Limpiando Ram-------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()