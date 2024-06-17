message( paste( rep( '-', 100 ), collapse = '' ) )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
message( '\tLectura estados financieros' )
load( file = paste0( parametros$RData_seg, 'IESS_IVM_analisis_financiero.RData' ) )

# Gráfico Activos-----------------------------------------------------------------------------------
unidad <- 1e6
aux <- copy(activos_ivm[, .(Anio, Activo)] )
aux[ , Activo := Activo / unidad ]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 4000, 10000 )
y_brk <- seq( y_lim[1], y_lim[2], 1000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_activo_ivm <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = Activo, 
                  color = parametros$iess_blue ), 
             linewidth = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Activo' ) +
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

# iess_activo_ivm
ggsave( plot = iess_activo_ivm, 
        filename = paste0( parametros$resultado_graficos, 'iess_activo_ivm', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Cuentas por cobrar -------------------------------------------------------------------------------
unidad <- 1e6
aux <- copy( ctasxcobrar_ivm[, .(Anio, Cuentas_por_cobrar)] )
aux[ , Cuentas_por_cobrar := Cuentas_por_cobrar / unidad ]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 500, 2500 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cuentas_cobrar_ivm <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = Cuentas_por_cobrar, 
                  color = parametros$iess_blue ), 
             linewidth = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Cuentas por cobrar' ) +
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

# iess_cuentas_cobrar_ivm
ggsave( plot = iess_cuentas_cobrar_ivm, 
        filename = paste0( parametros$resultado_graficos, 'iess_cuentas_cobrar_ivm', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráficos Pasivos----------------------------------------------------------------------------------
unidad <- 1e6
aux <- copy( pasivos_IVM )
aux <- aux[ , .(Anio, Pasivos)]
aux[ , Pasivos := Pasivos / unidad ]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 100, 1200)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pasivo_ivm <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = Pasivos, 
                  color = parametros$iess_blue ), 
             linewidth = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Pasivo' ) +
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

# iess_pasivo_ivm
ggsave( plot = iess_pasivo_ivm, 
        filename = paste0( parametros$resultado_graficos, 'iess_pasivo_ivm', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Cuentas por pagar --------------------------------------------------------------------------------
unidad <- 1e6
aux <- copy( ctasxpagar_IVM[, .(Anio, Cuentas_por_pagar)] )
aux[ , Cuentas_por_pagar := Cuentas_por_pagar / unidad ]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 400 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cuentas_pagar_ivm <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = Cuentas_por_pagar, 
                  color = parametros$iess_blue ), 
             linewidth = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Cuentas por pagar' ) +
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

# iess_cuentas_pagar_ivm
ggsave( plot = iess_cuentas_pagar_ivm, 
        filename = paste0( parametros$resultado_graficos, 'iess_cuentas_pagar_ivm', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráficos Patrimonio ------------------------------------------------------------------------------
unidad <- 1e6
aux <- copy(patrimonio_IVM)
aux <- aux[ , .(Anio, Patrimonio)]
aux[ , Patrimonio := Patrimonio / unidad ]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 3000, 10000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_patrimonio_ivm <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = Patrimonio, 
                  color = parametros$iess_blue ), 
             linewidth = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Patrimonio' ) +
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

# iess_patrimonio_ivm
ggsave( plot = iess_patrimonio_ivm, 
        filename = paste0( parametros$resultado_graficos, 'iess_patrimonio_ivm', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Ingresos -----------------------------------------------------------------------------------------
unidad <- 1e6
aux <- copy(ingresos_IVM [, .(Anio, Ingresos)] )
aux[ , Ingresos:= Ingresos / unidad ]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 1000, 6000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 8 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_ingresos_ivm <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = Ingresos, 
                  color = parametros$iess_blue ), 
             linewidth = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Ingresos' ) +
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

# iess_ingresos_ivm
ggsave( plot = iess_ingresos_ivm, 
        filename = paste0( parametros$resultado_graficos, 'iess_ingresos_ivm', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Ingresos por contribuciones del Estado aportes ---------------------------------------------------
unidad <- 1e6
aux <- copy(contriEstadovsPensPag_IVM[, .( Anio,Contribución_del_Estado) ] )
aux[ , Contribución_del_Estado := Contribución_del_Estado / unidad ]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1900 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_contribucion_estado_ivm <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = Contribución_del_Estado, 
                  color = parametros$iess_blue ), 
             linewidth = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Contribución del Estado' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

# iess_contribucion_estado_ivm
ggsave( plot = iess_contribucion_estado_ivm, 
        filename = paste0( parametros$resultado_graficos, 'iess_contribucion_estado_ivm', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Egresos por pago de pensiones --------------------------------------------------------------------
unidad <- 1e6
aux <- copy( contriEstadovsPensPag_IVM [, .( Anio, Pensiones_Pagadas_de_Invalidez_Vejez_Montepio) ] )
aux[ , Pensiones_Pagadas_de_Invalidez_Vejez_Montepio := Pensiones_Pagadas_de_Invalidez_Vejez_Montepio / unidad ]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 1000, 5000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_egresos_ivm <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = Pensiones_Pagadas_de_Invalidez_Vejez_Montepio, 
                  color = parametros$iess_blue ), 
             linewidth = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Egresos' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

# iess_egresosxpagpensiones_ivm
ggsave( plot = iess_egresos_ivm, 
        filename = paste0( parametros$resultado_graficos, 'iess_egresos_ivm', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Egresos Prestacionales ---------------------------------------------------------------------------
unidad <- 1e6
aux <- copy( egresos_prestacionales)
x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 1000, 5000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_egresos_prestacionales_ivm <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = egresos_prestacionales/1000000, 
                  color = parametros$iess_blue ), 
             linewidth = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Egresos prestacionales' ) +
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

ggsave( plot = iess_egresos_prestacionales_ivm, 
        filename = paste0( parametros$resultado_graficos, 'iess_egresos_prestacionales_ivm', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gastos Administrativos----------------------------------------------------------------------------
unidad <- 1e6
aux <- copy( gastos_administracion)
x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c(4 , 11 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_gastos_administracion_ivm <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = Gastos/10000000, 
                  color = parametros$iess_blue ), 
             linewidth = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Gastos Administrativos' ) +
  scale_color_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

ggsave( plot = iess_gastos_administracion_ivm, 
        filename = paste0( parametros$resultado_graficos, 'iess_gastos_administracion_ivm', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráficos de barras de los porcentajes promedios --------------------------------------------------

# Activos ------------------------------------------------------------------------------------------
aux <- copy(porcent_prom_comp_activos_ivm)

x_lim <- c( 0, 0.9 )
x_brk <- seq( x_lim[1], x_lim[2], length.out = 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

iess_porcent_prom_comp_activos_ivm <- ggplot(aux, aes( x = Porcentaje, y = Cuentas, fill = " ") ) + 
  geom_bar( stat = "identity", width = 0.9 ) + 
  geom_text( aes( label = paste0( round( Porcentaje * 100, 2), "%") ), size = 2.5, position = position_dodge(width = 1), vjust = 0.7 , hjust = 0.01) +
  labs( x = NULL, y = NULL ) +
  scale_x_continuous( breaks = x_brk,labels = scales::percent, limits = x_lim ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                     labels = c( '', '' ) ) + theme_classic() +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 1, size = 7 ) ) +
  theme( axis.text.y = element_text( angle = 0, hjust = 1, size = 7 ) )

# iess_porcent_prom_comp_activos_ivm
ggsave( plot = iess_porcent_prom_comp_activos_ivm, 
        filename = paste0( parametros$resultado_graficos, 'iess_porcent_prom_comp_activos_ivm', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pasivos ------------------------------------------------------------------------------------------
aux <- copy(porcent_prom_comp_pasivos_ivm)

x_lim <- c( 0, 1)
x_brk <- seq( x_lim[1], x_lim[2], length.out = 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

iess_porcent_prom_comp_pasivos_ivm <- ggplot(aux, aes( x = Porcentaje, y = Cuentas, fill = " ") ) + 
  geom_bar( stat = "identity", width = 0.9 ) + 
  geom_text( aes( label = paste0( round( Porcentaje * 100, 2), "%") ), size = 2.5, position = position_dodge(width = 1), vjust = 0.7 , hjust = 0.01) +
  labs( x = NULL, y = NULL ) +
  scale_x_continuous( breaks = x_brk,labels = scales::percent, limits = x_lim ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                     labels = c( '', '' ) ) + theme_classic() +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 1, size = 7 ) ) +
  theme( axis.text.y = element_text( angle = 0, hjust = 1, size = 7 ) )

# iess_porcent_prom_comp_activos_ivm
ggsave( plot = iess_porcent_prom_comp_pasivos_ivm, 
        filename = paste0( parametros$resultado_graficos, 'iess_porcent_prom_comp_pasivos_ivm', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Patrimonio ---------------------------------------------------------------------------------------
aux <- copy(porcent_prom_comp_patrimonio_ivm)

x_lim <- c( 0, 1 )
x_brk <- seq( x_lim[1], x_lim[2], length.out = 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

iess_porcent_prom_comp_patrimonio_ivm <- ggplot(aux, aes( x = Porcentaje, y = Cuentas, fill = " ") ) + 
  geom_bar( stat = "identity", width = 0.9 ) + 
  geom_text( aes( label = paste0( round( Porcentaje * 100, 2), "%") ), size = 2.5, position = position_dodge(width = 1), vjust = 0.7 , hjust = 0.01) +
  labs(  x = NULL, y = NULL ) +
  scale_x_continuous( breaks = x_brk,labels = scales::percent, limits = x_lim ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                     labels = c( '', '' ) ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 1, size = 7 ) ) +
  theme( axis.text.y = element_text( angle = 0, hjust = 1, size = 7 ) )


# iess_porcent_prom_comp_patrimonio_ivm
ggsave( plot = iess_porcent_prom_comp_patrimonio_ivm, 
        filename = paste0( parametros$resultado_graficos, 'iess_porcent_prom_comp_patrimonio_ivm', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Ingresos -----------------------------------------------------------------------------------------
aux <- copy(porcent_prom_comp_ingres_ivm)

x_lim <- c( 0, 0.9)
x_brk <- seq( x_lim[1], x_lim[2], length.out = 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

iess_porcent_prom_comp_ingres_ivm <- ggplot(aux, aes( x = Porcentaje, y = Cuentas, fill = " ") ) + 
  geom_bar( stat = "identity", width = 0.9 ) + 
  geom_text( aes( label = paste0( round( Porcentaje * 100,8), "%") ), size = 2.5, position = position_dodge(width = 1), vjust = 0.7 , hjust = 0.01) +
  labs(  x = NULL, y = NULL ) +
  scale_x_continuous( breaks = x_brk,labels = scales::percent, limits = x_lim ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                     labels = c( '', '' ) ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 1, size = 7 ) ) +
  theme( axis.text.y = element_text( angle = 0, hjust = 1, size = 7 ) )

# iess_porcent_prom_comp_ingres_ivm
ggsave( plot = iess_porcent_prom_comp_ingres_ivm, 
        filename = paste0( parametros$resultado_graficos, 'iess_porcent_prom_comp_ingres_ivm', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Egresos ------------------------------------------------------------------------------------------
aux <- copy(porcent_prom_comp_egres_ivm)

x_lim <- c( 0, 1)
x_brk <- seq( x_lim[1], x_lim[2], length.out = 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

iess_porcent_prom_comp_egres_ivm <- ggplot(aux, aes( x = Porcentaje, y = Cuentas, fill = " ") ) + 
  geom_bar( stat = "identity", width = 0.9 ) + 
  geom_text( aes( label = paste0( round( Porcentaje * 100, 2), "%") ), size = 2, position = position_dodge(width = 1), vjust = 0.7 , hjust = 0.01) +
  labs( x = NULL, y = NULL) +
  scale_x_continuous( breaks = x_brk,labels = scales::percent, limits = x_lim ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ), 
                     labels = c( '', '' ) ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 1, size = 7 ) ) +
  theme( axis.text.y = element_text( angle = 0, hjust = 1, size = 7 ) )

# iess_porcent_prom_comp_egres_ivm
ggsave( plot = iess_porcent_prom_comp_egres_ivm, 
        filename = paste0( parametros$resultado_graficos, 'iess_porcent_prom_comp_egres_ivm', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()

