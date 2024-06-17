message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
# Carga de datos -----------------------------------------------------------------------------------
load( parametros$demo_rdata_rtr_pob_proy )
pob_proy <- pob_proy_rtr[ t <= parametros$horizonte ]

# Población pensionistas incapacidad permanente parcial, total y absoluta---------------------------
message( '\tGraficando proyección pensionistas incapacidad permanente total y absoluta' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 20, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 20 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l_12 ) ]
plt_l12_f <- ggplot() +
            geom_line( data = aux_f, aes( x = x, y = l_12, color = t ), linewidth = graf_line_size ) +
            scale_color_manual( values = cols_graf ) +
            scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
            scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
            xlab(TeX("edad $x$"))+
            ylab(TeX("$mujeres \\, \\, l_{t,1,x}^{12}$")) +
            theme_bw() +
            plt_theme

ggsave( plot = plt_l12_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l12_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

y_lim <- c( 0, 200 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'H', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l_12 ) ]
plt_l12_m <- ggplot() +
            geom_line( data = aux_m, aes( x = x, y = l_12, color = t ), linewidth = graf_line_size ) +
            scale_color_manual( values = cols_graf ) +
            scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
            scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
            xlab(TeX("edad $x$"))+
            ylab(TeX("$hombres \\, \\, l_{t,2,x}^{12}$")) +
            theme_bw() +
            plt_theme

ggsave( plot = plt_l12_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l12_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población indemnizaciones por incapacidad permanente parcial--------------------------------------
message( '\tGraficando proyección indemnizaciones de incapacidad permanente parcial' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 20, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 10 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l_13 ) ]
plt_l13_f <- ggplot() +
            geom_line( data = aux_f, aes( x = x, y = l_13, color = t ), linewidth = graf_line_size ) +
            scale_color_manual( values = cols_graf ) +
            scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
            scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
            xlab(TeX("edad $x$"))+
            ylab(TeX("$mujeres \\, \\, l_{t,1,x}^{13}$")) +
            theme_bw() +
            plt_theme

ggsave( plot = plt_l13_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l13_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

y_lim <- c( 0, 30 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'H', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l_13 ) ]
plt_l13_m <- ggplot() +
            geom_line( data = aux_m, aes( x = x, y = l_13, color = t ), linewidth = graf_line_size ) +
            scale_color_manual( values = cols_graf ) +
            scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
            scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
            xlab(TeX("edad $x$"))+
            ylab(TeX("$hombres \\, \\, l_{t,2,x}^{13}$")) +
            theme_bw() +
            plt_theme

ggsave( plot = plt_l13_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l13_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Población subsidios por incapacidad temporal------------------------------------------------------
message( '\tGraficando proyección de subsidios de incapacidad temporal' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 20, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 300 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l_14 ) ]
plt_l14_f <- ggplot() +
  geom_line( data = aux_f, aes( x = x, y = l_14, color = t ), linewidth = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, l_{t,1,x}^{14}$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l14_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l14_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

y_lim <- c( 0, 1000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'H', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l_14 ) ]
plt_l14_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l_14, color = t ), linewidth = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, l_{t,2,x}^{14}$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l14_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l14_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población pensionistas por orfandad---------------------------------------------------------------
message( '\tGraficando proyección pensionistas por orfandad' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 120 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l_15 ) ]
plt_l15_f <- ggplot() +
  geom_line( data = aux_f, aes( x = x, y = l_15, color = t ), linewidth = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, l_{t,1,x}^{15}$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l15_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l15_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

y_lim <- c( 0, 120 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'H', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l_15 ) ]
plt_l15_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l_15, color = t ), linewidth = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, l_{t,2,x}^{15}$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l15_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l15_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población pensionistas por viudedad---------------------------------------------------------------
message( '\tGraficando proyección pensionistas por viudedad' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 120 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l_16 ) ]
plt_l16_f <- ggplot() +
  geom_line( data = aux_f, aes( x = x, y = l_16, color = t ), linewidth = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, l_{t,1,x}^{16}$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l16_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l16_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

y_lim <- c( 0, 5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'H', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l_16 ) ]
plt_l16_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l_16, color = t ), linewidth = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, l_{t,2,x}^{16}$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l16_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l16_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Agrupar en un solo gráfico-------------------------------------------------------------------------
plt_pob_1 <- marrangeGrob( list( plt_l12_f, plt_l12_m, plt_l13_f, plt_l13_m, plt_l14_f, plt_l14_m ),
                         nrow = 2, ncol = 3, top = '' )

ggsave( plot = plt_pob_1, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_proy_1', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )


plt_pob_2 <- marrangeGrob( list( plt_l15_f, plt_l15_m, plt_l16_f, plt_l16_m ),
                             nrow = 2, ncol = 2, top = '' )

ggsave( plot = plt_pob_2, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_proy_2', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()