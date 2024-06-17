# Análisis de variables globales para calibrar el modelo actuarial con respecto a datos que ya
# fueron observados en la realidad.

rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )

source( paste0( parametros$work_dir, "R/401_graf_plantilla.R" ), encoding = 'UTF-8', echo = FALSE )
graf_width <- 25
graf_height <- 17

# Carga de datos -----------------------------------------------------------------------------------
escenario <- 'escenario_1.RData'
load( parametros$demo_rdata_sgo_pob_proy )
load( parametros$demo_rdata_sgo_est_dem )
load( paste0( parametros$ivm_rdata_icomp_balance, escenario ) )
load( paste0( parametros$ivm_rdata_icomp_ratios, escenario ) )
load( parametros$demo_rdata_sgo_din_dec )
load( parametros$demo_rdata_sgo_tasas_tran )
load( parametros$demo_rdata_inec_pea )
load( parametros$demo_rdata_sgo_pea_proj )

balance_anual$V / 1e6

# Evolución población ------------------------------------------------------------------------------
x_lst <- seq( 0, 105, 1 )
xlim <- c( -1, 105 )
xbrk <- seq( xlim[1], xlim[2], 10 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

s_lst <- seq( 0, 70, 1 )
ylim <- c( -1, 70 )
ybrk <- seq( ylim[1], ylim[2], 10 )
ylab <- formatC( ybrk, digits = 0, format = 'f', big.mark = ',' )

nd <- 6

t <- 1 # tiempo 
i <- 2 # estado
dat <- data.table( t( lm[ t, , , i ] + lh[ t, , , i ] ) )
dat <- as.data.table( cbind( x_lst, dat ) )
setnames( dat, c( 'x', as.character( s_lst ) ) )
dat <- melt.data.table( data = dat, id.vars = 'x', variable.name = 's', value.name = 'l', variable.factor = FALSE )
dat[ , s := as.numeric( s ) ]

plt <- ggplot() +
  geom_tile( data = dat, aes( x, s, fill = l ) ) +
  scale_fill_distiller(palette = "Spectral") +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim, expand = c( 0, 0 ) ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim, expand = c( 0, 0 ) ) +
  labs( title ='Proyección población', x = 'edad', y = 'ts' ) +
  plt_theme_legend
plt

t <- 10
i <- 4
j <- 6
I <- nd * ( i - 1 ) + j
dat <- data.table( t( ltm[ t, , , I ] + lth[ t, , , I ] ) )
dat <- as.data.table( cbind( x_lst, dat ) )
setnames( dat, c( 'x', as.character( s_lst ) ) )
dat <- melt.data.table( data = dat, id.vars = 'x', variable.name = 's', value.name = 'l', variable.factor = FALSE )
dat[ , s := as.numeric( s ) ]

plt <- ggplot() +
  geom_tile( data = dat, aes( x, s, fill = l ) ) +
  scale_fill_distiller(palette = "Spectral") +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim, expand = c( 0, 0 ) ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim, expand = c( 0, 0 ) ) +
  labs( title ='Proyección población', x = 't', y = '' ) +
  plt_theme_legend
plt

aux <- pob_proy_tot[ , list( lces = l2_3 + l2_4 + l2_5 + l2_6 + l3_4 + l3_5 ) ]
aux$lces

# 2 Evolución de activos, proyectado vs estadística ------------------------------------------------
aux1 <- pob_proy_tot[ , list( t = t + parametros$anio_ini, l2 ) ]
aux1[ , diff_l2 := l2 - shift( l2, type = 'lag') ]

aux2 <- est_sal_anio[, list( t = anio, l2_obs = M2 ) ]
aux2[ , diff_l2_obs := l2_obs - shift( l2_obs, type = 'lag') ]
compara <- merge.data.table( aux1, aux2, by = 't', all = TRUE )

xlim <- c( 2012, 2062 )
xbrk <- seq( xlim[1], xlim[2], 10 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 5.5e6 )
ybrk <- seq( ylim[1], ylim[2], 500e3 )
ylab <- formatC( ybrk, digits = 0, format = 'f', big.mark = ',' )

plt <- ggplot() +
  geom_line( data = compara, aes( x = t, y = l2, colour = 'a' ) , linewidth = 0.5 ) +
  geom_line( data = compara, aes( x = t, y = l2_obs, colour = 'b' ), linewidth = 0.5 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'Proyectado', 'Observado' ),
    breaks = c( 'a', 'b' ), 
    values = c( 'darkred', 'orange' ) ) +
  labs( title ='COMPARACION PROYECCION', 
        x = 't', 
        y = '' ) +
  plt_theme_legend

plt

pob_proy_tot$l2
est_sal_anio$l2

diff( pob_proy_tot$l2 )
diff( est_sal_anio$M2 )

# 4 Evolución de pensionistas de vejez, proyectado vs estadística ----------------------------------
aux1 <- pob_proy_tot[ , list( t = t + parametros$anio_ini, l4 ) ]
aux1[ , diff_l4 := l4 - shift( l4, type = 'lag') ]

aux2 <- est_pen_anio_tipo[ tipo == 'VEJEZ' , list( t = anio, l4_obs = l ) ]
aux2[ , diff_l4_obs := l4_obs - shift( l4_obs, type = 'lag') ]
compara <- merge.data.table( aux1, aux2, by = 't', all = TRUE )

xlim <- c( 2012, 2062 )
xbrk <- seq( xlim[1], xlim[2], 10 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 2.5e6 )
ybrk <- seq( ylim[1], ylim[2], 500e3 )
ylab <- formatC( ybrk, digits = 0, format = 'f', big.mark = ',' )

plt <- ggplot() +
  geom_line( data = compara, aes( x = t, y = l4, colour = 'a' ) , linewidth = 0.5 ) +
  geom_line( data = compara, aes( x = t, y = l4_obs, colour = 'b' ), linewidth = 0.5 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'Proyectado', 'Observado' ),
    breaks = c( 'a', 'b' ), 
    values = c( 'darkred', 'orange' ) ) +
  labs( title ='COMPARACION PROYECCION', 
        x = 't', 
        y = '' ) +
  plt_theme_legend

plt

aux <- pob_proy_tot[ , list( lpen = l4 + l5 + l7 + l8 ) ]
aux$lpen
est_pen_anio$l

pob_proy_tot$l4
est_pen_anio_tipo[ tipo == 'VEJEZ' ]$l

diff( pob_proy_tot$l4 )
diff( est_pen_anio_tipo[ tipo == 'VEJEZ' ]$l )

balance_anual$B_pen / 1e6
est_pen_anio$P / 1e6

balance_anual$B4 / 1e6
est_pen_anio_tipo[ tipo == 'VEJEZ' ]$P / 1e6

balance_anual$B5 / 1e6
est_pen_anio_tipo[ tipo %in% c( 'INVALIDEZ', 'DISCAPACIDAD' ), list( P = sum( P ) ), by = list( anio ) ]$P / 1e6

balance_anual$B7 / 1e6
est_pen_anio_tipo[ tipo == 'VIUDEDAD' ]$P / 1e6

balance_anual$B8 / 1e6
est_pen_anio_tipo[ tipo == 'ORFANDAD' ]$P / 1e6

# 5 Evolución de pensionistas de invalidez, proyectado vs estadística ------------------------------
aux1 <- pob_proy_tot[ , list( t = t + parametros$anio_ini, l5 ) ]
aux1[ , diff_l5 := l5 - shift( l5, type = 'lag') ]

aux2 <- est_pen_anio_tipo[ tipo %in% c( 'INVALIDEZ', 'DISCAPACIDAD' ) , list( t = anio, l5_obs = l ) ]
aux2 <- aux2[ , list( l5_obs = sum( l5_obs, na.rm = TRUE ) ), by = list( t ) ]
aux2[ , diff_l5_obs := l5_obs - shift( l5_obs, type = 'lag') ]
compara <- merge.data.table( aux1, aux2, by = 't', all = TRUE )

xlim <- c( 2012, 2062 )
xbrk <- seq( xlim[1], xlim[2], 10 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 250e3 )
ybrk <- seq( ylim[1], ylim[2], 25e3 )
ylab <- formatC( ybrk, digits = 0, format = 'f', big.mark = ',' )

plt <- ggplot() +
  geom_line( data = compara, aes( x = t, y = l5, colour = 'a' ) , linewidth = 0.5 ) +
  geom_line( data = compara, aes( x = t, y = l5_obs, colour = 'b' ), linewidth = 0.5 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'Proyectado', 'Observado' ),
    breaks = c( 'a', 'b' ), 
    values = c( 'darkred', 'orange' ) ) +
  labs( title ='COMPARACION PROYECCION', 
        x = 't', 
        y = '' ) +
  plt_theme_legend

plt

diff( pob_proy_tot$l5 )
diff( est_pen_anio_tipo[ tipo %in% c( 'INVALIDEZ', 'DISCAPACIDAD' ), list( l = sum( l ) ), by = list( anio ) ]$l )

# 7 Evolución de pensionistas de viudedad, proyectado vs estadística -------------------------------
aux1 <- pob_proy_tot[ , list( t = t + parametros$anio_ini, l7 ) ]
aux1[ , diff_l7 := l7 - shift( l7, type = 'lag') ]

aux2 <- est_pen_anio_tipo[ tipo %in% c( 'VIUDEDAD' ) , list( t = anio, l7_obs = l ) ]
aux2[ , diff_l7_obs := l7_obs - shift( l7_obs, type = 'lag') ]
compara <- merge.data.table( aux1, aux2, by = 't', all = TRUE )

xlim <- c( 2012, 2062 )
xbrk <- seq( xlim[1], xlim[2], 10 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 600e3 )
ybrk <- seq( ylim[1], ylim[2], 50e3 )
ylab <- formatC( ybrk, digits = 0, format = 'f', big.mark = ',' )

plt <- ggplot() +
  geom_line( data = compara, aes( x = t, y = l7, colour = 'a' ) , linewidth = 0.5 ) +
  geom_line( data = compara, aes( x = t, y = l7_obs, colour = 'b' ), linewidth = 0.5 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'Proyectado', 'Observado' ),
    breaks = c( 'a', 'b' ), 
    values = c( 'darkred', 'orange' ) ) +
  labs( title ='COMPARACION PROYECCION', 
        x = 't', 
        y = '' ) +
  plt_theme_legend

plt

pob_proy_tot$l7
est_pen_anio_tipo[ tipo %in% c( 'VIUDEDAD' ), list( l = sum( l ) ), by = list( anio ) ]$l

diff( pob_proy_tot$l7 )
diff( est_pen_anio_tipo[ tipo %in% c( 'VIUDEDAD' ), list( l = sum( l ) ), by = list( anio ) ]$l )

# 8 Evolución de pensionistas de orfandad, proyectado vs estadística -------------------------------
aux1 <- pob_proy_tot[ , list( t = t + parametros$anio_ini, l8 ) ]
aux1[ , diff_l8 := l8 - shift( l8, type = 'lag') ]

aux2 <- est_pen_anio_tipo[ tipo %in% c( 'ORFANDAD' ) , list( t = anio, l8_obs = l ) ]
aux2[ , diff_l8_obs := l8_obs - shift( l8_obs, type = 'lag') ]
compara <- merge.data.table( aux1, aux2, by = 't', all = TRUE )

xlim <- c( 2012, 2062 )
xbrk <- seq( xlim[1], xlim[2], 10 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 100e3 )

ybrk <- seq( ylim[1], ylim[2], 10e3 )
ylab <- formatC( ybrk, digits = 0, format = 'f', big.mark = ',' )

plt <- ggplot() +
  geom_line( data = compara, aes( x = t, y = l8, colour = 'a' ) , linewidth = 0.5 ) +
  geom_line( data = compara, aes( x = t, y = l8_obs, colour = 'b' ), linewidth = 0.5 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'Proyectado', 'Observado' ),
    breaks = c( 'a', 'b' ), 
    values = c( 'darkred', 'orange' ) ) +
  labs( title ='COMPARACION PROYECCION', 
        x = 't', 
        y = '' ) +
  plt_theme_legend

plt

pob_proy_tot$l8
est_pen_anio_tipo[ tipo %in% c( 'ORFANDAD' ), list( l = sum( l ) ), by = list( anio ) ]$l

diff( pob_proy_tot$l8 )
diff( est_pen_anio_tipo[ tipo %in% c( 'ORFANDAD' ), list( l = sum( l ) ), by = list( anio ) ]$l )

# ( 4 -> 6 ) Evolución de muertes de pensionistas de vejez, proyectado vs estadística --------------
aux1 <- pob_proy_tot[ t > 0, list( t = t + parametros$anio_ini, l4_6 ) ]
aux1[ , diff_l4_6 := l4_6 - shift( l4_6, type = 'lag') ]

aux2 <- est_pen_anio_tipo[ tipo %in% c( 'VEJEZ' ) , list( t = anio, l4_6_obs = N_dec ) ]
aux2[ , diff_l4_6_obs := l4_6_obs - shift( l4_6_obs, type = 'lag') ]
compara_2 <- merge.data.table( aux1, aux2, by = 't', all = TRUE )

xlim <- c( 2012, 2062 )
xbrk <- seq( xlim[1], xlim[2], 10 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 80e3 )
ybrk <- seq( ylim[1], ylim[2], 10e3 )
ylab <- formatC( ybrk, digits = 0, format = 'f', big.mark = ',' )

plt <- ggplot() +
  geom_line( data = compara_2, aes( x = t, y = l4_6, colour = 'a' ) , linewidth = 0.5 ) +
  geom_line( data = compara_2, aes( x = t, y = l4_6_obs, colour = 'b' ), linewidth = 0.5 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'Proyectado', 'Observado' ),
    breaks = c( 'a', 'b' ), 
    values = c( 'darkred', 'orange' ) ) +
  labs( title ='COMPARACION PROYECCION', 
        x = 't', 
        y = '' ) +
  plt_theme_legend

plt

pob_proy_tot$l4_6
est_pen_anio_tipo[ tipo %in% c( 'VEJEZ' ), list( N_dec = sum( N_dec ) ), by = list( anio ) ]$N_dec

pob_proy_tot$l2_4
est_sal_anio$N_vej


# Evolución de muertes de pensionistas de invalidez, proyectado vs estadística ---------------------
aux1 <- pob_proy_tot[ t > 0, list( t = t + parametros$anio_ini, l5_6 ) ]
aux1[ , diff_l5_6 := l5_6 - shift( l5_6, type = 'lag') ]

aux2 <- est_pen_anio_tipo[ tipo %in% c( 'INVALIDEZ', 'DISCAPACIDAD' ) , list( t = anio, l5_6_obs = N_dec ) ]
aux2 <- aux2[ , list( l5_6_obs = sum( l5_6_obs, na.rm = TRUE ) ), by = list( t ) ]
aux2[ , diff_l5_6_obs := l5_6_obs - shift( l5_6_obs, type = 'lag') ]
compara <- merge.data.table( aux1, aux2, by = 't', all = TRUE )

xlim <- c( 2012, 2062 )
xbrk <- seq( xlim[1], xlim[2], 10 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 8e3 )
ybrk <- seq( ylim[1], ylim[2], 500 )
ylab <- formatC( ybrk, digits = 0, format = 'f', big.mark = ',' )

plt <- ggplot() +
  geom_line( data = compara, aes( x = t, y = l5_6, colour = 'a' ) , linewidth = 0.5 ) +
  geom_line( data = compara, aes( x = t, y = l5_6_obs, colour = 'b' ), linewidth = 0.5 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'Proyectado', 'Observado' ),
    breaks = c( 'a', 'b' ), 
    values = c( 'darkred', 'orange' ) ) +
  labs( title ='COMPARACION PROYECCION', 
        x = 't', 
        y = '' ) +
  plt_theme_legend

plt

pob_proy_tot$l5_6
est_pen_anio_tipo[ tipo %in% c( 'INVALIDEZ', 'DISCAPACIDAD' ), list( N_dec = sum( N_dec ) ), by = list( anio ) ]$N_dec

# Evolución de salida de pensionistas de viudedad, proyectado vs estadística -----------------------
aux1 <- pob_proy_tot[ t > 0, list( t = t + parametros$anio_ini, l7_0 ) ]
aux1[ , diff_l7_0 := l7_0 - shift( l7_0, type = 'lag') ]

aux2 <- est_pen_anio_tipo[ tipo %in% c( 'VIUDEDAD' ) , list( t = anio, l7_0_obs = N_sal ) ]
aux2[ , diff_l7_0_obs := l7_0_obs - shift( l7_0_obs, type = 'lag') ]
compara <- merge.data.table( aux1, aux2, by = 't', all = TRUE )

xlim <- c( 2012, 2062 )
xbrk <- seq( xlim[1], xlim[2], 10 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 40e3 )
ybrk <- seq( ylim[1], ylim[2], 5000 )
ylab <- formatC( ybrk, digits = 0, format = 'f', big.mark = ',' )

plt <- ggplot() +
  geom_line( data = compara, aes( x = t, y = l7_0, colour = 'a' ) , linewidth = 0.5 ) +
  geom_line( data = compara, aes( x = t, y = l7_0_obs, colour = 'b' ), linewidth = 0.5 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'Proyectado', 'Observado' ),
    breaks = c( 'a', 'b' ), 
    values = c( 'darkred', 'orange' ) ) +
  labs( title ='COMPARACION PROYECCION', 
        x = 't', 
        y = '' ) +
  plt_theme_legend

plt

pob_proy_tot$l7_0
est_pen_anio_tipo[ tipo %in% c( 'VIUDEDAD' ), list( N_sal = sum( N_sal ) ), by = list( anio ) ]$N_sal

# Evolución de salida de pensionistas de orfandad, proyectado vs estadística -----------------------
aux1 <- pob_proy_tot[ t > 0, list( t = t + parametros$anio_ini, l8_0 ) ]
aux1[ , diff_l8_0 := l8_0 - shift( l8_0, type = 'lag') ]

aux2 <- est_pen_anio_tipo[ tipo %in% c( 'ORFANDAD' ) , list( t = anio, l8_0_obs = N_sal ) ]
aux2[ , diff_l8_0_obs := l8_0_obs - shift( l8_0_obs, type = 'lag') ]
compara <- merge.data.table( aux1, aux2, by = 't', all = TRUE )

xlim <- c( 2012, 2062 )
xbrk <- seq( xlim[1], xlim[2], 10 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 8e3 )
ybrk <- seq( ylim[1], ylim[2], 500 )
ylab <- formatC( ybrk, digits = 0, format = 'f', big.mark = ',' )

plt <- ggplot() +
  geom_line( data = compara, aes( x = t, y = l8_0, colour = 'a' ) , linewidth = 0.5 ) +
  geom_line( data = compara, aes( x = t, y = l8_0_obs, colour = 'b' ), linewidth = 0.5 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'Proyectado', 'Observado' ),
    breaks = c( 'a', 'b' ), 
    values = c( 'darkred', 'orange' ) ) +
  labs( title ='COMPARACION PROYECCION', 
        x = 't', 
        y = '' ) +
  plt_theme_legend

plt

pob_proy_tot$l8_0
est_pen_anio_tipo[ tipo %in% c( 'ORFANDAD' ), list( N_sal = sum( N_sal ) ), by = list( anio ) ]$N_sal


# Evolución de salida de activos a cesantes, proyectado vs estadística -----------------------------
aux1 <- pob_proy_tot[ t > 0 , list( t = t + parametros$anio_ini, l2_3 ) ]
aux1[ , diff_l2_3 := l2_3 - shift( l2_3, type = 'lag') ]

# aux2 <- est_sal_anio[, list( t = anio, l2_3_obs = l2_3 ) ]  # no hay estadísticas
# aux2[ , diff_l2_3_obs := l2_3_obs - shift( l2_3_obs, type = 'lag') ]

compara <- aux1 #merge.data.table( aux1, aux2, by = 't', all = TRUE )

xlim <- c( 2012, 2062 )
xbrk <- seq( xlim[1], xlim[2], 10 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 200e3 )
ybrk <- seq( ylim[1], ylim[2], 20e3 )
ylab <- formatC( ybrk, digits = 0, format = 'f', big.mark = ',' )

plt <- ggplot() +
  geom_line( data = compara, aes( x = t, y = l2_3, colour = 'a' ) , linewidth = 0.5 ) +
  # geom_line( data = compara, aes( x = t, y = l2_3_obs, colour = 'b' ), linewidth = 0.5 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'Proyectado', 'Observado' ),
    breaks = c( 'a', 'b' ), 
    values = c( 'darkred', 'orange' ) ) +
  labs( title ='COMPARACION PROYECCION', 
        x = 't', 
        y = '' ) +
  plt_theme_legend

plt

diff( pob_proy_tot$l2_3 )
diff( est_sal_anio$l2_3 )


# Evolución masa de pensiones, proyectado vs estadística -------------------------------------------
aux1 <- balance_anual[ t > 0, list( t = t + parametros$anio_ini, B_pen ) ]
aux1[ , diff_B_pen := B_pen - shift( B_pen, type = 'lag') ]

aux2 <- est_pen_anio[ , list( t = anio, B_pen_obs = P ) ]
aux2[ , diff_B_pen_obs := B_pen_obs - shift( B_pen_obs, type = 'lag') ]
compara <- merge.data.table( aux1, aux2, by = 't', all = TRUE )

xlim <- c( 2012, 2062 )
xbrk <- seq( xlim[1], xlim[2], 10 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 5e10 )
ybrk <- seq( ylim[1], ylim[2], 5e9 )
ylab <- formatC( ybrk, digits = 0, format = 'f', big.mark = ',' )

plt <- ggplot() +
  geom_line( data = compara, aes( x = t, y = B_pen, colour = 'a' ) , linewidth = 0.5 ) +
  geom_line( data = compara, aes( x = t, y = B_pen_obs, colour = 'b' ), linewidth = 0.5 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'Proyectado', 'Observado' ),
    breaks = c( 'a', 'b' ), 
    values = c( 'darkred', 'orange' ) ) +
  labs( title ='COMPARACION PROYECCION', 
        x = 't', 
        y = '' ) +
  plt_theme_legend

plt

balance_anual$B_pen / 1e6
est_pen_anio$P / 1e6

diff( balance_anual$B_pen ) / 1e6
diff( est_pen_anio$P ) / 1e6

balance_anual$A2_vap / 1e6

balance_anual$A_est_vap / 1e6

balance_anual$B4 / 1e6
balance_anual$B4_vap / 1e6
est_pen_anio_tipo[ tipo == 'VEJEZ']$P / 1e6

balance_anual$B5 / 1e6
balance_anual$B5_vap / 1e6
est_pen_anio_tipo[ tipo %in% c('INVALIDEZ', 'DISCAPACIDAD' ), list( P = sum( P, na.rm = TRUE ) ), by = list( anio ) ]$P / 1e6

balance_anual$B7 / 1e6
balance_anual$B7_vap / 1e6
diff(balance_anual$B7 / 1e6)
est_pen_anio_tipo[ tipo %in% c( 'VIUDEDAD' ), list( P = sum( P, na.rm = TRUE ) ), by = list( anio ) ]$P / 1e6
diff(est_pen_anio_tipo[ tipo %in% c( 'VIUDEDAD' ), list( P = sum( P, na.rm = TRUE ) ), by = list( anio ) ]$P / 1e6)

balance_anual$B7 / pob_proy_tot$l7 
est_pen_anio_tipo[ tipo %in% c( 'VIUDEDAD' ), list( P = sum( P, na.rm = TRUE ) ), by = list( anio ) ]$P /
est_pen_anio_tipo[ tipo %in% c( 'VIUDEDAD' ), list( l = sum( l, na.rm = TRUE ) ), by = list( anio ) ]$l

balance_anual$B8 / 1e6
balance_anual$B8_vap / 1e6
diff(balance_anual$B8 / 1e6)
est_pen_anio_tipo[ tipo %in% c( 'ORFANDAD' ), list( P = sum( P, na.rm = TRUE ) ), by = list( anio ) ]$P / 1e6
diff(est_pen_anio_tipo[ tipo %in% c( 'ORFANDAD' ), list( P = sum( P, na.rm = TRUE ) ), by = list( anio ) ]$P / 1e6 )

balance_anual$B_aux / 1e6
balance_anual$B_aux_vap / 1e6

# Evolución masa salarial, proyectado vs estadística -----------------------------------------------
aux1 <- balance_anual[ t > 0, list( t = t + parametros$anio_ini, M ) ]
aux1[ , diff_M := M - shift( M, type = 'lag') ]
aux1[ , var_M := M / shift( M, type = 'lag') -1 ]

aux2 <- est_sal_anio[ , list( t = anio, M_obs = S ) ]
aux2[ , diff_M_obs := M_obs - shift( M_obs, type = 'lag') ]
aux2[ , var_M_obs := M_obs / shift( M_obs, type = 'lag') - 1 ]
compara <- merge.data.table( aux1, aux2, by = 't', all = TRUE )

xlim <- c( 2012, 2062 )
xbrk <- seq( xlim[1], xlim[2], 10 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 150e9 )
ybrk <- seq( ylim[1], ylim[2], 10e9 )
ylab <- formatC( ybrk, digits = 0, format = 'f', big.mark = ',' )

plt <- ggplot() +
  geom_line( data = compara, aes( x = t, y = M, colour = 'a' ) , linewidth = 0.5 ) +
  geom_line( data = compara, aes( x = t, y = M_obs, colour = 'b' ), linewidth = 0.5 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'Proyectado', 'Observado' ),
    breaks = c( 'a', 'b' ), 
    values = c( 'darkred', 'orange' ) ) +
  labs( title ='COMPARACION PROYECCION', 
        x = 't', 
        y = '' ) +
  plt_theme_legend

plt

balance_anual$M / 1e6
est_sal_anio$S / 1e6

diff( balance_anual$M ) / 1e6
diff( est_sal_anio$S ) / 1e6

balance_anual$A2_vap / 1e6
balance_anual$A_est_vap / 1e6

# Salario promedio del año -------------------------------------------------------------------------
sal_prom <- est_sal_anio[ , list( anio, sal_prom = S / l2 / 12 ) ]
aux <- balance_anual[ , list( t, masa_sal = M / 12 ) ]
sal_prom_proy <- merge.data.table( aux, pob_proy_tot[ , list( t, l2 ) ], by = 't', all.x = TRUE )
sal_prom_proy[ , sal_prom_proy :=  masa_sal / l2 ]
sal_prom_proy[ , masa_sal := NULL ]
sal_prom_proy[ , l2 := NULL ]
sal_prom_proy[ , anio := t + 2020 ]
sal_prom_proy[ , t := NULL ]

sal_prom <- merge.data.table( sal_prom, sal_prom_proy, by = 'anio', all = TRUE )
sal_prom[ ,  vari_obs := sal_prom / shift( sal_prom, type = 'lag', fill = NA) -1 ]
sal_prom[ ,  vari_proy := sal_prom_proy / shift( sal_prom_proy, type = 'lag', fill = NA) -1 ]
mean( sal_prom$vari_obs, na.rm = TRUE )

# Límites
unidad <- 1
xlim <- c( 2010, 2065 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 2000 )
ybrk <- seq( ylim[1], ylim[2], 200 )
ylab <- formatC( ybrk, digits = 0, big.mark = ',', format = 'f' )

plt <- ggplot() +
  geom_line( data = sal_prom, aes( x = anio, y = sal_prom / unidad, colour = 'a' ), linewidth = 1 ) +
  geom_line( data = sal_prom, aes( x = anio, y = sal_prom_proy / unidad, colour = 'b' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual(
    labels = c( 'Observado', 'Proyectado' ),
    breaks = c( 'a', 'b' ),
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='Salario promedio', x = 'Año',
        subtitle = 'Salario mensual en USD ',
        y = 'Salarios' ) +
  plt_theme_legend

plt

balance_anual$M / pob_proy_tot$l2 / 12

balance_anual$B_pen / (pob_proy_tot$l4 + pob_proy_tot$l5 + pob_proy_tot$l7 + pob_proy_tot$l8 ) / 12

100 * ( balance_anual$B_pen / (pob_proy_tot$l4 + pob_proy_tot$l5 + pob_proy_tot$l7 + pob_proy_tot$l8 ) / 12 ) /
( balance_anual$M / pob_proy_tot$l2 / 12 )

( 1 / esc$apo_act$cal_mas ) * balance_anual$M / pob_proy_tot$l2 / 12

# Pensión promedio del año -------------------------------------------------------------------------

pen_prom <- est_pen_anio[ , list( anio, pen_prom = P / l / 12) ]
aux <- balance_anual[ , list( t, masa_pen = B_pen ) ]
pen_prom_proy <- merge.data.table( aux, pob_proy_tot[ , list( t, l = l4 + l5 + l7 + l8 ) ], by = 't', all.x = TRUE )
pen_prom_proy[ , pen_prom_proy :=  masa_pen / l / 12 ]
pen_prom_proy[ , masa_pen := NULL ]
pen_prom_proy[ , l := NULL ]
pen_prom_proy[ , anio := t + 2020 ]
pen_prom_proy[ , t := NULL ]
pen_prom_proy[ pen_prom_proy == 0, pen_prom_proy := NA ]

pen_prom <- merge.data.table( pen_prom, pen_prom_proy, by = 'anio', all = TRUE )
pen_prom[ ,  vari_obs := pen_prom / shift( pen_prom, type = 'lag', fill = NA) -1 ]
pen_prom[ ,  vari_proy := pen_prom_proy / shift( pen_prom_proy, type = 'lag', fill = NA) -1 ]
mean( pen_prom$vari_obs, na.rm = TRUE )

# Límites
unidad <- 1
xlim <- c( 2010, 2065 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 1500 )
ybrk <- seq( ylim[1], ylim[2], 150 )
ylab <- formatC( ybrk, digits = 0, big.mark = ',', format = 'f' )

plt <- ggplot() +
  geom_line( data = pen_prom, aes( x = anio, y = pen_prom / unidad, colour = 'a' ), linewidth = 1 ) +
  geom_line( data = pen_prom, aes( x = anio, y = pen_prom_proy / unidad, colour = 'b' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual(
    labels = c( 'Observado', 'Proyectado' ),
    breaks = c( 'a', 'b' ),
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='Pensión promedio', x = 'Año',
        subtitle = 'Pensión mensual en USD ',
        y = 'Pension' ) +
  plt_theme_legend

plt

ratios$pen_mean
balance_anual$B4 / pob_proy_tot$l4 /12
est_pen_anio_tipo[ tipo == 'VEJEZ' ]$EPm

# número promedio de activos -----------------------------------------------------------------------
num_activos <- est_sal_anio[ , list( anio, num_activos = l2 ) ]
num_activos_proy <- pob_proy_tot[ , list( anio = t + 2020, num_activos_proy = l2 ) ]
num_activos <- merge.data.table( num_activos, num_activos_proy, by = 'anio', all = TRUE )
num_activos[ ,  vari_obs := num_activos / shift( num_activos, type = 'lag', fill = NA) -1 ]
num_activos[ ,  vari_proy := num_activos_proy / shift( num_activos_proy, type = 'lag', fill = NA) -1 ]
mean( num_activos$vari_obs, na.rm = TRUE )

# Límites
unidad <- 1e6
xlim <- c( 2010, 2065 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 7 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 0, big.mark = ',', format = 'f' )

plt <- ggplot() +
  geom_line( data = num_activos, aes( x = anio, y = num_activos / unidad, colour = 'a' ), linewidth = 1 ) +
  geom_line( data = num_activos, aes( x = anio, y = num_activos_proy / unidad, colour = 'b' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual(
    labels = c( 'Observado', 'Proyectado' ),
    breaks = c( 'a', 'b' ),
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='Población activa', x = 'Año',
        subtitle = 'Millones de personas',
        y = 'Activos' ) +
  plt_theme_legend

plt

# número promedio de pensionistas (vejez + invalidez + montepío) -----------------------------------
num_pensionistas <- est_pen_anio[ , list( anio, num_pensionistas = l ) ]
num_pensionistas_proy <- pob_proy_tot[ , list( anio = t + 2020, num_pensionistas_proy = l4 + l5 + l7 + l8 ) ]
num_pensionistas <- merge.data.table( num_pensionistas, num_pensionistas_proy, by = 'anio', all = TRUE )
num_pensionistas[ ,  vari_obs := num_pensionistas / shift( num_pensionistas, type = 'lag', fill = NA) -1 ]
num_pensionistas[ ,  vari_proy := num_pensionistas_proy / shift( num_pensionistas_proy, type = 'lag', fill = NA) -1 ]
mean( num_pensionistas$vari_obs, na.rm = TRUE )

# Límites
unidad <- 1e6
xlim <- c( 2010, 2065 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 4 )
ybrk <- seq( ylim[1], ylim[2], 0.5 )
ylab <- formatC( ybrk, digits = 2, big.mark = ',', format = 'f' )

plt <- ggplot() +
  geom_line( data = num_pensionistas, aes( x = anio, y = num_pensionistas / unidad, colour = 'a' ), linewidth = 1 ) +
  geom_line( data = num_pensionistas, aes( x = anio, y = num_pensionistas_proy / unidad, colour = 'b' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual(
    labels = c( 'Observado', 'Proyectado' ),
    breaks = c( 'a', 'b' ),
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='Población de pensionistas', x = 'Año',
        subtitle = 'Millones de personas',
        y = 'Pensionistas' ) +
  plt_theme_legend

plt


# Verificación de la PEA ---------------------------------------------------------------------------
pea <- pob_proy_tot[ , list( pea = l1 + l2 + l3 ) ]
print( pea )
pob_proy_tot$l1

pea_onu <- PEA_proy[ , list( pea_onu = sum( peax, na.rm = TRUE ) ), by = list( anio ) ]

act_pea <- PEA_inec[ tipo == 'observado', list( anio, pea ) ]
aux <- est_sal_anio[ , list( anio, num_activos = l2 ) ]
act_pea <- merge.data.table( act_pea, aux, by = 'anio', all = TRUE )
act_pea <- act_pea[ !is.na( num_activos ) ]
act_pea[ , act_pea := num_activos / pea ]
rm( aux )

act_pea_proy <- PEA_inec[ anio >= 2020, list( anio, pea_proy = pea ) ]
aux <- pob_proy_tot[ , list( anio = t + 2020, num_activos_proy = l2 ) ]
act_pea_proy <- merge.data.table( act_pea_proy, aux, by = 'anio', all = TRUE )
act_pea_proy[ , act_pea_proy := num_activos_proy / pea_proy ]
act_pea <- merge.data.table( act_pea, act_pea_proy, by = 'anio', all = TRUE )
act_pea <- merge.data.table( act_pea, pea_onu, by = 'anio', all.x = TRUE )

aux <- ratios[ , list( anio = t + 2020, lpea ) ]
act_pea <- merge.data.table( act_pea, aux, by = 'anio', all.x = TRUE )
act_pea[ , act_lpea := num_activos_proy / lpea ]

# Límites
unidad <- 1e6
xlim <- c( 2010, 2065 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 16 )
ybrk <- seq( ylim[1], ylim[2], 2 )
ylab <- formatC( ybrk, digits = 2, big.mark = ',', format = 'f' )

plt <- ggplot() +
  geom_point( data = act_pea, aes( x = anio, y = pea / unidad, colour = 'a' ), size = 2 ) +
  geom_line( data = act_pea, aes( x = anio, y = pea / unidad, colour = 'a' ), linewidth = 0.5 ) +
  geom_line( data = act_pea, aes( x = anio, y = pea_proy / unidad, colour = 'b' ), linewidth = 1 ) +
  geom_line( data = act_pea, aes( x = anio, y = pea_onu / unidad, colour = 'c' ), linewidth = 0.75 ) +
  geom_line( data = act_pea, aes( x = anio, y = lpea / unidad, colour = 'd' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual(
    labels = c( 'PEA-Observado', 'PEA-Proyectado', 'PEA-ONU', 'PEA-Estudio' ),
    breaks = c( 'a', 'b', 'c', 'd' ),
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='Población económicamente activa', x = 'Año',
        subtitle = 'Millones de personas',
        y = 'PEA' ) +
  plt_theme_legend

plt

# ACT / PEA ----------------------------------------------------------------------------------------
# Límites
xlim <- c( 2010, 2065 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 100 )
ybrk <- seq( ylim[1], ylim[2], 10 )
ylab <- formatC( ybrk, digits = 2, big.mark = ',', format = 'f' )

act_pea_ratio <- act_pea[ , list( anio, act_pea, act_pea_proy, act_lpea ) ]

plt <- ggplot() +
  geom_point( data = act_pea, aes( x = anio, y = 100 * act_pea, colour = 'a' ), size = 2 ) +
  geom_line( data = act_pea, aes( x = anio, y = 100 * act_pea, colour = 'a' ), linewidth = 0.5 ) +
  geom_line( data = act_pea, aes( x = anio, y = 100 * act_pea_proy, colour = 'b' ), linewidth = 1 ) +
  geom_line( data = act_pea, aes( x = anio, y = 100 * act_lpea , colour = 'c' ), linewidth = 1 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual(
    labels = c( 'ACT/PEA-Observado', 'ACT/PEA-Proyectado', 'ACT/PEA-ONU' ),
    breaks = c( 'a', 'b', 'c' ),
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='Activos respecto a la PEA', x = 'Año',
        subtitle = 'Activos / PEA',
        y = '%' ) +
  plt_theme_legend

plt

# Contribuciones del Estado respecto al PIB --------------------------------------------------------
cest_pib <- ratios[ , list( anio = t + 2020, cest_pib = ae_pib_tasa ) ]
mean( cest_pib$cest_pib, na.rm = TRUE )

# Límites
xlim <- c( 2010, 2065 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 10 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, big.mark = ',', format = 'f' )

plt <- ggplot() +
  # geom_point( data = cest_pib, aes( x = anio, y = 100 * cest_pib, colour = 'a' ), size = 2 ) +
  # geom_line( data = cest_pib, aes( x = anio, y = 100 * cest_pib, colour = 'a' ), linewidth = 0.5 ) +
  geom_line( data = cest_pib, aes( x = anio, y = 100 * cest_pib, colour = 'a' ), linewidth = 1 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual(
    labels = c( 'CE/PIBACT', '', '' ),
    breaks = c( 'a', 'b', 'c' ),
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='Contribuciones del Estado respecto al PIB', x = 'Año',
        subtitle = 'Contribuciones / PIB',
        y = '%' ) +
  plt_theme_legend

plt

# balance actuarial --------------------------------------------------------------------------------
balance_anual$V / 1e6


# final --------------------------------------------------------------------------------------------
# dev.off()
rm( list = ls( )[ !( ls( ) %in% 'parametros' ) ] )

