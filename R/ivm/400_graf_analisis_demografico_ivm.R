message( paste( rep( '-', 100 ), collapse = '' ) )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = parametros$demo_rdata_sgo_est_dem )
# load( file = parametros$ivm_rdata_est_demo )

# Población activa del SGO -------------------------------------------------------------------------
## Gráficos Afiliados SGO Activos ------------------------------------------------------------------
message( '\tGraficando población afiliada activa inicial SGO del IESS' )
aux <- copy( est_sal_anio_sexo_edad )
aux <- aux[ anio <= 2022, ]
aux <- data.table( dcast( aux, anio ~ sexo, value.var = 'ER_act', fun.aggregate = sum, na.rm = TRUE ) )
aux[ , total := H + M ]

x_lim <- c( 2012, 2022 )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], 1 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 3500000 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], by = 500000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

iess_pob_afi_ini_sgo <- ggplot( data = aux ) +
  geom_line( aes( x = anio, y = H, colour = 'Masculino' ), size = graf_line_size ) +
  geom_line( aes( x = anio, y = M, colour = 'Femenino' ), size = graf_line_size ) +
  geom_line( aes( x = anio, y = total, colour = 'Total Afiliados' ), size = graf_line_size ) +
  scale_colour_manual(
    '',
    breaks = c( 'Total Afiliados' , 'Masculino', 'Femenino' ),
    values = c( 'Masculino' = parametros$iess_blue,
                'Femenino' = parametros$iess_green,
                'Total Afiliados' = parametros$iess_total ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  plt_theme_legend +
  labs( x = 'Año', y = 'Afiliados' ) +
  theme( axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ),
         legend.text = element_text( colour = 'black' ) )

ggsave( plot = iess_pob_afi_ini_sgo,
        filename = paste0( parametros$resultado_graficos, 'iess_pob_afi_sgo_ini', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Gráficos Afiliados TNRH Activos -----------------------------------------------------------------
message( '\tGraficando población afiliada activa inicial TNRH del IESS' )
aux <- copy( est_sal_anio_sexo_edad )
aux <- aux[ anio > 2015 & anio <= 2022, ]
aux <- data.table( dcast( aux, anio ~ sexo, value.var = 'ER_tnrh_act', fun.aggregate = sum, na.rm = TRUE ) )
aux[ , total := H + M ]

x_lim <- c( 2015, 2022 )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], 1 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, max( aux$total ) )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

iess_pob_afi_ini_tnrh <- ggplot( data = aux ) +
  geom_line( aes( x = anio, y = H, colour = 'Masculino' ), size = graf_line_size ) +
  geom_line( aes( x = anio, y = M, colour = 'Femenino' ), size = graf_line_size ) +
  geom_line( aes( x = anio, y = total, colour = 'Total Afiliados' ), size = graf_line_size ) +
  scale_colour_manual(
    '',
    breaks = c( 'Total Afiliados' , 'Masculino', 'Femenino' ),
    values = c( 'Masculino' = parametros$iess_blue,
                'Femenino' = parametros$iess_green,
                'Total Afiliados' = parametros$iess_total ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  plt_theme_legend +
  labs( x = 'Año', y = 'Afiliados' ) +
  theme( axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ),
         legend.text = element_text( colour = 'black' ) )

ggsave( plot = iess_pob_afi_ini_tnrh,
        filename = paste0( parametros$resultado_graficos, 'iess_pob_afi_tnrh_ini', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Pirámide de Afiliados Activos SGO ---------------------------------------------------------------
message( '\tGraficando población afiliada activa inicial por edad y sexo SGO del IESS' )
x_max <- 105
x_min <- 0

aux <- est_sal_anio_sexo_edad[ , list( anio, sexo, x, PER = PER2 ) ]
aux <- aux[ anio == 2022, ]
aux <- aux[ x >= x_min & x <= x_max ]
aux[ is.na( PER ), PER := 0 ]
aux[ sexo == 'H', PER := -PER ]
aux[ , anio := NULL ]

y_lim <- c( -max( abs( aux$PER ) ), max( abs( aux$PER ) ) )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], length.out = 11 )
y_lbl <- formatC( 100 * abs( y_brk ), digits = 2, decimal.mark = ',', big.mark = '.', format = 'f' )

x_lim <- c( x_min, x_max )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], length.out = 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

iess_pir_afiliados <-  ggplot( data = aux ) +
  geom_bar( aes( x = x, y = PER, fill = sexo ), stat = 'identity', size = 0.1 ) +
  xlab( 'Número de personas expuestas' ) +
  ylab( 'Porcentaje por grupo' ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  coord_flip() +
  plt_theme_legend +
  guides( fill = guide_legend( title = NULL, label.position = 'right', label.hjust = 0 ) ) +
  scale_fill_manual( breaks = c( 'H', 'M' ), 
                     labels = c( 'Hombres', 'Mujeres' ), 
                     values = c( parametros$iess_blue, parametros$iess_green ) )

ggsave( plot = iess_pir_afiliados,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_afiliados_sgo', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Pirámide de Afiliados Activos TNRH SGO ----------------------------------------------------------
message( '\tGraficando población afiliada activa TNRH inicial por edad y sexo SGO del IESS' )

x_max <- 105
x_min <- 0

aux <- est_sal_anio_sexo_edad[ , list( anio, sexo, x, ER = ER_tnrh_act ) ]
aux <- aux[ anio == 2022, ]
aux <- aux[ x >= x_min & x <= x_max ]
aux[ is.na( ER ), ER := 0 ]
aux[ , PER := ER / sum( ER, na.rm = TRUE ) ]
aux[ sexo == 'H', PER := -PER ]
aux[ , anio := NULL ]

y_lim <- c( -max( abs( aux$PER ) ), max( abs( aux$PER ) ) )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], length.out = 11 )
y_lbl <- formatC( 100 * abs( y_brk ), digits = 2, decimal.mark = ',', big.mark = '.', format = 'f' )

x_lim <- c( x_min, x_max )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], length.out = 10 )
x_lbl <- formatC( x_brk, digits = 0, decimal.mark = ',', big.mark = '.', format = 'f' )

iess_pir_afiliados_tnrh <- ggplot( data = aux ) +
  geom_bar( aes( x = x, y = PER, fill = sexo ), stat = 'identity', size = 0.1 ) +
  xlab( 'Número de personas expuestas' ) +
  ylab( 'Porcentaje por grupo' ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  coord_flip() +
  plt_theme_legend +
  guides( fill = guide_legend( title = NULL, label.position = 'right', label.hjust = 0 ) ) +
  scale_fill_manual( breaks = c( 'H', 'M' ), 
                     labels = c( 'Hombres', 'Mujeres' ), 
                     values = c( parametros$iess_blue, parametros$iess_green ) )

ggsave( plot = iess_pir_afiliados_tnrh,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_afiliados_tnrh',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Masa salarial por sexo y total del SGO ----------------------------------------------------------
message( '\tGraficando masa salarial inicial SGO del IESS' )
unidad <- 1e6
aux <- est_sal_anio_sexo[ , list( anio, sexo, S = S / unidad ) ]
aux <- data.table( dcast( aux, anio ~ sexo, value.var = 'S' ) )
aux[ , ST := H + M ]

y_lim <- c( 0, max( aux$ST ) )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

x_stp <- 1
x_lim <- c( 2012, 2022 )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], x_stp )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

iess_masa_salarial_ini <- ggplot( data = aux ) +
  geom_line( aes( x = anio, y = H, colour = 'H' ), size = graf_line_size ) +
  geom_line( aes( x = anio, y = M, colour = 'M' ), size = graf_line_size ) +
  geom_line( aes( x = anio, y = ST, colour = 'T' ), size = graf_line_size ) +
  scale_colour_manual( breaks = c( 'H', 'M', 'T' ),
                       labels = c( 'Masa salarial hombres', 'Masa salarial mujeres', 'Masa salarial total' ),
                       values = c( parametros$col_male, parametros$col_female, parametros$iess_green ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  plt_theme_legend +
  labs( x = 'Año', y = 'Masa Salarial ( millones )' ) +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

ggsave( plot = iess_masa_salarial_ini,
        filename = paste0( parametros$resultado_graficos, 'iess_masa_salarial_sgo_ini', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Masa salarial por sexo y total del TNRH ---------------------------------------------------------
message( '\tGraficando masa salarial inicial TNRH del IESS' )
unidad <- 1e6
aux <- est_sal_anio_sexo[ , list( anio, sexo, S = ST / unidad ) ]
aux <- data.table( dcast( aux, anio ~ sexo, value.var = 'S' ) )
aux[ , ST := H + M ]

y_lim <- c( 0, 400 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

x_stp <- 1
x_lim <- c( 2012, 2022 )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], x_stp )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

iess_masa_salarial_ini <- ggplot( data = aux ) +
  geom_line( aes( x = anio, y = H, colour = 'H' ), size = graf_line_size ) +
  geom_line( aes( x = anio, y = M, colour = 'M' ), size = graf_line_size ) +
  geom_line( aes( x = anio, y = ST, colour = 'T' ), size = graf_line_size ) +
  scale_colour_manual( breaks = c( 'H', 'M', 'T' ),
                       labels = c( 'Masa salarial hombres', 'Masa salarial mujeres', 'Masa salarial total' ),
                       values = c( parametros$col_male, parametros$col_female, parametros$iess_green ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  plt_theme_legend +
  labs( x = 'Año', y = 'Masa Salarial ( millones )' ) +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

ggsave( plot = iess_masa_salarial_ini,
        filename = paste0( parametros$resultado_graficos, 'iess_masa_salarial_tnrh_ini', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando masa salarial del SGO por sexo y monto -----------------------------------------------
message( '\tGraficando masa salarial SGO por sexo y monto' )
aux <- est_sal_anio_sexo_edad[ anio == 2022, list( sexo, ER = ER_act, ESm ) ]
slst <- seq( 0, 3000, 100 )
aux[ , sg := cut( ESm, breaks = slst, include.lowest = TRUE, right = FALSE, ordered_result = TRUE, dig.lab = 4 ) ]
aux <- aux[ , list( n = sum( ER, na.rm = TRUE ) ), by = list( sexo, sg ) ]
aux[ , n := n / sum( n, na.rm = TRUE ) ]
aux[ sexo == 'H', n := -n ]
setorder( aux, sg )

y_lim <- c( -max( abs( aux$n ) ), max( abs( aux$n ) ) )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], length.out = 11 )
y_lbl <- paste0( formatC( 100 * abs( y_brk ), digits = 2, format = 'f', decimal.mark = ',' ), '%' )

iess_pir_masa_salarial <- ggplot( data = aux ) +
  geom_bar( aes( x = sg, y = n, fill = sexo ), stat = 'identity', size = 0.1 ) +
  xlab( 'Salario declarado ( USD )' ) +
  ylab( 'Porcentaje por grupo' ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  coord_flip() +
  plt_theme_legend +
  guides( fill = guide_legend( title = NULL, label.position = 'right', label.hjust = 0 ) ) +
  scale_fill_manual( breaks = c( 'H', 'M' ), 
                     labels = c( 'Hombres', 'Mujeres' ), 
                     values = c( parametros$iess_blue, parametros$iess_green ) )

ggsave( plot = iess_pir_masa_salarial,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_masa_salarial_sgo', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando masa salarial del TNRH por sexo y monto ----------------------------------------------
message( '\tGraficando masa salarial TNRH por sexo y monto' )

aux <- est_sal_anio_sexo_edad[ anio == 2022, list( sexo, ER = ER_tnrh_act, ESTm ) ]
slst <- seq( 0, 400, 25 )
aux[ , sg := cut( ESTm, breaks = slst, include.lowest = TRUE, right = FALSE, ordered_result = TRUE, dig.lab = 4 ) ]
aux <- aux[ , list( n = sum( ER, na.rm = TRUE ) ), by = list( sexo, sg ) ]
aux[ , n := n / sum( n, na.rm = TRUE ) ]
aux[ sexo == 'H', n := -n ]
setorder( aux, sg )

y_lim <- c( -max( abs( aux$n ) ), max( abs( aux$n ) ) )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], length.out = 11 )
y_lbl <- paste0( formatC( 100 * abs( y_brk ), digits = 2, format = 'f', decimal.mark = ',' ), '%' )

iess_pir_masa_salarial <- ggplot( data = aux ) +
  geom_bar( aes( x = sg, y = n, fill = sexo ), stat = 'identity', size = 0.1 ) +
  xlab( 'Salario declarado ( USD )' ) +
  ylab( 'Porcentaje por grupo' ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  coord_flip() +
  plt_theme_legend +
  guides( fill = guide_legend( title = NULL, label.position = 'right', label.hjust = 0 ) ) +
  scale_fill_manual( breaks = c( 'H', 'M' ), 
                     labels = c( 'Hombres', 'Mujeres' ), 
                     values = c( parametros$iess_blue, parametros$iess_green ) )

ggsave( plot = iess_pir_masa_salarial,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_masa_salarial_tnrh', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando proporción de afiliados activos e inactivos SGO --------------------------------------
message( '\tGraficando proporcion de afiliados del SGO activos e inactivos' )

plt_tree <- ggplot( est_act_ina_er, aes( area = por, fill = desc, label = desc, subgroup = porf ) ) +
  geom_treemap() +
  geom_treemap_text( colour = 'black', place = 'top', size = 15, alpha = 0.7, grow = FALSE ) +
  geom_treemap_subgroup_border( colour = 'black', size = 3 ) +
  geom_treemap_subgroup_text( colour = 'black', place = 'center', size = 15, alpha = 0.7, fontface = 'italic', grow = FALSE ) +
  scale_fill_brewer( palette = 'YlGnBu' ) +
  plt_theme

ggsave( plot = plt_tree,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_sgo_act_inact',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando proporcion de afiliados activos e inactivos TNRH -------------------------------------
message( '\tGraficando proporcion de afiliados del TNRH activos e inactivos' )

plt_tree <- ggplot( est_act_tnrh_ina_er, aes( area = por, fill = desc, label = desc, subgroup = porf ) ) +
  geom_treemap() +
  geom_treemap_text( colour = 'black', place = 'top', size = 15, alpha = 0.7, grow = FALSE ) +
  geom_treemap_subgroup_border( colour = 'black', size = 3 ) +
  geom_treemap_subgroup_text( colour = 'black', place = 'center', size = 15, alpha = 0.7, 
                              fontface = 'italic', grow = FALSE ) +
  scale_fill_brewer( palette = 'YlGnBu' ) +
  plt_theme

ggsave( plot = plt_tree,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_tnrh_act_inact', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando proporción afiliados activos mujeres por tipo de riesgos -----------------------------
message( '\tGraficando pastel afilaidos SGO activos mujeres por tipo de riesgos' )

plt_tree <- ggplot( est_act_er_risk_m, aes( area = por, fill = riesgo, label = riesgo, subgroup = porf ) ) +
  geom_treemap() +
  geom_treemap_text( colour = 'black', place = 'top', size = 15, alpha = 0.7, grow = FALSE ) +
  geom_treemap_subgroup_border( colour = 'black', size = 3 ) +
  geom_treemap_subgroup_text( colour = 'black', place = 'center', size = 15, alpha = 0.7, 
                              fontface = 'italic', grow = FALSE ) +
  scale_fill_brewer( palette = 'YlGnBu' ) +
  plt_theme

ggsave( plot = plt_tree,
        filename = paste0( parametros$resultado_graficos, 'iess_pastel_afiliados_sgo_fem_riesgo', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando proporción afiliados activos hombres por tipo de riesgos -----------------------------
message( '\tGraficando pastel afiliados activos hombres por tipo de riesgos' )

plt_tree <- ggplot( est_act_er_risk_h, aes( area = por, fill = riesgo, label = riesgo, subgroup = porf ) ) +
  geom_treemap() +
  geom_treemap_text( colour = 'black', place = 'top', size = 15, alpha = 0.7, grow = FALSE ) +
  geom_treemap_subgroup_border( colour = 'black', size = 3 ) +
  geom_treemap_subgroup_text( colour = 'black', place = 'center', size = 15, alpha = 0.7, 
                              fontface = 'italic', grow = FALSE ) +
  scale_fill_brewer( palette = 'YlGnBu' ) +
  plt_theme

ggsave( plot = plt_tree,
        filename = paste0( parametros$resultado_graficos, 'iess_pastel_afiliados_sgo_mas_riesgo', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando afiliados SGO activos por edad, años de imposiciones y riesgo, mujeres ---------------
message( '\tGraficando afiliados SGO activos por edad, años de imposiciones y riesgo, mujeres' )
aux <- est_act_rsk_xs[ sexo == 'M', list( sexo, x, ER = ER_act, s, riesgo ) ]
aux[ riesgo == 1, riesgos := 'Requisitos no cumplidos' ]
aux[ riesgo == 2, riesgos := 'Requisitos cumplidos para invalidez y muerte' ]
aux[ riesgo == 3, riesgos := 'Cumple requisitos para vejez en 5 años' ]
aux[ riesgo == 4, riesgos := 'Cumple requisitos para vejez en 1 año o menos' ]
aux <- aux[ !is.na( riesgo ), ]
df2 <- aux[ , list( ER = sum( ER, na.rm = TRUE ) ), by = list( sexo, x, s, riesgos ) ]
df2 <- df2[ x <= 105 ]

x_lim <- c( 0, 50 )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ', ' )

y_lim <- c( -105, -15 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], 5 )
y_lbl <- formatC( -y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

esc_cl <- c( 'firebrick4', 'gold', 'brown1', 'black' )

iess_afi_female_edad_riesgo <- ggplot( df2, aes( x = s , y = -x ) ) +
  geom_point( aes( size = ER , colour = riesgos ), alpha = 0.5 ) +
  scale_size( range = c( 1, 3 ) ) +
  scale_color_manual( 
    values = c( 'Requisitos no cumplidos' = esc_cl[ 1 ],
                'Requisitos cumplidos para invalidez y muerte' = esc_cl[ 2 ],
                'Cumple requisitos para vejez en 5 años' = esc_cl[ 3 ],
                'Cumple requisitos para vejez en 1 año o menos' = esc_cl[ 4 ] )
  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  labs( x = 'Imposiciones en años', y = 'Edad' ) +
  guides( size = F ) +
  plt_theme_legend +
  theme( legend.position = c( 0.7, 0.85 ), legend.direction = 'vertical',
         legend.key.size = unit( 0.2, 'cm' ), legend.text = element_text( size = 7 ) ) +
  theme( axis.text.x = element_text( angle = 0 , vjust = 1 ) ) +
  guides( color = guide_legend( override.aes = list( size = 4 ) ) )

ggsave( plot = iess_afi_female_edad_riesgo,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_sgo_fem_edad_riesgo', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando afiliados SGO activos por edad, años de imposiciones y riesgo, hombres ---------------
message( '\tGraficando afiliados SGO activos por edad, años de imposiciones y riesgo, hombres' )
aux <- est_act_rsk_xs[ sexo == 'H', list( sexo, x, ER = ER_act, s, riesgo ) ]
aux[ riesgo == 1, riesgos := 'Requisitos no cumplidos' ]
aux[ riesgo == 2, riesgos := 'Requisitos cumplidos para invalidez y muerte' ]
aux[ riesgo == 3, riesgos := 'Cumple requisitos para vejez en 5 años' ]
aux[ riesgo == 4, riesgos := 'Cumple requisitos para vejez en 1 año o menos' ]
aux <- aux[ !is.na( riesgo ), ]
df2 <- aux[ , list( ER = sum( ER, na.rm = TRUE ) ), by = list( sexo, x, s, riesgos ) ]
df2 <- df2[ x <= 105 ]

x_lim <- c( 0, 50 )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ', ' )

y_lim <- c( -105, -15 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], 5 )
y_lbl <- formatC( -y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

esc_cl <- c( 'firebrick4', 'gold', 'brown1', 'black' )

iess_afi_male_edad_riesgo <- ggplot( df2, aes( x = s , y = -x ) ) +
  geom_point( aes( size = ER , colour = riesgos ), alpha = 0.5 ) +
  scale_size( range = c( 1, 3 ) ) +
  scale_color_manual( values = c( 'Requisitos no cumplidos' = esc_cl[ 1 ],
                                  'Requisitos cumplidos para invalidez y muerte' = esc_cl[ 2 ],
                                  'Cumple requisitos para vejez en 5 años' = esc_cl[ 3 ],
                                  'Cumple requisitos para vejez en 1 año o menos' = esc_cl[ 4 ] )
  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  labs( x = 'Imposiciones en años', y = 'Edad' ) +
  guides( size = F ) +
  plt_theme_legend +
  theme( legend.position = c( 0.7, 0.85 ), legend.direction = 'vertical',
         legend.key.size = unit( 0.2, 'cm' ), legend.text = element_text( size = 7 ) ) +
  theme( axis.text.x = element_text( angle = 0 , vjust = 1 ) ) +
  guides( color = guide_legend( override.aes = list( size = 4 ) ) )

ggsave( plot = iess_afi_male_edad_riesgo,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_sgo_mas_edad_riesgo',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando afiliados TNRH activos por edad, años de imposiciones y riesgo, mujeres --------------
message( '\tGraficando afiliados TNRH activos por edad, años de imposiciones y riesgo, mujeres' )
aux <- est_act_rsk_xs[ sexo == 'M', list( sexo, x, ER = ER_tnrh_act, s, riesgo ) ]
aux[ riesgo == 1, riesgos := 'Requisitos no cumplidos' ]
aux[ riesgo == 2, riesgos := 'Requisitos cumplidos para invalidez y muerte' ]
aux[ riesgo == 3, riesgos := 'Cumple requisitos para vejez en 5 años' ]
aux[ riesgo == 4, riesgos := 'Cumple requisitos para vejez en 1 año o menos' ]
aux <- aux[ !is.na( riesgo ), ]
df2 <- aux[ , list( ER = sum( ER, na.rm = TRUE ) ), by = list( sexo, x, s, riesgos ) ]
df2 <- df2[ x <= 105 ]

x_lim <- c( 0, 50 )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ', ' )

y_lim <- c( -105, -15 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], 5 )
y_lbl <- formatC( -y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

esc_cl <- c( 'firebrick4', 'gold', 'brown1', 'black' )

iess_afi_female_edad_riesgo <- ggplot( df2, aes( x = s , y = -x ) ) +
  geom_point( aes( size = ER , colour = riesgos ), alpha = 0.5 ) +
  scale_size( range = c( 1, 3 ) ) +
  scale_color_manual( values = c( 'Requisitos no cumplidos' = esc_cl[ 1 ],
                                  'Requisitos cumplidos para invalidez y muerte' = esc_cl[ 2 ],
                                  'Cumple requisitos para vejez en 5 años' = esc_cl[ 3 ],
                                  'Cumple requisitos para vejez en 1 año o menos' = esc_cl[ 4 ] )
  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  labs( x = 'Imposiciones en años', y = 'Edad' ) +
  guides( size = F ) +
  plt_theme_legend +
  theme( legend.position = c( 0.7, 0.85 ), legend.direction = 'vertical',
         legend.key.size = unit( 0.2, 'cm' ), legend.text = element_text( size = 7 ) ) +
  theme( axis.text.x = element_text( angle = 0 , vjust = 1 ) ) +
  guides( color = guide_legend( override.aes = list( size = 4 ) ) )

ggsave( plot = iess_afi_female_edad_riesgo,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_tnrh_fem_edad_riesgo',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


## Graficando afiliados TNRH activos por edad, años de imposiciones y riesgo, hombres --------------
message( '\tGraficando afiliados TNRH activos por edad, años de imposiciones y riesgo, hombres' )
aux <- est_act_rsk_xs[ sexo == 'H', list( sexo, x, ER = ER_tnrh_act, s, riesgo ) ]
aux[ riesgo == 1, riesgos := 'Requisitos no cumplidos' ]
aux[ riesgo == 2, riesgos := 'Requisitos cumplidos para invalidez y muerte' ]
aux[ riesgo == 3, riesgos := 'Cumple requisitos para vejez en 5 años' ]
aux[ riesgo == 4, riesgos := 'Cumple requisitos para vejez en 1 año o menos' ]
aux <- aux[ !is.na( riesgo ), ]
df2 <- aux[ , list( ER = sum( ER, na.rm = TRUE ) ), by = list( sexo, x, s, riesgos ) ]
df2 <- df2[ x <= 105 ]

x_lim <- c( 0, 10 )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], 1 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ', ' )

y_lim <- c( -105, -15 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], 5 )
y_lbl <- formatC( -y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

esc_cl <- c( 'firebrick4', 'gold', 'brown1', 'black' )

iess_afi_male_edad_riesgo <- ggplot( df2, aes( x = s , y = -x ) ) +
  geom_point( aes( size = ER , colour = riesgos ), alpha = 0.5 ) +
  scale_size( range = c( 1, 3 ) ) +
  scale_color_manual( values = c( 'Requisitos no cumplidos' = esc_cl[ 1 ],
                                  'Requisitos cumplidos para invalidez y muerte' = esc_cl[ 2 ],
                                  'Cumple requisitos para vejez en 5 años' = esc_cl[ 3 ],
                                  'Cumple requisitos para vejez en 1 año o menos' = esc_cl[ 4 ] )
  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  labs( x = 'Imposiciones en años', y = 'Edad' ) +
  guides( size = F ) +
  plt_theme_legend +
  theme( legend.position = c( 0.7, 0.85 ), legend.direction = 'vertical',
         legend.key.size = unit( 0.2, 'cm' ), legend.text = element_text( size = 7 ) ) +
  theme( axis.text.x = element_text( angle = 0 , vjust = 1 ) ) +
  guides( color = guide_legend( override.aes = list( size = 4 ) ) )

ggsave( plot = iess_afi_male_edad_riesgo,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_tnrh_mas_edad_riesgo', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando afiliados SGO activos por años de cotizaciones, mujeres ------------------------------
message( '\tGraficando afiliados SGO por años de cotizaciones, mujeres' )
cotizantes <- est_act_xs[ sexo == 'M', list( num = sum( ER_act, na.rm = TRUE ) ), list( s ) ]

x_lim <- c( -0.5, 60 )
x_brk <- seq( x_lim[ 1 ] + 0.5, x_lim[ 2 ], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c ( 0, 200 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], 20 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f' )

iess_afi_female_num_cotiz <- ggplot( cotizantes, aes( x = s, y = num/1000, fill = ' ' ) ) +
  geom_bar( data = cotizantes, stat = 'identity', colour = 'white', size = 0.1 ) +
  xlab( 'Imposiciones en años' ) +
  ylab( 'Miles de personas' ) +
  theme_tufte() +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme +
  scale_fill_manual( values = parametros$iess_blue )

ggsave( plot = iess_afi_female_num_cotiz ,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_sgo_fem_num_cotiz', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando afiliados SGO activos por años de cotizaciones, hombres ------------------------------
message( '\tGraficando afiliados SGO por años de cotizaciones, hombres' )
cotizantes <- est_act_xs[ sexo == 'H', list( num = sum( ER_act, na.rm = TRUE ) ), list( s ) ]

x_lim <- c( -0.5, 62.5 )
x_brk <- seq( x_lim[ 1 ] + 0.5, x_lim[ 2 ], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c ( 0, 150 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], 15 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f' )

iess_afi_male_num_cotiz <- ggplot( cotizantes, aes( x = s, y = num/1000, fill = ' ' ) ) +
  geom_bar( data = cotizantes, stat = 'identity', colour = 'white', size = 0.1 ) +
  xlab( 'Imposiciones en años' ) +
  ylab( 'Miles de personas' ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme +
  scale_fill_manual( values = parametros$iess_blue )

ggsave( plot = iess_afi_male_num_cotiz ,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_sgo_mas_num_cotiz', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando afiliados TNRH activos por años de cotizaciones, mujeres -----------------------------
message( '\tGraficando afiliados TNRH por años de cotizaciones, mujeres' )
cotizantes <- est_act_xs[ sexo == 'M', list( ER = sum( ER_tnrh_act, na.rm = TRUE ) ), list( s ) ]
setorder( cotizantes, s )

x_lim <- c( -0.5, 60 )
x_brk <- seq( x_lim[ 1 ] + 0.5, x_lim[ 2 ], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c ( 0, 200 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], 20 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f' )

iess_afi_female_num_cotiz <- ggplot( cotizantes, aes( x = s, y = ER / 1000, fill = ' ' ) ) +
  geom_bar( data = cotizantes, stat = 'identity', colour = 'white', size = 0.1 ) +
  xlab( 'Imposiciones en años' ) +
  ylab( 'Miles de personas' ) +
  theme_tufte() +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme +
  scale_fill_manual( values = parametros$iess_blue )

ggsave( plot = iess_afi_female_num_cotiz ,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_tnrh_fem_num_cotiz',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando afiliados TNRH activos por años de cotizaciones, hombres -----------------------------
message( '\tGraficando afiliados TNRH por años de cotizaciones, hombres' )
cotizantes <- est_act_xs[ sexo == 'H', list( ER = sum( ER_tnrh_act, na.rm = TRUE ) ), list( s ) ]
setorder( cotizantes, s )

x_stp <- 5
x_lim <- c( -0.5, 70 )
x_brk <- seq( x_lim[ 1 ] + 0.5, x_lim[ 2 ], x_stp )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_stp <- 100
y_lim <- c ( 0, 1000 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], y_stp )
y_lbl <- formatC( y_brk, digits = 0, format = 'f' )

iess_afi_male_num_cotiz <- ggplot() +
  geom_bar( data = cotizantes, aes( x = s, y = ER, fill = ' ' ), stat = 'identity', colour = 'white', size = 0.1 ) +
  xlab( 'Imposiciones en años' ) +
  ylab( 'Personas' ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme +
  scale_fill_manual( values = parametros$iess_blue )

ggsave( plot = iess_afi_male_num_cotiz ,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_tnrh_mas_num_cotiz',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando pastel afiliados TNRH activos mujeres por tipo de riesgos ----------------------------
message( '\tGraficando pastel afilaidos TNRH activos mujeres por tipo de riesgos' )

plt_tree <- ggplot( est_act_tnrh_er_risk_m, aes( area = por, fill = riesgo, label = riesgo, subgroup = porf ) ) +
  geom_treemap() +
  geom_treemap_text( colour = 'black', place = 'top', size = 15, alpha = 0.7, grow = FALSE ) +
  geom_treemap_subgroup_border( colour = 'black', size = 3 ) +
  geom_treemap_subgroup_text( colour = 'black', place = 'center', size = 15, alpha = 0.7, 
                              fontface = 'italic', grow = FALSE ) +
  scale_fill_brewer( palette = 'YlGnBu' ) +
  plt_theme

ggsave( plot = plt_tree,
        filename = paste0( parametros$resultado_graficos, 'iess_pastel_afiliados_tnrh_fem_riesgo', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando pastel afiliados TNRH activos hombres por tipo de riesgos ----------------------------
message( '\tGraficando pastel afiliados TNRH activos hombres por tipo de riesgos' )

plt_tree <- ggplot( est_act_tnrh_er_risk_h, aes( area = por, fill = riesgo, label = riesgo, subgroup = porf ) ) +
  geom_treemap() +
  geom_treemap_text( colour = 'black', place = 'top', size = 15, alpha = 0.7, grow = FALSE ) +
  geom_treemap_subgroup_border( colour = 'black', size = 3 ) +
  geom_treemap_subgroup_text( colour = 'black', place = 'center', size = 15, alpha = 0.7, 
                              fontface = 'italic', grow = FALSE ) +
  scale_fill_brewer( palette = 'YlGnBu' ) +
  plt_theme

ggsave( plot = plt_tree,
        filename = paste0( parametros$resultado_graficos, 'iess_pastel_afiliados_tnrh_mas_riesgo', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población inactiva del SGO -----------------------------------------------------------------------
## Graficando pastel afiliados SGO inactivos mujeres por tipo de riesgos ---------------------------
message( '\tGraficando pastel afiliados SGO inactivos mujeres por tipo de riesgos' )

plt_tree <- ggplot( est_ina_er_risk_m, aes( area = por, fill = riesgo, label = riesgo, subgroup = porf ) ) +
  geom_treemap() +
  geom_treemap_text( colour = 'black', place = 'top', size = 15, alpha = 0.7, grow = FALSE ) +
  geom_treemap_subgroup_border( colour = 'black', size = 3 ) +
  geom_treemap_subgroup_text( colour = 'black', place = 'center', size = 15, alpha = 0.7,
                              fontface = 'italic', grow = FALSE ) +
  scale_fill_brewer( palette = 'YlGnBu' ) +
  plt_theme

ggsave( plot = plt_tree,
        filename = paste0( parametros$resultado_graficos, 'iess_pastel_afiliados_sgo_fem_riesgo_inac',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando pastel afiliados SGO inactivos hombres por tipo de riesgos ---------------------------
message( '\tGraficando pastel afiliados SGO inactivos hombres por tipo de riesgos' )

plt_tree <- ggplot( est_ina_er_risk_h, aes( area = por, fill = riesgo, label = riesgo, subgroup = porf ) ) +
  geom_treemap() +
  geom_treemap_text( colour = 'black', place = 'top', size = 15, alpha = 0.7, grow = FALSE ) +
  geom_treemap_subgroup_border( colour = 'black', size = 3 ) +
  geom_treemap_subgroup_text( colour = 'black', place = 'center', size = 15, alpha = 0.7,
                              fontface = 'italic', grow = FALSE ) +
  scale_fill_brewer( palette = 'YlGnBu' ) +
  plt_theme

ggsave( plot = plt_tree,
        filename = paste0( parametros$resultado_graficos, 'iess_pastel_afiliados_sgo_mas_riesgo_inac',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando pastel afiliados TNRH inactivos mujeres por tipo de riesgos --------------------------
message( '\tGraficando pastel afiliados TNRH inactivos mujeres por tipo de riesgos' )

plt_tree <- ggplot( est_ina_tnrh_er_risk_m, aes( area = por, fill = riesgo, label = riesgo, subgroup = porf ) ) +
  geom_treemap() +
  geom_treemap_text( colour = 'black', place = 'top', size = 15, alpha = 0.7, grow = FALSE ) +
  geom_treemap_subgroup_border( colour = 'black', size = 3 ) +
  geom_treemap_subgroup_text( colour = 'black', place = 'center', size = 15, alpha = 0.7, 
                              fontface = 'italic', grow = FALSE ) +
  scale_fill_brewer( palette = 'YlGnBu' ) +
  plt_theme

ggsave( plot = plt_tree,
        filename = paste0( parametros$resultado_graficos, 'iess_pastel_afiliados_tnrh_fem_riesgo_inac',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando pastel afiliados TNRH inactivos hombres por tipo de riesgos --------------------------
message( '\tGraficando pastel afiliados TNRH inactivos hombres por tipo de riesgos' )

plt_tree <- ggplot( est_ina_tnrh_er_risk_h, aes( area = por, fill = riesgo, label = riesgo, subgroup = porf ) ) +
  geom_treemap() +
  geom_treemap_text( colour = 'black', place = 'top', size = 15, alpha = 0.7, grow = FALSE ) +
  geom_treemap_subgroup_border( colour = 'black', size = 3 ) +
  geom_treemap_subgroup_text( colour = 'black', place = 'center', size = 15, alpha = 0.7, 
                              fontface = 'italic', grow = FALSE ) +
  scale_fill_brewer( palette = 'YlGnBu' ) +
  plt_theme

ggsave( plot = plt_tree,
        filename = paste0( parametros$resultado_graficos, 'iess_pastel_afiliados_tnrh_mas_riesgo_inac', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando afiliados SGO inactivos por años de cotizaciones, mujeres ----------------------------
message( '\tGraficando afiliados SGO por años de cotizaciones, mujeres' )
cotizantes <- est_act_xs[ sexo == 'M', list( num = sum( ER_ina, na.rm = TRUE ) ), list( s ) ]

x_lim <- c( -0.5, 60 )
x_brk <- seq( x_lim[ 1 ] + 0.5, x_lim[ 2 ], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c ( 0, 300 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], 50 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f' )

iess_jefe_female_num_cotiz_inac <- ggplot( cotizantes, aes( x = s, y = num / 1000, fill = ' ' ) ) +
  geom_bar( data = cotizantes, stat = 'identity', colour = 'white', size = 0.1 ) +
  xlab( 'Imposiciones en años' ) +
  ylab( 'Miles de personas' ) +
  
  theme_tufte() +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme +
  scale_fill_manual( values = parametros$iess_blue )

ggsave( plot = iess_jefe_female_num_cotiz_inac ,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_sgo_fem_num_cotiz_inac', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Para TNRH
## Graficando afiliados SGO inactivos por años de cotizaciones, hombres ----------------------------
message( '\tGraficando afiliados SGO inactivos por años de cotizaciones, hombres' )
cotizantes <- est_act_xs[ sexo == 'H', list( num = sum( ER_ina, na.rm = TRUE ) ), list( s ) ]
setorder( cotizantes, s )

x_lim <- c( -0.5, 60 )
x_brk <- seq( x_lim[ 1 ] + 0.5, x_lim[ 2 ], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c ( 0, 500 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], 50 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f' )

iess_jefe_male_num_cotiz_inac <- ggplot( cotizantes, aes( x = s, y = num / 1000, fill = ' ' ) ) +
  geom_bar( data = cotizantes, stat = 'identity', colour = 'white', size = 0.1 ) +
  xlab( 'Imposiciones en años' ) +
  ylab( 'Miles de personas' ) +
  theme_tufte() +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme +
  scale_fill_manual( values = parametros$iess_blue )

ggsave( plot = iess_jefe_male_num_cotiz_inac ,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_sgo_mas_num_cotiz_inac', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando afiliados TNRH inactivos por años de cotizaciones, mujeres ---------------------------
message( '\tGraficando afiliados TNRH por años de cotizaciones, mujeres' )
cotizantes <- est_act_xs[ sexo == 'M', list( ER = sum( ER_tnrh_ina, na.rm = TRUE ) ), list( s ) ]
setorder( cotizantes, s )

x_lim <- c( -0.5, 60 )
x_brk <- seq( x_lim[ 1 ] + 0.5, x_lim[ 2 ], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c ( 0, 5e5 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f' )

iess_jefe_female_num_cotiz_inac <- ggplot( cotizantes, aes( x = s, y = ER, fill = ' ' ) ) +
  geom_bar( data = cotizantes, stat = 'identity', colour = 'white', size = 0.1 ) +
  xlab( 'Imposiciones en años' ) +
  ylab( 'Miles de personas' ) +
  theme_tufte() +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme +
  scale_fill_manual( values = parametros$iess_blue )

ggsave( plot = iess_jefe_female_num_cotiz_inac ,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_tnrh_fem_num_cotiz_inac', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando afiliados TNRH inactivos por años de cotizaciones, hombres ---------------------------
message( '\tGraficando afiliados TNRH inactivos por años de cotizaciones, hombres' )
cotizantes <- est_act_xs[ sexo == 'H', list( ER = sum( ER_tnrh_ina, na.rm = TRUE ) ), list( s ) ]
setorder( cotizantes, s )

x_lim <- c( -0.5, 60 )
x_brk <- seq( x_lim[ 1 ] + 0.5, x_lim[ 2 ], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c ( 0, 6e3 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f' )

iess_jefe_male_num_cotiz_inac <- ggplot( cotizantes, aes( x = s, y = ER, fill = ' ' ) ) +
  geom_bar( data = cotizantes, stat = 'identity', colour = 'white', size = 0.1 ) +
  xlab( 'Imposiciones en años' ) +
  ylab( 'Miles de personas' ) +
  theme_tufte() +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  plt_theme +
  scale_fill_manual( values = parametros$iess_blue )

ggsave( plot = iess_jefe_male_num_cotiz_inac ,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_tnrh_mas_num_cotiz_inac',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando afiliados SGO inactivos por edad, años de imposiciones y riesgo, mujeres -------------
message( '\tGraficando jefes inactivos por edad, años de imposiciones y riesgo, mujeres' )
aux <- est_act_rsk_xs[ sexo == 'M', list( sexo, x, ER = ER_ina, s, riesgo ) ]
aux[ riesgo == 1, riesgos := 'Requisitos no cumplidos' ]
aux[ riesgo == 2, riesgos := 'Requisitos cumplidos para invalidez y muerte' ]
aux[ riesgo == 3, riesgos := 'Cumple requisitos para vejez en 5 años' ]
aux[ riesgo == 4, riesgos := 'Cumple requisitos para vejez en 1 año o menos' ]
aux <- aux[ !is.na( riesgo ), ]
df2 <- aux[ , list( ER = sum( ER, na.rm = TRUE ) ), by = list( sexo, x, s, riesgos ) ]
df2 <- df2[ x <= 105 ]

x_lim <- c( 0, 45 )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ', ' )

y_lim <- c( -80, -15 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], 5 )
y_lbl <- formatC( -y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

esc_cl <- c( 'firebrick4', 'gold', 'brown1', 'black' )

iess_jef_female_edad_riesgo_inac <- ggplot( df2, aes( x = s , y = -x ) ) +
  geom_point( aes( size = ER , colour = riesgos ), alpha = 0.5 ) +
  scale_size( range = c( 1, 3 ) ) +
  scale_color_manual( values = c( 'Requisitos no cumplidos' = esc_cl[ 1 ],
                                  'Requisitos cumplidos para invalidez y muerte' = esc_cl[ 2 ],
                                  'Cumple requisitos para vejez en 5 años' = esc_cl[ 3 ],
                                  'Cumple requisitos para vejez en 1 año o menos' = esc_cl[ 4 ] )
  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  labs( x = 'Imposiciones en años', y = 'Edad' ) +
  guides( size = F ) +
  plt_theme_legend +
  theme( legend.position = c( 0.7, 0.85 ), legend.direction = 'vertical',
         legend.key.size = unit( 0.2, 'cm' ), legend.text = element_text( size = 7 ) ) +
  theme( axis.text.x = element_text( angle = 0 , vjust = 1 ) ) +
  guides( color = guide_legend( override.aes = list( size = 4 ) ) )

ggsave( plot = iess_jef_female_edad_riesgo_inac,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_sgo_fem_edad_riesgo_inac',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando afiliados SGO inactivos por edad, años de imposiciones y riesgo, hombres -------------
message( '\tGraficando jefes inactivos por edad, años de imposiciones y riesgo, hombres' )
aux <- est_act_rsk_xs[ sexo == 'H', list( sexo, x, ER = ER_ina, s, riesgo ) ]
aux[ riesgo == 1, riesgos := 'Requisitos no cumplidos' ]
aux[ riesgo == 2, riesgos := 'Requisitos cumplidos para invalidez y muerte' ]
aux[ riesgo == 3, riesgos := 'Cumple requisitos para vejez en 5 años' ]
aux[ riesgo == 4, riesgos := 'Cumple requisitos para vejez en 1 año o menos' ]
aux <- aux[ !is.na( riesgo ), ]
df2 <- aux[ , list( ER = sum( ER, na.rm = TRUE ) ), by = list( sexo, x, s, riesgos ) ]
df2 <- df2[ x <= 105 ]

x_lim <- c( 0, 45 )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ', ' )

y_lim <- c( -80, -15 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], 5 )
y_lbl <- formatC( -y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

esc_cl <- c( 'firebrick4', 'gold', 'brown1', 'black' )

iess_jef_male_edad_riesgo_inac <- ggplot( df2, aes( x = s , y = -x ) ) +
  geom_point( aes( size = ER , colour = riesgos ), alpha = 0.5 ) +
  scale_size( range = c( 1, 3 ) ) +
  scale_color_manual( values = c( 'Requisitos no cumplidos' = esc_cl[ 1 ],
                                  'Requisitos cumplidos para invalidez y muerte' = esc_cl[ 2 ],
                                  'Cumple requisitos para vejez en 5 años' = esc_cl[ 3 ],
                                  'Cumple requisitos para vejez en 1 año o menos' = esc_cl[ 4 ] )
  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  labs( x = 'Imposiciones en años', y = 'Edad' ) +
  guides( size = F ) +
  plt_theme_legend +
  theme( legend.position = c( 0.7, 0.85 ), legend.direction = 'vertical',
         legend.key.size = unit( 0.2, 'cm' ), legend.text = element_text( size = 7 ) ) +
  theme( axis.text.x = element_text( angle = 0 , vjust = 1 ) ) +
  guides( color = guide_legend( override.aes = list( size = 4 ) ) )

ggsave( plot = iess_jef_male_edad_riesgo_inac,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_sgo_mas_edad_riesgo_inac',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando afiliados TNRH inactivos por edad, años de imposiciones y riesgo, mujeres ------------
message( '\tGraficando afiliados TNRH por edad, años de imposiciones y riesgo, mujeres' )
aux <- est_act_rsk_xs[ sexo == 'M', list( sexo, x, ER = ER_tnrh_ina, s, riesgo ) ]
aux[ riesgo == 1, riesgos := 'Requisitos no cumplidos' ]
aux[ riesgo == 2, riesgos := 'Requisitos cumplidos para invalidez y muerte' ]
aux[ riesgo == 3, riesgos := 'Cumple requisitos para vejez en 5 años' ]
aux[ riesgo == 4, riesgos := 'Cumple requisitos para vejez en 1 año o menos' ]
aux <- aux[ !is.na( riesgo ), ]
df2 <- aux[ , list( ER = sum( ER, na.rm = TRUE ) ), by = list( sexo, x, s, riesgos ) ]
df2 <- df2[ x <= 105 ]

x_lim <- c( 0, 10 )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], 1 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ', ' )

y_lim <- c( -105, -15 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], 5 )
y_lbl <- formatC( -y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

esc_cl <- c( 'firebrick4', 'gold', 'brown1', 'black' )

iess_jef_female_edad_riesgo_inac <- ggplot( df2, aes( x = s , y = -x ) ) +
  geom_point( aes( size = ER , colour = riesgos ), alpha = 0.5 ) +
  scale_size( range = c( 1, 3 ) ) +
  scale_color_manual( values = c( 'Requisitos no cumplidos' = esc_cl[ 1 ],
                                  'Requisitos cumplidos para invalidez y muerte' = esc_cl[ 2 ],
                                  'Cumple requisitos para vejez en 5 años' = esc_cl[ 3 ],
                                  'Cumple requisitos para vejez en 1 año o menos' = esc_cl[ 4 ] )
  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  labs( x = 'Imposiciones en años', y = 'Edad' ) +
  guides( size = F ) +
  plt_theme_legend +
  theme( legend.position = c( 0.7, 0.85 ), legend.direction = 'vertical',
         legend.key.size = unit( 0.2, 'cm' ), legend.text = element_text( size = 7 ) ) +
  theme( axis.text.x = element_text( angle = 0 , vjust = 1 ) ) +
  guides( color = guide_legend( override.aes = list( size = 4 ) ) )

ggsave( plot = iess_jef_female_edad_riesgo_inac,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_tnrh_fem_edad_riesgo_inac', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Graficando afiliados TNRH inactivos por edad, años de imposiciones y riesgo, hombres ------------
message( '\tGraficando afiliados TNRH inactivos por edad, años de imposiciones y riesgo, hombres' )
aux <- est_act_rsk_xs[ sexo == 'H', list( sexo, x, ER = ER_tnrh_ina, s, riesgo ) ]
aux[ riesgo == 1, riesgos := 'Requisitos no cumplidos' ]
aux[ riesgo == 2, riesgos := 'Requisitos cumplidos para invalidez y muerte' ]
aux[ riesgo == 3, riesgos := 'Cumple requisitos para vejez en 5 años' ]
aux[ riesgo == 4, riesgos := 'Cumple requisitos para vejez en 1 año o menos' ]
aux <- aux[ !is.na( riesgo ), ]
df2 <- aux[ , list( ER = sum( ER, na.rm = TRUE ) ), by = list( sexo, x, s, riesgos ) ]
df2 <- df2[ x <= 105 ]

x_lim <- c( 0, 10 )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], 1 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ', ' )

y_lim <- c( -105, -15 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], 5 )
y_lbl <- formatC( -y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

esc_cl <- c( 'firebrick4', 'gold', 'brown1', 'black' )

iess_jef_male_edad_riesgo_inac <- ggplot( df2, aes( x = s , y = -x ) ) +
  geom_point( aes( size = ER , colour = riesgos ), alpha = 0.5 ) +
  scale_size( range = c( 1, 3 ) ) +
  scale_color_manual( values = c( 'Requisitos no cumplidos' = esc_cl[ 1 ],
                                  'Requisitos cumplidos para invalidez y muerte' = esc_cl[ 2 ],
                                  'Cumple requisitos para vejez en 5 años' = esc_cl[ 3 ],
                                  'Cumple requisitos para vejez en 1 año o menos' = esc_cl[ 4 ] )
  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  labs( x = 'Imposiciones en años', y = 'Edad' ) +
  guides( size = F ) +
  plt_theme_legend +
  theme( legend.position = c( 0.7, 0.85 ), legend.direction = 'vertical',
         legend.key.size = unit( 0.2, 'cm' ), legend.text = element_text( size = 7 ) ) +
  theme( axis.text.x = element_text( angle = 0 , vjust = 1 ) ) +
  guides( color = guide_legend( override.aes = list( size = 4 ) ) )

ggsave( plot = iess_jef_male_edad_riesgo_inac,
        filename = paste0( parametros$resultado_graficos, 'iess_afiliados_tnrh_mas_edad_riesgo_inac', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población pensionistas SGO -----------------------------------------------------------------------
## Gráfico porcentaje de pensionistas por tipo -----------------------------------------------------
message( '\tGraficando proporcion de pensionistas por sexo y tipo' )
# aux <- est_pen_se
# aux <- aux[ anio == 2022 ]
# aux[ , desc := paste0( str_to_title( tipo ), ' ', ifelse( sexo == 'H', 'hombres', 'mujeres' ) ) ]
# aux[ , porf := paste0( formatC( 100 * PER, decimal.mark = ', ', digits = 2, format = 'f' ), '%' ) ]
# 
# plt_tree <- ggplot( aux, aes( area = PER, fill = desc, label = desc, subgroup = porf ) ) +
#   geom_treemap() +
#   geom_treemap_text( colour = 'black', place = 'top', size = 15, alpha = 0.7, grow = FALSE ) +
#   geom_treemap_subgroup_border( colour = 'black', size = 3 ) +
#   geom_treemap_subgroup_text( colour = 'black', place = 'center', size = 15, alpha = 0.7, fontface = 'italic', grow = FALSE ) +
#   scale_fill_brewer( palette = 'YlGnBu' ) +
#   plt_theme
# 
# ggsave( plot = plt_tree,
#         filename = paste0( parametros$resultado_graficos, 'iess_pensionistas_sgo_tipo', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Distribución de la edad de los pensionistas del SGO, hombres ------------------------------------
message( '\tTotal de Pensionistas del SGO' )
male_pen <- est_pen_anio_tipo_sexo_x[ 
  anio == 2022 & sexo == 'H',
  list( n = sum( ER, na.rm = TRUE ) ),
  by = list( tipo, sexo, x ) ]
male_pen <- male_pen[ x >= 0 & x <= 105 ]

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

y_lim <- c( 0, 15000 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], 3000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

iess_pension_ssc_male <- ggplot( male_pen ) +
  geom_bar( data = male_pen[ tipo == 'VEJEZ' ], aes( x = x, y = n, fill = 'VEJEZ' ),
            stat = 'identity', width = 1, size = 0.1, alpha = 0.5 ) +
  geom_bar( data = male_pen[ tipo == 'INVALIDEZ' ], aes( x = x, y = n, fill = 'INVALIDEZ' ),
            stat = 'identity', width = 1, size = 0.1, alpha = 0.7 ) +
  geom_bar( data = male_pen[ tipo == 'DISCAPACIDAD' ], aes( x = x, y = n, fill = 'DISCAPACIDAD' ),
            stat = 'identity', width = 1, size = 0.1, alpha = 0.7 ) +
  geom_bar( data = male_pen[ tipo == 'ORFANDAD' ], aes( x = x, y = n, fill = 'ORFANDAD' ),
            stat = 'identity', width = 1, size = 0.1, alpha = 0.7 ) +
  geom_bar( data = male_pen[ tipo == 'VIUDEDAD' ], aes( x = x, y = n, fill = 'VIUDEDAD' ),
            stat = 'identity', width = 1, size = 0.1, alpha = 0.7 ) +
  scale_fill_manual( values = c( 'VEJEZ' = parametros$iess_blue,
                                 'INVALIDEZ' = parametros$iess_green,
                                 'DISCAPACIDAD' = 'firebrick4',
                                 'ORFANDAD' = 'gold',
                                 'VIUDEDAD' = 'brown1' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  labs( x = NULL, y = NULL, fill = NULL ) +
  plt_theme_legend +
  theme( axis.text.x = element_text( angle = 0, hjust = 0.5 ) ) +
  theme( legend.position = 'bottom' ) +
  theme( legend.key.size = unit( 0.5, 'cm' ) ) +
  theme( legend.position = 'bottom', legend.direction = 'horizontal',
         legend.key.size = unit( 0.5, 'cm' ),
         legend.spacing.y = unit( -0.9, 'cm' ) ) +
  guides( fill = guide_legend( title = NULL, label.position = 'right', label.hjust = 0, label.vjust = 0.5 ) ) +
  guides( shape = guide_legend( title = NULL ) ) +
  theme( legend.text = element_text( size = 8, colour = 'black' ) )

ggsave( plot = iess_pension_ssc_male ,
        filename = paste0( parametros$resultado_graficos, 'iess_pension_sgo_mas', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Distribución de la edad de los pensionistas del SGO, mujeres -------------------------------------
female_pen <- est_pen_anio_tipo_sexo_x[ 
  anio == 2022 & sexo == 'M',
  list( n = sum( ER, na.rm = TRUE ) ),
  by = list( tipo, sexo, x ) ]
male_pen <- male_pen[ x >= 0 & x <= 105 ]

female_pen <- female_pen[ x >= 0 & x <= 105 ]

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

y_lim <- c( 0, 15000 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], 3000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

iess_pension_ssc_female <- ggplot( female_pen ) +
  geom_bar( data = female_pen[ tipo == 'VEJEZ' ], aes( x = x, y = n, fill = 'VEJEZ' ),
            stat = 'identity', width = 1, size = 0.1, alpha = 0.5 ) +
  geom_bar( data = female_pen[ tipo == 'INVALIDEZ' ], aes( x = x, y = n, fill = 'INVALIDEZ' ),
            stat = 'identity', width = 1, size = 0.1, alpha = 0.7 ) +
  geom_bar( data = male_pen[ tipo == 'DISCAPACIDAD' ], aes( x = x, y = n, fill = 'DISCAPACIDAD' ),
            stat = 'identity', width = 1, size = 0.1, alpha = 0.7 ) +
  geom_bar( data = male_pen[ tipo == 'ORFANDAD' ], aes( x = x, y = n, fill = 'ORFANDAD' ),
            stat = 'identity', width = 1, size = 0.1, alpha = 0.7 ) +
  geom_bar( data = male_pen[ tipo == 'VIUDEDAD' ], aes( x = x, y = n, fill = 'VIUDEDAD' ),
            stat = 'identity', width = 1, size = 0.1, alpha = 0.7 ) +
  scale_fill_manual( values = c( 'VEJEZ' = parametros$iess_blue,
                                 'INVALIDEZ' = parametros$iess_green,
                                 'DISCAPACIDAD' = 'firebrick4',
                                 'ORFANDAD' = 'gold',
                                 'VIUDEDAD' = 'brown1' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  labs( x = NULL, y = NULL, fill = NULL ) +
  plt_theme_legend +
  theme( axis.text.x = element_text( angle = 0, hjust = 0.5 ) ) +
  theme( legend.position = 'bottom' ) +
  theme( legend.key.size = unit( 0.5, 'cm' ) ) +
  theme( legend.position = 'bottom', legend.direction = 'horizontal',
         legend.key.size = unit( 0.5, 'cm' ),
         legend.spacing.y = unit( -0.9, 'cm' ) ) +
  guides( fill = guide_legend( title = NULL, label.position = 'right', label.hjust = 0, label.vjust = 0.5 ) ) +
  guides( shape = guide_legend( title = NULL ) ) +
  theme( legend.text = element_text( size = 8, colour = 'black' ) )

ggsave( plot = iess_pension_ssc_female ,
        filename = paste0( parametros$resultado_graficos, 'iess_pension_sgo_fem', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Porcentaje de pensionistas del SGO por tipo y sexo ----------------------------------------------
message( '\tPorcentaje de pensionistas del SGO por tipo y sexo' )
aux <- est_pen_anio_tipo_sexo[ anio == 2022 ]
aux[ , desc := paste0( str_to_title( tipo ), ' ', ifelse( sexo == 'H', 'hombres', 'mujeres' ) ) ]
aux[ , por := ER / sum( ER ) ]
aux[ , porf := paste0( formatC( 100 * por, decimal.mark = ', ', digits = 2, format = 'f' ), '%' ) ]

plt_tree <- ggplot( aux, aes( area = por, fill = desc, label = desc, subgroup = porf ) ) +
  geom_treemap() +
  geom_treemap_text( colour = 'black', place = 'top', size = 15, alpha = 0.7, grow = FALSE ) +
  geom_treemap_subgroup_border( colour = 'black', size = 3 ) +
  geom_treemap_subgroup_text( colour = 'black', place = 'center', size = 15, alpha = 0.7, 
                              fontface = 'italic', grow = FALSE ) +
  scale_fill_brewer( palette = 'YlGnBu' ) +
  plt_theme

ggsave( plot = plt_tree,
        filename = paste0( parametros$resultado_graficos, 'iess_pensionistas_sgo_tipo', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


## Beneficios pagados por pensiones en el SGO ------------------------------------------------------
message( '\tGraficando beneficios pagados por pensionistas por sexo y tipo' )
aux <- est_pen_anio_tipo_sexo[ anio == 2022 ]
aux[ , desc := paste0( str_to_title( tipo ), ' ', ifelse( sexo == 'H', 'hombres', 'mujeres' ) ) ]
aux[ , por := P / sum( P ) ]
aux[ , porf := paste0( formatC( 100 * por, decimal.mark = ', ', digits = 2, format = 'f' ), '%' ) ]

plt_tree <- ggplot( aux, aes( area = por, fill = desc, label = desc, subgroup = porf ) ) +
  geom_treemap() +
  geom_treemap_text( colour = 'black', place = 'top', size = 15, alpha = 0.7, grow = FALSE ) +
  geom_treemap_subgroup_border( colour = 'black', size = 3 ) +
  geom_treemap_subgroup_text( colour = 'black', place = 'center', size = 15, alpha = 0.7, 
                              fontface = 'italic', grow = FALSE ) +
  scale_fill_brewer( palette = 'YlGnBu' ) +
  plt_theme

ggsave( plot = plt_tree,
        filename = paste0( parametros$resultado_graficos, 'iess_gasto_pensional_sgo_tipo', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


## Gráficos de pensionistas ------------------------------------------------------------------------
x_max <- 105
x_min <- 0

pen_lst <- c( seq( 0, 1500, 100 ), Inf )

tipos <- c( 'VEJEZ', 'INVALIDEZ', 'DISCAPACIDAD', 'VIUDEDAD', 'ORFANDAD' )
tab_nom <- c( 'vej', 'inv', 'dis', 'viu', 'orf' )

for ( i in 1:length( tipos ) ) {
  
  ### Pirámide -------------------------------------------------------------------------------------
  message( '\tGraficando pirámide de pensionistas de ', tab_nom[ i ], ' por edad y sexo SGO del IESS' )
  
  aux <- est_pen_anio_tipo_sexo_x[ tipo == tipos[ i ], list( anio, sexo, x, PER ) ]
  aux <- aux[ anio == 2022, ]
  aux <- aux[ x >= x_min & x <= x_max ]
  aux[ is.na( PER ), PER := 0 ]
  aux[ sexo == 'H', PER := -PER ]
  aux[ , anio := NULL ]
  
  y_stp <- 5
  y_lim <- c( -max( abs( aux$PER ) ), max( abs( aux$PER ) ) )
  y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], length.out = 11 )
  y_lbl <- paste0( formatC( 100 * abs( y_brk ), digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' ), '%' )
  
  x_stp <- 5
  x_lim <- c( x_min, x_max )
  x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], x_stp )
  x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
  
  pir_pen <- ggplot( aux, aes( x = x, y = PER, fill = sexo ) ) +
    xlab( 'Edad' ) +
    ylab( 'Porcentaje' ) +
    geom_bar( data = aux[ sexo == 'M' ], stat = 'identity', colour = 'white', size = 0.1 ) +
    geom_bar( data = aux[ sexo == 'H' ], stat = 'identity', colour = 'white', size = 0.1 ) +
    scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
    scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
    coord_flip() +
    plt_theme_legend +
    guides( fill = guide_legend( title = NULL, label.position = 'right', label.hjust = 0, label.vjust = 0.5 ) ) +
    scale_fill_manual( values = c( parametros$iess_blue, parametros$iess_green ),
                       labels = c( 'Hombres', 'Mujeres' ) )
  
  ggsave( plot = pir_pen,
          filename = paste0( parametros$resultado_graficos, 'iess_pir_pen_', tab_nom[ i ], '_sgo',
                             parametros$graf_ext ),
          width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
  
  ### Evolución ------------------------------------------------------------------------------------
  message( '\tGraficando evolución de la población jubilada por ', tab_nom[ i ], ' del SGO' )
  
  aux <- est_pen_anio_tipo_sexo[ 
    tipo == tipos[ i ] & anio <= 2022,
    list( ER = sum( ER, na.rm = TRUE ) ),
    by = list( anio, sexo ) ]
  aux <- dcast.data.table( aux, anio ~ sexo, value.var = 'ER', fun.aggregate = sum , na.rm = TRUE )
  aux[ , total := H + M ]
  
  x_lim <- c( 2012, 2022 )
  x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], 1 )
  x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
  
  y_lim <- c( 0, 1.02 * max( aux$total ) )
  y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], length.out = 11 )
  y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )
  
  plt_evo_pen_pob <- ggplot( aux ) +
    geom_line( data = aux, aes( x = anio, y = total, colour = 'Total' ), size = graf_line_size ) +
    geom_line( data = aux, aes( x = anio, y = H, colour = 'Hombres' ), size = graf_line_size ) +
    geom_line( data = aux, aes( x = anio, y = M, colour = 'Mujeres' ), size = graf_line_size ) +
    labs( x = 'Año', y = 'Pensionistas' ) +
    scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
    scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
    scale_colour_manual( 
      '',
      breaks = c( 'Total', 'Hombres', 'Mujeres' ),
      values = c( 'Total' = parametros$iess_total, 
                  'Hombres' = parametros$iess_blue,
                  'Mujeres' = parametros$iess_green ),
      labels = c( 'Total', 'Hombres', 'Mujeres' ) ) +
    plt_theme +
    theme( legend.position = 'bottom', legend.direction = 'horizontal',
           axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ),
           legend.text = element_text( colour = 'black' ) )
  
  ggsave( plot = plt_evo_pen_pob,
          filename = paste0( parametros$resultado_graficos, 'iess_pen_', tab_nom[ i ], '_sgo', 
                             parametros$graf_ext ),
          width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
  
  
  ### Pirámide por monto de pensión ----------------------------------------------------------------
  message( '\tGraficando pensionistas por monto y sexo SGO del IESS' )
  
  aux <- est_pen[ anio == 2022, list( anio, tipo, sexo, x, ER, EPm ) ]
  aux <- aux[ x >= x_min & x <= x_max & tipo == tipos[ i ] ]
  aux[ , pen_clas := cut( EPm, breaks = pen_lst, include.lowest = TRUE, right = FALSE, 
                          ordered_result = TRUE, dig.lab = 5 ) ]
  aux <- aux[ , list( ER = sum( ER, na.rm = TRUE ) ), by = list( sexo, pen_clas ) ]
  
  aux[ , PER := ER / sum( ER, na.rm = TRUE ) ]
  aux[ sexo == 'H', PER := -PER ]
  
  y_lim <- c( -max( abs( aux$PER ) ), max( abs( aux$PER ) ) )
  y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], length.out = 11 )
  y_lbl <- paste0( formatC( 100 * abs( y_brk ), big.mark = '.', decimal.mark = ',', digits = 2, 
                            format = 'f' ), '%' )
  
  pir_monto_pen <- ggplot( data = aux ) +
    geom_bar( aes( x = pen_clas, y = PER, fill = sexo ), stat = 'identity', size = 0.1 ) +
    xlab( 'Pensión ( USD )' ) +
    ylab( 'Porcentaje por grupo' ) +
    scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
    coord_flip() +
    plt_theme_legend +
    guides( fill = guide_legend( title = NULL, label.position = 'right', label.hjust = 0 ) ) +
    scale_fill_manual( breaks = c( 'H', 'M' ), 
                       labels = c( 'Hombres', 'Mujeres' ), 
                       values = c( parametros$iess_blue, parametros$iess_green ) )
  
  ggsave( plot = pir_monto_pen,
          filename = paste0( parametros$resultado_graficos, 'iess_pir_monto_pen_', tab_nom[ i ], 
                             '_sgo', parametros$graf_ext ),
          width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
  
}
# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()
