message( paste( rep('-', 100 ), collapse = '' ) )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_bootstrap.RData' ) )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

#Gráfico Bootstrap----------------------------------------------------------------------------------
message( '\tGraficando porcentaje Bootstrap afiliado' )

probs <- c( 0.025, 0.975 )
quantiles <- quantile(muestreo$porc_A, prob=probs)
dent <- density(  adjust = 4,
                  muestreo$porc_A )
dent <- data.frame( x = dent$x, y = dent$y ) %>% 
  mutate( quant = if_else( x < quantiles[1],
                           '0',
                           if_else( x < quantiles[2],
                                    '1',
                                    '2' ) ) )


y_brk <- seq( 0, 1, 0.2 )
y_lbl <- paste0( formatC( y_brk * 100,
                          digits = 0, 
                          format = 'f', 
                          big.mark = '.', 
                          decimal.mark = ',' ), "%" )


x_brk <- seq( 90, 96, 1 )
x_lbl <- paste0( formatC( x_brk, 
                          digits = 0, 
                          format = 'f', 
                          big.mark = '.', 
                          decimal.mark = ',' ), "%" )

iess_porc_a <- ggplot( ) + 
  geom_histogram( data = muestreo, 
                  aes( x = porc_A, y = after_stat( density ) ),   
                  binwidth = .45,
                  colour = "black",
                  fill = "white" ) +
  geom_line( data = dent, aes( x = x, y = y ) ) +
  geom_ribbon( data = dent, aes( x = x, y = y, ymin = 0, ymax = y, fill = quant, alpha = 0.3 ) ) + 
  scale_fill_manual( values = c( "red",
                                 "gray90",
                                 "red" ) ) +
  theme_bw( ) +
  plt_theme +
  geom_vline( data = muestreo,
              aes( xintercept = mean( porc_A ),
                   color = "red" ),
              linetype = "dashed" )  +
  scale_y_continuous( name = 'densidad de probabilidades',
                      labels = y_lbl,
                      breaks = y_brk ) +
  scale_x_continuous( name = 'Porcentaje de montepío causados por afiliado fallecido',
                      labels = x_lbl,
                      breaks = x_brk ) 
  
ggsave( plot = iess_porc_a, 
        filename = paste0( parametros$resultado_graficos, 'iess_porc_a_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Gráfico Bootstrap jubilado-------------------------------------------------------------------------
message( '\tGraficando porcentaje Bootstrap jubilado' )

probs <- c( 0.025, 0.975 )
quantiles <- quantile(muestreo$porc_J, prob=probs)
dent <- density(  adjust = 4,
                  muestreo$porc_J )
dent <- data.frame( x = dent$x, y = dent$y ) %>% 
  mutate( quant = if_else( x < quantiles[1],
                           '0',
                           if_else( x < quantiles[2],
                                    '1',
                                    '2' ) ) )


y_brk <- seq( 0, 1, 0.2 )
y_lbl <- paste0( formatC( y_brk * 100,
                          digits = 0, 
                          format = 'f', 
                          big.mark = '.', 
                          decimal.mark = ',' ), "%" )


x_brk <- seq( 2, 12, 1 )
x_lbl <- paste0( formatC( x_brk, 
                          digits = 0, 
                          format = 'f', 
                          big.mark = '.', 
                          decimal.mark = ',' ), "%" )

iess_porc_j <- ggplot( ) + 
  geom_histogram( data = muestreo, 
                  aes( x = porc_J, y = after_stat( density ) ),   
                  binwidth = .45,
                  colour = "black",
                  fill = "white" ) +
  geom_line( data = dent, aes( x = x, y = y ) ) +
  geom_ribbon( data = dent, aes( x = x, y = y, ymin = 0, ymax = y, fill = quant, alpha = 0.3 ) ) + 
  scale_fill_manual( values = c( "red",
                                 "gray90",
                                 "red" ) ) +
  theme_bw( ) +
  plt_theme +
  geom_vline( data = muestreo,
              aes( xintercept = mean( porc_J ),
                   color = "red" ),
              linetype = "dashed" )  +
  scale_y_continuous( name = 'densidad de probabilidades',
                      labels = y_lbl,
                      breaks = y_brk ) +
  scale_x_continuous( name = 'Porcentaje de montepío causados por jubilado fallecido',
                      labels = x_lbl,
                      breaks = x_brk ) 

ggsave( plot = iess_porc_j, 
        filename = paste0( parametros$resultado_graficos, 'iess_porc_j_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Limpiando Ram--------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
