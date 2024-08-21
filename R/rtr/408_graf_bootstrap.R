message( paste( rep('-', 100 ), collapse = '' ) )

#0. Carga de datos ---------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_bootstrap.RData' ) )

##0.1 Plantilla gráfica ----------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

#1.Gráfico Bootstrap de huerfanos-------------------------------------------------------------------
message( '\tGraficando porcentaje Bootstrap de huerfanos' )

## 1.1. Gráfico de afiliado-------------------------------------------------------------------------

probs <- c( 0.025, 0.975 )
quantiles <- quantile(muestreo_of$porc_a, prob=probs)
dent <- density(  adjust = 4,
                  muestreo_of$porc_a )
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
  geom_histogram( data = muestreo_of, 
                  aes( x = porc_a, y = after_stat( density ) ),   
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
  geom_vline( data = muestreo_of,
              aes( xintercept = mean( porc_a ),
                   color = "red" ),
              linetype = "dashed" )  +
  scale_y_continuous( name = 'densidad de probabilidades',
                      labels = y_lbl,
                      breaks = y_brk ) +
  scale_x_continuous( name = 'Porcentaje de montepíos de orfandad causados por afiliado fallecido',
                      labels = x_lbl,
                      breaks = x_brk ) 
  
ggsave( plot = iess_porc_a, 
        filename = paste0( parametros$resultado_graficos, 'iess_bootstrap_of_porc_a_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## 1.2. Gráfico Bootstrap jubilado------------------------------------------------------------------

probs <- c( 0.025, 0.975 )
quantiles <- quantile(muestreo_of$porc_j, prob=probs)
dent <- density(  adjust = 4,
                  muestreo_of$porc_j )
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
  geom_histogram( data = muestreo_of, 
                  aes( x = porc_j, y = after_stat( density ) ),   
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
  geom_vline( data = muestreo_of,
              aes( xintercept = mean( porc_j ),
                   color = "red" ),
              linetype = "dashed" )  +
  scale_y_continuous( name = 'densidad de probabilidades',
                      labels = y_lbl,
                      breaks = y_brk ) +
  scale_x_continuous( name = 'Porcentaje de montepíos de orfandad causados por jubilado fallecido',
                      labels = x_lbl,
                      breaks = x_brk ) 

ggsave( plot = iess_porc_j, 
        filename = paste0( parametros$resultado_graficos, 'iess_bootstrap_of_porc_j_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Limpiando Ram--------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
