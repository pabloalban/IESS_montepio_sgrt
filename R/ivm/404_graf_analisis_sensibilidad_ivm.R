message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tGraficando sensibilidad de la tasa actuarial' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

load( file = parametros$ivm_rdata_ana_sen )

#Graficando variación del déficit/superavit tras la modificación de la tasa actuarial --------------
message( '\tGraficando variación del déficit/superávit tras la modificación de la tasa actuarial' )

aux <- balance_anual_sens[ sens == 1, list( t, sim = as.factor( sim ), V ) ]

cols_fun <- colorRampPalette( c( 'red', parametros$iess_green, 'gold', parametros$iess_blue ) )
cols_graf <- cols_fun( length( unique( aux$sim ) ) )

x_lim <- c( min( aux$t ), max( aux$t ) )
x_brk <- seq( x_lim[1], x_lim[2], length.out = 9 )
x_lbl <- formatC( x_brk , digits = 0, decimal.mark = ',', big.mark = '.', format = 'f' )

y_lim <- c( min( aux$V ), max( aux$V ) )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk / 1e6, digits = 2, decimal.mark = ',', big.mark = '.', format = 'f' )

plt_sens <- ggplot( data = aux ) +
  geom_hline( yintercept = 0, colour = 'darkgreen' ) +
  geom_line( aes( x = t, y = V, colour = sim ), linewidth = graf_line_size ) +
  xlab( 'Tiempo' ) +
  ylab( 'Millones de USD' ) +
  scale_y_continuous( limits = y_lim, breaks = y_brk, labels = y_lbl ) +
  scale_x_continuous( limits = x_lim, breaks = x_brk, labels = x_lbl ) +
  scale_colour_manual( "",  
                       breaks = unique( aux$sim ),
                       values = cols_graf ) +
  plt_theme_legend

ggsave( plot = plt_sens, 
        filename = paste0( parametros$resultado_graficos, 'iess_bal_ana_sen_1', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Graficando el balance actuarial tras la modificación de la tasa actuarial -------------------------
message( '\tGraficando variación del déficit/superavit tras la modificación del crecimiento de salarios' )

aux <- balance_anual_sens[ sens == 2, list( t, sim = as.factor( sim ), V ) ]

cols_fun <- colorRampPalette( c( 'red', parametros$iess_green, 'gold', parametros$iess_blue ) )
cols_graf <- cols_fun( length( unique( aux$sim ) ) )

x_lim <- c( min( aux$t ), max( aux$t ) )
x_brk <- seq( x_lim[1], x_lim[2], length.out = 9 )
x_lbl <- formatC( x_brk , digits = 0, decimal.mark = ',', big.mark = '.', format = 'f' )

y_lim <- c( min( aux$V ), max( aux$V ) )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk / 1e6, digits = 2, decimal.mark = ',', big.mark = '.', format = 'f' )

plt_sens <- ggplot( data = aux ) +
  geom_hline( yintercept = 0, colour = 'darkgreen' ) +
  geom_line( aes( x = t, y = V, colour = sim ), linewidth = graf_line_size ) +
  xlab( 'Tiempo' ) +
  ylab( 'Millones de USD' ) +
  scale_y_continuous( limits = y_lim, breaks = y_brk, labels = y_lbl ) +
  scale_x_continuous( limits = x_lim, breaks = x_brk, labels = x_lbl ) +
  scale_colour_manual( "",  
                       breaks = unique( aux$sim ),
                       values = cols_graf ) +
  plt_theme_legend

ggsave( plot = plt_sens, 
        filename = paste0( parametros$resultado_graficos, 'iess_bal_ana_sen_2', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()
