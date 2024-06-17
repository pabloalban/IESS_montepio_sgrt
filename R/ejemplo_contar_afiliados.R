load( parametros$demo_rdata_sgo_tran )

# conteo al mes de diciembre de todos los años -----------------------------------------------------
# muestreo
colmn <- grep( "_12", names(sgo_act_tran), value = TRUE, fixed = TRUE ) 
muestra <- 100000
act_muestra <- sgo_act_tran[ sample( 1:nrow( sgo_act_tran), muestra, replace = FALSE ), 
                             .SD, .SDcols = colmn ]
alfa <- nrow( sgo_act_tran ) / muestra
conteo <- act_muestra[ , lapply( .SD, function(x) alfa * sum( x, na.rm = TRUE ) / 30 ) ]

# toda la base
colmn <- grep( "_12", names(sgo_act_tran), value = TRUE, fixed = TRUE ) 
act_muestra <- sgo_act_tran[ , .SD, .SDcols = colmn ]
conteo <- act_muestra[ , lapply( .SD, function(x) sum( x, na.rm = TRUE ) / 30 ) ]
conteo$M2018_12


# conteo en todos los meses de todos los años ------------------------------------------------------
# muestreo
muestra <- 10000
colmn <- grep( "M20", names(sgo_act_tran), value = TRUE, fixed = TRUE ) 
act_muestra <- sgo_act_tran[ sample( 1:nrow( sgo_act_tran), muestra, replace = FALSE ), 
                             .SD,
                             .SDcols = colmn ]
alfa <- nrow( sgo_act_tran ) / muestra
conteo <- act_muestra[ , lapply( .SD, function(x) alfa * sum( x, na.rm = TRUE ) / 30.5 ) ]

measurevars <- names( conteo )
conteo2 <- melt.data.table( conteo, measure.vars = measurevars )

# toda la base
colmn <- grep( "M20", names(sgo_act_tran), value = TRUE, fixed = TRUE ) 
act_muestra <- sgo_act_tran[ , .SD, .SDcols = colmn ]
conteo <- act_muestra[ , lapply( .SD, function(x) sum( x, na.rm = TRUE ) / 30.5 ) ]
measurevars <- names( conteo )
conteo2 <- melt.data.table( conteo, measure.vars = measurevars )
conteo2[ , anio := as.numeric( substr( variable, 2, 5 ) ) ]
conteo2[ , mes := as.numeric( sub( ".*_", "", variable ) ) ]
conteo2[ , fecha := ymd( paste0( anio, '-', mes, '-15' ), tz = parametros$time_zone ) ]

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

## Gráficos Afiliados SGO Activos ------------------------------------------------------------------
message( '\tGraficando población afiliada activa inicial SGO del IESS' )

x_lim <- c( min( conteo2$fecha ), max( conteo2$fecha ) )
x_brk <- seq( x_lim[ 1 ], x_lim[ 2 ], by = 'years' )
x_lbl <- format( x_brk, '%Y' )

y_lim <- c( 2000000, 3500000 )
y_brk <- seq( y_lim[ 1 ], y_lim[ 2 ], by = 500000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ', ' )

iess_er <- ggplot( data = conteo2, 
                   aes( x = fecha, y = value, group = anio, color = mes ) ) +
  geom_line( linewidth = 1, lineend = 'round' ) +
  geom_point( size = 2.5 ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_datetime( breaks = x_brk, labels = x_lbl ) +
  plt_theme_legend +
  labs( x = 'Año', y = 'Afiliados' ) +
  theme( axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ),
         legend.text = element_text( colour = 'black' ),
         legend.title = element_text( 'mes', colour = 'black' ) )


library( gganimate )

iess_er + transition_reveal( fecha )
