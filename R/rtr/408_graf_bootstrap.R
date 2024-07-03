message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_bootstrap.RData' ) )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

#Portafolio de inversiones -------------------------------------------------------------------------
message( '\tGraficando evolución histórica de las inversiones en valor nominal' )




# Histogram overlaid with kernel density curve

probs <- c( 0.025, 0.975 )
quantiles <- quantile(muestreo$porc_A, prob=probs)
muestreo$quant <- factor(findInterval(muestreo$porc_A, quantiles))


ggplot(muestreo, aes(x=porc_A)) + 
  geom_histogram(aes(y=after_stat(density)),      # Histogram with density instead of count on y-axis
                 binwidth=.45,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666", adjust = 4) +# Overlay with transparent density plot
  scale_x_continuous(breaks=quantiles) + scale_fill_manual(values = c("red",
                                                                      "gray90",
                                                                      "red") )




dt <- data.frame(x=c(1:200),y=rnorm(200))
dens <- density(dt$y)
df <- data.frame(x=dens$x, y=dens$y)
probs <- c( 0.025, 0.975 )
quantiles <- quantile(dt$y, prob=probs)
df$quant <- factor(findInterval(df$x,quantiles))
ggplot(df, aes(x,y)) + geom_line() + 
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  scale_x_continuous(breaks=quantiles) + scale_fill_manual(values = c("red",
                                                                      "gray90",
                                                                      "red") )




ggplot(muestreo, aes(x=porc_A)) + 
  geom_histogram(aes(y=after_stat(density)),      # Histogram with density instead of count on y-axis
                 binwidth=.45,
                 colour="black", fill="white") +
  geom_ribbon(aes(ymin=0, ymax=porc_A, fill=quant)) +
  geom_density(color="darkblue",
               fill="lightblue",
               adjust = 4,
               alpha= 0.2 ) +
  theme_bw( ) +
  plt_theme+
  scale_x_continuous(breaks=quantiles) + scale_fill_manual(values = c("red",
                                                                      "gray90",
                                                                      "red") )

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
ggplot() + 
  geom_histogram(data = muestreo, aes(x=porc_A, y=after_stat(density) ),      # Histogram with density instead of count on y-axis
                 binwidth=.45,
                 colour="black", fill="white") +
  geom_line(data=dent, aes( x=x, y=y) ) +
  #geom_ribbon(data= base::subset(dent,dent$quant == '0'),aes(x=x,ymax=y),ymin=0, fill="green4",alpha=0.3)
geom_ribbon(data = dent, aes( x = x, y = y, ymin = 0, ymax=y, fill = quant,alpha=0.3)) + 
  #scale_x_continuous(breaks=quantiles) +
  scale_fill_manual(values = c("red",
                                                                      "gray90",
                                                                      "red") ) +
  theme_bw( ) +
  plt_theme +
  geom_vline( data = muestreo, aes( xintercept = mean( porc_A ), color = "red" ),
             linetype = "dashed" )
  
  
#Evolución histórica de las inversiones en préstamos quirografarios---------------------------------
message( '\tGraficando evolución histórica de las inversiones en préstamos quirografarios ' )
aux <- inv_instrumento %>%
  filter( instrumento == 'Créditos Quirografarios' ) %>% 
  dplyr::select(ano,
                instrumento,
                valor_nominal,
                rdto_prom_pond ) %>%
  mutate( periodo = ymd( paste0(ano, '/01/01') ) )

df_bar <- aux %>% dplyr::select(-rdto_prom_pond)
df_line = aux %>% dplyr::select(periodo, rdto_prom_pond)

scl = 10000000  # escala de millones
hmts = 130 #homotecia

y_lim <- c( 0, 750000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-seq(0.08,0.13,length.out = 6)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 1, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")


iess_prestamos_quirografarios <- ggplot(data = df_bar, 
                                        aes(x = periodo,
                                            y = valor_nominal,
                                            fill = instrumento)) +
  geom_bar(stat='identity',
           colour='black') +
  geom_line(data = df_line,
            aes(x = periodo,
                y = rdto_prom_pond*hmts*scl*10 - 1100000000,
                group = 1,
                linetype = 'Rendimiento Neto'),
            inherit.aes = FALSE,
            size=1 )  +
  scale_linetype_manual(NULL, values = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  scale_y_continuous(name = 'Saldo (millones USD)',
                     labels = y_lbl, breaks = y_brk, limits = y_lim,
                     sec.axis = sec_axis(~./(scl*hmts*10) + 0.08461538,
                                         name = 'Rendimiento Promedio Ponderado',
                                         labels = ydual_lbl,
                                         breaks = ydual_brk)) + 
  scale_fill_manual(values = c(parametros$iess_green,
                               parametros$iess_blue))+
  theme_bw() +
  plt_theme+
  guides(fill = guide_legend(title = NULL,
                             label.position = "right", 
                             label.hjust = 0))+
  theme(legend.position='bottom') +
  labs( x = '', y = '' )+
  theme(legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent', 
                                             colour = NA),
        legend.key = element_rect(fill = 'transparent'), 
        legend.spacing = unit(-1, 'lines') )

ggsave( plot = iess_prestamos_quirografarios, 
        filename = paste0( parametros$resultado_graficos, 'iess_prestamos_quirografarios_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )




#Limpiando Ram--------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
