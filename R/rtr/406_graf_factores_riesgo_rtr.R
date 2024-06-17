message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Cargando datos -----------------------------------------------------------------------------------
load( parametros$demo_rdata_rtr_tasas_tran )
load( paste0( parametros$RData_seg, 'IESS_RTR_porcentaje_incapacidad_ajustado.RData' ) )

#1. Gráficos de Indemnizaciones---------------------------------------------------------------------
#1. 1. Gráfico del alisado de tasa de uso de las Indemnizaciones------------------------------------
#Hombres
message( '\tGraficando alisado de tasa de uso de las Indemnizaciones en hombres' )

aux <-  tas_2_13 %>% 
  filter( sexo == 'H' )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.0005 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- paste0(formatC( y_brk * 100, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


siniestra_indem_h <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = t_2_13, color = parametros$iess_green ),
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = t_2_13_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, p^{2,13}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = siniestra_indem_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_siniestra_indem_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Mujeres
message( '\tGraficando alisado de tasa de uso de las Indemnizaciones en mujeres' )

aux <-  tas_2_13 %>% 
  filter( sexo == 'M' )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.0002 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- paste0( formatC( y_brk * 100, digits = 3, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


siniestra_indem_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = t_2_13, color = parametros$iess_green ), 
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = t_2_13_int, color = parametros$iess_blue ), 
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, p^{2,13}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = siniestra_indem_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_siniestra_indem_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#1. 2. Gráfico del alisado porcentaje de incapacidad PP utilizado en indemnizaciones----------------
#Hombres
message( '\tGraficando alisado del porcentaje de incapacidad PP utilizado en indemnizaciones hombres' )

aux <- coef_incap_13 %>% 
  filter( sexo == 'H' )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- paste0( formatC( y_brk * 100, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')

porc_incap_pp_indem_h <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = coef_incap_13,
                   color = parametros$iess_green ),
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = coef_incap_13_int, 
                  color = parametros$iess_blue ), 
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\beta^{Indem}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green,
                                   parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl,
                     breaks = y_brk,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = porc_incap_pp_indem_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_porc_incap_pp_indem_h',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Mujeres
message( '\tGraficando alisado del porcentaje de incapacidad PP utilizado en indemnizaciones mujeres' )

aux <- coef_incap_13 %>% 
  filter( sexo == 'M' )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- paste0( formatC( y_brk * 100, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


porc_incap_pp_indem_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = coef_incap_13, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = coef_incap_13_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\beta^{Indem}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl,
                     breaks = y_brk,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = porc_incap_pp_indem_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_porc_incap_pp_indem_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#2.Gráficos de subsidios----------------------------------------------------------------------------
#2. 1. Gráficos del alisado de la tasa de uso de los subsidios--------------------------------------
#Hombres
message( '\tGraficando alisado de la tasa de uso de los subsidios en hombres' )

aux <- tas_2_14 %>% 
  filter( sexo == 'H' )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.01 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- paste0( formatC( y_brk * 100, digits = 1, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


siniestra_subs_h <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = t_2_14, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = t_2_14_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, p^{2,14}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl,
                     breaks = y_brk,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = siniestra_subs_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_siniestra_subs_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Mujeres
message( '\tGraficando alisadode la tasa de uso de los subsidios en mujeres' )

aux <- tas_2_14 %>% 
  filter( sexo == 'M' )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.0050 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- paste0( formatC( y_brk * 100, digits = 1, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


siniestra_subs_m <- ggplot( data = aux ) + 
  geom_point( aes( x = x, y = t_2_14, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = t_2_14_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, p^{2,14}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl,
                     breaks = y_brk,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = siniestra_subs_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_siniestra_subs_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#2. 2. Gráficos del alisado del porcentaje de incapacidad temporal----------------------------------
#Hombres
message( '\tGraficando alisado del porcentaje de incapacidad temporal en hombres' )

aux <- coef_incap_14 %>% 
  filter( sexo == 'H' )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0.4, 0.7 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- paste0( formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ), "%" )

porc_incap_subs_h <-ggplot( data = aux ) + 
  geom_point( aes( x = x, y = coef_incap_14, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = coef_incap_14_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\beta^{Subs}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl,
                     breaks = y_brk,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = porc_incap_subs_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_porc_incap_subs_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Mujeres
message( '\tGraficando alisado del porcentaje de incapacidad temporal en mujeres' )

aux <- coef_incap_14 %>% 
  filter( sexo == 'M' )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0.5, 0.8 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 7 )
y_lbl <- paste0( formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ), "%" )

porc_incap_subs_m <-ggplot( data = aux ) + 
  geom_point( aes( x = x, y = coef_incap_14, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = coef_incap_14_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\beta^{Subs}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = y_lbl,
                     breaks = y_brk,
                     limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = porc_incap_subs_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_porc_incap_subs_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#2. 3. Gráficos del alisado de la duración de subsidios por incapacidad temporal--------------------
#Hombres
message( '\tGraficando alisado de la duración de subsidios por incapacidad temporal en hombres' )

aux <- dias_sub_14 %>%
  filter(sexo=='H')

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 60 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 7 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


dura_incap_subs_h <-ggplot( data = aux ) + 
  geom_point( aes( x = x, y = dias_sub_14, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = dias_sub_14_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\,días^{Subs}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = dura_incap_subs_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_dura_incap_subs_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Mujeres
message( '\tGraficando alisado de la duración de subsidios por incapacidad temporal en mujeres' )

aux <- dias_sub_14 %>%
  filter(sexo=='M')

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 10, 30 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


dura_incap_subs_m <- ggplot( data = aux ) + 
  geom_point( aes( x = x, y = dias_sub_14, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = dias_sub_14_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\,días^{Subs}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = dura_incap_subs_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_dura_incap_subs_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#3 .Gráficos de factores de riesgo en pensionistas de SGRT------------------------------------------

#3.1. Gráficos de entrada de pensionistas-----------------------------------------------------------

message( '\tGraficando alisado de la probabilidad de entrada de pensionistas del SGRT' )

#Hombres

aux <-  tas_2_12 %>% 
  filter( sexo == 'H' )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.0002 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- paste0(formatC( y_brk * 100, digits = 3, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')

ent_incap_rt_h <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = t_2_12, color = parametros$iess_green ),
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = t_2_12_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\,p^{2,12}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = ent_incap_rt_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_ent_incap_rt_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Mujeres

aux <-  tas_2_12 %>% 
  filter( sexo == 'M' )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.00002 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- paste0(formatC( y_brk * 100, digits = 4, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')

ent_incap_rt_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = t_2_12, color = parametros$iess_green ),
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = t_2_12_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\,p^{2,12}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = ent_incap_rt_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_ent_incap_rt_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#3.2. Gráficos de mortalidad de pensionistas--------------------------------------------------------

message( '\tGraficando alisado de la probabilidad de mortalidad de pensionistas del SGRT' )

#Hombres

aux <-  tas_12_6 %>% 
  filter( sexo == 'H' )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- paste0(formatC( y_brk * 100, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


mort_incap_rt_h <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = t_12_6, color = parametros$iess_green ),
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = t_12_6_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\,p^{12,6}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = mort_incap_rt_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_mort_incap_rt_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Mujeres

aux <-  tas_12_6 %>% 
  filter( sexo == 'M' )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- paste0(formatC( y_brk * 100, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


mort_incap_rt_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = t_12_6, color = parametros$iess_green ),
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = t_12_6_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\,p^{12,6}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = mort_incap_rt_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_mort_incap_rt_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#4 .Gráficos de factores de riesgo en montepío de orfandad------------------------------------------

#4.1 .Gráficos de entradas de montepío de orfandad--------------------------------------------------

message( '\tGraficando tasa de entrada de orfandad' )

#Hombres

aux <-  tas_0_15 %>% 
  filter( sexo == 'H' )

x_lim <- c( 0, 18 )
x_brk <- seq( x_lim[1], x_lim[2], 2 )
x_lbl <- x_brk

y_lim <- c( 0, 0.008 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- paste0(formatC( y_brk * 100, digits = 3, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


ent_orf_rt_h <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = t_0_15, color = parametros$iess_green ),
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = t_0_15_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\,p^{0,15}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = ent_orf_rt_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_ent_orf_rt_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Mujeres
aux <-  tas_0_15 %>% 
  filter( sexo == 'M' )

x_lim <- c( 0, 18 )
x_brk <- seq( x_lim[1], x_lim[2], 2 )
x_lbl <- x_brk

y_lim <- c( 0, 0.015 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- paste0(formatC( y_brk * 100, digits = 3, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


ent_orf_rt_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = t_0_15, color = parametros$iess_green ),
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = t_0_15_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\,p^{0,15}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = ent_orf_rt_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_ent_orf_rt_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#4.2 .Gráficos de mortalidad de montepío de orfandad------------------------------------------------

message( '\tGraficando de mortalidad de orfandad' )

#Hombres

aux <-  tas_15_0 %>% 
  filter( sexo == 'H' )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- paste0(formatC( y_brk * 100, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


salidas_orf_rt_h <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = t_15_0, color = parametros$iess_green ),
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = t_15_0_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\,p^{15,0}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = salidas_orf_rt_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_salidas_orf_rt_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Mujeres
aux <-  tas_15_0 %>% 
  filter( sexo == 'M' )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- paste0(formatC( y_brk * 100, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


salidas_orf_rt_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = t_15_0, color = parametros$iess_green ),
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = t_15_0_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\,p^{15,0}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = salidas_orf_rt_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_salidas_orf_rt_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#5 Gráficos de la mortalidad de pensionistas de viudez del SGRT-------------------------------------

#5.1 Gráficos de entradas de pensionistas de viudez del SGRT----------------------------------------

message( '\tGraficando alisado de la probabilidad de entrada de pensionistas de viudez del SGRT' )

#Hombres
aux <-  tas_0_16 %>% 
  filter( sexo == 'H' )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.00002 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- paste0(formatC( y_brk * 100, digits = 4, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


ent_viudez_rt_h <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = t_0_16, color = parametros$iess_green ),
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = t_0_16_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\,p^{0,16}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = ent_viudez_rt_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_ent_viudez_rt_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Mujeres

aux <-  tas_0_16 %>% 
  filter( sexo == 'M' )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.002 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- paste0(formatC( y_brk * 100, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


ent_viudez_rt_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = t_0_16, color = parametros$iess_green ),
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = t_0_16_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\,p^{0,16}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = ent_viudez_rt_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_ent_viudez_rt_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#5.2 Gráficos de entradas de pensionistas de viudez del SGRT----------------------------------------

message( '\tGraficando alisado de la probabilidad de entrada de pensionistas de viudez del SGRT' )

#Hombres
aux <-  tas_16_0 %>% 
  filter( sexo == 'H' )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- paste0(formatC( y_brk * 100, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


mort_viudez_rt_h <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = t_16_0, color = parametros$iess_green ),
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = t_16_0_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\,p^{16,6}_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = mort_viudez_rt_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_mort_viudez_rt_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Mujeres
aux <-  tas_16_0 %>% 
  filter( sexo == 'M' )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- paste0(formatC( y_brk * 100, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ), '%')


mort_viudez_rt_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = t_16_0, color = parametros$iess_green ),
              size = graf_point_size ) + 
  geom_line( aes( x = x, y = t_16_0_int, color = parametros$iess_blue ),
             size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\,p^{16,6}_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels =y_lbl,limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = mort_viudez_rt_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_mort_viudez_rt_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#6. Agrupar en un solo gráfico------------------------------------------------------------------------
factor_riesgo_1 <- marrangeGrob( list( ent_incap_rt_m,
                                       ent_incap_rt_h,
                                       mort_incap_rt_m, 
                                       mort_incap_rt_h ), 
                                 nrow = 2, ncol = 2, top = '' )

ggsave( plot = factor_riesgo_1,
        filename = paste0( parametros$resultado_graficos, 
                           'iess_factor_riesgo_1', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )


factor_riesgo_2 <- marrangeGrob( list( siniestra_indem_m, 
                                       siniestra_indem_h,
                                       porc_incap_pp_indem_m,
                                       porc_incap_pp_indem_h ),
                                 nrow = 2, ncol = 2, top = '' )

ggsave( plot = factor_riesgo_2, 
        filename = paste0( parametros$resultado_graficos, 
                           'iess_factor_riesgo_2', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )

factor_riesgo_3 <- marrangeGrob( list( siniestra_subs_m, 
                                       siniestra_subs_h,
                                       porc_incap_subs_m,
                                       porc_incap_subs_h,
                                       dura_incap_subs_m,
                                       dura_incap_subs_h ),
                                 nrow = 2, ncol = 3, top = '' )

ggsave( plot = factor_riesgo_3, 
        filename = paste0( parametros$resultado_graficos, 
                           'iess_factor_riesgo_3', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )


factor_riesgo_4 <- marrangeGrob( list( ent_orf_rt_m,
                                       ent_orf_rt_h,
                                       salidas_orf_rt_m, 
                                       salidas_orf_rt_h ), 
                                 nrow = 2, ncol = 2, top = '' )

ggsave( plot = factor_riesgo_4,
        filename = paste0( parametros$resultado_graficos, 
                           'iess_factor_riesgo_4', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )


factor_riesgo_5 <- marrangeGrob( list( ent_viudez_rt_m,
                                       ent_viudez_rt_h,
                                       mort_viudez_rt_m,
                                       mort_viudez_rt_h ), 
                                 nrow = 2, ncol = 2, top = '' )

ggsave( plot = factor_riesgo_5,
        filename = paste0( parametros$resultado_graficos, 
                           'iess_factor_riesgo_5', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )
#Limpiar memoria RAM--------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()