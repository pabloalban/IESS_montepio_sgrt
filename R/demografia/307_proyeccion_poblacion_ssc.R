message( paste( rep('-', 100 ), collapse = '' ) )

# Descripción estados ------------------------------------------------------------------------------
# 1 = Individuos en la PEA no activos
# 2 = Activos cotizantes
# 3 = Activos cesantes
# 4 = Pensionistas de vejez
# 5 = Pensionistas de invalidez
# 6 = Muertos
# 7 = Pensionistas de montepío viudas
# 8 = Pensionistas de montepío huérfanos
# 9 = Dependientes cónyuges
# 10 = Dependientes hijos
# 11 = Dependientes hijos menores de 18 años

# Cargando información -----------------------------------------------------------------------------
message( '\tCargando datos' )
load( parametros$demo_rdata_ssc_tasas_tran )
load( parametros$demo_rdata_ssc_probs_tran ) 
load( parametros$demo_rdata_ssc_din_dec )
load( parametros$demo_rdata_sgo_pea_proj )
load( parametros$demo_rdata_ssc_pob_ini )
#Proyección población SGO
load( file = parametros$demo_rdata_sgo_pob_proy  )

# Borrando variables, solo quedan variables a ser utilizadas
vars <- c( 'parametros', 'onu_pea_tot_int', 'l0', 'l0xs', 'Ph', 'Pm', 
           'tas_7_0', 'tas_8_0', 'tas_7_6', 'tas_8_6', 
           'tas_din_dec', 'PEA_proy', 'fer_dat', 'nup_dat', 'vars', 'pob_proy' )
rm( list = ls()[ !( ls() %in% vars ) ] )

# Parámetros ---------------------------------------------------------------------------------------
# Tiempo
t_min <- 0
t_max <- parametros$ssc_horizonte # horizonte de proyección
t_lst <- seq( t_min, t_max, 1 )

# Año inicial de proyección
anio_ini <- parametros$anio_ini

# Año final de proyección
anio_fin <- anio_ini + t_max

# Tiempos de servicio
s_min <- 0
s_max <- parametros$demo_ts_max   #Analizar este valor en el ssc
s_lst <- seq( s_min, s_max, 1 ) 

# Edades
x_min <- 0
x_max <- parametros$demo_edad_max
x_lst <- seq( x_min, x_max, 1 )

nt <- length( t_lst ) # 21 años horizonte
ns <- length( s_lst ) # 71 imposiciones máximo
nx <- length( x_lst ) # 106 añs edad

# Dimensión del número de estados 
nd <- 6 

# Proyección de activos y pensionistas -------------------------------------------------------------
message( '\tProyectando de activos y pensionistas' )

## Factores de crecimiento de la población cotizante del SGO ---------------------------------------
pob_proy_sgo <- copy( pob_proy )

aux <- pob_proy_sgo[ , list( l2 = sum( l2, na.rm = TRUE ) ), by = list( t ) ]
setorder( aux, t )
aux[ , tc := ( l2 - shift( l2 ) ) / shift( l2 ) ]
aux[ l2 == 0, tc := 0 ]
aux[ tc < 0, tc := 0 ]
aux[ is.na( tc ), tc := 0 ]
sgo_crec <- aux$tc
rm( aux )

## Preparación de la PEA ---------------------------------------------------------------------------
message( '\tPreparando población económicamente activa rural' )

### PEA hombres ------------------------------------------------------------------------------------
PEA_h <- data.table( expand_grid( anio = t_lst + anio_ini, x = x_lst ) )
PEA_h <- merge.data.table( 
  PEA_h, 
  PEA_proy[ anio >= parametros$anio_ini & sexo == 'H', list( anio, x, peax = peax_r ) ],
  by = c( 'anio', 'x' ), all.x = TRUE )
PEA_h[ is.na( peax ), peax := 0 ]

PEA_h <- dcast.data.table( data = PEA_h, anio ~ x, value.var = 'peax' )
setorder( PEA_h, anio )
PEA_h[ , anio := NULL ]
PEA_h <- as.matrix( PEA_h )

### PEA mujeres ------------------------------------------------------------------------------------
PEA_m <- data.table( expand_grid( anio = t_lst + anio_ini, x = x_lst ) )
PEA_m <- merge.data.table( 
  PEA_m, 
  PEA_proy[ anio >= parametros$anio_ini & sexo == 'M', list( anio, x, peax = peax_r ) ],
  by = c( 'anio', 'x' ), all.x = TRUE )
PEA_m[ is.na( peax ), peax := 0 ]

PEA_m <- dcast.data.table( data = PEA_m, anio ~ x, value.var = 'peax' )
setorder( PEA_m, anio )
PEA_m[ , anio := NULL ]
PEA_m <- as.matrix( PEA_m )

## Población inicial -------------------------------------------------------------------------------
message( '\tPreparando población inicial' )

# Arrays para la población inicial
l0_h <- array( 0.0, dim = c( ns, nx, nd + 2 ) )
l0_m <- array( 0.0, dim = c( ns, nx, nd + 2 ) )

for ( sx in c( 'H', 'M' ) ) { # sx <- 'H'
  for ( i in 1:ns ) { # i <- 1
    
    l0_i <- l0xs[ sexo == sx & s == s_lst[ i ] ] #Filtro por cada imposición y sexo
    setorder( l0_i, x )
    
    if ( sx == 'H' ) {
      l0_h[ i, , ] <- as.matrix( l0_i[ , list( l1, l2, l3, l4, l5, l6 = 0, l7, l8 ) ] ) # Modifica a toda la fila en toda las capas
    } else {
      l0_m[ i, , ] <- as.matrix( l0_i[ , list( l1, l2, l3, l4, l5, l6 = 0, l7, l8 ) ] )
    }
  }
}

## Proyección población activos, inactivos y pensionistas-------------------------------------------
message( '\tProyectando población' )

# Arrays para población
lm <- array( 0.0, dim = c( nt, ns, nx, nd ) ) # (horizonte, imposiciones, edad, dimensiones=6)
lh <- array( 0.0, dim = c( nt, ns, nx, nd ) )

# Conteos de transición
ltm <- array( 0.0, dim = c( nt, ns, nx, nd * nd + 1 ) ) #Porque multiplicación?
lth <- array( 0.0, dim = c( nt, ns, nx, nd * nd + 1 ) )

lm[ 1, , , ] <- l0_m[ , , 1:nd ] #Llama a todas las capas
lh[ 1, , , ] <- l0_h[ , , 1:nd ]

for ( t in 1:( nt - 1 ) ) { # t <- 1
  for ( x in 1:( nx - 1 ) ) { # x <- 1
    for ( s in 1:( ns - 1 ) ) { # s <- 1
      
      #Crecimiento jefes
      Pm_tsx <- Pm[ t, s, x, , ]
      
      #Crecimiento Invalidez
      Ph_tsx <- Ph[ t, s, x, , ]
      
      Lm_tsx <- t( Pm_tsx * lm[ t, s, x, ] )
      Lh_tsx <- t( Ph_tsx * lh[ t, s, x, ] ) #Multiplicación a nivel de columna
      
      # ( 1 ) Los del estado 1 no aumenta su tiempo de servicio
      lm[ t + 1, 1, x + 1, 1 ] <- lm[ t + 1, 1, x + 1, 1 ] + Lm_tsx[ 1, 1 ]
      lh[ t + 1, 1, x + 1, 1 ] <- lh[ t + 1, 1, x + 1, 1 ] + Lh_tsx[ 1, 1 ]
      
      # ( 2 ) Si aumenta el tiempo de servicio en activos 2
      # Los del estado 1 ingresa con 0 tiempo de servicio en 2
      if ( s == 1 ) {
        
        lm[ t + 1, 1, x + 1, 2 ] <- Lm_tsx[ 2, 1 ]
        lh[ t + 1, 1, x + 1, 2 ] <- Lh_tsx[ 2, 1 ]
        
        lm[ t + 1, s + 1, x + 1, 2 ] <- sum( Lm_tsx[ 2, 2:nd ] )
        lh[ t + 1, s + 1, x + 1, 2 ] <- sum( Lh_tsx[ 2, 2:nd ] )
        
      } else {
        
        lm[ t + 1, s + 1, x + 1, 2 ] <- sum( Lm_tsx[ 2, 1:nd ] )
        lh[ t + 1, s + 1, x + 1, 2 ] <- sum( Lh_tsx[ 2, 1:nd ] )
        
      }
      
      # ( 3 ) No aumenta el tiempo de servicio en cesantes/inactivos 3
      lm[ t + 1, s, x + 1, 3 ] <- sum( Lm_tsx[ 3, 1:nd ] )
      lh[ t + 1, s, x + 1, 3 ] <- sum( Lh_tsx[ 3, 1:nd ] )
      
      # ( 4 ) No aumenta el tiempo de servicio en pensionistas de vejez 4
      lm[ t + 1, s, x + 1, 4 ] <- sum( Lm_tsx[ 4, 1:nd ] )
      lh[ t + 1, s, x + 1, 4 ] <- sum( Lh_tsx[ 4, 1:nd ] )
      
      # ( 5 ) No aumenta el tiempo de servicio en pensionistas de invalidez 5
      lm[ t + 1, s, x + 1, 5 ] <- sum( Lm_tsx[ 5, 1:nd ] )
      lh[ t + 1, s, x + 1, 5 ] <- sum( Lh_tsx[ 5, 1:nd ] )
      
      # ( 6 ) No aumenta el tiempo de servicio, ni edad en muertos 6
      lm[ t + 1, s, x, 6 ] <- sum( Lm_tsx[ 6, 1:nd ] )
      lh[ t + 1, s, x, 6 ] <- sum( Lh_tsx[ 6, 1:nd ] )
      
      # Conteos de transiciones
      for ( u in 1:nd ) {
        for ( v in 1:nd ) {
          
          if ( u == 1 & v == 1 ) {
            
            ltm[ t + 1, 1, x, nd * ( v - 1 ) + u ] <- ltm[ t, 1, x, nd * ( v - 1 ) + u ] + Lm_tsx[ u, v ]
            lth[ t + 1, 1, x, nd * ( v - 1 ) + u ] <- lth[ t, 1, x, nd * ( v - 1 ) + u ] + Lh_tsx[ u, v ]
            
          } else {
            
            ltm[ t + 1, s, x, nd * ( v - 1 ) + u ] <- Lm_tsx[ u, v ]
            lth[ t + 1, s, x, nd * ( v - 1 ) + u ] <- Lh_tsx[ u, v ]
            
          }
        }
      }
    }
    
    # Inclusión de nuevos ingreos a partir de la proyección de la PEA
    ltm[ t + 1, 1, x, nd * nd + 1 ] <- max( PEA_m[ t + 1, x ] - sum( lm[ t + 1, , x, 1:3 ] ), 0 )
    lth[ t + 1, 1, x, nd * nd + 1 ] <- max( PEA_h[ t + 1, x ] - sum( lh[ t + 1, , x, 1:3 ] ), 0 )
    
    lm[ t + 1, 1, x, 1 ] <- lm[ t + 1, 1, x, 1 ] + ltm[ t + 1, 1, x, nd * nd + 1 ]
    lh[ t + 1, 1, x, 1 ] <- lh[ t + 1, 1, x, 1 ] + lth[ t + 1, 1, x, nd * nd + 1 ]
  }
  
  # Inclusión de nuevos ingreos a partir de la proyección de la PEA
  ltm[ t + 1, 1, nx, nd * nd + 1 ] <- max( PEA_m[ t + 1, nx ] - sum( lm[ t + 1, , nx, 1:3 ] ), 0 )
  lth[ t + 1, 1, nx, nd * nd + 1 ] <- max( PEA_h[ t + 1, nx ] - sum( lh[ t + 1, , nx, 1:3 ] ), 0 )
  
  lm[ t + 1, 1, nx, 1 ] <- lm[ t + 1, 1, nx, 1 ] + ltm[ t + 1, 1, nx, nd * nd + 1 ]
  lh[ t + 1, 1, nx, 1 ] <- lh[ t + 1, 1, nx, 1 ] + lth[ t + 1, 1, nx, nd * nd + 1 ]
}
rm( t, x, s )

## Transformación proyección a data.table ----------------------------------------------------------
message( '\tCreando data.table con población proyectada y conteos de transición' )
pob_proy_ts <- NULL
for ( t in 1:nt ) { # t <- 1
  plx <- NULL
  for ( s in 1:ns ) { # s <- 1
    
    lx <- rbind( lm[ t, s, , ], lh[ t, s, , ] )
    ltx <- rbind( ltm[ t, s, , ], lth[ t, s, , ] )
    
    lx <- data.table( t = t_lst[ t ],
                      s = s_lst[ s ],
                      sexo = rep( c( 'M', 'H' ), each = nx ),
                      x = rep( x_lst, 2 ),
                      l = lx,
                      lt = ltx )
    # cat( '\r', t, ', ', s, ', ', nrow( lx )  )
    setnames( lx,
              c( 't', 's', 'sexo', 'x',
                 paste0( 'l', 1:nd ),
                 paste0( 'l', as.vector( sapply( 1:nd, FUN = function( i ) paste0( i, '_', 1:nd ) ) ) ),
                 'l0_1'
              )
    )
    plx <- rbindlist( list( plx, lx ) )
  }
  pob_proy_ts <- rbindlist( list( pob_proy_ts, plx ) )
}
rm( plx, lx, ltx )

# Sumarizando por tiempo t, sexo y edad x
cols <- names( pob_proy_ts )
cols <- cols[ !( cols %in% c( 't', 's', 'sexo', 'x' ) ) ]
pob_proy <- pob_proy_ts[ , lapply( .SD, sum ), .SDcols = cols, by = list( t, sexo, x ) ]
setorder( pob_proy, t, sexo, x )

# pob_proy[ , list(l2=sum(l2,na.rm=T), l5=sum(l5,na.rm=T)), by=t]
# pob_proy[ , list(l2=sum(l2,na.rm=T), l5=sum(l5,na.rm=T)), by=list(t,sexo)]
# 
# a <- pob_proy[ , list(l2=sum(l2,na.rm=T)), by=t]
# a[ , l2_s:=shift( l2,1,type=c('lag'))]
# a[ , vssc:=(l2/l2_s-1)*100]
# a[ , vssg:=sgo_crec[1:21]*100]
# a
# plot(c(-8,-7,-6,-5,-4,-3,-2,-1,a$t),
#      c(190204, 232129, 256389,280384,300975, 313790, 318048, 334754, 343803, a$l2[-1] ),
#      ylim=c(100000, 700000))
# grid()

# Proyección de pensionistas viudas y huérfanos ----------------------------------------------------
message( '\tProyectando pensionistas de montepío, viudas y huérfanos' )

## Carga modelo de fertilidad ----------------------------------------------------------------------
load( parametros$demo_rdata_inec_fert_model )

## Anualidades de hijos ----------------------------------------------------------------------------
factor <- 1.0
ann_hij <- data.table( expand.grid(
  x = x_lst,
  z = x_lst,
  sexo = c( 'H', 'M' ),
  sexo_dep = c( 'H', 'M' ) ) )

ann_hij <- merge.data.table(
  ann_hij,
  fer_dat[ , list( x, z = y, sexo, sexo_dep, qh = pmin( factor * q, 1 ) ) ],
  by = c( 'x', 'z', 'sexo', 'sexo_dep' ),
  all.x = TRUE )
ann_hij[ is.na( qh ), qh := 0 ]

ann_hij_h_h <- dcast.data.table(
  data = ann_hij[ sexo_dep == 'H' & sexo == 'H' ],
  formula = z ~ x,
  value.var = 'qh',
  fill = 0 )
setorder( ann_hij_h_h, z )
ann_hij_h_h[ , z := NULL ]
ann_hij_h_h <- as.matrix( ann_hij_h_h )

ann_hij_m_h <- dcast.data.table(
  data = ann_hij[ sexo_dep == 'M' & sexo == 'H' ],
  formula = z ~ x,
  value.var = 'qh',
  fill = 0 )
setorder( ann_hij_m_h, z )
ann_hij_m_h[ , z := NULL ]
ann_hij_m_h <- as.matrix( ann_hij_m_h )

ann_hij_h_m <- dcast.data.table(
  data = ann_hij[ sexo_dep == 'H' & sexo == 'M' ],
  formula = z ~ x,
  value.var = 'qh',
  fill = 0 )
setorder( ann_hij_h_m, z )
ann_hij_h_m[ , z := NULL ]
ann_hij_h_m <- as.matrix( ann_hij_h_m )

ann_hij_m_m <- dcast.data.table(
  data = ann_hij[ sexo_dep == 'M' & sexo == 'M' ],
  formula = z ~ x,
  value.var = 'qh',
  fill = 0 )
setorder( ann_hij_m_m, z )
ann_hij_m_m[ , z := NULL ]
ann_hij_m_m <- as.matrix( ann_hij_m_m )

## Anualidades de cónyuges -------------------------------------------------------------------------
factor <- 1.5
ann_cony <- data.table( expand.grid(
  x = x_lst,
  y = x_lst
))
ann_cony <- merge.data.table(
  ann_cony,
  nup_dat[ x <= x_max & y <= x_max, list( x, y, qc = pmin( factor * q, 1 ) ) ],
  by = c( 'x', 'y' ),
  all.x = TRUE )
ann_cony[ is.na( qc ), qc := 0 ]

ann_cony <- dcast.data.table(
  data = ann_cony,
  formula = y ~ x,
  value.var = 'qc',
  fill = 0 )
setorder( ann_cony, y )
ann_cony[ , y := NULL ]
ann_cony <- as.matrix( ann_cony )

## Preparando probabilidades de salida -------------------------------------------------------------
message( '\tPreparando probabilidades de salida para montepios' )
p7h <- tas_7_0[ sexo == 'H', list( x, px ) ]
p7h <- merge.data.table( data.table( x = x_lst ), p7h, all.x = TRUE )
p7h[ is.na( px ), px := 0 ]
setorder( p7h, x )
p7h <- as.matrix( p7h[, 2 ] )

p7m <- tas_7_0[ sexo == 'M', list( x, px ) ]
p7m <- merge.data.table( data.table( x = x_lst ), p7m, all.x = TRUE )
p7m[ is.na( px ), px := 0 ]
setorder( p7m, x )
p7m <- as.matrix( p7m[, 2 ] )

p8h <- tas_8_0[ sexo == 'H', list( x, px ) ]
p8h <- merge.data.table( data.table( x = x_lst ), p8h, all.x = TRUE )
p8h[ is.na( px ), px := 0 ]
setorder( p8h, x )
p8h <- as.matrix( p8h[, 2 ] )

p8m <- tas_8_0[ sexo == 'M', list( x, px ) ]
p8m <- merge.data.table( data.table( x = x_lst ), p8m, all.x = TRUE )
p8m[ is.na( px ), px := 0 ]
setorder( p8m, x )
p8m <- as.matrix( p8m[, 2 ] )

p7_6h <- tas_7_6[ sexo == 'H', list( x, px ) ]
p7_6h <- merge.data.table( data.table( x = x_lst ), p7_6h, all.x = TRUE )
p7_6h[ is.na( px ), px := 0 ]
setorder( p7_6h, x )
p7_6h <- as.matrix( p7_6h[, 2 ] )

p7_6m <- tas_7_6[ sexo == 'M', list( x, px ) ]
p7_6m <- merge.data.table( data.table( x = x_lst ), p7_6m, all.x = TRUE )
p7_6m[ is.na( px ), px := 0 ]
setorder( p7_6m, x )
p7_6m <- as.matrix( p7_6m[, 2 ] )

p8_6h <- tas_8_6[ sexo == 'H', list( x, px ) ]
p8_6h <- merge.data.table( data.table( x = x_lst ), p8_6h, all.x = TRUE )
p8_6h[ is.na( px ), px := 0 ]
setorder( p8_6h, x )
p8_6h <- as.matrix( p8_6h[, 2 ] )

p8_6m <- tas_8_6[ sexo == 'M', list( x, px ) ]
p8_6m <- merge.data.table( data.table( x = x_lst ), p8_6m, all.x = TRUE )
p8_6m[ is.na( px ), px := 0 ]
setorder( p8_6m, x )
p8_6m <- as.matrix( p8_6m[, 2 ] )

## Preparando población inicial y objetos para proyección ------------------------------------------
message( '\tEstableciendo población inicial para montepios' )

l0_mont_h <- l0xs[ sexo == 'H', list( l7 = sum( l7 ), l8 = sum( l8 ) ), by = list( x ) ]
setorder( l0_mont_h, x )
l0_mont_h <- as.matrix( l0_mont_h[, list( l7, l8 ) ] )

l0_mont_m <- l0xs[ sexo == 'M', list( l7 = sum( l7 ), l8 = sum( l8 ) ), by = list( x ) ]
setorder( l0_mont_m, x )
l0_mont_m <- as.matrix( l0_mont_m[, list( l7, l8 ) ] )

## Proyección de población de montepios ------------------------------------------------------------
message( '\tProyección de población de montepios' )

factor_hij <- c( 1.0, 0.2, rep( 0.1, nt - 2 ) )
factor_con <- c( 1.2, 0.1, rep( 0.1, nt - 2 ) )

# Arrays para la población inicial. Dimensiones: t, x, estado
lm_mont <- array( 0.0, dim = c( nt, nx, 12 ) )
lh_mont <- array( 0.0, dim = c( nt, nx, 12 ) )

lm_mont[ 1, , 1:2 ] <- l0_mont_m
lh_mont[ 1, , 1:2 ] <- l0_mont_h

for ( t in 1:( nt - 1 ) ) { # t <- 1
  
  # Por muertes de activos, con tiempo de servicio mayor a 5 años
  u <- 6
  v <- 2
  vm <- matrix( colSums( ltm[ t + 1, 5:ns, , nd * ( v - 1 ) + u ] ), nx, 1 )
  vh <- matrix( colSums( lth[ t + 1, 5:ns, , nd * ( v - 1 ) + u ] ), nx, 1 )
  hij_h_2_m <- factor_hij[ t ] * ann_hij_h_m %*% vm
  hij_h_2_h <- factor_hij[ t ] * ann_hij_h_h %*% vh
  hij_m_2_m <- factor_hij[ t ] * ann_hij_m_m %*% vm
  hij_m_2_h <- factor_hij[ t ] * ann_hij_m_h %*% vh
  cony_2_m <- factor_con[ t ] * ann_cony %*% vh
  cony_2_h <- factor_con[ t ] * ann_cony %*% vm
  
  # Por muertes de pensionistas de vejez
  u <- 6
  v <- 4
  vm <- matrix( colSums( ltm[ t + 1, , , nd * ( v - 1 ) + u ] ), nx, 1 )
  vh <- matrix( colSums( lth[ t + 1, , , nd * ( v - 1 ) + u ] ), nx, 1 )
  hij_h_4_m <- factor_hij[ t ] * ann_hij_h_m %*% vm
  hij_h_4_h <- factor_hij[ t ] * ann_hij_h_h %*% vh
  hij_m_4_m <- factor_hij[ t ] * ann_hij_m_m %*% vm
  hij_m_4_h <- factor_hij[ t ] * ann_hij_m_h %*% vh
  cony_4_m <- factor_con[ t ] * ann_cony %*% vh
  cony_4_h <- factor_con[ t ] * ann_cony %*% vm
  
  # Por muertes de pensionistas de invalidez
  u <- 6
  v <- 5
  vm <- matrix( colSums( ltm[ t + 1, , , nd * ( v - 1 ) + u ] ), nx, 1 )
  vh <- matrix( colSums( lth[ t + 1, , , nd * ( v - 1 ) + u ] ), nx, 1 )
  hij_h_5_m <- factor_hij[ t ] * ann_hij_h_m %*% vm
  hij_h_5_h <- factor_hij[ t ] * ann_hij_h_h %*% vh
  hij_m_5_m <- factor_hij[ t ] * ann_hij_m_m %*% vm
  hij_m_5_h <- factor_hij[ t ] * ann_hij_m_h %*% vh
  cony_5_m <- factor_con[ t ] * ann_cony %*% vh
  cony_5_h <- factor_con[ t ] * ann_cony %*% vm
  
  for ( x in 1:( nx - 1 ) ) {
    
    cony_h_x <- cony_2_h[ x ] + cony_4_h[ x ] + cony_5_h[ x ]
    cony_m_x <- cony_2_m[ x ] + cony_4_m[ x ] + cony_5_m[ x ]
    hij_m_x <- hij_m_2_m[ x ] + hij_m_2_h[ x ] + hij_m_4_m[ x ] + hij_m_4_h[ x ] + hij_m_5_m[ x ] + hij_m_5_h[ x ]
    hij_h_x <- hij_h_2_m[ x ] + hij_h_2_m[ x ] + hij_h_4_m[ x ] + hij_h_4_h[ x ] + hij_h_5_m[ x ] + hij_h_5_h[ x ]
    if ( x + 1 > 18 ) {
      hij_m_x <- median( Pm[ t, , x, 2, 5 ], na.rm = TRUE ) * hij_m_x
      hij_h_x <- median( Ph[ t, , x, 2, 5 ], na.rm = TRUE ) * hij_h_x
    }
    
    lm_mont[ t + 1, x + 1, 1 ] <- p7m[ x ] * lm_mont[ t, x, 1 ] + cony_m_x
    lm_mont[ t + 1, x + 1, 2 ] <- p8m[ x ] * lm_mont[ t, x, 2 ] + hij_m_x
    
    lh_mont[ t + 1, x + 1, 1 ] <- p7h[ x ] * lh_mont[ t, x, 1 ] + cony_h_x
    lh_mont[ t + 1, x + 1, 2 ] <- p8h[ x ] * lh_mont[ t, x, 2 ] + hij_h_x
    
    lm_mont[ t + 1, x + 1, 3 ] <- cony_4_m[ x ] 
    lm_mont[ t + 1, x + 1, 4 ] <- cony_5_m[ x ]
    lm_mont[ t + 1, x + 1, 5 ] <- hij_m_4_m[ x ]
    lm_mont[ t + 1, x + 1, 6 ] <- hij_m_4_h[ x ]
    lm_mont[ t + 1, x + 1, 7 ] <- hij_m_5_m[ x ]
    lm_mont[ t + 1, x + 1, 8 ] <- hij_m_5_h[ x ]
    lm_mont[ t + 1, x + 1, 9 ] <- ( 1 - p7m[ x ] ) * lm_mont[ t, x, 1 ]  
    lm_mont[ t + 1, x + 1, 10 ] <- ( 1 - p8m[ x ] ) * lm_mont[ t, x, 2 ] 
    lm_mont[ t + 1, x + 1, 11 ] <- ( 1 - p7_6m[ x ] ) * lm_mont[ t, x, 1 ] 
    lm_mont[ t + 1, x + 1, 12 ] <- ( 1 - p8_6m[ x ] ) * lm_mont[ t, x, 2 ] 
    
    lh_mont[ t + 1, x + 1, 3 ] <- cony_4_h[ x ]
    lh_mont[ t + 1, x + 1, 4 ] <- cony_5_h[ x ]
    lh_mont[ t + 1, x + 1, 5 ] <- hij_h_4_m[ x ]
    lh_mont[ t + 1, x + 1, 6 ] <- hij_h_4_h[ x ]
    lh_mont[ t + 1, x + 1, 7 ] <- hij_h_5_m[ x ]
    lh_mont[ t + 1, x + 1, 8 ] <- hij_h_5_h[ x ]
    lh_mont[ t + 1, x + 1, 9 ] <- ( 1 - p7h[ x ] ) * lh_mont[ t, x, 1 ]
    lh_mont[ t + 1, x + 1, 10 ] <- ( 1 - p8h[ x ] ) * lh_mont[ t, x, 2 ]
    lh_mont[ t + 1, x + 1, 11 ] <- ( 1 - p7_6h[ x ] ) * lh_mont[ t, x, 1 ]
    lh_mont[ t + 1, x + 1, 12 ] <- ( 1 - p8_6h[ x ] ) * lh_mont[ t, x, 2 ]
    
  }
}

## Transformación proyección a data.table ----------------------------------------------------------
# Las transiciones se notan por:
# li_j transición del estado i al j
# lk_ti_j individuos generados en estado k debido a transiciones de i a j
# lk_ti_j_sn individuos generados en estado k debido a transiciones de i a j por individuos de sexo n
pob_proy_mont <- NULL
for ( t in 1:nt ) {
  lx <- rbind( lm_mont[ t, , ], lh_mont[ t, , ] )
  
  lx <- data.table( t = t_lst[ t ],
                    sexo = rep( c( 'M', 'H' ), each = nx ),
                    x = rep( x_lst, 2 ),
                    l = lx )
  
  setnames( lx, c( 't', 'sexo', 'x', paste0( 'l', 7:8 ),
                   'l7_t4_6', 'l7_t5_6', 'l8_t4_6_s1', 'l8_t4_6_s2', 'l8_t5_6_s1', 'l8_t5_6_s2',
                   'l7_0', 'l8_0', 'l7_6', 'l8_6' ) )
  
  pob_proy_mont <- rbind( pob_proy_mont, lx )
  
}

## Unión con proyección total ----------------------------------------------------------------------
pob_proy <- merge.data.table( pob_proy, pob_proy_mont, by = c( 't', 'sexo', 'x' ), all.x = TRUE )
pob_proy[ is.na( l7 ), l7 := 0 ]
pob_proy[ is.na( l8 ), l8 := 0 ]
pob_proy[ is.na( l7_t4_6 ), l7_t4_6 := 0 ]
pob_proy[ is.na( l7_t5_6 ), l7_t5_6 := 0 ]
pob_proy[ is.na( l8_t4_6_s1 ), l8_t4_6_s1 := 0 ]
pob_proy[ is.na( l8_t4_6_s2 ), l8_t4_6_s2 := 0 ]
pob_proy[ is.na( l8_t5_6_s1 ), l8_t5_6_s1 := 0 ]
pob_proy[ is.na( l8_t5_6_s2 ), l8_t5_6_s2 := 0 ]
pob_proy[ is.na( l7_0 ), l7_0 := 0 ]
pob_proy[ is.na( l8_0 ), l8_0 := 0 ]
pob_proy[ is.na( l7_6 ), l7_6 := 0 ]
pob_proy[ is.na( l8_6 ), l8_6 := 0 ]

pob_proy_mont[ , s := 0 ]
pob_proy_ts <- merge.data.table( pob_proy_ts, pob_proy_mont, by = c( 't', 'sexo', 'x', 's' ), all.x = TRUE )
pob_proy_ts[ is.na( l7 ), l7 := 0 ]
pob_proy_ts[ is.na( l8 ), l8 := 0 ]
pob_proy_ts[ is.na( l7_t4_6 ), l7_t4_6 := 0 ]
pob_proy_ts[ is.na( l7_t5_6 ), l7_t5_6 := 0 ]
pob_proy_ts[ is.na( l8_t4_6_s1 ), l8_t4_6_s1 := 0 ]
pob_proy_ts[ is.na( l8_t4_6_s2 ), l8_t4_6_s2 := 0 ]
pob_proy_ts[ is.na( l8_t5_6_s1 ), l8_t5_6_s1 := 0 ]
pob_proy_ts[ is.na( l8_t5_6_s2 ), l8_t5_6_s2 := 0 ]
pob_proy_ts[ is.na( l7_0 ), l7_0 := 0 ]
pob_proy_ts[ is.na( l8_0 ), l8_0 := 0 ]
pob_proy_ts[ is.na( l7_6 ), l7_6 := 0 ]
pob_proy_ts[ is.na( l8_6 ), l8_6 := 0 ]

# Proyección de dependientes -----------------------------------------------------------------------
## Preparando probabilidades de salida -------------------------------------------------------------
message( '\tPreparando probabilidades de salida para dependientes' )
# Se puede cambiar tabla mortalidad

ph <- tas_din_dec[ sexo == 'H' & x <= x_max, list( t = t - parametros$anio_ini, x, px_act ) ]
ph <- ph[ t <= t_max ]
ph <- dcast.data.table(
  data = ph,
  formula = t ~ x,
  value.var = 'px_act',
  fill = 0 )
setorder( ph, t )
ph[ , t := NULL ]
ph <- as.matrix( ph )

pm <- tas_din_dec[ sexo == 'M' & x <= x_max, list( t = t - parametros$anio_ini, x, px_act ) ]
pm <- pm[ t <= t_max ]
pm <- dcast.data.table(
  data = pm,
  formula = t ~ x,
  value.var = 'px_act',
  fill = 0 )
setorder( pm, t )
pm[ , t := NULL ]
pm <- as.matrix( pm )

## Preparando población incial y objetos para proyección -------------------------------------------
message( '\tEstableciendo población inicial para dependientes' )

lm_dep <- array( 0.0, dim = c( nt, nx, 4 ) )
lh_dep <- array( 0.0, dim = c( nt, nx, 4 ) )

vm <- colSums( lm[ 1, , , 2 ] + lm[ 1, , , 4 ] + lm[ 1, , , 5 ] )
vh <- colSums( lh[ 1, , , 2 ] + lh[ 1, , , 4 ] + lh[ 1, , , 5 ] )
lm_dep[ 1, , 1:2 ] <- cbind( 1.305 * ann_cony %*% vh, 0.64 * ( ann_hij_m_m %*% vm + ann_hij_m_h %*% vh ) )
lh_dep[ 1, , 1:2 ] <- cbind( 0.063 * ann_cony %*% vm, 0.64 * ( ann_hij_h_m %*% vm + ann_hij_h_h %*% vh ) )

# Verificaciones con estadísticas
# sum( 1.305 * ann_cony %*% vh ) + sum( 0.063 * ann_cony %*% vm )
# sum( 0.480 * ( ann_hij_h_m %*% vm + ann_hij_h_h %*% vh ) )
# sum( 0.480 * ( ann_hij_m_m %*% vm + ann_hij_m_h %*% vh ) )

## Proyección de población de dependientes ---------------------------------------------------------
message( '\tProyección de población de dependientes' )

tl <- list( c( 1, 2 ), c( 3, 2 ), c( 4, 2 ), c( 5, 2 ), c( 2, 4 ), c( 2, 5 ), c( 3, 4 ), c( 3, 5 ) )

factprobhij <- 0.9 # para afectar la probabilidad de salida y tratar de reflejar la migración en hijos
factprobcon <- 0.9 # para afectar la probabilidad de salida y tratar de reflejar la migración en cónyuges
factcon <- c( 0.45, 0.45, 0.55, 0.65, 0.7, 0.75, rep( 0.8, nt - 6) ) # para afectar la inserción de nuevos cónyuges
facthij <- c( 0.3, 0.3, 0.3, 0.3, 0.35, 0.4, 0.45, 0.55, 0.6, rep( 0.65, nt - 9) ) # para afectar la inserción de nuevos hijos

for ( t in 1:( nt - 1 ) ) {
  
  vm <- matrix( 0, nx, 1 )
  vh <- matrix( 0, nx, 1 )
  for ( i in 1:length( tl ) ) {
    
    u <- tl[[ i ]][ 2 ]
    v <- tl[[ i ]][ 1 ]
    vm <- vm + matrix( colSums( ltm[ t + 1, , , nd * ( v - 1 ) + u ] ), nx, 1 )
    vh <- vh + matrix( colSums( lth[ t + 1, , , nd * ( v - 1 ) + u ] ), nx, 1 )  
    
  }
  
  hij_h_m <- ann_hij_h_m %*% vm
  hij_h_m[ 1 ] <- hij_h_m[ 1 ] + as.numeric( ann_hij_h_m[ 1, ] %*% colSums( lm[ t, , , 2 ] ) )
  hij_h_h <- ann_hij_h_h %*% vh
  hij_h_h[ 1 ] <- hij_h_h[ 1 ] + as.numeric( ann_hij_h_h[ 1, ] %*% colSums( lh[ t, , , 2 ] ) )
  hij_m_m <- ann_hij_m_m %*% vm
  hij_m_m[ 1 ] <- hij_m_m[ 1 ] + as.numeric( ann_hij_m_m[ 1, ] %*% colSums( lm[ t, , , 2 ] ) )
  hij_m_h <- ann_hij_m_h %*% vh
  hij_m_h[ 1 ] <- hij_m_h[ 1 ] + as.numeric( ann_hij_m_h[ 1, ] %*% colSums( lh[ t, , , 2 ] ) )
  
  cony_m <- factcon[ t ] * ann_cony %*% vh 
  cony_h <- factcon[ t ] * ann_cony %*% vm
  hij_h_m <- facthij[ t ] * hij_h_m
  hij_h_h <- facthij[ t ] * hij_h_h
  hij_m_m <- facthij[ t ] * hij_m_m
  hij_m_h <- facthij[ t ] * hij_m_h
  
  lm_dep[ t + 1, 1, 1 ] <- cony_m[ 1 ]
  lh_dep[ t + 1, 1, 1 ] <- cony_h[ 1 ]
  lm_dep[ t + 1, 1, 2 ] <- hij_m_m[ 1 ] + hij_m_h[ 1 ]
  lh_dep[ t + 1, 1, 2 ] <- hij_h_m[ 1 ] + hij_h_h[ 1 ]
  
  for ( x in 1:( nx - 1 ) ) {
    
    lm_dep[ t + 1, x + 1, 1 ] <- min( 1, factprobcon * pm[ t, x ] ) * lm_dep[ t, x, 1 ] + cony_m[ x + 1 ]
    lh_dep[ t + 1, x + 1, 1 ] <- min( 1, factprobcon * ph[ t, x ] ) * lh_dep[ t, x, 1 ] + cony_h[ x + 1 ]
    
    lm_dep[ t + 1, x + 1, 2 ] <- min( 1, factprobhij * pm[ t, x ] ) * lm_dep[ t, x, 2 ] + hij_m_m[ x + 1 ] + hij_m_h[ x + 1 ]
    lh_dep[ t + 1, x + 1, 2 ] <- min( 1, factprobhij * ph[ t, x ] ) * lh_dep[ t, x, 2 ] + hij_h_m[ x + 1 ] + hij_h_h[ x + 1 ]
    
    lm_dep[ t + 1, x, 3 ] <- ( 1 - pm[ t, x ] ) * lm_dep[ t, x, 1 ]
    lh_dep[ t + 1, x, 3 ] <- ( 1 - ph[ t, x ] ) * lh_dep[ t, x, 1 ]
    
    lm_dep[ t + 1, x, 4 ] <- ( 1 - pm[ t, x ] ) * lm_dep[ t, x, 2 ]
    lh_dep[ t + 1, x, 4 ] <- ( 1 - ph[ t, x ] ) * lh_dep[ t, x, 2 ]
    
  }
}

## Transformación proyección a data.table ----------------------------------------------------------
pob_proy_dep <- NULL
for ( t in 1:nt ) {
  lx <- data.table( t = t_lst[ t ],
                    sexo = rep( c( 'M', 'H' ), each = nx ),
                    x = rep( x_lst, 2 ) )
  lx <- cbind( lx, rbind( lm_dep[ t, , ], lh_dep[ t, , ] ) )
  
  setnames( lx, c( 't', 'sexo', 'x', paste0( 'l', 9:10 ), 'l9_6', 'l10_6' ) )
  
  pob_proy_dep <- rbind( pob_proy_dep, lx )
  
}
pob_proy_dep[ , l11 := l10 ]
pob_proy_dep[ x > 18, l11 := 0 ]
pob_proy_dep[ , l11_6 := l10_6 ]
pob_proy_dep[ x > 18, l11_6 := 0 ]

# Código para verificación
# pob_proy_dep[ , list( l9 = sum( l9 ), l10 = sum( l10 ), l11 = sum( l11 ) ), by = list( t ) ]

## Unión con proyección total ----------------------------------------------------------------------
pob_proy <- merge.data.table( pob_proy, pob_proy_dep, by = c( 't', 'sexo', 'x' ), all.x = TRUE )
pob_proy[ is.na( l9 ), l9 := 0 ]
pob_proy[ is.na( l10 ), l10 := 0 ]
pob_proy[ is.na( l11 ), l11 := 0 ]
pob_proy[ is.na( l9_6 ), l9_6 := 0 ]
pob_proy[ is.na( l10_6 ), l10_6 := 0 ]
pob_proy[ is.na( l11_6 ), l11_6 := 0 ]

pob_proy_dep[ , s := 0 ]
pob_proy_ts <- merge.data.table( pob_proy_ts, pob_proy_dep, by = c( 't', 'sexo', 'x', 's' ), all.x = TRUE )
pob_proy_ts[ is.na( l9 ), l9 := 0 ]
pob_proy_ts[ is.na( l10 ), l10 := 0 ]
pob_proy_ts[ is.na( l11 ), l11 := 0 ]
pob_proy_ts[ is.na( l9_6 ), l9_6 := 0 ]
pob_proy_ts[ is.na( l10_6 ), l10_6 := 0 ]
pob_proy_ts[ is.na( l11_6 ), l11_6 := 0 ]

# Sumariza proyección población --------------------------------------------------------------------
message( '\tSumarizando proyección de población' )
cols <- names( pob_proy )
cols <- cols[ !( cols %in% c( 't', 'sexo', 'x' ) ) ]
pob_proy_tot <- pob_proy[ , lapply( .SD, sum ), .SDcols = cols, by = list( t ) ]
setorder( pob_proy_tot, t )

pob_proy_tot_sex <- pob_proy[ , lapply( .SD, sum ), .SDcols = cols, by = list( t, sexo ) ]
setorder( pob_proy_tot_sex, t, sexo )

# Guardo resultados --------------------------------------------------------------------------------
message( '\tGuardando proyección de población' )
save( lm, lh, ltm, lth, lm_mont, lh_mont, lm_dep, lh_dep,
      pob_proy_dep, pob_proy_mont,
      pob_proy_ts, pob_proy, pob_proy_tot, pob_proy_tot_sex,
      file = parametros$demo_rdata_ssc_pob_proy )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
