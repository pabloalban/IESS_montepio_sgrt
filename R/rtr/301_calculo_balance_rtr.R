# Descripción de campos del SGRT--------------------------------------------------------------------
# 12 = pensionistas de riesgos del trabajo;
# 13 = indemnizaciones por incapacidad permanente parcial;
# 14 = subsidios por incapacidad temporal;
# 15 = montepíos de orfandad de riesgos del trabajo;
# 16 = montepíos de viudedad de riesgos del trabajo.

message( paste( rep('-', 100 ), collapse = '' ) )

load( parametros$demo_rdata_rtr_pob_proy )
load( parametros$demo_rdata_sgo_pob_proy )
load( esc$rdata_sal_proy )
load( esc$rtr_rdata_icomp_proy_benef )

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% parametros_lista ) ] )
if ( exists( 'balance' ) ) rm( balance )
if ( exists( 'balance_anual' ) ) rm( balance_anual )

message( '\tBalance seguro ', parametros$seguro, ' calculando escenario: ', esc$nombre )

# Cruce con proyecciones ---------------------------------------------------------------------------

message( '\tProyectando masa salarial' )
balance <- merge.data.table(
  pob_proy_ts[ , list( t, sexo, x, s, l2 ) ], 
  sal_proy[ , list( t, sexo, x, s, sal ) ], 
  by = c( 't', 'sexo', 'x', 's' ),
  all.x = TRUE )
balance[ is.na( sal ), sal := 0 ]

balance <- merge( balance, 
                  esc$apo_act[ , list( t, cal_mas ) ],
                  by = 't', all.x = TRUE )

balance[ , M := cal_mas * sal * l2 ]
balance <- balance[ , list( M = sum( M, na.rm = TRUE ) ),
                    by = list( t, sexo, x ) ]

message( '\tCruce con proyecciones financieras' )
balance <- merge.data.table(
  balance,
  esc$apo_act[ , list( t, 
                       sbu, 
                       cal_mas,
                       cal_pen_incap,
                       cal_pen_indem,
                       cal_pen_sub, 
                       cal_pen_orf,
                       cal_pen_viu,
                       cal_gast ) ],
  by = c( 't' ), 
  all.x = TRUE )

balance <- merge( balance, 
                  esc$apo_act[ , list( t, por_apo, por_gast, por_apo_est,
                                       por_apo_pen_incap, apo_sal ) ],
                  by = 't', all.x = TRUE )

message( '\tCruce con proyecciones de beneficios promedios' )
balance <- merge.data.table( 
  balance, 
  ben_proy, 
  by = c( 't', 'sexo', 'x' ), 
  all.x = TRUE )
balance[ is.na( b_12 ), b_12 := 0 ]
balance[ is.na( b_13 ), b_13 := 0 ]
balance[ is.na( b_14 ), b_14 := 0 ]
balance[ is.na( b_15 ), b_15 := 0 ]
balance[ is.na( b_16 ), b_16 := 0 ]

message( '\tCruce con proyecciones número de beneficiarios' )
balance <- merge.data.table( 
  balance, 
  pob_proy_rtr, 
  by = c( 't', 'sexo', 'x' ), 
  all.x = TRUE )

setorder( balance, t, sexo, x )

# Proyección de beneficios -------------------------------------------------------------------------
message( '\tProyectando beneficios por pensiones, subsidios e indemnizaciones' )

## Beneficio por pensiones de SGRT------------------------------------------------------------------
balance[ t == 0, B12_nodec := cal_pen_incap * b_12 * l_12 ]
balance[ t > 0, B12_nodec := cal_pen_incap * b_12 * ( l_12_12 + l_2_12 ) ]
balance[ t == 0, B12_ant := 0 ]
balance[ t > 0, B12_ant :=  cal_pen_incap * ( ( 1 + coef_decima_tercera_b_12 ) * b_12 * l_12_12 + coef_decima_cuarta_b_12 * sbu * l_12_12 ) ]
balance[ t == 0, B12_nue := 0 ]
balance[ t > 0, B12_nue := cal_pen_incap * ( ( 1 + coef_decima_tercera_b_12 ) * b_12 * l_2_12 + coef_decima_cuarta_b_12 * sbu * l_2_12 ) ]
balance[ , B12 := B12_ant + B12_nue ]

## Beneficios por Indemnizaciones-------------------------------------------------------------------
balance[ , B13 := cal_pen_indem * b_13 * l_13 ]

## Beneficios por Subsidios-------------------------------------------------------------------------

balance[ , B14 := cal_pen_sub * b_14 * l_14 ]

## Beneficios de montepios huérfanos ---------------------------------------------------------------
balance[ t == 0, B15_nodec := cal_pen_orf * b_15 * l_15 ]
balance[ t > 0, B15_nodec := cal_pen_orf * b_15 * ( l_15_15 + l_0_15 ) ]
balance[ t == 0, B15_ant := 0 ]
balance[ t > 0, B15_ant :=  cal_pen_orf * ( ( 1 + coef_decima_tercera_b_15 ) * b_15 * l_15_15 + coef_decima_cuarta_b_15 * sbu * l_15_15 ) ]
balance[ t == 0, B15_nue := 0 ]
balance[ t > 0, B15_nue := cal_pen_orf * ( ( 1 + coef_decima_tercera_b_15 ) * b_15 * l_0_15 + coef_decima_cuarta_b_15 * sbu * l_0_15 ) ]
balance[ , B15 := B15_ant + B15_nue ]

## Beneficios de montepios viudas ------------------------------------------------------------------
balance[ t == 0, B16_nodec := cal_pen_viu * b_16 * l_16 ]
balance[ t > 0, B16_nodec := cal_pen_viu * b_16 * ( l_16_16 + l_0_16 ) ]
balance[ t == 0, B16_ant := 0 ]
balance[ t > 0, B16_ant :=  cal_pen_viu * ( ( 1 + coef_decima_tercera_b_16 ) * b_16 * l_16_16 + coef_decima_cuarta_b_16 * sbu * l_16_16 ) ]
balance[ t == 0, B16_nue := 0 ]
balance[ t > 0, B16_nue := cal_pen_viu * ( ( 1 + coef_decima_tercera_b_16 ) * b_16 * l_0_16 + coef_decima_cuarta_b_16 * sbu * l_0_16 ) ]
balance[ , B16 := B16_ant + B16_nue ]

## Beneficios por pensiones ------------------------------------------------------------------------
balance[ , B_pen_nodec := B12_nodec + B15_nodec + B16_nodec ]
balance[ , B_pen := B12 + B15 + B16 ]

# 1.6.  Pago de prestaciones médico asistenciales---------------------------------------------------
message( '\tProyectando el pago de prestaciones médico asistenciales' )
balance[ , B_sal := apo_sal * M ]

## Beneficios totales ------------------------------------------------------------------------------
balance[ , B := B_pen + B13 + B14 + B_sal]

# Proyección de aportes ----------------------------------------------------------------------------
message( '\tProyectando aportes' )

## Aportes de activos ------------------------------------------------------------------------------

balance[ t == 0 , A2 := 0 ]
balance[ , A2 := por_apo * M ]

## Aportes de pensionistas de SGRT------------------------------------------------------------------
balance[ , A12 := por_apo_pen_incap * B12_nodec ] 
balance[ t == 0 , A12 := 0 ]

## Aportes de montepios huérfano -------------------------------------------------------------------
balance[ , A15 := por_apo_pen_incap * B15_nodec ]
balance[ t == 0 , A15 := 0 ]

## Aportes de montepios viudas ---------------------------------------------------------------------
balance[ , A16 := por_apo_pen_incap * B16_nodec ]
balance[ t == 0 , A16 := 0 ]

# Gasto administrativo -----------------------------------------------------------------------------
balance[ , G := cal_gast * por_gast * M ]
balance[ t == 0 , G := 0 ]

# Contribución del estado --------------------------------------------------------------------------
balance[ , A_est := por_apo_est * ( B12 + B15 + B16 ) ]
balance[ t == 0 , A_est := 0 ]

## Aportes totales ---------------------------------------------------------------------------------
balance[ , A := A2 + A12 + A15 + A16 + A_est ]

# Activos
balance[ , Act := A2 + A_est ]

# Pasivo
balance[ , Pas := B + G ]

# Balance corriente --------------------------------------------------------------------------------
balance_anual <- balance[ , list( 
  M = sum( M ),
  
  Act = sum( Act, na.rm = TRUE ),
  Pas = sum( Pas , na.rm = TRUE ),
  
  A = sum( A ),
  A2 = sum( A2 ),
  A12 = sum( A12 ),
  A15 = sum( A15 ),
  A16 = sum( A16 ),
  A_est = sum( A_est ),
  
  B = sum( B ),
  B_pen = sum( B_pen ),
  B_pen_nodec = sum( B_pen_nodec ),
  
  B12 = sum( B12 ),
  B12_nodec = sum( B12_nodec ),

  B13 = sum( B13 ),
  
  B14 = sum( B14 ),
  
  B15 = sum( B15 ),
  B15_nodec = sum( B15_nodec ),

  B16 = sum( B16 ),
  B16_nodec = sum( B16_nodec ),
  
  B_sal = sum( B_sal , na.rm = TRUE ),
  G = sum( G ) ), 
  by = list( t ) ]

balance_anual <- merge( balance_anual, esc$apo_act[ , list( t, i_a ) ], by = 't' )
setorder( balance_anual, t )
balance_anual[ , r := i_a ]
balance_anual[ t == 0, r := 0 ]
balance_anual[ , r := 1 + r ]
balance_anual[ , r := cumprod( r ) ]
balance_anual[ , v := 1 / r  ]
balance_anual[ , V_cor := A - B - G ]
balance_anual[ t == 0, `:=`(
  A = 0, A2 = 0, A12 = 0, A15 = 0, A16 = 0, A_est = 0,
  B_pen = 0, B_pen_nodec = 0,
  B12 = 0, B12_nodec = 0,
  B15 = 0, B15_nodec = 0,
  B16 = 0, B16_nodec = 0,
  B13 = 0, B14 = 0, B = 0, B_sal = 0, 
  G = 0, V_cor = 0, 
  Act = 0, Pas = 0  ) ]
balance_anual[ , V_cap := V_cor ]
balance_anual[ t == 0, V_cap := esc$V0 ]
balance_anual[ t == 0, Act := esc$V0 ]
balance_anual[ , Act := r * cumsum( v * Act ) ]
balance_anual[ , Pas := r * cumsum( v * Pas ) ]
balance_anual[ , V_cap := r * cumsum( v * V_cap ) ]

# Balance actuarial --------------------------------------------------------------------------------
balance_anual[ , M_vap := cumsum( v * M ) ]

balance_anual[ , Act_vap := v * Act ]
balance_anual[ , Pas_vap := v * Pas ]

balance_anual[ , A_vap := cumsum( v * A ) ]
balance_anual[ , A2_vap := cumsum( v * A2 ) ]
balance_anual[ , A12_vap := cumsum( v * A12 ) ]
balance_anual[ , A15_vap := cumsum( v * A15 ) ]
balance_anual[ , A16_vap := cumsum( v * A16 ) ]
balance_anual[ , A_est_vap := cumsum( v * A_est ) ]

balance_anual[ , B_vap := cumsum( v * B ) ]
balance_anual[ , B_pen_vap := cumsum( v * B_pen ) ]
balance_anual[ , B_pen_nodec_vap := cumsum( v * B_pen_nodec ) ]

balance_anual[ , B12_vap := cumsum( v * B12 ) ]
balance_anual[ , B12_nodec_vap := cumsum( v * B12_nodec ) ]

balance_anual[ , B13_vap := cumsum( v * B13 ) ]

balance_anual[ , B14_vap := cumsum( v * B14 ) ]

balance_anual[ , B15_vap := cumsum( v * B15 ) ]
balance_anual[ , B15_nodec_vap := cumsum( v * B15_nodec ) ]

balance_anual[ , B16_vap := cumsum( v * B16 ) ]
balance_anual[ , B16_nodec_vap := cumsum( v * B16_nodec ) ]

balance_anual[ , B_sal_vap := cumsum( v * B_sal ) ]

balance_anual[ , G_vap := cumsum( v * G ) ]
balance_anual[ , V := v * V_cap ]
balance_anual[ , V0 := esc$V0 ]

# Guardando balances -------------------------------------------------------------------------------
message( '\tGuardando balances' )
save( balance, balance_anual, file = esc$rtr_rdata_icomp_balance )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% parametros_lista ) ] )
gc( )

