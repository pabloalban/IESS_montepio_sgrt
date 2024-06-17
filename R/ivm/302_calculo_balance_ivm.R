message( paste( rep('-', 100 ), collapse = '' ) )

# Carga de información -----------------------------------------------------------------------------
load( parametros$demo_rdata_sgo_pob_proy )
load( esc$rdata_sal_proy )
load( esc$rdata_ben_proy )

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% parametros_lista ) ] )
if ( exists( 'balance' ) ) rm( balance )
if ( exists( 'balance_anual' ) ) rm( balance_anual )

message( '\tBalance seguro ', parametros$seguro, ' calculando escenario: ', esc$nombre )

# Cruce con proyecciones ---------------------------------------------------------------------------
message( '\tCruce con proyecciones financieras' )

balance <- merge.data.table(
  pob_proy_ts, 
  sal_proy[ , list( t, sexo, x, s, sal ) ], 
  by = c( 't', 'sexo', 'x', 's' ),
  all.x = TRUE )

balance <- merge.data.table( 
  balance, 
  ben_proy[ , list( 
    t, sexo, x, s,
    pen_ant_4, pen_4, 
    pen_ant_5, pen_5, 
    pen_ant_7, pen_7_4, pen_7_5, 
    pen_ant_8, pen_8_4, pen_8_5,
    ben_2_6, ben_3_6, ben_4_6, ben_5_6, ben_7_6, ben_8_6 ) ], 
  by = c( 't', 'sexo', 'x', 's' ), 
  all.x = TRUE )

balance[ is.na( sal ), sal := 0 ]
balance[ is.na( pen_4 ), pen_4 := 0 ]
balance[ is.na( pen_5 ), pen_5 := 0 ]
balance[ is.na( pen_7_4 ), pen_7_4 := 0 ]
balance[ is.na( pen_7_5 ), pen_7_5 := 0 ]
balance[ is.na( pen_8_4 ), pen_8_4 := 0 ]
balance[ is.na( pen_8_5 ), pen_8_5 := 0 ]
balance[ is.na( pen_ant_4 ), pen_ant_4 := 0 ]
balance[ is.na( pen_ant_5 ), pen_ant_5 := 0 ]
balance[ is.na( pen_ant_7 ), pen_ant_7 := 0 ]
balance[ is.na( pen_ant_8 ), pen_ant_8 := 0 ]
balance[ is.na( ben_2_6 ), ben_2_6 := 0 ]
balance[ is.na( ben_3_6 ), ben_3_6 := 0 ]
balance[ is.na( ben_4_6 ), ben_4_6 := 0 ]
balance[ is.na( ben_5_6 ), ben_5_6 := 0 ]
balance[ is.na( ben_7_6 ), ben_7_6 := 0 ]
balance[ is.na( ben_8_6 ), ben_8_6 := 0 ]

balance <- merge.data.table(
  balance,
  esc$apo_act[ , list( t, sbu, cal_mas, cal_pen_vej, cal_pen_inv, cal_pen_viu, cal_pen_orf, cal_aux_fun, cal_gast ) ],
  by = c( 't' ), 
  all.x = TRUE )

setorder( balance, t, sexo, x, s )

message( '\tProyectando masa salarial' )
if ( esc$mas_sal_con_dec ) {  # incluye los décimos en la masa salarial
  balance[ , M := cal_mas * ( 13 * sal / 12 + sbu ) * l2 ]
} else {
  balance[ , M := cal_mas * sal * l2 ]
}

# Proyección de beneficios -------------------------------------------------------------------------
message( '\tProyectando beneficios por pensiones' )

## Beneficio por pensiones de vejez ----------------------------------------------------------------
balance[ , B4_nodec := cal_pen_vej * ( 12 / 13 ) * ( ( pen_ant_4 - sbu ) * l4_4 + ( pen_4 - sbu ) * ( l1_4 + l2_4 + l3_4 + l5_4 ) ) ]
balance[ t == 0, B4_nodec := 0 ]
balance[ , B4_ant := cal_pen_vej * pen_ant_4 * l4_4 ]
balance[ t == 0, B4_ant := 0 ]
balance[ , B4_nue := cal_pen_vej * pen_4 * ( l1_4 + l2_4 + l3_4 + l5_4 ) ]
balance[ t == 0, B4_nue := 0 ]
balance[ , B4 := B4_ant + B4_nue ]
balance[ t == 0, B4 := 0 ]

## Beneficio por pensiones de invalidez ------------------------------------------------------------
balance[ , B5_nodec := cal_pen_inv * ( 12 / 13 ) * ( ( pen_ant_5 - sbu ) * l5_5 + ( pen_5 - sbu ) * ( l1_5 + l2_5 + l3_5 + l4_5 ) ) ]
balance[ t == 0, B5_nodec := 0 ]
balance[ , B5_ant := cal_pen_inv * pen_ant_5 * l5_5 ]
balance[ t == 0, B5_ant := 0 ]
balance[ , B5_nue := cal_pen_inv * pen_5 * ( l1_5 + l2_5 + l3_5 + l4_5 ) ]
balance[ t == 0, B5_nue := 0 ]
balance[ , B5 := B5_ant + B5_nue ]
balance[ t == 0, B5 := 0 ]

## Beneficios de montepios viudas ------------------------------------------------------------------
balance[ , B7_nodec := cal_pen_viu * ( 12 / 13 ) * ( pen_ant_7 - sbu ) * l7 ]
balance[ t == 0, B7_nodec := 0 ]
# balance[ t > 0, B7_ant := cal_pen_viu * pen_ant_7 * l7 ]
# balance[ t > 0, B7_nue := cal_pen_viu * pen_7 * ( l1_7 + l2_ + l3_5 + l4_5 ) ]
# balance[ t > 0, B7_nodec := cal_pen_viu * ( 12 / 13 ) * ( ( pen_ant_7 - sbu ) * l7 + ( pen_7 - sbu ) * ( l1_4 + l2_4 + l3_4 + l5_4 ) ) ]
balance[ , B7 := cal_pen_viu * pen_ant_7 * l7 ]
balance[ t == 0, B7 := 0 ]

## Beneficios de montepios huérfanos ---------------------------------------------------------------
balance[ , B8_nodec := cal_pen_orf * ( 12 / 13 ) * ( pen_ant_8 - sbu ) * l8 ]
balance[ t == 0, B8_nodec := 0 ]
balance[ , B8 := cal_pen_orf * pen_ant_8 * l8 ]
balance[ t == 0, B8 := 0 ]

## Beneficios por pensiones ------------------------------------------------------------------------
balance[ , B_pen_nodec := B4_nodec + B5_nodec + B7_nodec + B8_nodec ]
balance[ , B_pen := B4 + B5 + B7 + B8 ]

## Beneficios de transición ------------------------------------------------------------------------
message( '\tProyectando beneficios por transiciones' )
balance[ , B2_6 := cal_aux_fun * ben_2_6 * l2_6 ]
balance[ t == 0, B2_6 := 0 ]
balance[ , B4_6 := cal_aux_fun * ben_4_6 * l4_6 ]
balance[ t == 0, B4_6 := 0 ]
balance[ , B5_6 := cal_aux_fun * ben_5_6 * l5_6 ]
balance[ t == 0, B5_6 := 0 ]
balance[ , B7_6 := cal_aux_fun * ben_7_6 * l7_6 ]
balance[ t == 0, B7_6 := 0 ]
balance[ , B8_6 := cal_aux_fun * ben_8_6 * l8_6 ]
balance[ t == 0, B8_6 := 0 ]

## Beneficios por auxilio de funerales -------------------------------------------------------------
balance[ , B_aux := B2_6 + B4_6 + B5_6 + B7_6 + B8_6 ]

## Beneficios totales ------------------------------------------------------------------------------
balance[ , B := B_pen + B_aux ]

# Proyección de aportes ----------------------------------------------------------------------------
message( '\tProyectando aportes' )

## Aportes de activos ------------------------------------------------------------------------------
balance <- merge( 
  balance, 
  esc$apo_act[ , list( t, por_apo, por_apo_pen_vej, por_apo_pen_inv, por_apo_pen_mon, por_gast, 
                       por_apo_est ) ], 
  by = 't', all.x = TRUE )
balance[ , A2 := por_apo * M ]
balance[ t == 0 , A2 := 0 ]

## Aportes de pensionistas de vejez sin décimos ----------------------------------------------------
balance[ , A4 := por_apo_pen_vej * B4_nodec ] 
balance[ t == 0 , A4 := 0 ]

## Aportes de pensionistas de invalidez sin décimos ------------------------------------------------
balance[ , A5 := por_apo_pen_inv * B5_nodec ]
balance[ t == 0 , A5 := 0 ]

## Aportes de montepios viudas ---------------------------------------------------------------------
balance[ , A7 := por_apo_pen_mon * B7_nodec ]
balance[ t == 0 , A7 := 0 ]

## Aportes de montepios huérfano -------------------------------------------------------------------
balance[ , A8 := por_apo_pen_mon * B8_nodec ]
balance[ t == 0 , A8 := 0 ]

## Aportes totales ---------------------------------------------------------------------------------
balance[ , A := A2 + A4 + A5 + A7 + A8 ]

# Gasto administrativo -----------------------------------------------------------------------------
balance[ , G := cal_gast * por_gast * A ]
balance[ t == 0 , G := 0 ]

# Contribución del estado --------------------------------------------------------------------------
balance[ , A_est := por_apo_est * B_pen ]
balance[ t == 0 , A_est := 0 ]

# Balance corriente --------------------------------------------------------------------------------
colsadd <- c( 'M', 'A2', 'A4', 'A5', 'A7', 'A8', 'A', 'G', 'A_est',
              'B4_nodec', 'B4_ant', 'B4_nue', 'B4', 
              'B5_nodec', 'B5_ant', 'B5_nue', 'B5', 
              'B7_nodec', 'B7', 
              'B8_nodec', 'B8', 
              'B_pen_nodec', 'B_pen', 
              'B2_6', 'B4_6', 'B5_6', 'B7_6', 'B8_6', 'B_aux', 'B' )
balance_anual <- balance[ , lapply( .SD, sum, na.rm = TRUE ), .SDcols = colsadd, by = list( t ) ]
setorder( balance_anual, t )

## Balance corriente -------------------------------------------------------------------------------
balance_anual[ , V_cor := A + A_est - B - G ]

# Balance actuarial --------------------------------------------------------------------------------
balance_anual <- merge.data.table( 
  balance_anual, 
  esc$apo_act[ , list( t, i_a, u_a, v_a ) ], 
  by = 't' )

# cat( names( balance_anual ), sep = "', '" )
colsadd <- c( 'M', 'A2', 'A4', 'A5', 'A7', 'A8', 'A', 'G', 'A_est', 
              'B4_nodec', 'B4_ant', 'B4_nue', 'B4', 
              'B5_nodec', 'B5_ant', 'B5_nue', 'B5', 
              'B7_nodec', 'B7', 
              'B8_nodec', 'B8', 
              'B_pen_nodec', 'B_pen', 
              'B2_6', 'B4_6', 'B5_6', 'B7_6', 'B8_6', 'B_aux', 'B', 'V_cor' )

colsvap <- paste0( colsadd, '_vap' )

balance_anual[ , ( colsvap ) := lapply( .SD, FUN = function( c ) cumsum( v_a * c ) ), .SDcols = colsadd ]

## Balance inicial ---------------------------------------------------------------------------------
balance_anual[ , V0 := esc$V0 ]

## Balance capitalizado ----------------------------------------------------------------------------
balance_anual[ , V_cap := V_cor ]
balance_anual[ t == 0, V_cap := esc$V0 ]
balance_anual[ , V_cap := u_a * cumsum( v_a * V_cap ) ]

## Balance actuarial -------------------------------------------------------------------------------
balance_anual[ , V := v_a * V_cap ]

## Intereses que genera la reserva -----------------------------------------------------------------
balance_anual[ , I := i_a * shift( V_cap, fill = 0 ) ]

## Activo y Pasivo ---------------------------------------------------------------------------------
balance_anual[ , Activo := A_vap + A_est_vap + V0 ]
balance_anual[ , Pasivo := B_vap + G_vap ]

# Guardando balances -------------------------------------------------------------------------------
message( '\tGuardando balances' )
save( balance, balance_anual, file = esc$rdata_balance )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% parametros_lista ) ] )
gc()
