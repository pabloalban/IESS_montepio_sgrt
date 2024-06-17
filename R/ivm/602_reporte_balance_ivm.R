# Cargando diccionario -----------------------------------------------------------------------------
message(' \tCargando dicccionario' )

source( 'R/ivm/510_descripcion_var_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )

# Formato para reporte -----------------------------------------------------------------------------
## Bordes blancos ----------------------------------------------------------------------------------

bordes1 <- createStyle( border = "TopBottomLeftRight", borderColour = "#FFFFFF" )
bordes2 <- createStyle( border = "Right", borderColour = "#FFFFFF", borderStyle = "thick" )
bordes3 <- createStyle( border = "TopBottom", borderColour = "#FFFFFF", borderStyle = "thick" )

## Titulos de la tabla -----------------------------------------------------------------------------

titulosos <- createStyle( fontColour = "#030303", fgFill = "#8ea9db", halign = "center", wrapText = TRUE,
                          valign = "center",textDecoration = "bold",fontName ="Times New Roman")

## Contendio de los datos de la tabla --------------------------------------------------------------

fosc <- createStyle( fontColour = "#030303", fgFill = "#b4c6e7",halign = "right",wrapText = TRUE,
                     textDecoration = "bold",fontName ="Times New Roman")
fcla <- createStyle( fontColour = "#030303", fgFill = "#d9e1f2",halign = "right",wrapText = TRUE,
                     textDecoration = "bold",fontName ="Times New Roman")
datoso <- createStyle( fontColour = "#030303", fgFill = "#d1d6e0",halign = "right",wrapText = TRUE,
                       fontName ="Times New Roman")
datosc <- createStyle( fontColour = "#030303", fgFill = "#ebeef5",halign = "right",wrapText = TRUE,
                       fontName ="Times New Roman")
datosol <- createStyle( fontColour = "#030303", fgFill = "#d1d6e0",halign = "left",wrapText = TRUE,
                        fontName ="Times New Roman")
datoscl <- createStyle( fontColour = "#030303", fgFill = "#ebeef5",halign = "left",wrapText = TRUE,
                        fontName ="Times New Roman")
contenido <- createStyle( halign = "center", valign = "center", textDecoration = "bold",
                          fontName = "Times New Roman", fontSize = 10 )
format_num <- createStyle(numFmt = "#,##0.00")

escenarios <- paste0( 'escenario_', 1:3 )

for ( escenario in escenarios ) { # escenario <- escenarios[1]
  
# Contenido ----------------------------------------------------------------------------------------
message( '\tCreando índice')
  
rep_nom <- paste0( parametros$resultado_seguro, 'IESS_IVM_balances_', parametros$anio_ini,'_', escenario, '.xlsx' )  # Nombre del Excel
wb <- createWorkbook()
sheet_name <- "Contenido"
addWorksheet( wb, sheet_name )
writeData( wb, sheet_name, 'Índice', startRow = 1, startCol = 2)
writeFormula( wb, sheet_name, startRow = 3, startCol = 2,  
                x = makeHyperlinkString(sheet = 'diccionario', text = 'Diccionarios de variables'))
writeFormula( wb, sheet_name, startRow = 4, startCol = 2,  
                x = makeHyperlinkString(sheet = 'balance_corriente', 
                                        text = 'Balance corriente '))
writeFormula( wb, sheet_name, startRow = 5, startCol = 2,  
                x = makeHyperlinkString(sheet = 'balance_actuarial', 
                                        text = 'Balance actuarial'))
addStyle( wb, sheet_name, contenido, rows = 1:20, cols = 1:3,  gridExpand = TRUE, stack = TRUE )
setColWidths( wb, sheet_name, cols = 2, widths = 40 )
  
# Diccionario variables ----------------------------------------------------------------------------
message( '\tCreando hoja (diccionario)' )

sheet_name <- 'diccionario'
addWorksheet( wb, sheet_name )
mergeCells(wb, sheet_name, c( 1:2 ), 1 )
mergeCells(wb, sheet_name, c( 1:2 ), 26 )
writeData( wb, sheet_name, 'Balance corriente', startRow = 1 )
writeDataTable( wb, sheet_name, diccionario_tot_corriente, startRow = 2, colNames = TRUE, rowNames = FALSE )
addStyle( wb, sheet_name, titulosos, 1:2, 1:dim( diccionario_tot_corriente )[ 2 ], gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, fcla, seq( 3, dim( diccionario_tot_corriente )[ 1 ] + 2, 2 ), 1, gridExpand = TRUE, 
          stack = TRUE )
addStyle( wb, sheet_name, fosc, seq( 4, dim( diccionario_tot_corriente )[ 1 ] + 2, 2 ), 1, gridExpand = TRUE, 
          stack = TRUE )
addStyle( wb, sheet_name, datoscl, seq( 3, dim( diccionario_tot_corriente )[ 1 ] + 2, 2 ), 
          2:dim( diccionario_tot_corriente )[ 2 ], gridExpand = TRUE, stack = TRUE)
addStyle( wb, sheet_name, datosol, seq( 4, dim( diccionario_tot_corriente )[ 1 ] + 2, 2 ), 
          2:dim( diccionario_tot_corriente )[ 2 ], gridExpand = TRUE, stack = TRUE)
addStyle( wb, sheet_name, bordes1, 1:dim( diccionario_tot_corriente )[ 1 ] + 2, 
          1:dim( diccionario_tot_corriente )[ 2 ], gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, bordes2, 1:dim( diccionario_tot_corriente )[ 1 ], 1, gridExpand = TRUE, 
          stack = TRUE )
addStyle( wb, sheet_name, bordes3, 1, 1:dim( diccionario_tot_corriente )[ 2 ], gridExpand = TRUE, 
          stack = TRUE )
writeData( wb, sheet_name, 'Balance actuarial', startRow = 26 )
writeDataTable( wb, sheet_name, diccionario_tot_dinamico , startRow = 4 + dim( diccionario_tot_corriente )[ 1 ], 
                colNames = TRUE, rowNames = FALSE )
addStyle( wb, sheet_name, titulosos, 26:27, 1:dim( diccionario_tot_dinamico )[ 2 ], gridExpand = TRUE, 
          stack = TRUE )
addStyle( wb, sheet_name, fcla, seq( 28, 48, 2 ), 1, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, fosc, seq( 29, 48, 2 ), 1, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, datoscl, seq( 28, 48, 2 ), 2:dim( diccionario_tot_dinamico )[ 2 ], 
          gridExpand = TRUE, stack = TRUE)
addStyle( wb, sheet_name, datosol, seq( 29, 48, 2 ), 2:dim( diccionario_tot_dinamico )[ 2 ],
          gridExpand = TRUE, stack = TRUE)
addStyle( wb, sheet_name, bordes1, 27:48, 1:dim( diccionario_tot_dinamico )[ 2 ], 
          gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, bordes2, 27:48, 1, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, bordes3, 26, 1:dim( diccionario_tot_dinamico )[ 2 ], 
          gridExpand = TRUE, stack = TRUE )
setColWidths( wb, sheet_name, 1, widths = 13 )
setColWidths( wb, sheet_name, 2, widths = 55 )

# Reporte balance corriente ------------------------------------------------------------------------
message( '\tGenerando balance corriente_', escenario )

load( paste0( parametros$ivm_rdata_icomp_balance, escenario, '.RData' ) )
rep_balance <- balance_anual[ , list( anio = t + parametros$anio_ini, t, M, A, A_est, B, G, V_cor,
                                        V_cap, A2, A4, A5, A7, A8, B4, B5, B7, B8,
                                        B_pen, B_aux ) ]
setorder( rep_balance, t )

sheet_name <- 'balance_corriente'
addWorksheet( wb, sheet_name )
writeDataTable( wb, sheet_name, rep_balance ,startRow = 1, colNames = TRUE, rowNames = FALSE )
addStyle( wb, sheet_name, titulosos, 1, 1:dim( rep_balance )[ 2 ], gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, fcla, seq( 2, dim( rep_balance )[ 1 ] + 1, 2 ), 1, gridExpand = TRUE, 
          stack = TRUE )
addStyle( wb, sheet_name, fosc, seq( 3, dim( rep_balance )[ 1 ] + 1, 2 ), 1, gridExpand = TRUE, 
          stack = TRUE )
addStyle( wb, sheet_name, datosc, seq( 2, dim( rep_balance )[ 1 ] + 1, 2 ), 2:dim( rep_balance )[ 2 ], 
          gridExpand = TRUE, stack = TRUE)
addStyle( wb, sheet_name, datoso, seq( 3, dim( rep_balance )[ 1 ] + 1, 2 ), 2:dim( rep_balance )[ 2 ],
          gridExpand = TRUE, stack = TRUE)
addStyle( wb, sheet_name, bordes1, 1:dim( rep_balance )[ 1 ] + 1, 1:dim( rep_balance )[ 2 ], 
          gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, bordes2, 1:dim( rep_balance )[ 1 ], 1, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, bordes3, 1, 1:dim( rep_balance )[ 2 ], gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, format_num , rows = 1:dim( rep_balance )[ 1 ] + 1, cols = 3:dim( rep_balance )[ 2 ], 
          gridExpand = TRUE, stack = TRUE )
setColWidths( wb, sheet_name, c( 1, 2, 11:14 ), widths = 6 )
setColWidths( wb, sheet_name, c( 3:10, 15:dim(rep_balance)[ 2 ] ), widths = 18 )

# Reporte balance actuarial ----------------------------------------------------------------------
message( '\tGenerando balance actuarial_', escenario )
rep_balance <- balance_anual[ , list( anio = t + parametros$anio_ini, t,  M_vap, A, A_est, B, G,
                                      V0, V, A2_vap, A4_vap, A5_vap, A7_vap, A8_vap, A_est_vap, A_vap,
                                      B4_vap, B5_vap, B7_vap, B8_vap, B_pen_vap, B_aux_vap) ]
setorder( rep_balance, t )

sheet_name <- 'balance_actuarial'
addWorksheet( wb, sheet_name )
writeDataTable( wb, sheet_name, rep_balance, startRow = 1, colNames = TRUE, rowNames = FALSE )
addStyle( wb, sheet_name, titulosos, 1, 1:dim( rep_balance )[ 2 ], gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, fcla, seq( 2, dim( rep_balance )[ 1 ] + 1, 2 ), 1, gridExpand = TRUE, 
          stack = TRUE )
addStyle( wb, sheet_name, fosc, seq( 3, dim( rep_balance )[ 1 ] + 1, 2 ), 1, gridExpand = TRUE, 
          stack = TRUE )
addStyle( wb, sheet_name, datosc, seq( 2, dim( rep_balance )[ 1 ] + 1, 2 ), 2:dim( rep_balance )[ 2 ], 
          gridExpand = TRUE, stack = TRUE)
addStyle( wb, sheet_name, datoso, seq( 3, dim( rep_balance )[ 1 ] + 1, 2 ), 2:dim( rep_balance )[ 2 ],
          gridExpand = TRUE, stack = TRUE)
addStyle( wb, sheet_name, bordes1, 1:dim( rep_balance )[ 1 ] + 1, 1:dim( rep_balance )[ 2 ], 
          gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, bordes2, 1:dim( rep_balance )[ 1 ], 1, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, bordes3, 1, 1:dim( rep_balance )[ 2 ], gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, format_num , rows = 1:dim( rep_balance )[ 1 ] + 1, cols = 3:dim( rep_balance )[ 2 ], 
          gridExpand = TRUE, stack = TRUE )
setColWidths( wb, sheet_name, c( 1, 2, 11:14 ), widths = 8 )
setColWidths( wb, sheet_name, c( 3:10, 15:dim(rep_balance)[ 2 ] ), widths = 18 )

# Guardando resultados -----------------------------------------------------------------------------
message( '\tGuardando resultados' )

openxlsx::saveWorkbook(wb, file = rep_nom, overwrite = TRUE )

}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
