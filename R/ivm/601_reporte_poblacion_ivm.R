
# Cargando diccionario -----------------------------------------------------------------------------
message(' \tCargando dicccionario' )

source( 'R/ivm/509_descripcion_var_poblacion_ivm.R', encoding = 'UTF-8', echo = FALSE )

# Cargando población proyectada --------------------------------------------------------------------
message(' \tCargando población proyectada' )

load( parametros$demo_rdata_sgo_pob_proy )

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

# Preparación de la población proyectada -----------------------------------------------------------
message( '\tGenerando reporte de población proyectada' )

rep_pob_proy <- pob_proy[ , list( anio = t + parametros$anio_ini, sexo, t, x, l1, l2, l3, l4, l5, 
                                  l6,l7,l8, l9, l10, l11, l1_2, l1_6, l2_3, l2_4, l2_5, l2_6, l3_2, l4_6, l5_6 ) ]

rep_pob_proy[ sexo=='H', sexo := 'Hombre' ]
rep_pob_proy[ sexo=='M', sexo := 'Mujer' ]
rep_pob_proy <- as.data.table( rep_pob_proy )
setorder( rep_pob_proy, t, sexo, x )

# Contenido ----------------------------------------------------------------------------------------
message( '\tCreando índice')

nombre <- paste0( parametros$resultado_seguro, 'IESS_IVM_poblacion_evolucion.xlsx')   # Nombre del Excel
wb <- createWorkbook( )
sheet_name <- "Contenido"
addWorksheet( wb, sheet_name )
writeData( wb, sheet_name, 'Índice', startRow = 1, startCol = 2 )
writeFormula( wb, sheet_name, startRow = 3, startCol = 2,  
              x = makeHyperlinkString( sheet = 'diccionario', text = 'Diccionarios de variables' ) )
writeFormula( wb, sheet_name, startRow = 4, startCol = 2,  
              x = makeHyperlinkString( sheet = 'poblacion_proyectada_edad_sexo', 
                                       text = 'Población proyectada por edad y sexo' ) )
writeFormula( wb, sheet_name, startRow = 5, startCol = 2,  
              x = makeHyperlinkString( sheet = 'poblacion_total_proyectada', 
                                       text = 'Población total proyectada' ) )
addStyle( wb, sheet_name, contenido, rows = 1:20, cols = 1:3,  gridExpand = TRUE, stack = TRUE )
setColWidths( wb, sheet_name, cols = 2, widths = 40 )

# Diccionario variables ----------------------------------------------------------------------------
message( '\tCreando hoja (diccionario)' )

sheet_name <- 'diccionario'
addWorksheet( wb, sheet_name )
diccionario_tot <- diccionario_tot[ , .( Item = item , Descripción = descripcion ) ]
writeData(wb, sheet_name, diccionario_tot, startRow = 1, colNames = TRUE, rowNames = FALSE )
addStyle( wb, sheet_name, titulosos, 1, 1:dim( diccionario_tot )[ 2 ], gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, fcla, seq( 2, dim( diccionario_tot )[ 1 ] + 1, 2 ), 1, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, fosc, seq( 3, dim( diccionario_tot )[ 1 ] + 1, 2 ), 1, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, datoscl, seq( 2, dim( diccionario_tot )[ 1 ] + 1, 2 ), 2:dim( diccionario_tot )[ 2 ], 
          gridExpand = TRUE, stack = TRUE)
addStyle( wb, sheet_name, datosol, seq( 3, dim( diccionario_tot )[ 1 ] + 1, 2 ), 2:dim( diccionario_tot )[ 2 ],
          gridExpand = TRUE, stack = TRUE)
addStyle( wb, sheet_name, bordes1, 1:dim( diccionario_tot )[ 1 ], 1:dim( diccionario_tot )[ 2 ], 
          gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, bordes2, 1:dim( diccionario_tot )[ 1 ], 1, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, bordes3, 1, 1:dim( diccionario_tot )[ 2 ], 
          gridExpand = TRUE, stack = TRUE )
setColWidths( wb, sheet_name, 2, widths = 65 )

# Población proyectada -----------------------------------------------------------------------------
message( '\tCreando hoja (poblacion_proyectada)' )

sheet_name <- 'poblacion_proyectada_edad_sexo'
addWorksheet( wb, sheet_name )
writeDataTable( wb, sheet_name, rep_pob_proy, startRow = 1, colNames = TRUE, rowNames = FALSE )
addStyle( wb, sheet_name, titulosos, 1, 1:dim( rep_pob_proy )[ 2 ], gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, fcla, seq( 2, dim( rep_pob_proy )[ 1 ] + 1, 2 ), 1, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, fosc, seq( 3, dim( rep_pob_proy )[ 1 ] + 1, 2 ), 1, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, datosc, seq( 2, dim( rep_pob_proy )[ 1 ] + 1, 2 ), 2:dim( rep_pob_proy )[ 2 ], 
          gridExpand = TRUE, stack = TRUE)
addStyle( wb, sheet_name, datoso, seq( 3, dim( rep_pob_proy )[ 1 ] + 1, 2 ), 2:dim( rep_pob_proy )[ 2 ],
          gridExpand = TRUE, stack = TRUE)
addStyle( wb, sheet_name, bordes1, 1:dim( rep_pob_proy )[ 1 ] + 1, 1:dim( rep_pob_proy )[ 2 ], 
          gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, bordes2, 1:dim( rep_pob_proy )[ 1 ] + 1, 1, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, bordes3, 1, 1:dim( rep_pob_proy )[ 2 ] + 1, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, format_num, 1:dim( rep_pob_proy )[ 1 ] + 1, 5:dim( rep_pob_proy )[ 2 ], 
          gridExpand = TRUE, stack = TRUE )
setColWidths( wb, sheet_name, c( 1, 3, 4 ), widths = 6 )
setColWidths( wb, sheet_name, c( 2 ), widths = 8 )

# Población total proyectada -----------------------------------------------------------------------
message( '\tCreando hoja (poblacion_total_proyectada)' )

rep_pob_proy_total <- pob_proy_tot[ , list( t, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l1_2, l1_6, l2_3, l2_4, l2_5,
                                            l2_6, l3_2, l4_6, l5_6 ) ]
sheet_name <- 'poblacion_total_proyectada'
addWorksheet( wb, sheet_name )
writeDataTable( wb, sheet_name, rep_pob_proy_total, startRow = 1, colNames = TRUE, rowNames = FALSE )
addStyle( wb, sheet_name, titulosos, 1, 1:dim( rep_pob_proy_total )[ 2 ], gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, fcla, seq( 2, dim( rep_pob_proy_total )[ 1 ] + 1, 2 ), 1, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, fosc, seq( 3, dim( rep_pob_proy_total )[ 1 ] + 1, 2 ), 1, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, datosc, seq( 2, dim( rep_pob_proy_total )[ 1 ] + 1, 2 ), 2:dim( rep_pob_proy_total )[ 2 ], 
          gridExpand = TRUE, stack = TRUE)
addStyle( wb, sheet_name, datoso, seq( 3, dim( rep_pob_proy_total )[ 1 ] + 1, 2 ), 2:dim( rep_pob_proy_total )[ 2 ],
          gridExpand = TRUE, stack = TRUE)
addStyle( wb, sheet_name, bordes1, 1:dim( rep_pob_proy_total )[ 1 ] + 1, 1:dim( rep_pob_proy_total )[ 2 ], 
          gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, bordes2, 1:dim( rep_pob_proy_total )[ 1 ] + 1, 1, gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, bordes3, 1, 1:dim( rep_pob_proy_total )[ 2 ], gridExpand = TRUE, stack = TRUE )
addStyle( wb, sheet_name, format_num, 1:dim( rep_pob_proy_total )[ 1 ] + 1, 2:dim( rep_pob_proy_total )[ 2 ],
          gridExpand = TRUE, stack = TRUE )
setColWidths( wb, sheet_name, 1, widths = 5 )
setColWidths( wb, sheet_name, 2:dim( rep_pob_proy_total )[ 2 ], widths = 13 )

# Guardando resultados -----------------------------------------------------------------------------
message( '\tGuardando resultados' )

openxlsx::saveWorkbook( wb, file = nombre, overwrite = TRUE )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
