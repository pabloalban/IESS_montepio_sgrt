# Script generado para compilar el informe
message( paste( rep( '-', 100 ), collapse = '' ) )

# Carga información --------------------------------------------------------------------------------
message('\tCargando información' )
setwd( parametros$work_dir )

# Control ------------------------------------------------------------------------------------------
REP_copy_final <- FALSE
REP_knit_quiet <- TRUE
REP_hacer_graficos <- TRUE
REP_hacer_tablas <- TRUE
REP_latex_clean <- TRUE
REP_latex_aux_clean <- FALSE
REP_latex_quiet <- TRUE

# Parámetros ---------------------------------------------------------------------------------------
message('\tEstableciendo parámetros')
REP_rep_latex_script <- parametros$reporte_latex_script
REP_style <- 'style_ssc.tex'
REP_rep_dir <- parametros$resultado_seguro
REP_style_math <- 'comandosEPN.sty'
REP_bib_lib <- 'bibliografia_libros.bib'
REP_bib_art <- 'bibliografia_articulos.bib'
REP_bib_ley <- 'bibliografia_leyes.bib'

REP_watermark <- paste0( 'Borrador ', parametros$fec_eje, ' ', format( Sys.time(), '%H:%M:%S' ) )

# Automatización de gráficas para Script -----------------------------------------------------------
message('\tAutomatización de gráficas para Script') 

# Carpeta R ----------------------------------------------------------------------------------------
message('\t\tCarpeta R') 
dir_lec_int <- paste0( parametros$work_dir, 'R/' )
setwd( dir_lec_int )

s_file <- data.frame( file.info( list.files( dir_lec_int ,pattern = "\\.R$", recursive = FALSE),
                                 extra_cols = FALSE ) )

s_file <- rownames( s_file[ s_file$isdir==FALSE, ] )

# Script a borrarse --------------------------------------------------------------------------------
del <- c('001_correr_proyecto.R', '002_cargar_paquetes.R', '003_configurar_proyecto.R',
         '004_calcular_checksums.R', '005_verificacion_checksums.R', '400_preparar_reporte.R',
         '401_graf_plantilla.R', '402_graf_tasas_macro.R', '403_graf_tasa_familia_tipo.R',
         '500_tildes_a_latex.R', '502_tab_plantilla.R')

s_file <- s_file[ s_file%notin%del]

REP_file_latex_org  <- NULL
REP_file_latex_des <- NULL
for ( j in 1:length( s_file)) { # j <- 1
  REP_file_latex_org  <- c( REP_file_latex_org, paste( parametros$work_dir, 'R/', s_file[j], sep = '' ))
  REP_file_latex_des  <- c( REP_file_latex_des, paste( REP_rep_dir, 'graficos/', s_file[j], sep = '' ) )
}

# Carpeta R\SSC-------------------------------------------------------------------------------------
message('\t\tCarpeta R-IVM') 
dir_lec_int <- paste0( parametros$work_dir, 'R/IVM/' )
setwd( dir_lec_int )
aux <- data.frame( file.info( list.files( dir_lec_int ,pattern = "\\.R$", recursive = FALSE),
                              extra_cols = FALSE ) )
aux <- rownames( aux[ aux$isdir==FALSE, ] )

# Script a borrarse --------------------------------------------------------------------------------
del <- c('100_lectura_analisis_demografico_ssc.R', '101_lectura_analisis_financiero_ssc.R',
         '102_lectura_fuentes_externas_ssc.R', '102_lectura_inversiones_ssc.R',
         '103_lectura_grapher_ssc.R', '104_lectura_tasas_brutas_mortalidad_ssc.R',
         '105_lectura_datos_otros_editores_ssc.R', '106_lectura_analisis_contexto_economico_ssc.R',
         '107_lectura_input_proyecciones_ssc.R', '108_lectura_outputs_modelo_ilo_pensions_ssc.R',
         '109_lectura_data_2019_sal.R', '110_lectura_data_sal.R', '111_filtrado_datos_sal.R', 
         '112_estimacion_sal.R', '113_lectura_causas_desfinanciamiento_ssc.R', '120_lectura_actas_ssc.R',
         
         '200_direccionamiento_output_ilo_pension_ssc.R', '201_ejecucion_lecturas_ssc.R', 
         
         '304_calculo_indicadores_ssc.R', '310_calculo_balance_salud.R', '312_analisis_ratios_ssc.R',
         '315_analisis_sensibilidad_medica_ssc.R', 
         
         '400_graf_analisis_demografico_ssc.R', 
         '401_graf_analisis_financiero_ssc.R', '402_graf_inversiones_ssc.R"', '403_graf_grapher_ssc.R',
         '404_graf_analisis_contexto_economico_ssc.R', '405_graf_proyecciones_ssc.R', 
         '406_graf_balance_actuarial_ssc.R', '407_graf_analisis_sensibilidad_ssc.R',
         '410_graf_biometricas_estaticas_ssc.R', '411_graf_convertir_pdf_png_ssc.R', 
         
         '500_tab_analisis_demografico_ssc.R', '501_tab_analisis_financiero_ssc.R', 
         '502_tab_analisis_contexto_economico_ssc.R', '505_tab_inversiones_ssc.R', 
         '506_tab_proyecciones_ssc.R', '507_tab_escenarios_ssc.R', '508_tab_balance_ssc.R', 
         '509_descripcion_var_poblacion_ssc.R', '510_descripcion_var_balance_ssc.R', 
         '511_tab_resumen_resultados_ssc.R', '512_tab_causas_desfinanciamiento_ssc.R', 
         '513_tab_analisis_sensibilidad_ssc.R', '515_tab_biometricas_estaticas_ssc.R', 
         '520_tab_actas_trabajo_ssc.R', 
         
         '600_reporte_latex_ssc.R', '601_reporte_poblacion_ssc.R', 
         '602_reporte_balance_ssc.R')

aux <- aux[ aux%notin%del]

for ( j in 1:length( aux )) { 
  REP_file_latex_org  <- c( REP_file_latex_org, paste( parametros$work_dir, 'R/IVM/', aux[j], sep = '' ))
  REP_file_latex_des  <- c( REP_file_latex_des, paste( REP_rep_dir, 'graficos/', aux[j], sep = '' ) )
}

#Jendry --------------------------------------------------------------------------------------------
message('\t\tCarpeta Jendry') 
dir_lec_int <- paste0( parametros$Data_seg, 'Scripts/Jendry_Toapanta/' )
setwd( dir_lec_int )
aux1 <- data.frame( file.info( list.files( dir_lec_int ,pattern = "\\.R$", recursive = FALSE),
                               extra_cols = FALSE ) )
aux1 <- rownames( aux1[ aux1$isdir==FALSE, ] )

# Script a borrarse --------------------------------------------------------------------------------
del <- NULL

aux1 <- aux1[ aux1%notin%del]

for ( j in 1:length( aux1 )) { 
  REP_file_latex_org  <- c( REP_file_latex_org, paste( parametros$Data_seg, 'Scripts/Jendry_Toapanta/', aux1[j], sep = '' ))
  REP_file_latex_des  <- c( REP_file_latex_des, paste( REP_rep_dir, 'graficos/', aux1[j], sep = '' ) )
}

#Karen Tapia ---------------------------------------------------------------------------------------
message('\t\tCarpeta Karen Tapia') 
dir_lec_int <- paste0( parametros$Data_seg, 'Scripts/Karen_Tapia/' )
setwd( dir_lec_int )
aux2 <- data.frame( file.info( list.files( dir_lec_int ,pattern = "\\.sql$", recursive = FALSE),
                               extra_cols = FALSE ) )
aux2 <- rownames( aux2[ aux2$isdir==FALSE, ] )

# Script a borrarse --------------------------------------------------------------------------------
del <- NULL

aux2 <- aux2[ aux2%notin%del]

for ( j in 1:length( aux2 )) { 
  REP_file_latex_org  <- c( REP_file_latex_org, paste( parametros$Data_seg, 'Scripts/Karen_Tapia/', aux2[j], sep = '' ))
  REP_file_latex_des  <- c( REP_file_latex_des, paste( REP_rep_dir, 'graficos/', aux2[j], sep = '' ) )
}

#Consolidación en un solo archivo ------------------------------------------------------------------
REP_file_latex_org <- c( REP_file_latex_org ,
                         paste(  parametros$work_dir, 'Reportes/', 'style_ssc.tex', sep = '' ),
                         paste(  parametros$work_dir, 'Reportes/', 'comandosEPN.sty', sep = '' ),
                         paste(  parametros$work_dir, 'Reportes/logo_iess_azul.png', sep = '' )
)

REP_file_latex_des <- c( REP_file_latex_des,
                         paste(  REP_rep_dir, 'style_ssc.tex', sep = '' ),
                         paste(  REP_rep_dir, 'comandosEPN.sty', sep = '' ),
                         paste(  REP_rep_dir, 'graficos/logo_iess_azul.png', sep = '' )
)

file.copy( REP_file_latex_org, REP_file_latex_des, overwrite = TRUE  )

#Coversión LaTeX -----------------------------------------------------------------------------------
message('\tConversión a LaTeX legnguaje R') 
s_file <- c( s_file, aux, aux1 )

for(i in 1:length(s_file)){ # i <- 1
  file <- readLines( paste0(parametros$resultado_graficos, s_file[i] ))
  file <- grep("^[^#]", file, value = TRUE)# Remove comments
  writeLines( file, paste0( parametros$resultado_graficos , gsub(".R", ".txt", s_file[i])) )
  file.rename( paste0( parametros$resultado_graficos , gsub(".R", ".txt", s_file[i])),
               paste0(parametros$resultado_graficos, s_file[i] ) )
}

a <- NULL

for( i in 1:length(s_file)){
  a[i] <- paste0('\\textbf{', str_replace_all( s_file[i], "_", "\\\\_"),'}',
                 '\\par\\lstinputlisting[language=R]{graficos/',
                 s_file[i], '}') 
}

tem <- paste0( '\\@for\\sun:={', paste( a, collapse=",") , '}\\do{\\sun}')
cat( tem, file = paste0( parametros$resultado_tablas, 'iess_input_script', '.tex' ))

#Conversión a LaTeX legnguaje SQL-------------------------------------------------------------------
message('\tConversión a LaTeX lenguaje SQL') 
s_file <- aux2

for(i in 1:length(s_file)){ # i <- 1
  file <- readLines( paste0(parametros$resultado_graficos, s_file[i] ))
  file <- grep("^[^-]", file, value = TRUE)# Remove comments
  writeLines( file, paste0( parametros$resultado_graficos , gsub(".sql", ".txt", s_file[i])) )
  file.rename( paste0( parametros$resultado_graficos , gsub(".sql", ".txt", s_file[i])),
               paste0(parametros$resultado_graficos, s_file[i] ) )
}

a <- NULL
for( i in 1:length(s_file)){
  a[i] <- paste0('\\textbf{', str_replace_all(s_file[i], "_", "\\\\_"),'}',
                 '\\par\\lstinputlisting[language=SQL]{graficos/',
                 s_file[i], '}')
}
tem <- NULL
tem <- paste0( '\\@for\\sun:={', paste( a, collapse=",") , '}\\do{\\sun}')

cat( tem, file = paste0( parametros$resultado_tablas, 'iess_input_script_sql', '.tex' ))


REP_file_latex_clean <- c( paste( REP_rep_dir, 'bibliografia_libros.bib', sep = '' ), 
                           paste( REP_rep_dir, 'bibliografia_articulos.bib', sep = '' ),
                           paste( REP_rep_dir, 'bibliografia_leyes.bib', sep = '' ), 
                           paste( REP_rep_dir, 'style.tex', sep = '' ),
                           paste( REP_rep_dir, 'style_ssc.tex', sep = '' ),
                           paste( REP_rep_dir, 'comandosEPN.sty', sep = '' ))

setwd( parametros$work_dir )

# Compilación reporte ------------------------------------------------------------------------------
message('\tInicio compilación para Script')

# Kniting reporte  for script-----------------------------------------------------------------------
setwd( parametros$reporte_seguro ) 
knit( input = "reporte_script.Rnw", 
      output = paste0( REP_rep_dir, REP_rep_latex_script ),
      quiet = REP_knit_quiet, encoding = 'utf8' )

# Compilacion LaTeX --------------------------------------------------------------------------------
message('\tInicio compilación LaTeX Script')
setwd( REP_rep_dir )
tools::texi2pdf( REP_rep_latex_script, quiet = REP_latex_quiet, clean = REP_latex_clean )  
setwd( parametros$work_dir )
message('\tFin compilación LaTeX Script')

if( REP_latex_aux_clean ) {
  unlink( REP_file_latex_clean, recursive = TRUE )
}

del_R <- REP_file_latex_des[REP_file_latex_des%notin%REP_file_latex_clean]

unlink( del_R, recursive = TRUE )

message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()