# Preparación --------------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos genéricos--------------------------------------------------------------------------------
source( 'R/macro/400_graf_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )
#source( 'R/demografia/401_graf_tasa_familia_tipo.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/demografia/402_graficos_tasas.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/demografia/407_graf_proyeccion_poblacion.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/400_graf_analisis_demografico_ivm.R', encoding = 'UTF-8', echo = FALSE )

# Tablas genéricas ---------------------------------------------------------------------------------
source( 'R/macro/500_tab_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/demografia/503_tab_proyeccion_poblacion.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/500_tab_analisis_demografico_ivm.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos específicas de RTR ----------------------------------------------------------------------
source( 'R/rtr/002_configuracion_seguro.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/400_graf_analisis_demografico_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/401_graf_inversiones_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/402_graf_analisis_financiero_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/403_graf_comp_primas_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/404_graf_balance_actuarial_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/405_graf_proyeccion_poblacion_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/406_graf_factores_riesgo_rtr.R', encoding = 'UTF-8', echo = FALSE )

# Tablas específicas de RTR ------------------------------------------------------------------------
source( 'R/rtr/500_tab_analisis_financiero_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/501_tab_analisis_demografico_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/502_tab_situacion_actual_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/503_tab_causas_desfinanciamientos_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/504_tab_inversiones_rtr.R', encoding = 'UTF-8', echo = FALSE )
#source( 'R/rtr/511_tab_decrementos.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/507_tab_primas_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/508_tab_proyeccion_poblacion_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/509_tab_decrementos_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/510_tab_resumen_resultados_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/505_tab_balance_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/506_tab_escenarios_rtr.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX ------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

# Reportes excel -----------------------------------------------------------------------------------