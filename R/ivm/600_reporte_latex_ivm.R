# Preparación --------------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/002_configuracion_seguro.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos genéricos -------------------------------------------------------------------------------
source( 'R/macro/400_graf_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/demografia/401_graf_tasa_familia_tipo.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/demografia/402_graficos_tasas.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/demografia/407_graf_proyeccion_poblacion.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos específicos IVM -------------------------------------------------------------------------
source( 'R/ivm/400_graf_analisis_demografico_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/401_graf_analisis_financiero_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/403_graf_inversiones_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/403_graf_balance_actuarial_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/404_graf_analisis_sensibilidad_ivm.R', encoding = 'UTF-8', echo = FALSE )

# Tablas genéricas ---------------------------------------------------------------------------------
source( 'R/macro/500_tab_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/demografia/503_tab_proyeccion_poblacion.R', encoding = 'UTF-8', echo = FALSE )

# Tablas específicas IVM ---------------------------------------------------------------------------
source( 'R/ivm/500_tab_analisis_demografico_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/501_tab_analisis_financiero_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/502_tab_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/503_tab_inversiones_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/504_tab_escenarios_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/506_tab_analisis_sensibilidad_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/508_tab_causas_desfinanciamientos_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/505_tab_primas_ivm.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX ------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

# Reportes excel -----------------------------------------------------------------------------------
source( 'R/ivm/601_reporte_poblacion_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/602_reporte_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )

