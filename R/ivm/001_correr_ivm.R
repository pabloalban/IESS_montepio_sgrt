# Ejecución IVM ------------------------------------------------------------------------------------
source( 'R/ivm/002_configuracion_seguro.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------

if ( parametros$ivm_cargar_ilo_res ) {

  source( 'R/ivm/100_lectura_analisis_demografico_ivm.R', encoding = 'UTF-8', echo = FALSE )  
  source( 'R/ivm/108_lectura_causas_desfinanciamiento_ivm.R', encoding = 'UTF-8', echo = FALSE )

}

# Filtros e imputación de datos --------------------------------------------------------------------

# Estadísticas y estimaciones ----------------------------------------------------------------------
source( 'R/ivm/200_estadisticas_demograficas_ivm.R', encoding = 'UTF-8', echo = FALSE )

# Cálculos de escenarios ---------------------------------------------------------------------------
source( 'R/ivm/303_calculo_escenarios_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )

## Análisis actuariales ----------------------------------------------------------------------------
source( 'R/ivm/304_calculo_prima_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/305_analisis_ratios_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ivm/306_analisis_sensibilidad_ivm.R', encoding = 'UTF-8', echo = FALSE )

# Generación reporte -------------------------------------------------------------------------------
source( parametros$ivm_rep_gen, encoding = 'UTF-8', echo = FALSE )
