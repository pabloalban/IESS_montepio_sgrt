message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de causas de desfinanciamiento' )

#Cargando información financiera--------------------------------------------------------------------
file<-paste0(parametros$Data_seg, 'IESS_IVM_causas_desfinanciamiento.xlsx' )


#Carga de causas de desfinanciamiento---------------------------------------------------------------
causa_desf_estado <- read_excel(file,
                             sheet = 'Contri_Est',
                             col_names = TRUE,
                             col_types = NULL,
                             na = "",
                             skip = 0) %>% clean_names()

causa_desf_CD501 <- read_excel(file,
                      sheet = 'CD501',
                      col_names = TRUE,
                      col_types = NULL,
                      na = "",
                      skip = 0) %>% clean_names()

causa_desf_desinversiones <- read_excel(file,
                             sheet = 'Desinversiones',
                             col_names = TRUE,
                             col_types = NULL,
                             na = "",
                             skip = 0) %>% clean_names()

causa_desf_total <- read_excel(file,
                                        sheet = 'Total',
                                        col_names = TRUE,
                                        col_types = NULL,
                                        na = "",
                                        skip = 0) %>% clean_names()

comparacion_primas <- read_excel(file,
                               sheet = 'Comp_primas',
                               col_names = TRUE,
                               col_types = NULL,
                               na = "",
                               skip = 0) %>% clean_names()

causa_desf_desinversiones_anual <- read_excel(file,
                                 sheet = 'Desinversiones_anuales',
                                 col_names = TRUE,
                                 col_types = NULL,
                                 na = "",
                                 skip = 0) %>% clean_names()

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando causas de desfinanciamiento' )

save( causa_desf_estado,
      causa_desf_CD501,
      causa_desf_desinversiones,
      causa_desf_total,
      comparacion_primas,
      causa_desf_desinversiones,
      causa_desf_desinversiones_anual,
      file = paste0( parametros$RData_seg, 'IESS_IVM_causas_desfinanciamiento.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()