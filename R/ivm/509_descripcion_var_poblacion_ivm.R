message( paste( rep('-', 100 ), collapse = '' ) )

# Creando Diccionarios -----------------------------------------------------------------------------
message( '\tCreando diccionarios' )

diccionario_default <- data.table( item = c('anio', 'sexo', 't', 'x', 'H', 'M'), 
                                   descripcion = c('Año',
                                                   'Sexo de la Persona', 
                                                   'Año del Horizonte de Estudio', 
                                                   'Edad Simple',
                                                   'Hombre', 
                                                   'Mujer') )

diccionario_poblacion <- data.table( item = c(paste('l', 1:10, sep = '' ) ),
                                     descripcion = c( 'Económicamente activo no afiliado',
                                                      'Afiliado activo',
                                                      'Afiliado inactivo',
                                                      'Pensionista por vejez',
                                                      'Pensionista por invalidez',
                                                      'Muerto',
                                                      'Pensionado de montepío viudas',
                                                      'Pensionado de montepío huerfanos',
                                                      'Dependientes cónyugues',
                                                      'Dependientes hijos' ) )
                
diccionario_transicion_poblacion <- data.table( item = c( 'l1_2', 'l1_6', 'l2_3', 'l2_4', 'l2_5',
                                                          'l2_6', 'l3_2', 'l4_6', 'l5_6' ),
            descripcion = c( 'Transición desde económicamente activo no afliado hacia afliado activo',
                             'Transición desde económicamente activo no afliado hacia muerto',
                             'Transición desde afliado activo hacia afliado inactivo',
                             'Transición desde afliado activo hacia pensionado por vejez',
                             'Transición desde afliado activo hacia pensionado por invalidez',
                             'Transición desde afliado activo hacia muerto',
                             'Transición desde afliado inactivo hacia afliado activo',
                             'Transición desde pensionado por vejez hacia muerto',
                             'Transición desde pensionado por invalidez hacia muerto' ) )

diccionario_tot <- do.call( 'rbind', list( diccionario_default, 
                                           diccionario_poblacion,
                                           diccionario_transicion_poblacion ) )

