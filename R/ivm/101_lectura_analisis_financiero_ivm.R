message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo analisis financiero IVM del IESS' )

#file
file <- paste0( parametros$Data_seg, 'IESS_IVM_analisis_financiero.xlsx' )

# Activos IVM --------------------------------------------------
message( '\tLeyendo activos IVM del IESS' )
col_nom <- c( 'Anio', 'Activo', 'Incremento anual', 'Incremento porcentual anual')
col_tip <- c( "text", "numeric", "numeric" , "text")

activos_ivm<-read_excel(file,sheet="Activo",col_names=TRUE)
activos_ivm <- as.data.table( activos_ivm )[1:11,]
setnames( activos_ivm, col_nom )
 
# Componentes de los Activos IVM ----------------------------
message( '\tLeyendo componentes de activos IVM del IESS' )
col_nom <- c( 'descripcion_cuentas', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
col_tip <- c( 'text','numeric','numeric', 'numeric' , 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

comp_activos_ivm<-read_excel(file,sheet="Comp_Acti",col_names=TRUE,guess_max = 24000,col_types = col_tip)
comp_activos_ivm <- as.data.table(comp_activos_ivm )[1:8,]
setnames( comp_activos_ivm, col_nom )

# Análisis horizontal de los Activos IVM ----------------------------
message( '\tLeyendo análisis horizontal de los activos IVM del IESS' )
col_nom <- c( 'DESCRIPCIÓN_DE_LAS_CUENTAS', '2011/2010', '2012/2011', '2013/2012', '2014/2013', '2015/2014', '2016/2015', '2017/2016', '2018/2017', '2019/2018', '2020/2019')
col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

activos_horizon_ivm<-read_excel(file,sheet="Acti_Horiz"
                             ,col_names=TRUE,guess_max = 24000)
activos_horizon_ivm <- as.data.table(activos_horizon_ivm )[1:8,]
setnames( activos_horizon_ivm, col_nom )

# Análisis vertical de los Activos IVM ----------------------------
message( '\tLeyendo análisis vertical de los activos IVM del IESS' )
col_nom <- c( 'DESCRIPCIÓN_DE_LAS_CUENTAS', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

activos_vertical_ivm<-read_excel(file,sheet="Acti_Vert"
                                ,col_names=TRUE,guess_max = 24000)
activos_vertical_ivm <- as.data.table(activos_vertical_ivm )[1:8,]
setnames( activos_vertical_ivm, col_nom )


# Cuentas por cobrar IVM -------------------------------
message( '\tLeyendo cuentas por cobrar IVM del IESS' )
col_nom <- c( 'Anio', 'Cuentas_por_cobrar', 'Incremento anual', 'Incremento porcentual anual')
col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric', 'numeric')

ctasxcobrar_ivm<-read_excel(file,sheet="Cuent_x_Cobrar"
                                  ,col_names=TRUE,guess_max = 24000)
ctasxcobrar_ivm <- as.data.table(ctasxcobrar_ivm )[1:11,]
setnames( ctasxcobrar_ivm, col_nom )

# Pasivos IVM -----------------------------------------------------------
message( '\tLeyendo pasivos de IVM del IESS' )

col_nom <- c( 'Anio', 'Pasivos', 'Incremento anual', 'Incremento porcentual anual')
col_tip <- c( 'text', 'numeric', 'numeric' , 'text')

pasivos_IVM<-read_excel(file,sheet="Pasivo"
                              ,col_names=TRUE,guess_max = 24000)
pasivos_IVM <- as.data.table(pasivos_IVM )[1:11,]
setnames( pasivos_IVM, col_nom )

# Componentes de los pasivos IVM -----------------------------------------------------------
message( '\tLeyendo componentes pasivos de IVM del IESS' )

col_nom <- c( 'descripcion_cuentas', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
col_tip <- c( 'text','numeric','numeric', 'numeric' , 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

comp_pasivos_IVM<-read_excel(file,sheet="Comp_Pasi",col_names=TRUE,guess_max = 24000,col_types = col_tip)
comp_pasivos_IVM <- as.data.table(comp_pasivos_IVM )[1:7,]
setnames( comp_pasivos_IVM, col_nom )

# Analisis horizontal de los pasivos IVM -----------------------------------------------------------
message( '\tLeyendo analisis horizontal de los pasivos de IVM del IESS' )

col_nom <- c( 'DESCRIPCIÓN_DE_LAS_CUENTAS', '2011/2010', '2012/2011', '2013/2012', '2014/2013', '2015/2014', '2016/2015', '2017/2016', '2018/2017', '2019/2018', '2020/2019')
col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

pasivos_horiz_IVM<-read_excel(file,sheet="Pasi_Horiz"
                             ,col_names=TRUE,guess_max = 24000)
pasivos_horiz_IVM <- as.data.table(pasivos_horiz_IVM )[1:7,]
setnames( pasivos_horiz_IVM, col_nom )

# Analisis vertical de los pasivos IVM -----------------------------------------------------------
message( '\tLeyendo analisis vertical de los pasivos de IVM del IESS' )

col_nom <- c( 'DESCRIPCIÓN_DE_LAS_CUENTAS', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

pasivos_vertical_IVM<-read_excel(file,sheet="Pasi_Vert"
                              ,col_names=TRUE,guess_max = 24000)
pasivos_vertical_IVM <- as.data.table(pasivos_vertical_IVM )[1:6,]
setnames( pasivos_vertical_IVM, col_nom )

# Cuentas por pagar IVM-------------------------------------------
message( '\tLeyendo cuentas por pagar de IVM del IESS' )

col_nom <- c( 'Anio', 'Cuentas_por_pagar', 'Incremento anual', 'Incremento porcentual anual')
col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric', 'numeric')

ctasxpagar_IVM <-read_excel(file,sheet="Cuentas_x_Pagar"
                            ,col_names=TRUE,guess_max = 24000)
ctasxpagar_IVM <- as.data.table( ctasxpagar_IVM ) [1:11,]
setnames(ctasxpagar_IVM, col_nom )

# Patrimonio IVM-------------------------------------------
message( '\tLeyendo patrimonio de IVM del IESS' )

col_nom <- c( 'Anio', 'Patrimonio', 'Incremento anual', 'Incremento porcentual anual')
col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric', 'numeric')

patrimonio_IVM <-read_excel(file,sheet="Patrimonio"
                            ,col_names=TRUE,guess_max = 24000)
patrimonio_IVM<- as.data.table( patrimonio_IVM ) [1:11,]
setnames(patrimonio_IVM, col_nom )

# Componentes del Patrimonio IVM-------------------------------------------
message( '\tLeyendo componentes del patrimonio de IVM del IESS' )

col_nom <- c( 'DESCRIPCIÓN_DE_LAS_CUENTAS', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
col_tip <- c( 'text', 'numeric', 'numeric' , 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
comp_patrimonio_IVM <-read_excel(file,sheet="Comp_Patri",col_names=TRUE,guess_max = 24000,col_types = col_tip)
comp_patrimonio_IVM<- as.data.table( comp_patrimonio_IVM ) [1:6,]
setnames(comp_patrimonio_IVM, col_nom )

# Analisis horizontal del Patrimonio IVM-------------------------------------------
message( '\tLeyendo analisis horizontal del patrimonio de IVM del IESS' )

col_nom <- c( 'DESCRIPCIÓN_DE_LAS_CUENTAS', '2011/2010', '2012/2011', '2013/2012', '2014/2013', '2015/2014', '2016/2015', '2017/2016', '2018/2017', '2019/2018', '2020/2019')
col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

patrimonio_horiz_IVM <-read_excel(file,sheet="Patrim_Horiz"
                                 ,col_names=TRUE,guess_max = 24000)
patrimonio_horiz_IVM<- as.data.table( patrimonio_horiz_IVM ) [1:6,]
setnames(patrimonio_horiz_IVM, col_nom )

# Analisis vertical del Patrimonio IVM-------------------------------------------
message( '\tLeyendo analisis vertical del patrimonio de IVM del IESS' )

col_nom <- c( 'DESCRIPCIÓN_DE_LAS_CUENTAS', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

patrimonio_vert_IVM <-read_excel(file,sheet="Patrim_Vert"
                                  ,col_names=TRUE,guess_max = 24000)
patrimonio_vert_IVM<- as.data.table( patrimonio_vert_IVM ) [1:6,]
setnames(patrimonio_vert_IVM, col_nom )

# Ingresos IVM-------------------------------------------
message( '\tLeyendo ingresos de IVM del IESS' )

col_nom <- c( 'Anio', 'Ingresos', 'Incremento anual', 'Incremento porcentual anual')
col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric', 'numeric')

ingresos_IVM <-read_excel(file,sheet="ingresos"
                            ,col_names=TRUE,guess_max = 24000)
ingresos_IVM<- as.data.table( ingresos_IVM ) [1:11,]
setnames(ingresos_IVM, col_nom )

# Componentes del Ingreso IVM-------------------------------------------
message( '\tLeyendo componentes del ingreso de IVM del IESS' )

col_nom <- c( 'DESCRIPCIÓN_DE_LAS_CUENTAS', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
col_tip <- c( 'text', 'numeric', 'numeric' , 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

comp_ingreso_IVM <-read_excel(file,sheet="Comp_ingresos",col_names=TRUE,col_type=col_tip,guess_max = 24000)
comp_ingreso_IVM<- as.data.table( comp_ingreso_IVM ) [1:6,]
setnames(comp_ingreso_IVM, col_nom )

# Analisis horizontal del Ingreso IVM-------------------------------------------
message( '\tLeyendo analisis horizontal del ingreso de IVM del IESS' )

col_nom <- c( 'DESCRIPCIÓN_DE_LAS_CUENTAS', '2011/2010', '2012/2011', '2013/2012', '2014/2013', '2015/2014', '2016/2015', '2017/2016', '2018/2017', '2019/2018', '2020/2019')
col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

ingreso_horiz_IVM <-read_excel(file,sheet="Ingres_horiz"
                                  ,col_names=TRUE,guess_max = 24000)
ingreso_horiz_IVM<- as.data.table( ingreso_horiz_IVM ) [1:6,]
setnames(ingreso_horiz_IVM, col_nom )

# Analisis vertical del Ingreso IVM-------------------------------------------
message( '\tLeyendo analisis vertical del ingreso de IVM del IESS' )

col_nom <- c( 'DESCRIPCIÓN_DE_LAS_CUENTAS', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

ingreso_vert_IVM <-read_excel(file,sheet="ingres_vert"
                                 ,col_names=TRUE,guess_max = 24000)
ingreso_vert_IVM<- as.data.table( ingreso_vert_IVM ) [1:6,]
setnames(ingreso_vert_IVM, col_nom )


# Ingresos por aportes IVM-------------------------------------------
message( '\tLeyendo ingresos por aportes de IVM del IESS' )

col_nom <- c( 'Anio', 'Aportes Personales',	'Aportes Patronales',	'Aportes Jubilados y Pensionistas',
              'Aportes adicionales magisterio',	'Aporte ley Discapacidad',	'Aporte personal Amas de Casa',
              'Aporte Ley Discapacidad ISSPOL', 'Aportes_Afiliados')

col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric', 'numeric', 'numeric', 'numeric' , 'numeric', 'numeric')

ingresosxaportes_IVM <-read_excel(file,sheet="Ing_Aportes"
                          ,col_names=TRUE,guess_max = 24000)
ingresosxaportes_IVM<- as.data.table( ingresosxaportes_IVM ) [1:11,]
setnames(ingresosxaportes_IVM, col_nom )


# Contribución del Estado vs Pensiones Pagadas IVM-------------------------------------------
message( '\tLeyendo Contribución del Estado vs Pensiones Pagadas de IVM del IESS' )

col_nom <- c( 'Anio', 'Contribución_del_Estado',	'Pensiones_Pagadas_de_Invalidez_Vejez_Montepio',	'Participación del Estado en el pago de pensiones')
col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric')

contriEstadovsPensPag_IVM <-read_excel(file,sheet="C_del_Est_vs_Pensi_Pag"
                          ,col_names=TRUE,guess_max = 24000)
contriEstadovsPensPag_IVM<- as.data.table( contriEstadovsPensPag_IVM ) [1:11,]
setnames(contriEstadovsPensPag_IVM, col_nom )

# Egresos Prestacionales IVM-------------------------------------------
message( '\tLeyendo egresos prestacionales de IVM del IESS' )

col_nom <- c( 'Anio', 'egresos_prestacionales', 'Incremento anual', 'variacion anual')
col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric')

egresos_prestacionales<-read_excel(file,sheet="egresos prestacionales"
                        ,col_names=TRUE,guess_max = 24000)
egresos_prestacionales<- as.data.table( egresos_prestacionales ) [1:11,]
setnames(egresos_prestacionales, col_nom )

# Gastos Administrativos IVM-------------------------------------------

col_nom <- c( 'Anio', 'Gastos', 'Incremento anual', 'variacion anual')
col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric')

gastos_administracion <-read_excel(file,sheet="gastos administracion"
                        ,col_names=TRUE,guess_max = 24000)
gastos_administracion<- as.data.table( gastos_administracion ) [1:11,]
setnames(gastos_administracion, col_nom )

# Componentes de egresos ----------------------------------

message( '\tLeyendo componentes de egresos de IVM del IESS' )

col_nom <- c( 'DESCRIPCIÓN_DE_LAS_CUENTAS', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
col_tip <- c( 'text', 'numeric', 'numeric' , 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

comp_egresos_IVM <-read_excel(file,sheet="Comp_Egre_v4",col_names=TRUE,guess_max = 24000,col_types = col_tip)
comp_egresos_IVM <- as.data.table( comp_egresos_IVM ) [1:10,]
setnames(comp_egresos_IVM, col_nom )

# Analisis horizontal de egresos ----------------------------------

message( '\tLeyendo analisis horizontal de egresos de IVM del IESS' )

col_nom <- c( 'DESCRIPCIÓN_DE_LAS_CUENTAS', '2011/2010', '2012/2011', '2013/2012', '2014/2013', '2015/2014', '2016/2015', '2017/2016', '2018/2017', '2019/2018', '2020/2019')
egresos_horiz_IVM <-read_excel(file,sheet="Egres_Horiz_v4",col_names=TRUE,guess_max = 24000)
egresos_horiz_IVM <- as.data.table( egresos_horiz_IVM) [1:4,]
setnames(egresos_horiz_IVM, col_nom )

# Analisis vertical de egresos ----------------------------------

message( '\tLeyendo analisis vertical de egresos de IVM del IESS' )

col_nom <- c( 'DESCRIPCIÓN_DE_LAS_CUENTAS', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
egresos_vert_IVM <-read_excel(file,sheet="Egres_Vert_v4",col_names=TRUE,guess_max = 24000)
egresos_vert_IVM <- as.data.table( egresos_vert_IVM) [1:4,]
setnames(egresos_vert_IVM, col_nom )

# Gastos prestados por pensiones ----------------------------------

message( '\tLeyendo Gastos prestados por pensiones de IVM del IESS' )

col_nom <- c('Anio', 'Pensiones_de_Invalidez_Vejez_Montepío', 'Tasa de variación de las pensiones pagadas')
col_tip <- c( 'character', 'numeric', 'numeric')

gastxprest_IVM <-read_excel(file,sheet="Gast_prestxpensi"
                              ,col_names=TRUE,guess_max = 24000)
gastxprest_IVM <- as.data.table( gastxprest_IVM) [1:11,]
setnames(gastxprest_IVM, col_nom )


# Otros Gastos Pres IVM-------------------------------------------
message( '\tLeyendo Gastos prest 1 de IVM del IESS' )

col_nom <- c( 'Anio', '13ra Pensión',	'14ta Pensión', 'Auxilios Funerales',	'Total')
col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric', 'numeric')

otrosgstospres_IVM <-read_excel(file,sheet="Otros_Gast_Pres"
                              ,col_names=TRUE,guess_max = 24000)
otrosgstospres_IVM <- as.data.table(otrosgstospres_IVM ) [1:11,]
setnames(otrosgstospres_IVM, col_nom )


# Gastos Contribución Administradora IVM-------------------------------------------
message( '\tLeyendo Gastos Contribución Administradora de IVM del IESS' )

col_nom <- c( 'Anio', 'Gasto Contribución Administradora','Crecimiento de Gasto Contribución Administradora', 'Crecimiento Porcentual de Gasto Contribución Administradora')

col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric')

gastcontadmin_IVM <-read_excel(file,sheet="Gst_Cont_Admini"
                                ,col_names=TRUE,guess_max = 24000)
gastcontadmin_IVM<- as.data.table(gastcontadmin_IVM ) [1:11,]
setnames(gastcontadmin_IVM, col_nom )

# Ingresos vs egresos prestacionales IVM-------------------------------------------
message( '\tLeyendo Ingresos vs egresos prestacionales de IVM del IESS' )

col_nom <- c( 'anio', 'aportes de afiliados', 'egresos prestacionales', '60% egresos prestacionales','resultado', 'utilizacion anual','acumulacion reserva')

col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric', 'numeric' , 'numeric','numeric' )

ing_vs_egre_pres_IVM <-read_excel(file,sheet="ing_vs_egre_pres",col_names=TRUE,guess_max = 24000)
ing_vs_egre_pres_IVM<- as.data.table(ing_vs_egre_pres_IVM) [1:11,]
setnames(ing_vs_egre_pres_IVM, col_nom )

# Ingresos vs gastos administracion IVM-------------------------------------------
message( '\tLeyendo Ingresos vs gastos administrativos de IVM del IESS' )

col_nom <- c( 'Anio', 'Aportes de afiliados', 'gastos administracion', 'Resultado', 'Utilización anual')

col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric', 'numeric'  )

ing_vs_gas_adm_IVM <-read_excel(file,sheet="ing_vs_gas_adm"
                               ,col_names=TRUE,guess_max = 24000)
ing_vs_gas_adm_IVM<- as.data.table(ing_vs_gas_adm_IVM ) [1:11,]
setnames(ing_vs_gas_adm_IVM, col_nom )

# Reservas Disponibles -------------------------------------------
message( '\tLeyendo Reservas Disponibles del IVM del IESS' )

col_nom <- c( 'Anio', 'Gastos prestacionales por pensiones', 'Patrimonio', 'Relación Patrimonio/Gastos')

col_tip <- c( 'character', 'numeric', 'numeric' , 'numeric')

reservas_IVM <-read_excel(file,sheet="Reservas"
                            ,col_names=TRUE,guess_max = 24000)
reservas_IVM<- as.data.table(reservas_IVM ) [1:11,]
setnames(reservas_IVM, col_nom )

# Porcentaje promedio de representatividad de los componentes del Activo  IVM ----------------------------
message( '\tLeyendo Porcentaje promedio de representatividad de los componentes del Activo IVM del IESS' )
col_nom <- c( 'Cuentas', 'Porcentaje')
col_tip <- c( 'character', 'numeric')

porcent_prom_comp_activos_ivm<-read_excel(file,sheet="Comp_Acti"
                             ,col_names=TRUE,guess_max = 24000)
porcent_prom_comp_activos_ivm <- as.data.table(porcent_prom_comp_activos_ivm)[11:17,2:3]
setnames( porcent_prom_comp_activos_ivm, col_nom )

# Porcentaje promedio de representatividad de los componentes del Pasivos  IVM ----------------------------
message( '\tLeyendo Porcentaje promedio de representatividad de los componentes del Pasivos IVM del IESS' )
col_nom <- c( 'Cuentas', 'Porcentaje')
col_tip <- c( 'character', 'numeric')

porcent_prom_comp_pasivos_ivm<-read_excel(file,sheet="Comp_Pasi"
                                          ,col_names=TRUE,guess_max = 24000)
porcent_prom_comp_pasivos_ivm <- as.data.table(porcent_prom_comp_pasivos_ivm)[10:14,3:4]
setnames( porcent_prom_comp_pasivos_ivm, col_nom )

# Porcentaje promedio de representatividad de los componentes del Patrimonio  IVM ----------------------------
message( '\tLeyendo Porcentaje promedio de representatividad de los componentes del Patrimonio IVM del IESS' )
col_nom <- c( 'Cuentas', 'Porcentaje')
col_tip <- c( 'character', 'numeric')

porcent_prom_comp_patrimonio_ivm<-read_excel(file,sheet="Comp_Patri"
                                          ,col_names=TRUE,guess_max = 24000)
porcent_prom_comp_patrimonio_ivm <- as.data.table(porcent_prom_comp_patrimonio_ivm)[10:14,3:4]
setnames( porcent_prom_comp_patrimonio_ivm, col_nom )

# Porcentaje promedio de representatividad de los componentes de los ingresos IVM ----------------------------
message( '\tLeyendo Porcentaje promedio de representatividad de los componentes de los ingresos IVM del IESS' )
col_nom <- c( 'Cuentas', 'Porcentaje')
col_tip <- c( 'character', 'numeric')

porcent_prom_comp_ingres_ivm<-read_excel(file,sheet="Comp_ingresos"
                                             ,col_names=TRUE,guess_max = 24000)
porcent_prom_comp_ingres_ivm <- as.data.table(porcent_prom_comp_ingres_ivm)[10:14,4:5]
setnames( porcent_prom_comp_ingres_ivm, col_nom )

# Porcentaje promedio de representatividad de los componentes de los egresos IVM ----------------------------
message( '\tLeyendo Porcentaje promedio de representatividad de los componentes de los egresos IVM del IESS' )
col_nom <- c( 'Cuentas', 'Porcentaje')
col_tip <- c( 'character', 'numeric')

porcent_prom_comp_egres_ivm<-read_excel(file,sheet="Comp_Egre"
                                         ,col_names=TRUE,guess_max = 24000)
porcent_prom_comp_egres_ivm <- as.data.table(porcent_prom_comp_egres_ivm)[12:18,4:5]
setnames( porcent_prom_comp_egres_ivm, col_nom )


# Guardando ---------------------------------------------------------------

lista <- c('activos_ivm', 'comp_activos_ivm', 'activos_horizon_ivm','activos_vertical_ivm','ctasxcobrar_ivm',
           'pasivos_IVM', 'comp_pasivos_IVM', 'pasivos_horiz_IVM','pasivos_vertical_IVM', 
           'ctasxpagar_IVM', 'patrimonio_IVM', 'comp_patrimonio_IVM', 'patrimonio_horiz_IVM', 'patrimonio_vert_IVM',
           'ingresos_IVM','comp_ingreso_IVM', 'ingreso_horiz_IVM', 'ingreso_vert_IVM', 'ingresosxaportes_IVM', 'contriEstadovsPensPag_IVM',
           'egresos_prestacionales','comp_egresos_IVM','gastos_administracion', 'egresos_horiz_IVM', 'egresos_vert_IVM', 'gastxprest_IVM', 'otrosgstospres_IVM',
           'gastcontadmin_IVM', 'ing_vs_egre_pres_IVM', 'ing_vs_gas_adm_IVM', 'reservas_IVM' , 'porcent_prom_comp_activos_ivm', 'porcent_prom_comp_patrimonio_ivm',
           'porcent_prom_comp_ingres_ivm', 'porcent_prom_comp_egres_ivm', 'porcent_prom_comp_pasivos_ivm')

save( list=lista,
      file = paste0( parametros$RData_seg, 'IESS_IVM_analisis_financiero.RData' ) )


###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

