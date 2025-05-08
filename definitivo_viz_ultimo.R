##“TP1_GRUPO_3”##
## Visualizaciones Plazo Medio Cobranzas
### Algoritmos y Estructura de Datos
### v20220830 CM
### 20220831
### 20220903



library(readr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(gganimate)
library(gifski)
library(janitor)
library(DT)
library(devtools)
#devtools::install_github("jeromefroe/circlepackeR") 
library(circlepackeR)
library(Hmisc)
library(treemap)
library(d3Tree)
library(sqldf)
library(plotly)
library(ggfortify)

###### se importa dataset completo con transformaciones a factor, numeric y date para poder crear visualizaciones acordes
###esto lo cree desde el archivo generado por Alberto y Seba llamado dataset_completo.csv y plazo_medio.csv
dataset_completo <- read_delim("dataset_completo.csv", 
                               delim = ";", escape_double = FALSE, col_types = cols(...1 = col_skip(), 
                                                                                    codcom_factura = col_factor(levels = c("1311", 
                                                                                                                           "1313", "1314", "1317")), codtal_factura = col_factor(levels = c("1302", 
                                                                                                                                                                                            "1304", "1306", "1312", "1313", 
                                                                                                                                                                                            "1341", "1342", "1346")), fecha_factura = col_date(format = "%Y-%m-%d"), 
                                                                                    fecha_de_vencimiento_factura = col_date(format = "%Y-%m-%d"), 
                                                                                    importe = col_number(), codtal_cobranza = col_factor(levels = c("3011", 
                                                                                                                                                    "3012", "3211", "3213", "3214", 
                                                                                                                                                    "3215", "3216", "3217", "3218", 
                                                                                                                                                    "3220", "3221")), numero_cobranza = col_number(), 
                                                                                    fecha_cobranza = col_date(format = "%Y-%m-%d"), 
                                                                                    concepto_de_cobranza = col_factor(levels = c("221", 
                                                                                                                                 "223", "224", "232", "241", "242", 
                                                                                                                                 "243", "244", "251")), medio_de_cobranza = col_factor(levels = c("CHEQUES TERCEROS - creacion", 
                                                                                                                                                                                                  "DEPOSITO BANCARIO", "DIF.COT EN CONTRA", 
                                                                                                                                                                                                  "EFECTIVO", "RETENCION  GAN SUFRIDA", 
                                                                                                                                                                                                  "RETENCION  IVA SUFRIDA", "RETENCION IIBB SUFRIDA", 
                                                                                                                                                                                                  "RETENCION SUSS SUFRIDA", "TRANSFERENCIA BANCARIA")), 
                                                                                    fecha_de_vencimiento_medio_de_pago = col_date(format = "%Y-%m-%d"), 
                                                                                    dias_de_cobro = col_number()), locale = locale(grouping_mark = ""), 
                               trim_ws = TRUE)



levels(dataset_completo$medio_de_cobranza) <- list(dataset_completo$medio_de_cobranza, CHEQUES_TERCEROS = "CHEQUES TERCEROS - creacion", DEPOSITO = "DEPOSITO BANCARIO", DIF.COT_CONTRA = "DIF.COT EN CONTRA", EFECT = "EFECTIVO", RET.GAN_SUFRIDA = "RETENCION  GAN SUFRIDA",
                                                   RET.IVA_SUFRIDA = "RETENCION  IVA SUFRIDA", RET.IIBB_SUFRIDA = "RETENCION IIBB SUFRIDA", RET.SUSS_SUFRIDA = "RETENCION SUSS SUFRIDA", TRANSF = "TRANSFERENCIA BANCARIA" )


plazo_medio <- read_delim("plazo_medio.csv", 
                          delim = ";", escape_double = FALSE, col_types = cols(...1 = col_skip()), 
                          trim_ws = TRUE)

plazo_medio1 <- read_delim("plazo_medio.csv", 
                           delim = ";", escape_double = FALSE, col_types = cols(...1 = col_skip()), 
                           trim_ws = TRUE)

plazo_medio1 <- rename(plazo_medio1, plazo_medio_numerico = "plazo_medio")


plazo_medio1 <- mutate(plazo_medio1, plazo_medio_numerico)

###empezamos con limpieza de datos

clientes_pagos <- mutate(plazo_medio, plazo_medio = case_when (
  (plazo_medio < 0 & plazo_medio <= 30) ~  1,
  (plazo_medio > 30 & plazo_medio < 60) ~ 2,
  plazo_medio > 60 ~ 3,
  TRUE ~ 1
))

clientes_pagos$plazo_medio <- factor(clientes_pagos$plazo_medio, levels = c(1,2,3), labels = c("Cliente al día", "Cliente promedio", "Cliente con mora"), ordered = T)




plazo_medio <- clientes_pagos


#unión de datos y columnas con guardado de csv. para facilitar la visualización 

nuevo_dataset_completo <- merge(dataset_completo, plazo_medio1)
nuevo_dataset_completo <- select(nuevo_dataset_completo, -dias_de_cobro)

write_csv(nuevo_dataset_completo, "nuevo_dataset_completo.csv")

solorazonsocialyplazomedio <- merge(plazo_medio, plazo_medio1) 
write_csv(solorazonsocialyplazomedio, "solorazonsocialyplazo.csv")  


clientes_pagos1 <- mutate(dataset_completo, dias_de_cobro = case_when (
  (dias_de_cobro < 0 & dias_de_cobro <= 30) ~  1,
  (dias_de_cobro> 30 & dias_de_cobro < 60) ~ 2,
  dias_de_cobro > 60 ~ 3,
  TRUE ~ 1
))
clientes_pagos1$tipo_cliente<- factor(clientes_pagos1$dias_de_cobro, levels = c(1,2,3), labels = c("Cliente al día", "Cliente promedio", "Cliente con mora"), ordered = T)

dataset_completo <- clientes_pagos1

#primera visualización simple, a modo de ejemplo, con conlusión
annotations <- data.frame(
  x = c(round(min(plazo_medio1$plazo_medio_numerico),2), round(mean(plazo_medio1$plazo_medio_numerico),2), round(max(plazo_medio1$plazo_medio_numerico), 2)),
  y = c(3, 300, 5),
  label = c("Días de pago min:", "Dias de pago mean:", "max de dias de pago:"))
  
  
primer_Grafico <- ggplot (plazo_medio1, (aes (x = plazo_medio_numerico))) +
  geom_histogram(binwidth = 50, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  geom_text(data = annotations, aes(x = x,y = y, label = paste(label, x)), size = 3, fontface = "bold") +
  theme(
  ) +
  labs(
  title = "Histograma de plazo de pagos de clientes",
  subtitle = "Plazo en días",
  x = "Plazo medio de pago ",
  y = "Días"
)

pl000 <- ggplotly(primer_Grafico)

  
#conclusión primer visualizacióbn simple <- se observa que los clientes pagan al día la mayoría de las veces.

#filtro de variables para responder a preguntas particulares

empresas_al_día <- filter(plazo_medio, plazo_medio == "Cliente al día")
empresas_promedio <- filter(plazo_medio, plazo_medio == "Cliente promedio")
empresas_con_mora<- filter(plazo_medio, plazo_medio == "Cliente con mora")

#visualicemos formas de pago preferidas y si el cliente esta al dia o no

grafico_formadepagopreferida <- ggplot(data= dataset_completo, aes(x = medio_de_cobranza, fill = tipo_cliente))+
  geom_bar(position = "dodge") +
  labs(x = "Medio de cobranza", fill = "Tipo de cliente")

pl <- ggplotly(grafico_formadepagopreferida)
pl 



#se observa que la mayoría de los clientes al día pagan en efectivo/// 
#esta visualización se mejora más abajo

#vamos a comparar formas de pago con importes y tipo de cliente /// 
#todas estas visualizaciones se mejoran y filtran más abajo en el script
#Este es un primer gráfico, con todos los medios de pagos, para mostrar tendencias de forma de pago, no es definitivo.
grafico_formadepagopreferida1 <-   ggplot(data=dataset_completo, (aes(x = tipo_cliente, fill = medio_de_cobranza))) +
  geom_bar() +
  labs(x = "Tipo de cliente", fill = "Medio de Cobranza")
pl1 <- ggplotly(grafico_formadepagopreferida1)



grafico_formadepagoparecidapoint <- ggplot(dataset_completo, aes(tipo_cliente,importe, shape  = medio_de_cobranza))+
  geom_point(aes(colour = factor(medio_de_cobranza)), size = 2) +
  coord_flip()

pl2 <-   ggplotly(grafico_formadepagoparecidapoint)
pl2


##visualizaciones avanzadas con datasets que creamos más arriba, 


completo <- read_csv("nuevo_dataset_completo.csv", 
                     col_types = cols(codcom_factura = col_factor(levels = c("1311", 
                                                                             "1313", "1314", "1317")), codtal_factura = col_factor(levels = c("1302", 
                                                                                                                                              "1304", "1306", "1312", "1313", "1341", 
                                                                                                                                              "1342", "1346")), fecha_factura = col_date(format = "%Y-%m-%d"), 
                                      fecha_de_vencimiento_factura = col_date(format = "%Y-%m-%d"), 
                                      codtal_cobranza = col_factor(levels = c("3011", 
                                                                              "3012", "3211", "3213", "3214", 
                                                                              "3215", "3216", "3217", "3218", 
                                                                              "3220", "3221")), fecha_cobranza = col_date(format = "%Y-%m-%d"), 
                                      concepto_de_cobranza = col_factor(levels = c("221", 
                                                                                   "223", "224", "232", "241", "242", 
                                                                                   "243", "244", "251")), medio_de_cobranza = col_factor(levels = c("CHEQUES_TERCEROS", 
                                                                                                                                                    "DEPOSITO", "DIF.COT_CONTRA", 
                                                                                                                                                    "EFECT", "RET.GAN_SUFRIDA", "RET.IVA_SUFRIDA", 
                                                                                                                                                    "RET.IIBB_SUFRIDA", "RET.SUSS_SUFRIDA", 
                                                                                                                                                    "TRANSF")), fecha_de_vencimiento_medio_de_pago = col_date(format = "%Y-%m-%d"), 
                                      tipo_cliente = col_skip()), locale = locale(grouping_mark = ""))
options("scipen"=1, "digits"=4)
clientes_pagos1 <- mutate(completo, plazo_medio_numerico = case_when (
  (plazo_medio_numerico< 0 & plazo_medio_numerico <= 30) ~  1,
  (plazo_medio_numerico> 30 & plazo_medio_numerico < 60) ~ 2,
  plazo_medio_numerico > 60 ~ 3,
  TRUE ~ 1
))
clientes_pagos1$tipo_cliente<- factor(clientes_pagos1$plazo_medio_numerico, levels = c(1,2,3), labels = c("Cliente al día", "Cliente promedio", "Cliente con mora"), ordered = T)

clientes_pagos1 <- rename(clientes_pagos1, plazo_medio_factor = "plazo_medio_numerico")

nuevo_completo <- merge(completo, clientes_pagos1)

write_csv(nuevo_completo, "nuevo_completo.csv")  




#empezamos con las visualizaciones avanzadas

#lo primero que quiero visualizar es el medio de pago más utilizados por nuestros clientes 
#filtramos los medios de pago que más nos interesan

pagostransf <- filter(nuevo_completo, medio_de_cobranza == "TRANSF")
pagoscheques <- filter(nuevo_completo, medio_de_cobranza == "CHEQUES_TERCEROS")
pagosdeposito <- filter(nuevo_completo, medio_de_cobranza == "DEPOSITO")
pagosefect <- filter(nuevo_completo, medio_de_cobranza == "EFECT")

cobranzas <- rbind(pagostransf, pagoscheques)
cobranzas1 <- rbind(pagosdeposito, pagosefect)
todos_cobranzas <- rbind(cobranzas, cobranzas1)


#distintas visualizaciones

grafico_formadepagofavoritafiltrada <- ggplot(todos_cobranzas, aes(x=tipo_cliente,y=importe, shape = medio_de_cobranza, alpha = medio_de_cobranza, color = medio_de_cobranza))+
  geom_point(aes(color = medio_de_cobranza)) +
  labs(title= "Importes de pago por tiempo de clientes")+
  xlab("Tipo de cliente") +
  ylab("Importe") +
  ylim(0,15000000) 

pl30 <- ggplotly(grafico_formadepagofavoritafiltrada)


cobranzaviz <- ggplot(todos_cobranzas, aes(x = medio_de_cobranza , y = importe)) +
  geom_jitter(colour = as.numeric(todos_cobranzas$medio_de_cobranza), size = 4) +
  ylim(0, 1500000) +
  labs(title = "Formas de pago elegidas", subtitle = "Medio de pago favorito - Fecha: {frame_time}", x = "Medio de cobranza", y = "Importe") +
  theme_bw() +
  transition_time(fecha_cobranza) +
  shadow_mark(past = TRUE)

animate(cobranzaviz, fps = 5, end_pause = 15)

#ahora queremos saber cuantos clientes usan distintos medios de pago

razonsocial_formaspago <- tabyl(todos_cobranzas, razon_social, medio_de_cobranza)


razonsocial_formaspago %>%
  datatable(extensions = 'Buttons',
            filter     = "top",
            class      = "display nowrap compact",
            caption    = htmltools::tags$caption(
              style      = 'caption-side: bottom; text-align: center;',
              'Table 1: ', htmltools::em('Tabla de clientes y medios de pago')),
            options = list(dom = 'Blfrtip',
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           lengthMenu = list(c(10,25,50,-1),
                                             c(10,25,50,"todos"))) )


#queremos saber cuales de nuestros clientes tiene mejores hábitos de pago para ofrecer mejores condiciones
#importamos dataset
plazo_medio <- read_delim("plazo_medio.csv", 
                          delim = ";", escape_double = FALSE, col_types = cols(...1 = col_skip(), 
                                                                               plazo_medio = col_number()), trim_ws = TRUE)
razonsocialtipocliente <- mutate(plazo_medio, plazo_medio = case_when (
  (plazo_medio < 0 & plazo_medio <= 30) ~  1,
  (plazo_medio > 30 & plazo_medio < 60) ~ 2,
  plazo_medio  > 60 ~ 3,
  TRUE ~ 1
))
razonsocialtipocliente$tipocliente<- factor(razonsocialtipocliente$plazo_medio, levels = c(1,2,3), labels = c("Cliente al día", "Cliente promedio", "Cliente con mora"), ordered = T)

clientes <- select(razonsocialtipocliente, c(-plazo_medio))
clientepromedio <- filter(clientes, tipocliente == "Cliente promedio")
clientealdia <- filter(clientes, tipocliente== "Cliente al día")
clientemora <- filter(clientes, tipocliente == "Cliente con mora")


clientesparapromo <- rbind(clientepromedio, clientealdia)


#listado de clientes completa
razonsocialtipocliente %>%
  datatable(
    extensions = 'Buttons',
    editable = list(target = "row", disable = list(columns = c(1, 3)),
                    filter     = "top",
                    class      = "cell-border stripe",
                    caption    = htmltools::tags$caption(
                      style      = 'caption-side: bottom; text-align: center;',
                      'Table 1: ', htmltools::em('Clientes')),
                    options = list(dom = 'Blfrtip',
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,25,50,-1),
                                                     c(10,25,50,"Clientes")))))


#queremos solo listas de clientes con mora


clientemora %>%
  datatable(colnames = c('Clientes', 'Tipo de Cliente'),
            extensions = 'Buttons',
            filter     = "top",
            class      = "cell-border stripe",
            caption    = htmltools::tags$caption(
              style      = 'caption-side: bottom; text-align: center;',
              'Table 1: ', htmltools::em('Clientes que pagan despúes de término')),
            options = list(dom = 'Blfrtip',
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           lengthMenu = list(c(10,25,50,-1),
                                             c(10,25,50,"Clientes en mora"))))


#clientes aptos para promo
clientesparapromo %>%
  datatable(colnames = c('Clientes', 'Tipo de Cliente'),
            extensions = 'Buttons',
            filter     = "top",
            class      = "cell-border stripe",
            caption    = htmltools::tags$caption(
              style      = 'caption-side: bottom; text-align: center;',
              'Table 1: ', htmltools::em('Clientes que podrían recibir promos o beneficios')),
            options = list(dom = 'Blfrtip',
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           lengthMenu = list(c(10,25,50,-1),
                                             c(10,25,50,"Clientes para promoción"))))

#podemos hacer cada lista con cada tipo de cliente (consultar) Nota para mi.



#Queremos promediosde pagos y promedio de días de pago (con esto se puede hacer mensajesdentro de loa chunks en
#RMarkdown, por eso los asigne a una variable)

promedio_importes <- nuevo_completo %>%
  summarise(promedio_pagos = mean(importe))

#el importe promedio de pagos es de $59130

promedio_dias <- nuevo_completo %>%
  summarise(promedio_dias = mean(plazo_medio_numerico))

#el promedio de días de pago es de 51.56 días

#resumenes de info para agregarle a conclusiones del informe

resumen_info <- nuevo_completo %>%
  summarise(importe  = sum(importe) ,
            avg_importe = mean(importe),
            min_importe = min(importe),
            max_cantidad = max(importe),
            cant_clientes = n_distinct(razon_social),
            avg_plazopago = mean(plazo_medio_numerico),
            ds_plazopago = sd(plazo_medio_numerico))



describe(nuevo_completo)


#otras viz - el heatmap esta mas o menos
#nuevo limpieza de datos para últimas visualizaciones
razonsocial_formaspago$razon_social<- as.factor(razonsocial_formaspago$razon_social)

razonsocial_formaspago$razon_social <- as.numeric(razonsocial_formaspago$razon_social)

razonsocial_formaspago <- select(razonsocial_formaspago,  c(-DIF.COT_CONTRA, -RET.GAN_SUFRIDA, -RET.IVA_SUFRIDA, -RET.IIBB_SUFRIDA, -RET.SUSS_SUFRIDA))
data <- as.matrix(razonsocial_formaspago)
colnames(data) <- paste("Forma de pago.", 1:5)
rownames(data) <- paste("Cliente", 1:528)


heatmap(data, ColSideColors = rainbow(ncol(razonsocial_formaspago)),
        RowSideColors = rainbow(nrow(razonsocial_formaspago))) 

#última visualización

datos <- select(todos_cobranzas, razon_social, importe, tipo_cliente, medio_de_cobranza, plazo_medio_factor, plazo_medio_numerico, fecha_cobranza, fecha_de_vencimiento_medio_de_pago)
datos <- as.data.frame(datos)
elipses <- ggplot(todos_cobranzas, aes(x = plazo_medio_factor, y = importe)) +
  geom_jitter(col = "orange", shape=13, stroke = 0) +
  stat_ellipse(geom = "polygon",
               aes(fill = medio_de_cobranza),
               alpha = 0.25) +
  scale_size_area(max_size = 0.5, guide = NULL) +
  scale_y_continuous(limits = c(0,1000000)) +
  labs(title = "Gráfico de importes de pagos según forma de pago y tipo de cliente",
       subtitle = "Cobranzas 2020 a 2022",
       caption = "Los números del 1 al 3 representan el tipo de cliente que hizo el pago y las elipses representan el medio de pago que se utilizó, cada punto representa un pago",
       x = "Tipo de cliente según comportamiento de pago",
       y = "Importe de pagos",
       color = "")
elipses  

elipses <- ggplotly(elipses, tooltip = c("importe")) %>%
  layout(legend = list(
    orientation = "h",
    x = 0.7,
    y = 0
  )
)
#por último, necesito saber el ranking de mejores pagadores por importe de pagos.

clientes_ranking <- sqldf("SELECT razon_social,
                             ROUND(SUM(importe)) AS importe_total
                        FROM dataset_completo
                    GROUP BY razon_social 
                    ORDER BY razon_social")



dataset_ultimaviz <- select(clientes_ranking, importe_total, razon_social)

dataset_ultimaviz = clientes_ranking %>%
  filter(importe_total > 20000000)

dataset_ultimaviz$importe_total <- dataset_ultimaviz$importe_total / 1000000

porclientepago <- ggplot(dataset_ultimaviz ,aes(x = reorder(razon_social, importe_total) , y = importe_total, label = razon_social ))  +
  geom_segment(size = 0.08, aes(xend = razon_social, yend=0)) +
  coord_flip() +    # para girar el  grafico
  geom_point( size=2, color="blue") +
  ylim(0, 200)+
  ggtitle(paste0("Ranking de clientes por importes de pago")) +
  theme(plot.title = element_text(lineheight = 1,face ='bold'))   +
  ylab("") +
  xlab("") +
  labs(x = "Clientes", y = "Importes de pagos (por millones de $)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(importe_total,1)), size = 2, position = position_stack(vjust = 0.5))#+
  #geom_text(aes(label = razon_social), size = 2, position = position_stack(vjust = 3.8))
  
porclientepago <- ggplotly(porclientepago, tooltip = c("importe de pagos")) %>%
  layout(legend = list(
    orientation = "h",
    x = 0.7,
    y = 1
  )
  )

porclientepago

