
##########################################
##########################################
#####
#####  Mexico city incidents - Data preparation
#####  
#####
##########################################
##########################################



##### Data extraction
#####
#####################################
######################################

#list.files()

if(!exists("dt_datapull"))
{
dt_datapull <- read_csv(file = "./data/incidentes_viales_2014_2021oct.csv",
                        locale = readr::locale(encoding = "latin1") # avoid encoding problems with spanish language
                        )
}



#####################
##################### BACKUP
#####################
dt <- dt_datapull


##### General - Data preparation and EDA
#####
#####################################
######################################


### Relevant columns and rename into English
dt <- dt %>% 
        select(
                # "X"
                # "folio"
                fecha_creacion,
                hora_creacion,
                dia_semana,
                # "fecha_cierre
                # "hora_cierre
                incidente_c4,
                delegacion_inicio,
                latitud,
                longitud,
                # "ano_cierre
                clas_con_f_alarma,
                tipo_entrada,
                # "delegacion_cierre
                # "mes_cierre      
                codigo_cierre, 
        ) %>% 
        rename(
                report_date = fecha_creacion,
                report_time = hora_creacion,
                report_weekday = dia_semana,
                incident_type = incidente_c4, # accident, injured, earthquake / with injured, no injured, etc
                delegation = delegacion_inicio,
                lat = latitud,
                long = longitud,
                incident_class = clas_con_f_alarma, # crime, emergency, medical urgency, etc.
                incident_reporttype = tipo_entrada, #how was the incident reported?
                incident_code = codigo_cierre
        )



### Convert to lower case all character columns
dt <- dt %>% mutate_if(is.character, tolower)

### Replace all non-alphanumeric characters with space in incident_type
dt$incident_type <- str_replace_all(dt$incident_type, "[^[:alnum:]]", " ")


##### Data validation and correct types
#####
#####################################
######################################

### Date & time variables
# - We'll have NAs since there are some invalid dates and time data

dt <- dt %>% 
        rename(report_date_orig = report_date,
               report_time_orig = report_time) %>%
        mutate(report_date= ymd(report_date_orig),
               report_time = hms(report_time_orig)) 

### Creating additional date/time variables
dt <- dt %>% 
        mutate(
                report_year = as.factor(year(report_date)),
                report_month = as.factor(month(report_date)),
                report_day = as.factor(day(report_date)),
                report_hour = as.factor(report_time@hour)
        )




# dt[which(is.na(dt$report_date)),]
# dt[which(is.na(dt$report_time)),]

### Numerics
col_num <- c("lat", "long")
dt[,col_num] <- lapply(dt[,col_num], as.numeric)


### Factors
col_factors <- c("report_weekday", "incident_type", "delegation", "incident_reporttype","incident_code", "incident_class")
dt[,col_factors] <- lapply(dt[,col_factors], as.factor)




### Rename levels into english
###
###

levels(dt$report_weekday)
dt$report_weekday<- recode_factor(dt$report_weekday,
                                  domingo= "sunday",
                                  jueves="thursday",
                                  lunes="monday",
                                  martes="tuesday",
                                  miercoles="wednesday",
                                  sabado="saturday",
                                  viernes="friday")
#table(dt$report_weekday_recode, dt$report_weekday) #test
dt$report_weekday <- fct_relevel(dt$report_weekday, "monday", "tuesday", "wednesday",
                                 "thursday", "friday", "saturday", "sunday")



#levels(dt$incident_class)
dt$incident_class <- recode_factor(dt$incident_class,
                                   delito= "crime",
                                   emergencia="emergency",
                                   'falsa alarma'="false_alarm",
                                   'urgencias medicas'="medical_urgency")
#table(dt$incident_class) #test


#levels(dt$incident_reporttype) 
dt$incident_reporttype<- recode_factor(dt$incident_reporttype,
                                       'aplicativos'= "unknown",
                                       'botón de auxilio'= "help_button",   
                                       'cámara'= "camera",
                                       'llamada app911'= "app911",
                                       'llamada del 066'= "call066",
                                       'llamada del 911'= "call911",
                                       radio= "radio",
                                       redes= "social_media",
                                       zello= "appZello")

#table(dt$incident_reporttype)





# ### Missing values - exploring missingness mechanism => will not do now to save time
# ###
# ###
# 
# # Are they missing at random?
# # Seems like not, but let's take a closer look since data is too big
# vis_miss(dt, warn_large_data=FALSE)
# 
# # Which variables are affected?
# dt %>% is.na() %>% colSums()
# 
# ### Investigating
# # report_date: missing mostly when incidet_code = F (false)
# # delegation: missing only on incident_class = FALSA alarma => unimportant; delete
# # lat, long: only missing with "delito"
# str(dt)
# gg_miss_var(dt)
# gg_miss_fct(dt, fct = report_date)
# gg_miss_fct(dt, fct = report_weekday)
# gg_miss_fct(dt, fct = incident_type)
# gg_miss_fct(dt, fct = delegation)
# gg_miss_fct(dt, fct = incident_class)
# gg_miss_fct(dt, fct = incident_code)




### Breaking down potential important information
###
###

#cat(paste0('"',levels(dt$incident_type),'",'), sep="\n")

dt <-  dt %>% 
        mutate(injured_flag = case_when(
                incident_type %in% c("accidente choque con lesionados",
                                     "accidente choque con prensados",
                                     "accidente persona atrapada   desbarrancada",
                                     "cadáver accidente automovilístico",
                                     "cadáver atropellado",
                                     "detención ciudadana atropellado",
                                     "lesionado accidente automovilístico",
                                     "lesionado atropellado",
                                     "sismo choque con lesionados",
                                     "sismo choque con prensados",
                                     "sismo persona atropellada") ~ "yes",
                incident_type %in% c("accidente choque sin lesionados",
                                     "sismo choque sin lesionados") ~ "no",
                incident_type %in% c("accidente ciclista",
                                     "accidente ferroviario",
                                     "accidente monopatín",
                                     "accidente motociclista",
                                     "accidente otros",
                                     "accidente volcadura",
                                     "detención ciudadana accidente automovilístico",
                                     "mi ciudad calle incidente de tránsito",
                                     "mi ciudad taxi incidente de tránsito",
                                     "accidente vehiculo atrapado",
                                     "accidente vehículo atrapado varado",
                                     "accidente vehiculo desbarrancado")  ~ "unspecified"),
               death_flag = case_when(
                       incident_type %in% c(
                               "cadáver accidente automovilístico",
                               "cadáver atropellado") ~ "yes",
                       incident_type %in% c(
                               "accidente choque con lesionados",
                               "accidente choque con prensados",
                               "accidente choque sin lesionados",
                               "sismo choque sin lesionados",
                               "sismo choque con lesionados",
                               "sismo choque con prensados",
                               "lesionado accidente automovilístico",
                               "lesionado atropellado") ~ "no",
                       incident_type %in% c(
                               "accidente ciclista",
                               "accidente ferroviario",
                               "accidente monopatín",
                               "accidente motociclista",
                               "accidente otros",
                               "sismo persona atropellada", 
                               "mi ciudad calle incidente de tránsito",
                               "mi ciudad taxi incidente de tránsito",
                               "accidente persona atrapada   desbarrancada",
                               "accidente vehiculo atrapado",
                               "accidente vehículo atrapado varado",
                               "accidente vehiculo desbarrancado",
                               "accidente volcadura",
                               "detención ciudadana accidente automovilístico",
                               "detención ciudadana atropellado")  ~ "unspecified")
        )

### Transform the new vars to factor
dt[,c("injured_flag", "death_flag")] <- lapply(dt[,c("injured_flag", "death_flag")], as.factor)




### Sense check and filtering
###
###

dt <- dt %>% 
        filter(lat <= 20)
