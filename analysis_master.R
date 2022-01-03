

##########################################
##########################################
#####
#####  Mexico city incidents - Analysis - Master file
#####  
#####
##########################################
##########################################


# Questions - confirmed incidents

# Number of incidents are going down with years. Does this represent 
# the reality?

# Where do we have the highest number of incidents? Time? Weekday? 
# Do Saturday nights have more incidents? (possibly related to drink and drive)

# To predict: incident counts on a particular day / time / delegation.

# Where do most of the "false" incidents occur? why could this be happening?

# which delegation hass the most detentions/deaths? 
#Is this correlated to economical status average of felegation?

# Sources: 
#https://datos.cdmx.gob.mx/dataset/incidentes-viales-c5

##### Settings
#####
#####
rm(list=ls())
Sys.setenv(LANG = "en")
options( scipen = 20 )
dev.off()

getwd()
setwd("/Users/Muro/Documents/DS/Project_MexicoCityAccidents")

### Packages
#install.packages("esquisse")

library(dplyr)
library(forcats) #fct_relevels

library(readr) #read_csv, parse_number (drop non-numeric characters before or after first number)
library(lubridate) # dates, times
library(DataExplorer)

library(naniar) #missing values visualization and investigation
library(stringr) #str_replace

library(ggmap)

library(esquisse)



##### Useful
#####
#####################################
######################################
#cat(paste0(levels(dt$incident_reporttype),"="), sep="\n")


#####################################
######################################             
##### Data preparation
#####
#####################################
######################################

source("datapreparation.R")

#####################################
######################################             
##### Dataset 1: confirmed incidents
#####
#####################################
######################################




#table(dt$incident_code)
# All received incidents (all incident codes)
# a = emergency unit sent and incident confirmed (real incidents)
# b = emergency unit sent but incident could not be confirmed
# i = information requests
# f = incident initially reported was confimed to be fake
# d = duplicated reports


# Confirmed incidents
dtc <- dt %>% filter( incident_code == "a") %>% 
        select(-"report_date_orig", -"report_time_orig" )


##### EDA 
#####
#####################################
#################¬#####################

### Fast EDA
###

colnames(dtc)
str(dtc)
summary(dtc)
https://yuzar-blog.netlify.app/posts/2021-01-09-exploratory-data-analysis-and-beyond-in-r-in-progress/
#create_report(dtc) #without response variables



### Map
###

### Plotting on map
cdmx <- get_stamenmap(
        bbox = c(left = -99.28, 
                 bottom = 19.25, 
                 right = -99.00, 
                 top = 19.58), 
        maptype = "toner",
        zoom = 12)

ggmap(cdmx) +
        geom_point(data = dtc ,
                   aes(x = long, y= lat, color = injured_flag),
                   alpha = 1/10, size = 1/2) +
        facet_wrap(report_weekday ~ .) + 
        guides(colour = guide_legend(override.aes = list(size=10)))


ggmap(cdmx) +
        geom_point(data = dtc ,
                   aes(x = long, y= lat, color = injured_flag),
                   alpha = 1/10, size = 1/2) +
        facet_wrap( report_year ~ .)  + 
        guides(colour = guide_legend(override.aes = list(size=10)))

ggplot(dtc) + geom_histogram(aes(report_year, fill = injured_flag),
                             stat="count") 
 








