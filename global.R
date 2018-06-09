#Sourcing Libraries
rm(list = ls())
library(shinythemes) # used for theme = shinytheme("united") in ui.R
library(shiny)  # used to shiny APP
library(shinydashboard)  # used to shiny dashboard design.
library(rhandsontable)  # used for rhandsontable.
library(plyr)
library(car)
library(scales)
library(shinyjs)  # used for shiny java script implementation.
library(shinyBS)  # used to create BS model and alert message.
library(DT)  # used for datatable output.
library(Rcpp) 
library(data.table)  # used for data.table processing.
library(lubridate)  # used for date formating
library(ggplot2)
library(broom)  # used for tidy the model result.
library(plotly) # used for ploting AVM in plotly graph.
library(stringr) # used for string processing.
library(dtplyr) # used for data manupulation like arrange().
library(lazyeval)  # used this for interp
library(nlme)  # used for lmer function to generate random effect model.
library(MLmetrics)  # used for MAPE calculation in Automodeller
library(rlist) # used for list.clean() to remove NULL from nested list
library(shinyalert)  # used to custom error message in popup

#Setting home directory
CleanEnvir <- function() {
  objs <- ls(pos = ".GlobalEnv")
  rm(list = objs, pos = ".GlobalEnv")
}

options(shiny.maxRequestSize = 9*1024^2,scipen = 999,
        contrasts = c(factor = "contr.SAS", ordered = "contr.poly"))

source(file.path(getwd(),"ModellerEngineFunctions.R"))

if(file.exists("C:/RBuildTools/3.4/bin/zip.exe")){
  Sys.setenv(R_ZIPCMD = "C:/RBuildTools/3.4/bin/zip.exe")
}else if(file.exists("C:/Rtools/bin/zip.exe")){
  Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
}else {
  installr::install.rtools()
  if(file.exists("C:/RBuildTools/3.4/bin/zip.exe")){
    Sys.setenv(R_ZIPCMD = "C:/RBuildTools/3.4/bin/zip.exe")
  }else if(file.exists("C:/Rtools/bin/zip.exe")){
    Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
  }
} 

