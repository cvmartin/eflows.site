library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dygraphs)
library(shinyjs)

library(eflows)
library(eflows.viz)
library(dplyr)
library(R6)

dy_height <- 210

source("functions/data_preprocessing.R", local = TRUE)
source("functions/utils.R", local = TRUE)
source("functions/utils_ui.R", local = TRUE)
source("functions/modules.R", local = TRUE)