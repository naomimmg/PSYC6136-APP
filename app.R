# Load libraries
library(vcd)
library(vcdExtra)
library(shiny)
library(tidyverse)
library(shinyjs)
library(colourpicker)

# Source preliminary data and functions
source("Functions.R")
source("data.R")

# Source user interface and server
source("ui.R")
source("server.R")

# Run app
shinyApp(ui, server)


