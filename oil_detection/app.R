library(shiny)
Sys.setlocale("LC_ALL", "Russian_Russia.20866")

source("visualization/ui.R", encoding="UTF-8")
source("visualization/server.R", encoding="UTF-8")
source("visualization/processing.R", encoding="UTF-8")

shinyApp(ui, server)