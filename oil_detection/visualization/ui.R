library(data.table)

statistical_data <- fread('statisics_for_maxar_data_clustered.csv')
dates <- unique(lapply(statistical_data$file, function(x) {
  strsplit(x, "_")[[1]][2]
}))
dates <- unlist(dates)

ui <- fluidPage(
  titlePanel("Где находится нефть?"),
    sidebarPanel(
      selectInput(inputId = "date",
                  label = "Дата",
                  choices = dates),
      textInput(inputId = "data_source",
                label = "Источник данных",
                value = "/home/alexey.osipov/gitlab/eco-oil-detection/data/"),
      numericInput(inputId = "random_seed",
                   label = "Сид",
                   value = 239),
      checkboxInput(inputId = "withOil",
                    label = "Картинки с нефтью?",
                    value = FALSE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Исходный снимок",
          plotOutput(outputId = "oilPlot", width = "100%", height = "1000px")
        ),
        tabPanel(
          "Нефть на снимке",
          plotOutput(outputId = "oilPlot3", width = "100%", height = "1000px")
        ),
        tabPanel(
          "Детекция пятна",
          plotOutput(outputId = "oilPlot2", width = "100%", height = "1000px")
        )
      )
    )
)
