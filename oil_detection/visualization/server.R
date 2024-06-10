library(imager)

server <- function(input, output) {
  date <- reactive(input$date)
  data_source <- reactive(input$data_source)
  random_seed <- reactive(input$random_seed)
  withOil <- reactive(input$withOil)
  date_deb <- debounce(date, 2000)
  data_source_deb <- debounce(data_source, 2000)
  random_seed_deb <- debounce(random_seed, 2000)
  withOil_deb <- debounce(withOil, 2000)
  
  filename_res <- reactive({
    masks_folder <- get_folder_with_masks(data_source_deb())
    masks <- list.files(masks_folder)
    filename <- choose_random_picture(random_seed_deb(), date_deb(),
                                      withOil_deb(), masks)
    list(filename = filename, masks_folder = masks_folder)
  })
  
  output$oilPlot <- renderPlot({
    filename <- filename_res()$filename
    img <- load.image(paste0(data_source_deb(), filename))
    plot(img, main = filename)
  })
  
  output$oilPlot3 <- renderPlot({
    results <- filename_res()
    filename <- results$filename
    if (filename %in%
        statistical_data[(noOil == 0) & (share_black >= 0.001), file]) {
      masks_folder <- results$masks_folder
      img <- load.image(paste(masks_folder, filename, sep = "/"))
      plot(img, main = filename)
    } else {
      stop('Мы считаем, что на снимке нефти нет!')
    }
  })
  
  output$oilPlot2 <- renderPlot({
    filename <- filename_res()$filename
    saved_folder <- getwd()
    setwd("/home/alexey.osipov/gitlab/eco-oil-detection/modelling/")
    write_to_temp_file("/home/alexey.osipov/gitlab/eco-oil-detection/temp_data/",
                       filename)
    copy_png_file(filename, data_source_deb(),
                  "/home/alexey.osipov/gitlab/eco-oil-detection/temp_data/data_with_oil/")
    system("python apply_model_to_one_file.py")
    setwd(saved_folder)
    img <- load.image("/home/alexey.osipov/gitlab/eco-oil-detection/temp_data/temp_file.png")
    plot(img, main = filename)
  })
}