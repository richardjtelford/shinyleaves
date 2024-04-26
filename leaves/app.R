library(shiny)
library(bslib)
library(EBImage)
library(autothresholdr)
library(purrr)


ui <- page_fillable(
  navset_pill(
    nav_panel(
      title = "Import and trim",
      layout_sidebar(
        sidebar = sidebar(
          open = "open",
          accordion(
            fileInput("file", label = "Select a file", multiple = FALSE, accept = "image/"),
            accordion_panel(
              title = "Trim",
              numericInput("left", "left", value = 0, min = 0),
              numericInput("right", "right", value = 0, min = 0),
              numericInput("top", "top", value = 0, min = 0),
              numericInput("bottom", "bottom", value = 0, min = 0)
            )
          )
        ),
        card(
          displayOutput("display")
        )
      )
    ),
    nav_panel(
      title = "Threshold",
      layout_sidebar(
        sidebar = sidebar(
          open = TRUE,
          sliderInput("manual", "Manual threshold", min = 0, max = 1, value = 0.5),
          checkboxGroupInput("autothresh",
            label = "Auto threshold algorithm",
            choices = c(
              "IJDefault", "Huang", "Huang2", "Intermodes", "IsoData",
              "Li", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu",
              "Percentile", "RenyiEntropy", "Shanbhag", "Triangle"
            ),
            selected = c("Minimum", "Otsu")
          )
        ),
        layout_columns(
          col_widths = c(8, 4, 12),
          card(
            plotOutput("histogram"),
            fill = TRUE
          ),
          card(tableOutput("threshold_tbl")),
          card(plotOutput("thresholded_image"))
        )
      )
    ),
    nav_panel(
      title = "Segmenting",
      layout_sidebar(
        sidebar = sidebar(
          open = TRUE,
          sliderInput("threshold", "Threshold", min = 0, max = 1, value = 0.5)
        ),
      )
    )
  )
)



server <- function(input, output) {

  # load image
  img <- reactive({
    if (is.null(input$file)) {
      f <- system.file("images", "sample-color.png", package = "EBImage")
      x <- readImage(f)
    } else {
      EBImage::readImage(input$file$datapath)
    }
  }
  )

  # crop image
  cropped_image <- reactive({
    img()[input$left:(nrow(img()) - input$right), input$top:(ncol(img()) - input$bottom), ]
  })

  # render cropped image
  output$display <- renderDisplay({
    display(cropped_image())
  })

  # convert image to greyscale
  grey_scale <- reactive({
    channel(cropped_image(), mode = "grey")
  })


  # calculate autothresholds
  thresholds <- reactive({
    data <- round(grey_scale() * 256) # force to 8-bits (hopefully)
    data.frame(
      method = input$autothresh,
      threshold = map_int(input$autothresh, \(thr) auto_thresh(data, thr)) / 256
    )
  })

  # histogram of intensities
  output$histogram <- renderPlot({
    hist(grey_scale())
    rug(thresholds()$threshold)
    text(
      thresholds()$threshold,
      rep(0, length(input$autothresh)),
      labels = thresholds()$method,
      srt = 90,
      adj = c(0, 1)
    )
  })

  # table of autothresholds
  output$threshold_tbl <- renderTable(thresholds())


  # thresholded image
  output$thresholded_image <- renderPlot({
    c(input$manual, thresholds()$threshold) |>
      map(\(x) {grey_scale() <= x}) |>
      combine() |>
      display(method = "raster", all = TRUE)
  })
}

shinyApp(ui, server)
