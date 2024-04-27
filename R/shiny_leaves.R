#' Shiny leaves
#' @description
#' An app to explain how leaf area is calculated from a scan.
#' It can also be used to work interactively with problematic leaves to better
#' understand how to process them.
#' @import shiny
#' @import bslib
#' @import EBImage
#' @import autothresholdr
#' @importFrom purrr map_int
#' @importFrom graphics rug text
#' @importFrom grDevices rainbow
#' @examples
#' # Run app
#' if (interactive()) {
#'  shiny_leaves()
#' }
#' @export
#'

shiny_leaves <- function() {
  ui <- page_fillable(
    navset_pill(
      nav_panel(
        title = "Import and trim",
        layout_sidebar(
          sidebar = sidebar(
            open = "open",
            accordion(
              accordion_panel(
                title = "Instructions",
                p("Select a leaf image file and trim off the edges to remove black lines, rulers, etc.")
              ),
              fileInput("file", label = "Select a file", multiple = FALSE, accept = "image/"),
              accordion_panel(
                title = "Trim off the edges",
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
            accordion(
              accordion_panel(
                title = "Instructions",
                p("The next step is to convert the image to grey-scale, and make a histogram of the intensities of each pixel. High intensities indicate the scan background. Low intensities indicate the leaf. Intermediate values might be smears on the scanner, dirt, or a pale part of the leaf."),
                p("We need to choose the intensity that best separates the leaf from the background. This could be done manually, but doing this for each of hundreds of leaves would be too much work (and not very reproducible), so we can also use an autothreshold algorithm."),
                p("For more information about the autothreshold algorithms, see the", a("imagej documentation", href = "https://imagej.net/plugins/auto-threshold", target = "_blank"), ".")
              ),
              sliderInput("manual", "Manual threshold", min = 0, max = 1, value = 0.5),
              accordion_panel(
                title = "Select autothreshold",
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
              radioButtons("selected_thresh", label = "Select one algorithm to use", choices = "Manual", selected = "Manual")
            )
          ),
          layout_columns(
            col_widths = c(4, 4, 4, 12),
            card(
              plotOutput("grey_scale"),
              fill = TRUE
            ),
            card(
              plotOutput("histogram"),
              fill = TRUE
            ),
            card(tableOutput("threshold_tbl")),
            card(plotOutput("threshold_img")),
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
          layout_columns(
            col_widths = c(4, 4),
            card(
              plotOutput("segmented"),
              fill = TRUE
            ),
            card(tableOutput("features"))
          )
        )
      )
    )
  )



  server <- function(input, output) {
    # load image
    img <- reactive({
      if (is.null(input$file)) {
        f <-  system.file("extdata/AAZ7235.jpeg", package = "shinyleaves")
        x <- readImage(f)
      } else {
        EBImage::readImage(input$file$datapath)
      }
    })

    # crop image
    cropped_image <- reactive({
      left <- ifelse(is.na(input$left), 0, input$left)
      right <- ifelse(is.na(input$right), 0, input$right)
      top <- ifelse(is.na(input$top), 0, input$top)
      bottom <- ifelse(is.na(input$bottom), 0, input$bottom)
      img()[left:(nrow(img()) - right), top:(ncol(img()) - bottom), ]
    })

    # render cropped image
    output$display <- renderDisplay({
      display(cropped_image())
    })

    # convert image to greyscale
    grey_scale <- reactive({
      channel(cropped_image(), mode = "grey")
    })

    output$grey_scale <- renderPlot(display(grey_scale(), method = "raster"))


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



    # update options for thresholding
    observe({
      updateRadioButtons(inputId = "selected_thresh", choices = c("Manual", input$autothresh))
    })

    threshold_img <- reactive({
      thresh <- switch(input$selected_thresh,
        Manual = input$manual,
        thresholds()$threshold[thresholds()$method == input$selected_thresh]
      )
      img <- grey_scale() >= thresh
    })

    # thresholded image
    output$threshold_img <- renderPlot({
      display(threshold_img(), method = "raster")
    })

    # segmented image
    segmented <- reactive({
      bwlabel(!threshold_img())
    })

    # features
    features <- reactive({
      computeFeatures.shape(segmented())
    })

    output$segmented <- renderPlot({
      cols <- c("black", sample(rainbow(max(segmented()))))
      zrainbow <- Image(cols[1 + segmented()], dim = dim(segmented()))
      display(zrainbow, title = "Leaves (recolored)", method = "raster")
    })

    output$features <- renderTable(features())
  }

  shinyApp(ui, server)
}

# object analysis (with sliders)
# better default image
