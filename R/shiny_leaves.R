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
#' @importFrom tibble tibble
#' @importFrom dplyr filter
#' @examples
#' # Run app
#' if (interactive()) {
#'   shiny_leaves()
#' }
#' @export
#'

shiny_leaves <- function() {
  ui <- page_fillable(
    navset_pill(
      footer = p(
        "shinyleaves app is a contribution to",
        a("PTFC", href = "https://plantfunctionaltraitscourses.w.uib.no/", target = "_blank"),
        "and",
        a("DURIN", href = "https://betweenthefjords.w.uib.no/durin/", target = "_blank"),
        ". It is written by",
        a("Richard J Telford", href = "https://richardjtelford.github.io/", target = "_blank")
      ),
      nav_panel(
        title = "Introduction",
        layout_sidebar(
          sidebar = p("Please send bug reports and suggestions for improvements to", a(icon("github"), href = "https://github.com/richardjtelford/shinyleaves/issues", target = "_blank")),
          open = "closed",
          h1("Calculating leaf area"),
          p("Leaf area is a key functional plant trait, both in its own right and as a component of specific leaf area (SLA - the ratio of leaf area to leaf dry mass"),
          p("Leaves are scanned on a flatbed scanner, and then the area of the leaves calculated with image processing software."),
          p("shinyleaves is a shiny app that explains the steps involved in processing leaves."),
          p("It can be used for teaching, or to experiment with difficult leaves.")
        )
      ),
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
              accordion_panel(
                title = "Choose a leaf from Svalbard or your own file",
                uiOutput("file_list"),
                fileInput("file", label = "Select a file", multiple = FALSE, accept = "image/")
              ),
              accordion_panel(
                title = "Trim off the edges",
                numericInput("left", "left", value = 0, min = 0),
                numericInput("right", "right", value = 0, min = 0),
                numericInput("top", "top", value = 0, min = 0),
                numericInput("bottom", "bottom", value = 0, min = 0),
                checkboxInput("default_trim", "Use PFTC defaults")
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
              radioButtons("selected_thresh", label = "Select one algorithm to use", choices = "Manual", selected = "Manual"),
              numericInput("zoom", "Zoom histogram y-axis", min = 0, max = Inf, value = 0)
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
            accordion(
              accordion_panel(
                title = "Instructions",
                p("Now we can identify different objects as groups of connected pixels."),
                p("To convert the area in pixels to mm^2, we need to know the resolution of the scan, by convention given in dots per inch (dpi)."),
                p("To remove dust, smears etc, we can set the minimum area required to be treated as a leaf."),
                p("Now we have the area of each leaf. Various shape attributes can also be calculated (e.g. circularity).")
              )
            ),
            numericInput("dpi", "Resolution dpi", min = 0, value = 300),
            numericInput("min_area", "Minimum area mm", min = 0, value = 0)
          ),
          layout_columns(
            col_widths = c(8, 4),
            card(
              displayOutput("segmented"),
              fill = TRUE
            ),
            card(tableOutput("features"))
          )
        )
      ),
      nav_panel(
        title = "Software",
        layout_sidebar(
          open = "closed",
          h1("R packages for processing leaf images"),
          h3("leafArea"),
          p("The leafArea package calculates leaf area from images using ImageJ to do the calculations. It is on CRAN, but PTFC has its own version that increases flexibility with, for example, cropping."),
          h3("pliman"),
          p("The pliman package calculates leaf area and some plant disease related metrics. It is available from CRAN."),
          h3("EBImage"),
          p("The EBImage package is a general image processing package designed for biological data. Is available from bioconductor.
            This app uses EBImage for its calculations"),
          h3("autothresholdr"),
          p("The autothresoldr package calculates the full range of autothresholds implimented in ImageJ.")
        )
      ),
      nav_spacer(),
      nav_item(a(icon("github"), href = "https://github.com/richardjtelford/shinyleaves", target = "_blank"))
    )
  )



  server <- function(input, output) {
    # list built-in images
    svalbard_leaves <- list.files(system.file("extdata/", package = "shinyleaves"))
    random_leaf <- sample(svalbard_leaves, size = 1)

    # built-in leaf ui
    output$file_list <- renderUI({
      radioButtons("chosen_image", "Choose an image", choices = svalbard_leaves, selected = random_leaf)
    })

    # load image
    img <- reactive({
      if (!is.null(input$file)) {
        readImage(input$file$datapath)
      } else if (!is.null(input$chosen_image)) {
        f <- system.file("extdata/", input$chosen_image, package = "shinyleaves")
        readImage(f)
      } else {
        f <- system.file("extdata/", random_leaf, package = "shinyleaves")
        readImage(f)
      }
    })

    # set default trim
    observe({
      if (input$default_trim) {
        updateNumericInput(inputId = "left", value = 100)
        updateNumericInput(inputId = "right", value = 200)
        updateNumericInput(inputId = "top", value = 100)
        updateNumericInput(inputId = "bottom", value = 100)
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
      display(cropped_image(), method = "browser")
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
      if (is.na(input$zoom) || input$zoom == 0) {
        hist(grey_scale())
      } else {
        hist(grey_scale(), ylim = c(0, input$zoom))
      }

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
      px <- computeFeatures.shape(segmented())[, "s.area"]
      tibble(
        n = seq_along(px),
        `Area (pixels)` = px,
        `Area (mm^2)` = (px / input$dpi^2) * 25.4^2
      )
    })

    output$segmented <- renderDisplay({
      cols <- c("black", sample(rainbow(max(segmented()))))
      cols[which(features()$`Area (mm^2)` < input$min_area) + 1] <- "white"
      zrainbow <- Image(cols[1 + segmented()], dim = dim(segmented()))
      display(zrainbow, title = "Leaves (recolored)", method = "browser")
    })

    output$features <- renderTable({
      features() |>
        filter(`Area (mm^2)` > input$min_area)
    })
  }

  shinyApp(ui, server)
}
