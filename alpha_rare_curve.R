rare_curve_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Rarefaction Curve",
                       status = "warning",
                       collapsible = TRUE,
                       fluidRow(
                           column(6,
                                  pickerInput(ns("index"), "Alpha-diversity Index:",
                                              options = list(`actions-box` = TRUE),
                                              choices = alpha_index, multiple = T,
                                              selected = c("Observe", "ACE", "Chao1"))
                           ),
                           column(6,
                                  pickerInput(ns("group"), "Group:", NULL)
                           )
                       ),
                       actionButton(ns("btn"), "Submit")
                   ),
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Setting Plot",
                       status = "warning",
                       collapsible = TRUE,
                       fluidRow(
                           column(width = 6,
                                  style=list("padding-right: 5px;"),
                                  numericInput(ns("width_slider"), "Width:", 10,1, 20)
                           ),
                           column(width = 6,
                                  style=list("padding-left: 5px;"),
                                  numericInput(ns("height_slider"), "Height:", 8, 1, 20)
                           )
                       ),
                       fluidRow(
                           column(width = 6,
                                  style=list("padding-right: 5px;"),
                                  selectInput(inputId = ns('extPlot'),
                                              label = 'Output format',
                                              choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff')
                                  ),
                           ),
                           column(width = 6,
                                  style=list("padding-left: 5px;"),
                                  numericInput(ns("dpi"), "DPI:", 300, 100, 600)
                           )
                       ),
                       fluidRow(
                           column(width = 6,
                                  downloadButton(ns("downloadPlot"), "Download Plot")),
                           column(width = 6,
                                  downloadButton(ns("downloadTable"), "Download Table"))
                       )
                   )
            ),
            column(9,
                   jqui_resizable(
                       plotOutput(ns("rare_curve_plot"), width = '900px', height = '600px'),
                       operation = c("enable", "disable", "destroy", "save", "load"),
                       options = list(
                           minHeight = 100, maxHeight = 900,
                           minWidth = 300, maxWidth = 1200
                       )
                   )
            )
        )

        # shinydashboardPlus::box(
        #     width = 12, 
        #     title = "Rarefaction Curve",
        #     status = "warning",
        #     collapsible = TRUE,
        #     pickerInput(ns("index"), "Alpha-diversity Index:",
        #                     options = list(`actions-box` = TRUE),
        #                     choices = alpha_index, multiple = T,
        #                     selected = c("Observe", "ACE", "Chao1")),
        #     pickerInput(ns("group"), "Group:", NULL),
        #     actionButton(ns("btn"), "Submit")
        # ),
        # shinydashboardPlus::box(
        #     width = 12,
        #     title = "Plot Download",
        #     status = "success",
        #     solidHeader = FALSE,
        #     collapsible = TRUE,
        #     plotOutput(ns("rare_curve_plot")),
        #     numericInput(ns("width_slider"), "width:", 10,1, 20),
        #     numericInput(ns("height_slider"), "height:", 8, 1, 20),
        #     radioButtons(inputId = ns('extPlot'),
        #                  label = 'Output format',
        #                  choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
        #                  inline = TRUE),
        #     downloadButton(ns("downloadPlot"), "Download Plot")
        #     #downloadButton(ns("downloadTable"), "Download Table")
        # )
        # fluidRow(),
        # jqui_resizable(
        #     plotOutput(ns("plot"), width = "600px"),
        #     operation = c("enable", "disable", "destroy", "save", "load"),
        #     options = list(
        #         minHeight = 100, maxHeight = 900,
        #         minWidth = 300, maxWidth = 1200
        #     )
        # )
    )
    return(res)
}





rare_curve_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            observe({
                req(inherits(mpse, "MPSE"))
                group <- names(mp_extract_sample(mpse))
                updatePickerInput(session, "group", choices = group)
            })
            mp_rare <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$submit
                # res <- tryCatch(
                #     {
                #         rare <- readRDS("data/mp_rare.rda")
                #         samples <- intersect(
                #             mp_extract_sample(mpse)[[1]],
                #             mp_extract_sample(rare)[[1]]
                #         )
                #         rare %>% filter(Sample %in% samples)
                #     },
                #     error = function(e) e
                # )
                # if (! inherits(res, "MPSE")) {
                    res <- mpse %>%
                        mp_cal_rarecurve(
                            .abundance = Abundance,
                            chunks = 50,
                            action = "add",
                            force = T
                        )
                # }
                return(res)
            })
            p_rare_curve <- reactive({
                req(inherits(mp_rare(), "MPSE"))
                input$btn
                group <- isolate({input$group})
                index <- isolate({input$index})
                p <- mp_rare() %>%
                    mp_plot_rarecurve(.rare=AbundanceRarecurve,
                                      .alpha=!!index,
                                      .group=!!group) +
                    cmap_theme
                if (group=="Sample") {
                    p <- p + theme(legend.position = "none")
                }
                return(p)
                
            })
            output$rare_curve_plot <- renderPlot({
                req(p_rare_curve())
                p_rare_curve()
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("rare_curve_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_rare_curve())
                    ggsave(file, 
                           plot = p_rare_curve(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
        }
    )
}

