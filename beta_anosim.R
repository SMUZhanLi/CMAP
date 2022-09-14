beta_anosim_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Anosim Analysis",
                       status = "warning",
                       collapsible = TRUE,
                       fluidRow(
                           column(6,
                                  pickerInput(ns("std_method"),
                                              "Standardization method:",
                                              choices = std_method,
                                              selected = "total"
                                  )
                           ),
                           column(6,
                                  pickerInput(ns("dist_method"),
                                              "Distance method:",
                                              choices = dist_method,
                                              selected = "bray"
                                  )
                           )
                       ),
                       fluidRow(
                           column(6,
                                  pickerInput(ns("group"), "Group:", NULL)
                           ),
                           column(6,
                                  numericInput(ns("permutations"), "Permutations:", value = 999)
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
                       plotOutput(ns("anosim_plot"), width = '900px', height = '600px'),
                       operation = c("enable", "disable", "destroy", "save", "load"),
                       options = list(
                           minHeight = 300, maxHeight = 900,
                           minWidth = 300, maxWidth = 1200
                       )
                   ))
        )
        # shinydashboardPlus::box(
        #     width = 12, title = "Anosim Analysis",
        #     status = "warning",
        #     collapsible = TRUE,
        #     pickerInput(ns("std_method"),
        #                 "Standardization method:",
        #                 choices = std_method,
        #                 selected = "total"
        #     ),
        #     pickerInput(ns("dist_method"),
        #                 "Distance method:",
        #                 choices = dist_method,
        #                 selected = "bray"
        #     ),
        #     pickerInput(ns("group"), "Group:", NULL),
        #     numericInput(ns("permutations"), "Permutations:", value = 999),
        #     actionButton(ns("btn"), "Submit")
        # ),
        # shinydashboardPlus::box(
        #     width = 12,
        #     title = "Plot Download",
        #     status = "success",
        #     solidHeader = FALSE,
        #     collapsible = TRUE,
        #     plotOutput(ns("anosim_plot")),
        #     numericInput(ns("width_slider"), "width:", 10,1, 20),
        #     numericInput(ns("height_slider"), "height:", 8, 1, 20),
        #     radioButtons(inputId = ns('extPlot'),
        #                  label = 'Output format',
        #                  choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
        #                  inline = TRUE),
        #     downloadButton(ns("downloadPlot"), "Download Plot"),
        #     downloadButton(ns("downloadTable"), "Download Table")
        # )
        # fluidRow(),
        # verbatimTextOutput(ns("text")),
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


beta_anosim_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            treeda <- reactiveVal({
                readRDS("data/treeda.rds")
            })
            observe({
                req(inherits(mpse, "MPSE"))
                lev <- sapply(mp_extract_sample(mpse)[-1], function(n) length(unique(n)))
                group <- names(lev[lev > 1])
                updatePickerInput(session, "group",
                                  choices = group,
                                  selected = tail(names(lev[lev == 2]), 1)
                )
                if (!is.null(treeda())) {
                    updatePickerInput(session, "dist_method",
                                      choices = c(dist_method, c("unweighted uniFrac" = "unifrac", "weighted uniFrac" = "wunifrac"))
                    )
                }
            })
            anosim_res <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$submit
                std <- isolate({
                    input$std_method
                })
                dist <- isolate({
                    input$dist_method
                })
                group <- isolate({
                    input$group
                })
                permutations <- isolate({
                    input$permutations
                })
                if (dist %in% c("unifrac", "wunifrac")) {
                    otutree(mpse) <- treeda()
                }
                verbose <- capture.output(
                    tbl <- mpse %>%
                        mp_decostand(
                            .abundance = Abundance,
                            method = std
                        ) %>%
                        mp_anosim(
                            .abundance = !!std,
                            .group = !!group,
                            distmethod = dist,
                            permutations = permutations,
                            action = "only"
                        )
                )
                return(list(tbl = tbl, verbose = verbose))
            })
            
            # output$text <- renderText({
            #     req(inherits(anosim_res()$tbl, "tbl"))
            #     paste0(anosim_res()$verbose, collapse = " ")
            # })
            
            p_anosim <- eventReactive(input$btn, {
                req(inherits(anosim_res()$tbl, "tbl"))
                #req(input$submit)
                plt <-  ggbetweenstats(
                    data = anosim_res()$tbl,
                    x = class,
                    y = rank,
                    bf.message = FALSE,
                    results.subtitle = FALSE,
                    plot.type = "box",
                    ylab = "Bray-Curtis Rank",
                    xlab = input$group,
                    title = "Bray-Curtis Anosim",
                    subtitle = paste0(anosim_res()$verbose[1:2], collapse = " "),
                    centrality.plotting = FALSE
                ) + 
                    theme(
                        text = element_text(
                            #family = "Roboto", 
                            size = 8, 
                            color = "black"),
                        plot.title = element_text(
                            #family = "Lobster Two", 
                            size = 20,
                            face = "bold",
                            color = "#2a475e"
                        ),
                        # Statistical annotations below the main title
                        plot.subtitle = element_text(
                            #family = "Roboto", 
                            size = 12, 
                            face = "bold",
                            color="#1b2838"
                        ),
                        plot.title.position = "plot",# slightly different from default
                        axis.text = element_text(size = 10, color = "black"),
                        axis.title = element_text(size = 12),
                        axis.ticks = element_blank(),
                        axis.line = element_line(colour = "grey50"),
                        panel.grid = element_line(color = "#b4aea9"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        panel.grid.major.y = element_line(linetype = "dashed"),
                        # panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
                        # plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
                    )
                return(plt)
                # anosim_res()$tbl %>%
                #     ggplot(aes(x = class, y = rank, fill = class)) +
                #     geom_boxplot(notch = TRUE, varwidth = TRUE) +
                #     cmap_theme
            })
            
            output$anosim_plot <- renderPlot({
                req(p_anosim())
                p_anosim()
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("anosim_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_anosim())
                    ggsave(file, 
                           plot = p_anosim(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "Anosim_Data.csv" },
                content = function(file){
                    req(p_anosim())
                    table <- p_anosim()$data
                    n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table %>% select(-c(n)), 
                              file,
                              row.names = FALSE)
                })
            
        }
    )
}