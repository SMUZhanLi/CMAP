beta_nmds_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "NMDS Analysis",
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
                       pickerInput(ns("group"), "Group:", NULL),
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
                       ),
                       h4("Color Palette"),
                       fluidRow(
                           column(6,
                                  uiOutput(ns("color")))
                       )
                   )
            ),
            column(9,
                   jqui_resizable(
                       plotOutput(ns("nmds_plot"), width = '900px', height = '600px'),
                       operation = c("enable", "disable", "destroy", "save", "load"),
                       options = list(
                           minHeight = 300, maxHeight = 900,
                           minWidth = 300, maxWidth = 1200
                       )
                   ))
        )
        
        
        # shinydashboardPlus::box(
        #     width = 12, title = "NMDS Analysis",
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
        #     actionButton(ns("btn"), "Submit")
        # ),
        # shinydashboardPlus::box(
        #     width = 12,
        #     title = "Plot Download",
        #     status = "success",
        #     solidHeader = FALSE,
        #     collapsible = TRUE,
        #     plotOutput(ns("nmds_plot")),
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

beta_nmds_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            treeda <- reactiveVal({
                readRDS("data/treeda.rds")
            })
            #update input
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
            
            mp_nmds <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$submit
                std <- isolate({
                    input$std_method
                })
                dist <- isolate({
                    input$dist_method
                })
                if (dist %in% c("unifrac", "wunifrac")) {
                    otutree(mpse) <- treeda()
                }
                nmds_log <- capture.output(
                    res <- mpse %>%
                        mp_decostand(
                            .abundance = Abundance,
                            method = std
                        ) %>%
                        mp_cal_nmds(
                            .abundance = !!std,
                            distmethod = dist,
                            action = "add"
                        )
                )
                stress <- tail(nmds_log[grepl("^Run", nmds_log)], 1)
                stress <- gsub("^.+(stress.+$)", "\\1", stress)
                return(list(stress = stress, res = res))
            })
            
            p_nmds <- reactive({
                req(inherits(mp_nmds()$res, "MPSE"))
                input$btn
                group <- isolate({
                    input$group
                })
                ellipse <- mpse %>%
                    mp_extract_sample() %>%
                    pull(!!group) %>%
                    is.character()
                p <- mp_nmds()$res %>%
                    mp_plot_ord(
                        .ord = nmds,
                        .group = !!sym(group),
                        .color = !!sym(group),
                        ellipse = ellipse
                    ) +
                    cmap_theme
                
                p <- p +
                    ggtitle(mp_nmds()$stress) +
                    theme(plot.title = element_text(hjust = 0.5))
                
                color_content <- mpse %>% mp_extract_sample %>%
                    select(!!sym(group)) %>% unique #It is a tibble
                
                if(color_content[[1]] %>% is.numeric) {
                    return(p)
                }
                
                ncolors <- color_content[[1]] %>% length #length of group 
                color_input <- lapply(seq(ncolors), function (i){
                    input[[paste0("colors",i)]]
                }) %>% unlist #calling input color by length of group 
                
                if(length(color_input) != ncolors) {
                    p <- p + 
                        scale_color_manual(values = cc(ncolors)) + 
                        scale_fill_manual(values = cc(ncolors)) 
                }else{
                    p <- p + 
                        scale_color_manual(values = color_input) + 
                        scale_fill_manual(values = color_input)
                    
                }

                return(p)
            })
            
            #Modify color
            color_list <- reactive({
                req(mp_nmds())
                input$btn
                group <- isolate({
                    input$group
                })
                ns <- NS(id)
                color_content <- mpse %>% mp_extract_sample %>% 
                    select(!!sym(group)) %>% unique #It is a tibble
                name_colors <- color_content[[1]] %>% sort #getting chr.
                pal <- cc(length(name_colors)) #calling color palette
                names(pal) <- name_colors #mapping names to colors 
                
                picks <- lapply(seq(pal), function(i) {#building multiple color pickers
                    colorPickr(
                        inputId = ns(paste0("colors",i)),
                        label = names(pal[i]),
                        selected = pal[[i]],
                        swatches = cols,
                        theme = "monolith",
                        useAsButton = TRUE
                    )
                })
                return(picks)
            })
            
            output$nmds_plot <- renderPlot({
                req(p_nmds())
                p_nmds()
            })
            
            output$color <- renderUI(
                #req(color_list)
                color_list()
            )
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("nmds_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_nmds())
                    ggsave(file, 
                           plot = p_nmds(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "nmda_Data.csv" },
                content = function(file){
                    req(p_nmds())
                    table <- p_nmds()$data 
                    n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table %>% select(-c(n)), 
                              file,
                              row.names = FALSE)
                })
        }
    )
}