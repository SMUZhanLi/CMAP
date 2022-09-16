p_adjust <- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")


lefse_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "LEfSe Analysis",
                       status = "warning",
                       collapsible = TRUE,
                       fluidRow(
                           column(6,
                                  numericInput(ns("lda"),
                                               "LDA score threshold:",
                                               value = 3)
                           ),
                           column(6,
                                  numericInput(ns("p_val"),
                                               "P value cutoff:",
                                               value = 0.05,
                                               step = 0.001)
                           )
                       ),
                       fluidRow(
                           column(6,
                                  pickerInput(ns("p_adj"),
                                              "P value adjust:",
                                              choices = p_adjust,
                                              selected = "fdr")
                           ),
                           column(6,
                                  pickerInput(ns("level"),
                                              "Taxonomy level:",
                                              choices = NULL)
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
                       )
                   )
            ),
            column(9,
                   jqui_resizable(
                       plotOutput(ns("diff_plot"), width = '900px', height = '600px'),
                       operation = c("enable", "disable", "destroy", "save", "load"),
                       options = list(
                           minHeight = 300, maxHeight = 900,
                           minWidth = 300, maxWidth = 1200
                       )
                   ))
        )
     
        # shinydashboardPlus::box(
        #     width = 12,
        #     title = "LEfSe Analysis",
        #     status = "warning",
        #     collapsible = TRUE,
        #     numericInput(ns("lda"),
        #                  "LDA score threshold:",
        #                  value = 3),
        #     numericInput(ns("p_val"),
        #                  "P value cutoff:",
        #                  value = 0.05,
        #                  step = 0.001),
        #     pickerInput(ns("p_adj"),
        #                 "P value adjust:",
        #                 choices = p_adjust,
        #                 selected = "fdr"),
        #     pickerInput(ns("level"),
        #                 "Taxonomy level:",
        #                 choices = NULL),
        #     pickerInput(ns("group"), "Group:", NULL),
        #     radioButtons(ns("layout"), "Graph layout:",
        #         c(
        #             "Box plot" = "box",
        #             "Clade plot" = "clade"
        #         ),
        #         inline = T
        #     ),
        #     actionButton(ns("btn"), "Submit")
        #     
        # ),
        # fluidRow(),
        # uiOutput(ns("ui"))
    )
    return(res)
}



lefse_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            observe({
                req(inherits(mpse, "MPSE")) 
                lev <- sapply(mp_extract_sample(mpse)[-1], function(n) length(unique(n)))
                group <- names(lev[lev > 1])
                taxa <- names(mp_extract_taxonomy(mpse))[-1]
                updatePickerInput(session, "group", choices = group,
                                  selected = tail(names(lev[lev == 2]), 1))
                updatePickerInput(session, "level", choices = taxa,
                                  selected = tail(taxa, 1))
            })
            # observeEvent(input$btn, {
            #     input$btn
            #     layout <- isolate({input$layout})
            #     if (layout == "clade") {
            #         options <- list(
            #             minHeight = 400, maxHeight = 1400,
            #             minWidth = 500, maxWidth = 1500
            #         )
            #         width <-  "1000px"
            #         height <- "900px"
            #     } else {
            #         options <- list(
            #             minHeight = 100, maxHeight = 900,
            #             minWidth = 300, maxWidth = 1200
            #         )
            #         width <-  "900px"
            #         height <- "600px"
            #     }
            #     output$ui <- renderUI({
            #         jqui_resizable(
            #             plotOutput(ns("plot"), width = width, height = height),
            #             operation = c("enable", "disable", "destroy", "save", "load"),
            #             options = options
            #         )
            #     })
            # })
            mp_lefse <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$btn
                level <- isolate({input$level})
                lda <- isolate({input$lda})
                p_val <- isolate({input$p_val})
                p_adj <- isolate({input$p_adj})
                # is.relative <- isolate({
                #     ifelse(input$ytype == "relative", TRUE, FALSE)
                # })
                group <- isolate({input$group})
                
                mp_lefse <- mpse %>%
                    mp_diff_analysis(.abundance = RareAbundance,
                                     .group = !!sym(group),
                                     action = "get",
                                     tip.level = "Speies",
                                     first.test.alpha = p_val,
                                     second.test.alpha = p_val,
                                     p.adjust = p_adj,
                                     #relative = is.relative,
                                     ldascore = lda,
                                     force = TRUE)
            })
            
            p_diffclade <- reactive({
                req(inherits(mp_lefse(), "diffAnalysisClass"))
                input$btn
                
                p <- mp_lefse() %>% ggdiffclade(linewd = 0.1)
                return(p)
            })
            
            # observe({
            #     layout <- isolate({input$layout})
            #     if (layout == "box") {
            #         res <- 120
            #     } else {
            #         res <- 200
            #     }
                output$diff_plot <- renderPlot({
                # ggfun::set_font(p, size = 3)
                req(p_diffclade())
                    p_diffclade()
                #layout <- isolate({input$layout})
                # if (layout == "box") {
                #     p <- mp_lefse() %>% ggdiffbox()
                # } else {
                    #p <- mp_lefse() %>% ggdiffclade(linewd = 0.1)
                #}
                #return(p)
            })
                
                output$downloadPlot <- downloadHandler(
                    filename = function(){
                        paste("diff_taxa_plot", input$extPlot, sep='')},
                    content = function(file){
                        req(p_diffclade())
                        ggsave(file, 
                               plot = p_diffclade(), 
                               width = input$width_slider, 
                               height = input$height_slider,
                               dpi = input$dpi)
                    })
                
                output$downloadTable <- downloadHandler(
                    filename = function(){ "diff_taxa_Data.csv" },
                    content = function(file){
                        req(p_diffclade())
                        table <- p_diffclade()$data
                        n <- names(table)[sapply(table, class) == "list"] 
                        write.csv(table %>% select(-c(n)), 
                                  file,
                                  row.names = FALSE)
                    })  
                
                
            #})
        }
    )
}


