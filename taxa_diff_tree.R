#p_adjust <- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")


diff_clade_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Different Clade",
                       status = "warning",
                       collapsible = TRUE,
                       fluidRow(
                           column(6,
                                  pickerInput(ns("group"), "Group:", NULL),
                           ),
                           column(6,
                                  pickerInput(ns("level"),
                                              "Taxonomy level:",
                                              choices = NULL)
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
                                  numericInput(ns("offset.abun"), "offset.abun:", 0.04, 0, 5)
                           ),
                           column(width = 6,
                                  style=list("padding-left: 5px;"),
                                  numericInput(ns("pwidth.abun"), "pwidth.abun:", 0.8, 0, 5)
                           )
                       ),
                       fluidRow(
                           column(width = 6,
                                  style=list("padding-right: 5px;"),
                                  numericInput(ns("offset.effsize"), "offset.effsize:", 0.3, 0, 5)
                           ),
                           column(width = 6,
                                  style=list("padding-left: 5px;"),
                                  numericInput(ns("pwidth.effsize"), "pwidth.effsize:", 0.5, 0, 5)
                           )
                       ),
                       h3("Download"),
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
                   )
                   )
        )
    )
    return(res)
}



diff_clade_mod <- function(id, mpse) {
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
            mp_diff <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$btn
                level <- isolate({input$level})
                group <- isolate({input$group})
                
                mp_diff <- mpse %>%
                    mp_diff_analysis(.abundance = RareAbundance,
                                     .group = !!sym(group),
                                     tip.level = "Genus",
                                     first.test.alpha = 0.01,
                                     action = "add"
                                     )
                return(mp_diff)
            })
            
            p_difftree <- reactive({
                req(mp_diff())
                input$btn
                
                p <- mp_diff() %>%
                    mp_plot_diff_res(offset.abun = input$offset.abun,
                                     pwidth.abun = input$pwidth.abun,
                                     offset.effsize = input$offset.effsize,
                                     pwidth.effsize = input$pwidth.effsize
                                     )
                return(p)
            })

            output$diff_plot <- renderPlot({
                req(p_difftree())
                p_difftree()

            })
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("diff_tree_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_difftree())
                    ggsave(file, 
                           plot = p_difftree(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "diff_tree_Data.csv" },
                content = function(file){
                    req(p_diffclade())
                    table <- p_difftree()$data
                    n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table %>% select(-c(n)), 
                              file,
                              row.names = FALSE)
                })  
        }
    )
}


