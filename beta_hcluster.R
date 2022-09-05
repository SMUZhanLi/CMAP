beta_hcluster_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "hcluster-body",
        fluidRow(
            column(
                width = 3,
                shinydashboardPlus::box(
                    width = NULL,
                    title = "Hierarchical clustering analysis",
                    status = "warning",
                    collapsible = TRUE,
                    fluidRow(
                        column(width = 6,
                               style=list("padding-right: 5px;"),
                               pickerInput(ns("std_method"),
                                           "Standardization method:",
                                           choices = std_method,
                                           selected = "hellinger"
                               )),
                        column(width = 6,
                               style=list("padding-left: 5px;"),
                               pickerInput(ns("dist_method"),
                                           "Distance method:",
                                           choices = dist_method,
                                           selected = "bray"
                               ))
                    ),
                    fluidRow(
                        column(width = 6,
                               style=list("padding-right: 5px;"),
                               pickerInput(ns("hclust_method"),
                                           "Clustering method:",
                                           choices = c(
                                               "average",
                                               "single",
                                               "complete")
                               )
                        ),
                        column(width = 6,
                               style=list("padding-left: 5px;"),
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
                    # pickerInput(ns("layout"),
                    #             "Graph type:",
                    #             choices = hcl_layout,
                    #             selected = "rectangular"
                    # ),
                    uiOutput(ns("color")),
                    fluidRow(
                        column(width = 6,
                               style=list("padding-right: 5px;"),
                               numericInput(ns("width_slider"), "width:", 10,1, 20)
                        ),
                        column(width = 6,
                               style=list("padding-left: 5px;"),
                               numericInput(ns("height_slider"), "height:", 8, 1, 20)
                        )
                    ),
                    radioButtons(inputId = ns('extPlot'),
                                 label = 'Output format',
                                 choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
                                 inline = TRUE),
                    fluidRow(
                        column(width = 6,
                               downloadButton(ns("downloadPlot"), "Plot")),
                        column(width = 6,
                               downloadButton(ns("downloadTable"), "Table"))
                    )
                )
            ),
            column(width = 9,
                   jqui_resizable(
                       plotOutput(ns("hcluster_plot"), width = "600px"),
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

beta_hcluster_mod <- function(id, mpse) {
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
                                      choices = c(dist_method,
                                                  c("unweighted uniFrac" = "unifrac", 
                                                    "weighted uniFrac" = "wunifrac"))
                    )
                }
            })
            mp_hcl <- eventReactive(input$btn, {
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
                mpse %>%
                    mp_decostand(.abundance = Abundance, method = std) %>%
                    mp_cal_clust(.abundance = !!std, distmethod = dist, action = "get")
                
            })
            
            p_hcluster <- reactive({
                req(inherits(mp_hcl(), "treedata"))
                input$btn
                group <- isolate({
                    input$group
                })
                # layout <- isolate({
                #     input$layout
                # })
                dist <- isolate({
                    input$dist_method
                })
                
                #sample.clust <- mp_hcl() %>% mp_extract_internal_attr(name = 'SampleClust')
                p <- mp_hcl() %>% ggtree(layout = "rectangular") +
                    geom_tippoint(aes(color = !!sym(group)))  +
                    geom_tiplab(as_ylab = TRUE) +
                    scale_x_continuous(expand=c(0, 0.01)) + 
                theme(
                    text = element_text(size = 18, family = "serif"),
                    legend.text = element_text(size = 16, family = "serif")
                )
                
                return(p)
            })
            
            output$hcluster_plot <- renderPlot({
                req(p_hcluster())
                p_hcluster()
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("hcluster_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_hcluster())
                    ggsave(file, 
                           plot = p_hcluster(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = 300)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "hcluster_Data.csv" },
                content = function(file){
                    req(p_hcluster())
                    table <- p_hcluster()$data
                    n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table %>% select(-c(n)), 
                              file,
                              row.names = FALSE)
                })
            
            #Motify color
            color_list <- reactive({
                ns <- NS(id)
                req(p_hcluster())
                input$btn
                
                group <- isolate({
                    input$group
                })
                
                group_content <- mpse %>% 
                    mp_extract_sample %>% 
                    select(!!sym(group)) %>% 
                    unique  #It is tibble
                
                ncolors <- group_content[[1]] #convert to chr
                pal <- cc(length(ncolors)) #calling color pallter
                names(pal) <- ncolors #mapping legend names to colors 
                
                c <- lapply(seq(pal), function(i) {
                    colorPickr(
                        inputId = ns(paste0("cols",i)),
                        label = names(pal[i]),
                        #swatches = scales::viridis_pal()(10),
                        selected = pal[[i]],
                        swatches = cols,
                        theme = "monolith",
                        useAsButton = TRUE
                    )
                })
                return(c)
            })
            
            output$color <- renderUI(
                
                #req(color_list)
                color_list()
                #tagList(color_list())
            )
            
        }
    )
}

