std_method <- c(
    "total", "max", "frequency", "normalize",
    "range", "hellinger", "standardize", "pa",
    "chi.square", "log", "rank", "rrank"
)

dist_method <- c(
    # "unweighted uniFrac" = "unifrac", "weighted uniFrac" = "wunifrac",
    "manhattan" = "manhattan", "euclidean" = "euclidean",
    "canberra" = "canberra", "bray" = "bray", "kulczynski" = "kulczynski"
)

hcl_layout <- c(
    "rectangular", "dendrogram", "slanted", "ellipse",
    "roundrect", "fan", "circular", "inward_circular",
    "radial", "equal_angle", "daylight", "ape"
)

beta_pca_ui <- function(id) {
    ns <- NS(id)
    res <- div(
       # class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "PCA Analysis",
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
                       plotOutput(ns("PCA_plot"), width = '900px', height = '600px'),
                       operation = c("enable", "disable", "destroy", "save", "load"),
                       options = list(
                           minHeight = 300, maxHeight = 900,
                           minWidth = 300, maxWidth = 1200
                       )
                   ))
        )
        
    )
    return(res)
}

beta_pca_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            observe({
                req(inherits(mpse, "MPSE"))
                lev <- sapply(mp_extract_sample(mpse)[-1], function(n) length(unique(n)))
                group <- names(lev[lev > 1])
                updatePickerInput(session, "group",
                    choices = group,
                    selected = tail(names(lev[lev == 2]), 1)
                )
            })
            mp_pca <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$submit
                std <- isolate({
                    input$std_method
                })
                mpse %>%
                    mp_decostand(.abundance = Abundance, method = std) %>%
                    mp_cal_pca(.abundance = !!std, action = "add")
            })
            p_PCA <- reactive({
                req(inherits(mp_pca(), "MPSE"))
                input$btn
                group <- isolate({
                    input$group
                })
                
                ellipse <- mpse %>%
                    mp_extract_sample() %>%
                    pull(!!group) %>%
                    is.character()
                
                p <- mp_pca() %>%
                    mp_plot_ord(
                        .ord = pca,
                        .group = !!sym(group),
                        .color = !!sym(group),
                        ellipse = FALSE
                    ) +
                    cmap_theme
                
                
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
                req(mp_pca())
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
            
            output$PCA_plot <- renderPlot({
                req(p_PCA())
                p_PCA()
            })
            
            output$color <- renderUI(
                #req(color_list)
                color_list()
            )
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("PCA_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_PCA())
                    ggsave(file, 
                           plot = p_PCA(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "PCA_Data.csv" },
                content = function(file){
                    req(p_PCA())
                    table <- mp_pca() %>% mp_extract_sample 
                    n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table %>% select(-c(n)), 
                              file,
                              row.names = FALSE)
                })
            
            
        }
    )
}



# beta_hcluster_ui <- function(id) {
#     ns <- NS(id)
#     res <- div(
#         class = "hcluster-body",
#         fluidRow(
#             column(
#                 width = 3,
#                 shinydashboardPlus::box(
#                     width = NULL,
#                     title = "Hierarchical clustering analysis",
#                     status = "warning",
#                     collapsible = TRUE,
#                     fluidRow(
#                         column(width = 6,
#                                style=list("padding-right: 5px;"),
#                                pickerInput(ns("std_method"),
#                                            "Standardization method:",
#                                            choices = std_method,
#                                            selected = "hellinger"
#                                )),
#                         column(width = 6,
#                                style=list("padding-left: 5px;"),
#                                pickerInput(ns("dist_method"),
#                                            "Distance method:",
#                                            choices = dist_method,
#                                            selected = "bray"
#                                ))
#                     ),
#                     fluidRow(
#                         column(width = 6,
#                                style=list("padding-right: 5px;"),
#                                pickerInput(ns("hclust_method"),
#                                            "Clustering method:",
#                                            choices = c(
#                                                "average",
#                                                "single",
#                                                "complete")
#                                )
#                         ),
#                         column(width = 6,
#                                style=list("padding-left: 5px;"),
#                                pickerInput(ns("group"), "Group:", NULL)
#                         )
#                     ),
#                     actionButton(ns("btn"), "Submit")
#                     ),
#                 
#                 shinydashboardPlus::box(
#                     width = NULL,
#                     title = "Setting Plot",
#                     status = "warning",
#                     collapsible = TRUE,
#                     pickerInput(ns("layout"),
#                                 "Graph type:",
#                                 choices = hcl_layout,
#                                 selected = "rectangular"
#                     ),
#                     colourpicker::colourInput(ns("in_track_colour_2"),
#                                               label = "Colour:",
#                                               palette = "limited",
#                                               allowedCols = cols,
#                                               value = cols[2]),
#                     fluidRow(
#                         column(width = 6,
#                                style=list("padding-right: 5px;"),
#                                numericInput(ns("width_slider"), "width:", 10,1, 20)
#                                ),
#                         column(width = 6,
#                                style=list("padding-left: 5px;"),
#                                numericInput(ns("height_slider"), "height:", 8, 1, 20)
#                                )
#                     ),
#                     radioButtons(inputId = ns('extPlot'),
#                                  label = 'Output format',
#                                  choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
#                                  inline = TRUE),
#                     fluidRow(
#                         column(width = 6,
#                                downloadButton(ns("downloadPlot"), "Plot")),
#                         column(width = 6,
#                                downloadButton(ns("downloadTable"), "Table"))
#                     )
#                 )
#             ),
#             column(width = 9,
#                    jqui_resizable(
#                        plotOutput(ns("hcluster_plot"), width = "600px"),
#                        operation = c("enable", "disable", "destroy", "save", "load"),
#                        options = list(
#                            minHeight = 300, maxHeight = 900,
#                            minWidth = 300, maxWidth = 1200
#                        )
#                    )
#             )
#         )
#     )
#     return(res)
# }



# beta_hcluster_ui <- function(id) {
#     ns <- NS(id)
#     res <- div(
#         class = "hcluster-body",
#         fluidPage(
#             pageWithSidebar(
#                 headerPanel(title= NULL),
#                 sidebarPanel(
#                     width = 6,
#                     h3("Select methods"),
#                     pickerInput(ns("std_method"),
#                                 "Standardization method:",
#                                 choices = std_method,
#                                 selected = "hellinger"
#                     ),
#                     pickerInput(ns("hclust_method"),
#                                 "Hierarchical clustering method:",
#                                 choices = c(
#                                     "average",
#                                     "single",
#                                     "complete"
#                                 ),
#                                 selected = "average"
#                     ),
#                     pickerInput(ns("dist_method"),
#                                 "Distance method:",
#                                 choices = dist_method,
#                                 selected = "bray"
#                     ),
#                     actionButton(ns("btn"), "Submit"),
#                     
#                     h3("Plot Settings"),
#                     fluidRow(
#                         column(6,style=list("padding-right: 5px;"),
#                                pickerInput(ns("layout"),
#                                            "Layout:",
#                                            choices = hcl_layout,
#                                            selected = "rectangular")
#                         ),
#                         column(6,style=list("padding-left: 5px;"),
#                                colourpicker::colourInput(ns("in_track_colour_2"),
#                                                          label="Colour:",
#                                                          palette="limited",
#                                                          allowedCols=cols,
#                                                          value=cols[2])
#                         )
#                     ),
#                     br(),
#                     h3("Plot Download"),
#                     fluidRow(
#                         column(width = 6,style=list("padding-right: 5px;"),
#                                numericInput(ns("width_slider"), "width:", 10, 1, 20),
#                                ),
#                         column(width = 6,style=list("padding-left: 5px;"),
#                                numericInput(ns("height_slider"), "height:", 8, 1, 20),
#                                )
#                     ),
#                     radioButtons(inputId = ns('extPlot'),
#                                  label = 'Output format',
#                                  choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
#                                  inline = TRUE),
#                     tags$br(),
#                     fluidRow(
#                         column(6,style=list("padding-right: 5px; "),
#                                downloadButton(ns("downloadPlot"), "Plot")
#                         ),
#                         column(6,style=list("padding-right: 5px;"),
#                                downloadButton(ns("downloadTable"), "Table")
#                         )
#                     )
#                 ),
#                 mainPanel(plotOutput(ns("hcluster_plot")))
#             )
#         )
#     )
#     return(res)
# }

# beta_hcluster_mod <- function(id, mpse) {
#     moduleServer(
#         id,
#         function(input, output, session) {
#             treeda <- reactiveVal({
#                 readRDS("data/treeda.rds")
#             })
#             observe({
#                 req(inherits(mpse, "MPSE"))
#                 lev <- sapply(mp_extract_sample(mpse)[-1], function(n) length(unique(n)))
#                 group <- names(lev[lev > 1])
#                 updatePickerInput(session, "group",
#                     choices = group,
#                     selected = tail(names(lev[lev == 2]), 1)
#                 )
#                 if (!is.null(treeda())) {
#                     updatePickerInput(session, "dist_method",
#                         choices = c(dist_method, c("unweighted uniFrac" = "unifrac", "weighted uniFrac" = "wunifrac"))
#                     )
#                 }
#             })
#             mp_hcl <- eventReactive(input$btn, {
#                 req(inherits(mpse, "MPSE"))
#                 input$submit
#                 std <- isolate({
#                     input$std_method
#                 })
#                 dist <- isolate({
#                     input$dist_method
#                 })
#                 if (dist %in% c("unifrac", "wunifrac")) {
#                     otutree(mpse) <- treeda()
#                 }
#                 mpse %>%
#                     mp_decostand(.abundance = Abundance, method = std) %>%
#                     mp_cal_clust(.abundance = !!std, distmethod = dist, action = "get")
#                 
#             })
#             
#             p_hcluster <- reactive({
#                 req(inherits(mp_hcl(), "treedata"))
#                 input$btn
#                 group <- isolate({
#                     input$group
#                 })
#                 layout <- isolate({
#                     input$layout
#                 })
#                 dist <- isolate({
#                     input$dist_method
#                 })
#                 
#                 #sample.clust <- mp_hcl() %>% mp_extract_internal_attr(name = 'SampleClust')
#                 p <- mp_hcl() %>% ggtree(layout = layout) +
#                     geom_tippoint(aes(color = !!sym(group)))  +
#                     geom_tiplab(as_ylab = TRUE) +
#                     scale_x_continuous(expand=c(0, 0.01))
#                     # theme(
#                     #     text = element_text(size = 18, family = "serif"),
#                     #     legend.text = element_text(size = 16, family = "serif")
#                     # )
#                 
#                 return(p)
#             })
#             
#             output$hcluster_plot <- renderPlot({
#                 req(p_hcluster())
#                 p_hcluster()
#             })
#             
#             output$downloadPlot <- downloadHandler(
#                 filename = function(){
#                     paste("hcluster_plot", input$extPlot, sep='')},
#                 content = function(file){
#                     req(p_hcluster())
#                     ggsave(file, 
#                            plot = p_hcluster(), 
#                            width = input$width_slider, 
#                            height = input$height_slider,
#                            dpi = 300)
#                 })
#             
#             output$downloadTable <- downloadHandler(
#                 filename = function(){ "hcluster_Data.csv" },
#                 content = function(file){
#                     req(p_hcluster())
#                     table <- p_hcluster()$data
#                     n <- names(table)[sapply(table, class) == "list"] 
#                     write.csv(table %>% select(-c(n)), 
#                               file,
#                               row.names = FALSE)
#                 })
#             
#         }
#     )
# }

# mp_plot_dist(.distmethod=bray, .group=time)


beta_adonis_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "tab-body",
        shinydashboardPlus::box(
            width = 12, title = "Adonis Analysis",
            status = "warning",
            collapsible = TRUE,
            pickerInput(ns("std_method"),
                "Standardization method:",
                choices = std_method,
                selected = "total"
            ),
            pickerInput(ns("dist_method"),
                "Distance method:",
                choices = dist_method,
                selected = "bray"
            ),
            pickerInput(ns("group"), "Group:", NULL),
            numericInput(ns("permutations"), "Permutations:", value = 999),
            actionButton(ns("btn"), "Submit")
        ),
        fluidRow(),
        verbatimTextOutput(ns("text"))
    )
    return(res)
}

beta_adonis_mod <- function(id, mpse) {
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
            adonis_res <- eventReactive(input$btn, {
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
                mpse %>%
                    mp_decostand(.abundance = Abundance, method = std) %>%
                    mp_adonis(
                        .abundance = !!std,
                        .formula = as.formula(paste0("~", group)),
                        distmethod = dist,
                        permutations = permutations
                    )
            })
            output$text <- renderPrint({
                req(inherits(adonis_res(), "adonis"))
                list(
                    paste0("R2 Value: ", adonis_res()$aov.tab$R2[1]),
                    paste0("P value: ", adonis_res()$aov.tab$`Pr(>F)`[1]),
                    adonis_res()
                )
            })
        }
    )
}




