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

cols <- toupper(c(
    "#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#FDBF6F","#A6CEE3",
    "#56B4E9","#B2DF8A","#FB9A99","#CAB2D6","#A9C4E2","#79C360","#FDB762","#9471B4",
    "#A4A4A4","#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6","#ffffcc","#e5d8bd",
    "#fddaec","#f2f2f2","#8dd3c7","#d9d9d9"))

beta_pca_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "tab-body",
        shinydashboardPlus::box(
            width = 12, title = "PCA Analysis",
            status = "warning",
            collapsible = TRUE,
            pickerInput(ns("std_method"),
                "Standardization method:",
                choices = std_method,
                selected = "total"
            ),
            pickerInput(ns("group"), "Group:", NULL),
            actionButton(ns("btn"), "Submit")
        ),
        shinydashboardPlus::box(
            width = 12,
            title = "Plot Download",
            status = "success",
            solidHeader = FALSE,
            collapsible = TRUE,
            plotOutput(ns("PCA_plot")),
            numericInput(ns("width_slider"), "width:", 10,1, 20),
            numericInput(ns("height_slider"), "height:", 8, 1, 20),
            radioButtons(inputId = ns('extPlot'),
                         label = 'Output format',
                         choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
                         inline = TRUE),
            downloadButton(ns("downloadPlot"), "Download Plot"),
            downloadButton(ns("downloadTable"), "Download Table")
        )
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
                        ellipse = ellipse
                    ) +
                    cmap_theme
                return(p)
            })
            
            output$PCA_plot <- renderPlot({
                req(p_PCA())
                p_PCA()
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("PCA_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_PCA())
                    ggsave(file, 
                           plot = p_PCA(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = 300)
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




beta_pcoa_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "tab-body",
        shinydashboardPlus::box(
          width = 12, title = "PCoA Analysis",
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
          materialSwitch(ns("btn_adonis"), value = TRUE,label = "Adonis:",status = "primary"),
          actionButton(ns("btn"), "Submit")
        ),
        shinydashboardPlus::box(
          width = 12,
          title = "Plot Download",
          status = "success",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotOutput(ns("plot")),
          numericInput(ns("width_slider"), "width:", 10,1, 20),
          numericInput(ns("height_slider"), "height:", 8, 1, 20),
          radioButtons(inputId = ns('extPlot'),
                       label = 'Output format',
                       choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
                       inline = TRUE),
          downloadButton(ns("downloadPlot"), "Download Plot"),
          downloadButton(ns("downloadTable"), "Download Table")
        ),
        uiOutput(ns("box_order"))
        # orderInput('source', 'Source', items = c("Jan", "Feb", "Mar", "Apr", "May"),
        #            as_source = TRUE, connect = 'dest'),
        # orderInput('dest', 'Dest', items = NULL, placeholder = 'Drag items here...'),

    )
    return(res)
        #jqui_resizable(
            #plotOutput(ns("plot"), width = "600px"),
            # operation = c("enable", "disable", "destroy", "save", "load"),
            # options = list(
            #     minHeight = 100, maxHeight = 900,
            #     minWidth = 300, maxWidth = 1200
            # )
        #)
    #)
    #return(res)
}

beta_pcoa_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
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
            mp_pcoa <- eventReactive(input$btn, {
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
                group <- isolate({input$group})

                mp_pcoa <- mpse %>%
                  mp_decostand(.abundance = Abundance, method = std) %>%
                  mp_cal_pcoa(.abundance = !!std, 
                              distmethod = dist, 
                              action = "add") %>%
                  mp_adonis(.abundance = !!std,
                            .formula = as.formula(paste0("~", group)),
                            distmethod = dist,
                            permutations = 999,
                            action = "add"
                  )
                return(mp_pcoa)
            })
            
            p_PCoA <- reactive({
                req(mp_pcoa())
                input$btn
                group <- isolate({
                    input$group
                })
                ellipse <- mpse %>%
                    mp_extract_sample() %>%
                    pull(!!group) %>%
                    is.character()
                
                p <- mp_pcoa() %>%
                    mp_plot_ord(
                        .ord = pcoa,
                        .group = !!sym(group),
                        .color = !!sym(group),
                        ellipse = ellipse,
                        show.legend = FALSE
                    ) + cmap_theme 
                    
                
                p$data[[group]] %<>% factor(level = input$items1)
                
                if(input$btn_adonis) {
                    adonis_value <- mp_pcoa() %>% mp_extract_internal_attr(name='adonis')
                    #NEW VERSION OF MP mp_extract_internal_attr()
                    eq <- substitute(expr = italic(R)^2~"="~r2~","~italic(p)~"="~pvalue,
                                     env = list(r2 = adonis_value$R2[1] %>% round(5),
                                                pvalue = adonis_value$`Pr(>F)`[1])
                    ) %>% as.expression
                    
                    #older versionmp_pcoa()$aov.tab
                    # eq <- substitute(expr = italic(R)^2~"="~r2~","~italic(p)~"="~pvalue,
                    #                  env = list(r2 = adonis_value$aov.tab$R2[1] %>% round(5),
                    #                             pvalue = adonis_value$aov.tab$`Pr(>F)`[1])
                    # ) %>% as.expression
                    
                    p <- p + geom_text(aes(x = Inf, y = Inf),
                                       label = eq,
                                       hjust = 1.1, 
                                       vjust = 1.1,
                                       check_overlap = TRUE,
                                       inherit.aes = FALSE)
                    return(p)
                }
                return(p)
            })
            
            box_leves <- reactive({
              req(mp_pcoa())
              input$btn
              box_leves <- mp_extract_sample(mp_pcoa())[[input$group]] %>% unique
              return(box_leves)
              })
            
            output$plot <- renderPlot({
                req(p_PCoA())
                p_PCoA()
            })
            
            output$downloadPlot <- downloadHandler(
              filename = function(){
                paste("PCoA_plot", input$extPlot, sep='')},
              content = function(file){
                req(p_PCoA())
                ggsave(file, 
                       plot = p_PCoA(), 
                       width = input$width_slider, 
                       height = input$height_slider,
                       dpi = 300)
                })
            
            output$downloadTable <- downloadHandler(
              filename = function(){ "PCoA_Data.csv" },
              content = function(file){
                req(mp_pcoa())
                table <- mp_pcoa() %>% mp_extract_sample 
                n <- names(table)[sapply(table, class) == "list"] 
                write.csv(table %>% select(-c(n)), 
                          file,
                          row.names = FALSE)
              })
            
            output$box_order <- renderUI({
              req(input$btn)
              orderInput(ns('items1'), 'Boxes order (Drag items below)', items = box_leves())

            })
            
        }
    )
}


beta_nmds_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "tab-body",
        shinydashboardPlus::box(
            width = 12, title = "NMDS Analysis",
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
            actionButton(ns("btn"), "Submit")
        ),
        shinydashboardPlus::box(
            width = 12,
            title = "Plot Download",
            status = "success",
            solidHeader = FALSE,
            collapsible = TRUE,
            plotOutput(ns("nmds_plot")),
            numericInput(ns("width_slider"), "width:", 10,1, 20),
            numericInput(ns("height_slider"), "height:", 8, 1, 20),
            radioButtons(inputId = ns('extPlot'),
                         label = 'Output format',
                         choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
                         inline = TRUE),
            downloadButton(ns("downloadPlot"), "Download Plot"),
            downloadButton(ns("downloadTable"), "Download Table")
        )
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
                return(p)
            })
            
            output$nmds_plot <- renderPlot({
                req(p_nmds())
                p_nmds()
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("nmds_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_nmds())
                    ggsave(file, 
                           plot = p_nmds(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = 300)
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

# beta_hcluster_ui <- function(id) {
#     ns <- NS(id)
#     res <- div(
#         class = "hcluster-body",
#         shinydashboardPlus::box(
#             width = NULL,
#             title = "Hierarchical clustering analysis",
#             status = "warning",
#             collapsible = TRUE,
#             pickerInput(ns("std_method"),
#                         "Standardization method:",
#                         choices = std_method,
#                         selected = "hellinger"
#             ),
#             pickerInput(ns("dist_method"),
#                         "Distance method:",
#                         choices = dist_method,
#                         selected = "bray"
#             ),
#             pickerInput(ns("hclust_method"),
#                         "Hierarchical clustering method:",
#                         choices = c(
#                             "average",
#                             "single",
#                             "complete"
#                         ),
#                         selected = "average"
#             ),
#             pickerInput(ns("layout"),
#                         "Graph type:",
#                         choices = hcl_layout,
#                         selected = "rectangular"
#             ),
#             pickerInput(ns("group"), "Group:", NULL),
#             actionButton(ns("btn"), "Submit")
#         ),
#         shinydashboardPlus::box(
#             width = NULL,
#             title = "Plot Download",
#             status = "success",
#             solidHeader = FALSE,
#             collapsible = TRUE,
#             plotOutput(ns("hcluster_plot")),
#             numericInput(ns("width_slider"), "width:", 10,1, 20),
#             numericInput(ns("height_slider"), "height:", 8, 1, 20),
#             radioButtons(inputId = ns('extPlot'),
#                          label = 'Output format',
#                          choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
#                          inline = TRUE),
#             downloadButton(ns("downloadPlot"), "Download Plot"),
#             downloadButton(ns("downloadTable"), "Download Table")
#         )
#             
#         # fluidRow(),
#         # jqui_resizable(
#         #     plotOutput(ns("plot"), width = "600px"),
#         #     operation = c("enable", "disable", "destroy", "save", "load"),
#         #     options = list(
#         #         minHeight = 100, maxHeight = 900,
#         #         minWidth = 300, maxWidth = 1200
#         #     )
#         # )
#     )
#     return(res)
# }


beta_hcluster_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "hcluster-body",
        fluidPage(
            pageWithSidebar(
                headerPanel(title= NULL),
                sidebarPanel(
                    width = 6,
                    h3("Select methods"),
                    pickerInput(ns("std_method"),
                                "Standardization method:",
                                choices = std_method,
                                selected = "hellinger"
                    ),
                    pickerInput(ns("hclust_method"),
                                "Hierarchical clustering method:",
                                choices = c(
                                    "average",
                                    "single",
                                    "complete"
                                ),
                                selected = "average"
                    ),
                    pickerInput(ns("dist_method"),
                                "Distance method:",
                                choices = dist_method,
                                selected = "bray"
                    ),
                    actionButton(ns("btn"), "Submit"),
                    
                    h3("Plot Settings"),
                    fluidRow(
                        column(6,style=list("padding-right: 5px;"),
                               pickerInput(ns("layout"),
                                           "Layout:",
                                           choices = hcl_layout,
                                           selected = "rectangular")
                        ),
                        column(6,style=list("padding-left: 5px;"),
                               colourpicker::colourInput(ns("in_track_colour_2"),
                                                         label="Colour:",
                                                         palette="limited",
                                                         allowedCols=cols,
                                                         value=cols[2])
                        )
                    ),
                    
                    h3("Plot Download"),
                    fluidRow(
                        column(width = 6,style=list("padding-right: 5px;"),
                               numericInput(ns("width_slider"), "width:", 10, 1, 20),
                               ),
                        column(width = 6,style=list("padding-left: 5px;"),
                               numericInput(ns("height_slider"), "height:", 8, 1, 20),
                               )
                    ),
                    radioButtons(inputId = ns('extPlot'),
                                 label = 'Output format',
                                 choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
                                 inline = TRUE),
                    tags$br(),
                    fluidRow(
                        column(6,style=list("padding-right: 5px; "),
                               downloadButton(ns("downloadPlot"), "Download Plot")
                        ),
                        column(6,style=list("padding-left: 5px;"),
                               downloadButton(ns("downloadTable"), "Download Table")
                        )
                    )
                ),
                mainPanel(plotOutput("out_plot"))
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
                        choices = c(dist_method, c("unweighted uniFrac" = "unifrac", "weighted uniFrac" = "wunifrac"))
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
                layout <- isolate({
                    input$layout
                })
                dist <- isolate({
                    input$dist_method
                })
                
                #sample.clust <- mp_hcl() %>% mp_extract_internal_attr(name = 'SampleClust')
                p <- mp_hcl() %>% ggtree(layout = layout) +
                    geom_tippoint(aes(color = !!sym(group)))  +
                    geom_tiplab(as_ylab = TRUE) +
                    scale_x_continuous(expand=c(0, 0.01))
                    # theme(
                    #     text = element_text(size = 18, family = "serif"),
                    #     legend.text = element_text(size = 16, family = "serif")
                    # )
                
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
            
        }
    )
}

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



beta_anosim_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "tab-body",
        shinydashboardPlus::box(
            width = 12, title = "Anosim Analysis",
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
        shinydashboardPlus::box(
            width = 12,
            title = "Plot Download",
            status = "success",
            solidHeader = FALSE,
            collapsible = TRUE,
            plotOutput(ns("anosim_plot")),
            numericInput(ns("width_slider"), "width:", 10,1, 20),
            numericInput(ns("height_slider"), "height:", 8, 1, 20),
            radioButtons(inputId = ns('extPlot'),
                         label = 'Output format',
                         choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
                         inline = TRUE),
            downloadButton(ns("downloadPlot"), "Download Plot"),
            downloadButton(ns("downloadTable"), "Download Table")
        )
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
            
            output$text <- renderText({
                req(inherits(anosim_res()$tbl, "tbl"))
                paste0(anosim_res()$verbose, collapse = " ")
            })
            
            p_anosim <- reactive({
                req(inherits(anosim_res()$tbl, "tbl"))
                anosim_res()$tbl %>%
                    ggplot(aes(x = class, y = rank, fill = class)) +
                    geom_boxplot(notch = TRUE, varwidth = TRUE) +
                    cmap_theme
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
                           dpi = 300)
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
