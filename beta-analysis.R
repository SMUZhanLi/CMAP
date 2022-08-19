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
        fluidRow(),
        jqui_resizable(
            plotOutput(ns("plot"), width = "600px"),
            operation = c("enable", "disable", "destroy", "save", "load"),
            options = list(
                minHeight = 100, maxHeight = 900,
                minWidth = 300, maxWidth = 1200
            )
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
            output$plot <- renderPlot({
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
        }
    )
}




beta_pcoa_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "tab-body",
        fluidRow(column(width=6,
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
                          actionButton(ns("btn"), "Submit"),
                          downloadButton(ns("downloadTable"), "Download Table")
                        )
                        ),
                 column(width=6,
                        shinydashboardPlus::box(
                          width = 12,
                          title = "Plot Download",
                          status = "success",
                          solidHeader = FALSE,
                          collapsible = TRUE,
                          numericInput(ns("width_slider"), "width:", 10,1, 20),
                          numericInput(ns("height_slider"), "height:", 8, 1, 20),
                          radioButtons(inputId = ns('extPlot'),
                                       label = 'Output format',
                                       choices = c('PDF' = '.pdf',"PNG" = '.png','JPEG'='.jpeg'),
                                       inline = TRUE),
                          downloadButton(ns("downloadPlot"), "Download Plot")
                        )
                        )
                 ),
        plotOutput(ns("plot"))
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
                mpse %>%
                    mp_decostand(.abundance = Abundance, method = std) %>%
                    mp_cal_pcoa(.abundance = !!std, distmethod = dist, action = "add")
            })
            
            p_PCoA <- reactive({
              req(mp_pcoa())
              req(inherits(mp_pcoa(), "MPSE"))
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
                  ellipse = ellipse
                ) +
                cmap_theme
              return(p)
            })
            
            # ----show hiddenbox----
            observeEvent(input$btn, {
              shinyjs::show(id = "hiddenplot")
            })
            
            output$plot <- renderPlot({
              req(p_PCoA())
              p_PCoA()
                # req(inherits(mp_pcoa(), "MPSE"))
                # input$btn
                # group <- isolate({
                #     input$group
                # })
                # ellipse <- mpse %>%
                #     mp_extract_sample() %>%
                #     pull(!!group) %>%
                #     is.character()
                # p <- mp_pcoa() %>%
                #     mp_plot_ord(
                #         .ord = pcoa,
                #         .group = !!sym(group),
                #         .color = !!sym(group),
                #         ellipse = ellipse
                #     ) +
                #     cmap_theme
                # return(p)
            })
          
            
            # observeEvent(input$btn, {
            #   shinyjs::show(id = "plotdownload_box")
            # })
            
            output$downloadPlot <- downloadHandler(
              filename = function(){
                paste("PCoA_plot", input$extPlot, sep='')},
              content = function(file){
                req(p_PCoA())
                ggsave(file, 
                       plot = p_PCoA(), 
                       width = input$width_slider, 
                       height = input$height_slider)
                })
            
            # output$downloadTable <- downloadHandler(
            #   filename = function(){ "PCoA_data.csv" },
            #   content = function(file){
            #     req(mp_pcoa())
            #     write.csv(mp_pcoa() %>% mp_extract_sample, file)
            #   })
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
        fluidRow(),
        jqui_resizable(
            plotOutput(ns("plot"), width = "600px"),
            operation = c("enable", "disable", "destroy", "save", "load"),
            options = list(
                minHeight = 100, maxHeight = 900,
                minWidth = 300, maxWidth = 1200
            )
        )
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
            output$plot <- renderPlot({
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
        }
    )
}



beta_hcluster_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "tab-body",
        shinydashboardPlus::box(
            width = 12,
            title = "Hierarchical clustering analysis",
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
            pickerInput(ns("hclust_method"),
                "Hierarchical clustering method:",
                choices = c(
                    "average",
                    "single",
                    "complete"
                ),
                selected = "average"
            ),
            pickerInput(ns("layout"),
                "Graph type:",
                choices = hcl_layout,
                selected = "rectangular"
            ),
            pickerInput(ns("group"), "Group:", NULL),
            actionButton(ns("btn"), "Submit")
        ),
        fluidRow(),
        jqui_resizable(
            plotOutput(ns("plot"), width = "600px"),
            operation = c("enable", "disable", "destroy", "save", "load"),
            options = list(
                minHeight = 100, maxHeight = 900,
                minWidth = 300, maxWidth = 1200
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
            output$plot <- renderPlot({
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
                p <- mp_hcl() %>%
                    ggtree(layout = layout) +
                    geom_tippoint(aes(color = !!sym(group))) +
                    theme(
                        text = element_text(size = 18, family = "serif"),
                        legend.text = element_text(size = 16, family = "serif")
                    )
                return(p)
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
        fluidRow(),
        verbatimTextOutput(ns("text")),
        jqui_resizable(
            plotOutput(ns("plot"), width = "600px"),
            operation = c("enable", "disable", "destroy", "save", "load"),
            options = list(
                minHeight = 100, maxHeight = 900,
                minWidth = 300, maxWidth = 1200
            )
        )
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
            output$plot <- renderPlot({
                req(inherits(anosim_res()$tbl, "tbl"))
                anosim_res()$tbl %>%
                    ggplot(aes(x = class, y = rank, fill = class)) +
                    geom_boxplot(notch = TRUE, varwidth = TRUE) +
                    cmap_theme
            })
        }
    )
}
