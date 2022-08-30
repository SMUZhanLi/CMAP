alpha_index <- c("Observe", "Chao1", "ACE", "Shannon", "Simpson", "Pielou")
# names(alpha_index) <- alpha_index

alpha_index_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "tab-body",
        
        shinydashboardPlus::box(
            width = 12,
            title = "Alpha-diversity Index",
            status = "warning",
            collapsible = TRUE,
            pickerInput(ns("index"), "Alpha-diversity Index:",
                options = list(`actions-box` = TRUE),
                choices = alpha_index, multiple = T,
                selected = c("Observe", "ACE", "Chao1")
            ),
            pickerInput(
                ns("test"), "Statistical method:",
                choices = c("parametric", "nonparametric", "robust", "bayes"),
                selected = "robust"
            ),
            pickerInput(ns("group"), "Group:", NULL),
            pickerInput(ns("type"), "Type:", c("discrete")),
            actionButton(ns("btn"), "Submit")
        ),
        shinydashboardPlus::box(
          width = 12,
          title = "Plot Download",
          status = "success",
          solidHeader = FALSE,
          collapsible = TRUE,
          plotOutput(ns("alpha_index_plot")),
          numericInput(ns("width_slider"), "width:", 10,1, 20),
          numericInput(ns("height_slider"), "height:", 8, 1, 20),
          radioButtons(inputId = ns('extPlot'),
                       label = 'Output format',
                       choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
                       inline = TRUE),
          downloadButton(ns("downloadPlot"), "Download Plot"),
          downloadButton(ns("downloadTable"), "Download Table")
        )
        #fluidRow(),
        # jqui_resizable(
        #         plotOutput(ns("plot"), width = "600px"),
        #         operation = c("enable", "disable", "destroy", "save", "load"),
        #         options = list(
        #             minHeight = 100, maxHeight = 900,
        #             minWidth = 300, maxWidth = 1200
        #         )
        #     )
    )
    return(res)
}


alpha_index_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            treeda <- reactiveVal({readRDS("data/treeda.rds")})
            observe({
                req(inherits(mpse, "MPSE")) 
                lev <- sapply(mp_extract_sample(mpse)[-1], function(n) length(unique(n)))
                group <- names(lev[lev > 1])
                updatePickerInput(session, "group", choices = group,
                                  selected = tail(names(lev[lev == 2]), 1))
                if (!is.null(treeda())) {
                    updatePickerInput(session, "index",
                        choices = c(alpha_index, c("PD Whole Tree" = "PD", "Species Richness" = "SR")),
                        selected = c("Observe", "ACE", "Chao1")
                        )
                }
                
            })
            observe({
                req(inherits(mpse, "MPSE"), input$group)
                if(is.numeric(mp_extract_sample(mpse)[[input$group]])) {
                    updatePickerInput(session, "type", choices = c("discrete", "continuous"), selected = "continuous")
                    updatePickerInput(session, "test", selected = "nonparametric")
                } else {
                   updatePickerInput(session, "type", choices = c("discrete"))
                   updatePickerInput(session, "test", selected = "robust")
                }
            })
            mp_alpha <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$submit
                mp_alpha <- mp_cal_alpha(mpse, .abundance = Abundance, force = T)
                if (!is.null(treeda())) {
                    pd_a_index <- pd(
                        t(mp_extract_assays(mpse, Abundance)),
                        treeda(), ape::is.rooted(treeda())
                    )
                    pd_a_index <- data.frame(
                        Sample = row.names(pd_a_index),
                        pd_a_index, row.names = NULL
                    )
                    mp_alpha <- mp_alpha %>%
                        left_join(y = pd_a_index, by = "Sample")
                }
                return(mp_alpha)
            })
            
            p_alpha_index <- reactive({
                req(inherits(mp_alpha(), "MPSE"))
                input$btn
                group <- isolate({input$group})
                index <- isolate({input$index})
                # fac <- isolate({as.numeric(input$factor)})
                test <- isolate({input$test})
                type <- isolate({input$type})
                fac <- unique(mp_extract_sample(mpse)[[group]])
                # validate(need(fac >= 2,
                #          "Please select a group with 2 (or >2) different levels."))
                
                tbl <- mp_alpha() %>%
                    mp_extract_sample() %>%
                    select(c(group, index)) %>%
                    tidyr::pivot_longer(
                        cols = !!index,
                        names_to = "Alpha")
                if (is.numeric(tbl[[group]]) && type == "continuous") {
                    p <- grouped_ggscatterstats(tbl, x = !!sym(group), y = value, grouping.var = Alpha, type = test)
                } else {
                    p <- grouped_ggbetweenstats(tbl, x = !!sym(group), y = value, grouping.var = Alpha, type = test)
                }
                return(p)
            })
            
            output$alpha_index_plot <- renderPlot({
                req(p_alpha_index())
                p_alpha_index()
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("alpha_index_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_alpha_index())
                    ggsave(file, 
                           plot = p_alpha_index(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = 300)
                    })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "alpha_Data.csv" },
                content = function(file){
                    req(p_alpha_index())
                    table <- mp_alpha() %>% mp_extract_sample 
                    n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table %>% select(-c(n)), 
                              file,
                              row.names = FALSE)
                })
        }
    )
}


# alpha_index_mod <- function(id, mpse) {
#     moduleServer(
#         id,
#         function(input, output, session) {
#             observe({
#                 req(inherits(mpse, "MPSE")) 
#                 lev <- sapply(mp_extract_sample(mpse)[-1], function(n) length(unique(n)))
#                 group <- names(lev[lev > 1])
#                 updatePickerInput(session, "group", choices = group,
#                                   selected = tail(names(lev[lev == 2]), 1))
#                 if (!is.null(mp_extract_tree(mpse, "otutree"))) {
#                     updatePickerInput(session, "index",
#                         choices = c(alpha_index, c("PD Whole Tree" = "PD", "Species Richness" = "SR")),
#                         selected = c("Observe", "ACE", "Chao1")
#                         )
#                 }
#             })
#             mp_alpha <- eventReactive(input$btn, {
#                 req(inherits(mpse, "MPSE"))
#                 input$submit
#                 mp_alpha <- mp_cal_alpha(mpse, .abundance = Abundance, force = T)
#                 if (!is.null(mp_extract_tree(mpse, "otutree"))) {
#                     treeda <- mp_extract_tree(mpse, "otutree")@phylo
#                     pd_a_index <- pd(
#                         t(mp_extract_assays(mpse, Abundance)),
#                         treeda, ape::is.rooted(treeda)
#                     )
#                     pd_a_index <- data.frame(
#                         Sample = row.names(pd_a_index),
#                         pd_a_index, row.names = NULL
#                     )
#                     mp_alpha <- mp_alpha %>%
#                         left_join(y = pd_a_index, by = "Sample")
#                 }

#                 return(mp_alpha)
#             })
#     output$plot <- renderPlot({
#         req(inherits(mp_alpha(), "MPSE"))
#         input$btn
#         group <- isolate({input$group})
#         index <- isolate({input$index})
#         # fac <- isolate({as.numeric(input$factor)})
#         test <- isolate({input$test})
#         fac <- unique(mp_extract_sample(mpse)[[group]])

#         if (test == "parm" && length(fac) == 2) {
#             method <- "t.test"
#         } else if (test == "parm" && length(fac) > 2) {
#             # method <- "anova"
# 	    method <- "t.test"
#         } else if (test == "non-parm" && length(fac) == 2) {
#             method <- "wilcox.test"
#         } else if (test == "non-parm" && length(fac) > 2) {
#             # method <- "kruskal.test"
# 	    method <- "wilcox.test"
#         }
#         validate(need(fac >= 2,
#                  "Please select a group with 2 (or >2) different levels."))
#         p <- mp_alpha() %>%
#                         mp_plot_alpha(.group = !!group,
#                                       .alpha = !!index,
#                                       test = method) +
#                                       cmap_theme
#         return(p)

#     })
#         }
#     )
# }

rare_curve_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "tab-body",
        shinydashboardPlus::box(
            width = 12, 
            title = "Rarefaction Curve",
            status = "warning",
            collapsible = TRUE,
            pickerInput(ns("index"), "Alpha-diversity Index:",
                            options = list(`actions-box` = TRUE),
                            choices = alpha_index, multiple = T,
                            selected = c("Observe", "ACE", "Chao1")),
            pickerInput(ns("group"), "Group:", NULL),
            actionButton(ns("btn"), "Submit")
        ),
        shinydashboardPlus::box(
            width = 12,
            title = "Plot Download",
            status = "success",
            solidHeader = FALSE,
            collapsible = TRUE,
            plotOutput(ns("rare_curve_plot")),
            numericInput(ns("width_slider"), "width:", 10,1, 20),
            numericInput(ns("height_slider"), "height:", 8, 1, 20),
            radioButtons(inputId = ns('extPlot'),
                         label = 'Output format',
                         choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
                         inline = TRUE),
            downloadButton(ns("downloadPlot"), "Download Plot")
            #downloadButton(ns("downloadTable"), "Download Table")
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
                           dpi = 300)
                })
            
            
        }
    )
}

