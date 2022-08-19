# taxa_level <- c("Kingdom", "Phylum", "Class", "Order",
#                 "Family", "Genus", "Species")


taxa_composition_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "tab-body",
        shinydashboardPlus::box(
            width = 12, title = "Taxonomy composition Analysis",
            status = "warning", collapsible = TRUE,
            pickerInput(ns("level"),
                        "Taxonomic level:",
                        choices = NULL),
            pickerInput(ns("group"), "Group:", NULL),
            pickerInput(ns("ytype"), "Y axis:", c("relative", "count")),
            pickerInput(ns("xtype"), "X axis:", c("sample", "group")),
            numericInput(ns("topn"), "Top most abundant:", value = 10),
            actionButton(ns("btn"), "Submit")
        ),
        fluidRow(),
        jqui_resizable(
            plotOutput(ns("plot"), width = "900px"),
            operation = c("enable", "disable", "destroy", "save", "load"),
            options = list(
                minHeight = 300, maxHeight = 900,
                minWidth = 600, maxWidth = 1200
            )
        )
    )
    return(res)
}


taxa_composition_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            observe({
                req(inherits(mpse, "MPSE")) 
                lev <- sapply(mp_extract_sample(mpse), function(n) length(unique(n)))
                group <- names(lev[lev > 1])
                taxa <- names(mp_extract_taxonomy(mpse))[-1]
                updatePickerInput(session, "group", choices = group,
                                  selected = tail(names(lev[lev == 2]), 1))
                updatePickerInput(session, "level", choices = taxa,
                                  selected = taxa[3])
            })
            mp_abu <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$submit
                level <- isolate({input$level})
                is.relative <- isolate({
                    ifelse(input$ytype == "relative", TRUE, FALSE)
                })
                group <- isolate({input$group})
                topn <- isolate({input$topn})
                is.group <- isolate({
                    ifelse(input$xtype == "group", TRUE, FALSE)
                })
                mpse %>%
                mp_plot_abundance(.abundance = Abundance,
                                  .group = !!sym(group),
                                  taxa.class = !!sym(level),
                                  topn = topn,
                                  relative = is.relative,
                                  plot.group = is.group,
                                  force = TRUE)
            })
            output$plot <- renderPlot({
                req(inherits(mp_abu(), "ggplot"))
                mp_abu() +
                # geom_col(position = position_stack(reverse = TRUE)) +
                theme(
                    text = element_text(size = 18, family = "serif"),
                    axis.text.x = element_text(size = 11, family = "serif"),
                    axis.text.y = element_text(size = 16, family = "serif"),
                    legend.text = element_text(size = 16, family = "serif")
                )
            })
        }
    )
}





# mpse %>%
#     mp_extract_tree() %>%
#     tidyr::unnest(RareAbundanceBySample) %>%
#     dplyr::filter(nodeClass == "Phylum") %>%
#     ggplot(mapping = aes(x = strain, y = RelRareAbundanceBySample, fill = strain)) +
#     geom_boxplot() +
#     facet_wrap(facets = vars(label), scales = "free_y") +
#     geom_signif(comparisons = list(c("mutant", "wildtype")))


feature_composition_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "tab-body",
        shinydashboardPlus::box(
            width = 12, title = "Taxonomy composition Analysis",
            status = "warning", collapsible = TRUE,
            pickerInput(ns("level"),
                        "Taxonomic level:",
                        choices = NULL),
            pickerInput(ns("feature"), NULL, NULL,
            options = list(`actions-box` = TRUE), multiple = TRUE),
            pickerInput(ns("group"), "Group:", NULL),
            actionButton(ns("btn"), "Submit")
        ),
        fluidRow(),
        jqui_resizable(
            plotOutput(ns("plot"), width = "900px"),
            operation = c("enable", "disable", "destroy", "save", "load"),
            options = list(
                minHeight = 300, maxHeight = 900,
                minWidth = 600, maxWidth = 1200
            )
        )
    )
    return(res)
}


feature_composition_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            observe({
                req(inherits(mpse, "MPSE"))
                lev <- sapply(mp_extract_sample(mpse), function(n) length(unique(n)))
                group <- names(lev[lev > 1])
                taxa <- names(mp_extract_taxonomy(mpse))[-1]
                updatePickerInput(session, "group",
                    choices = group,
                    selected = tail(names(lev[lev == 2]), 1)
                )
                updatePickerInput(session, "level",
                    choices = taxa,
                    selected = taxa[3]
                )
                
            })
            observe({
                req(inherits(mpse, "MPSE"), input$level)
                updatePickerInput(session, "feature",
                    choices = unique(mp_extract_taxonomy(mpse)[[input$level]])
                )
            })
            mp_abu <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$submit
                level <- isolate({input$level})
                group <- isolate({input$group})
                feature <- isolate({input$feature})
                mpse %>%
                    mp_cal_abundance(.abundance=Abundance, action="add", force = T) %>%
                    mp_extract_tree() %>%
                    tidyr::unnest(AbundanceBySample) %>%
                    dplyr::filter(nodeClass == level) %>%
                    dplyr::filter(label %in% feature) %>%
                    grouped_ggbetweenstats(x = !!sym(group), y = RelAbundanceBySample, grouping.var = label, type = "robust")
            })

            output$plot <- renderPlot({

                req(inherits(mp_abu(), "ggplot"))
                mp_abu()
            })
        }
    )
}
