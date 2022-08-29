Taxa_level <- c(
                "Kingdom", "Phylum", "Class", "Order",
                "Family", "Genus", "Species"
            )
error_info <- "Invalid operation: Attempt to delete all data"

filter_data_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "tab-body",
        shinydashboardPlus::box(
            width = 12,
            title = "Feature filter",
            status = "warning",
            collapsible = TRUE,
            numericInput(ns("abun"),
                "Minimum counts of features:",
                value = 1
            ),
            numericInput(ns("prop"),
                "Prevalence in samples (%):",
                value = 5,
                max = 100,
                min = 0
            ),
            radioButtons(ns("rarefying"), "Data rarefying",
                c(
                    "Minimum" = "min",
                    "Default" = "default",
                    "Do not" = "no"
                ),
                inline = T
            ),
            conditionalPanel(
                "input.rarefying == 'default'",
                ns = ns,
                numericInput(ns("default"),
                    "Default size:",
                    value = 0
                )
            ),
            fluidRow(),
            actionButton(ns("feature"), "Submit")
        ),
        shinydashboardPlus::box(
            width = 12,
            title = "Taxonomy filter",
            status = "warning",
            collapsible = TRUE,
            lapply(
                Taxa_level,
                function(i) {
                    pickerInput(ns(tolower(i)), paste0(i, ":"), NULL, multiple = T, options = list(width = "275px"))
                }
            ),
            fluidRow(),
            actionButton(ns("taxonomy"), "Submit")
        ),
        shinydashboardPlus::box(
            width = 12,
            title = "Sample filter",
            status = "warning",
            collapsible = TRUE,
            pickerInput(ns("group"), "Group:", NULL),
            uiOutput(ns("ui")),
            fluidRow(),
            actionButton(ns("sample"), "Submit")
        )
    )
    return(res)
}



filter_data_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            mpse_filter <- reactiveValues(mpse = NULL)
            observe({
                req(inherits(mpse, "MPSE"))
                mpse_filter$mpse <- mpse
            })
            observe({
                req(inherits(mpse_filter$mpse, "MPSE"))
                mpse_filter$mpse
                sampleda <- mp_extract_sample(mpse_filter$mpse)
                group <- names(sampleda)
                subtext <- sapply(sampleda, function(n) {
                    paste0("<", class(n), ">", " ", length(unique(n)))
                })
                taxda <- lapply(mp_extract_taxonomy(mpse_filter$mpse)[, -1], unique)
                updatePickerInput(session, "kingdom", choices = taxda[[1]])
                updatePickerInput(session, "phylum", choices = taxda[[2]])
                updatePickerInput(session, "class", choices = taxda[[3]])
                updatePickerInput(session, "order", choices = taxda[[4]])
                updatePickerInput(session, "family", choices = taxda[[5]])
                updatePickerInput(session, "genus", choices = taxda[[6]])
                updatePickerInput(session, "species", choices = taxda[[7]])
                updatePickerInput(session, "group", choices = group, choicesOpt = list(subtext = subtext))
            })
            output$ui <- renderUI({
                req(input$group)
                multiInput(
                    inputId = ns("sub"),
                    label = NULL,
                    choices = unique(mp_extract_sample(mpse_filter$mpse)[[input$group]]),
                    options = list(
                        enable_search = FALSE,
                        non_selected_header = "Choose between:",
                        selected_header = "Remove:"
                    ),
                    width = "100%"
                )
            })
            observeEvent(input$feature, {
                req(inherits(mpse_filter$mpse, "MPSE"))
                input$feature
                abun <- isolate({input$abun})
                prop <- isolate({input$prop})
                res <- tryCatch(
                    mpse_filter$mpse %>%
                    mp_filter_taxa(.abundance = Abundance, abun, prop, TRUE),
                    error = function(e) e
                )
                if (inherits(res, "MPSE")) {
                    mpse_filter$mpse  <- res
                } else {
                    showNotification(error_info, type = "warning")
                }

                # print(mpse_filter$mpse)
            })
            observeEvent(input$sample, {
                req(inherits(mpse_filter$mpse, "MPSE"))
                input$sample
                group <- isolate({input$group})
                sub <- isolate({input$sub})
                res <- tryCatch(
                    mpse_filter$mpse %>%
                    filter(!(!!sym(group) %in% sub)), 
                    error = function(e) e
                )
                if (inherits(res, "MPSE")) {
                    mpse_filter$mpse  <- res
                } else {
                    showNotification(error_info, type = "warning")
                }
                # print(mpse_filter$mpse)
            })
            observeEvent(input$taxonomy, {
                req(inherits(mpse_filter$mpse, "MPSE"))
                input$taxonomy
                kingdom <- isolate({
                    if (is.null(input$kingdom)) {
                        ""
                    } else {
                        input$kingdom
                    }
                    
                })
                phylum <- isolate({
                    if (is.null(input$phylum)) {
                        ""
                    } else {
                        input$phylum
                    }
                    
                })
                class <- isolate({
                    if (is.null(input$class)) {
                        ""
                    } else {
                        input$class
                    }
                    
                })
                order <- isolate({
                    if (is.null(input$order)) {
                        ""
                    } else {
                        input$order
                    }
                    
                })
                family <- isolate({
                    if (is.null(input$family)) {
                        ""
                    } else {
                        input$family
                    }
                })
                genus <- isolate({
                    if (is.null(input$genus)) {
                        ""
                    } else {
                        input$genus
                    }
                })
                species <- isolate({
                    if (is.null(input$species)) {
                        ""
                    } else {
                        input$species
                    }
                })
                taxa <- mp_extract_taxonomy(mpse_filter$mpse) %>% names()
                res <- tryCatch(
                    mpse_filter$mpse %>%
                    filter(
                        !(!!sym(taxa[2]) %in% kingdom) &
                        !(!!sym(taxa[3]) %in% phylum) &
                        !(!!sym(taxa[4]) %in% class) &
                        !(!!sym(taxa[5]) %in% order) &
                        !(!!sym(taxa[6]) %in% family) &
                        !(!!sym(taxa[7]) %in% genus) &
                        !(!!sym(taxa[8]) %in% species)
                    ), error = function(e) e
                )
                if (inherits(res, "MPSE")) {
                    mpse_filter$mpse  <- res
                } else {
                    showNotification(error_info, type = "warning")
                }
            })
            return(mpse_filter)
        }
    )
}



