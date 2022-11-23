filter_ui <- function(id) {
    ns <- NS(id)
   # res <- div(
        fluidPage(
        #tags$h2("Filter data.frame"),
        #actionButton(ns("saveFilterButton"),"Save Filter Values"),
        # radioButtons(
        #     inputId = ns("dataset"),
        #     label = "Data:",
        #     choices = c(
        #         "metaData",
        #         "Taxonomy"
        #     ),
        #     inline = TRUE
        # ),
        
        fluidRow(
            column(
                width = 3,
                shinydashboardPlus::box(
                    width = NULL,
                    title = "Filter meta data",
                    status = "primary",
                    collapsible = TRUE,
                    #actionButton(ns("saveFilterButton"),"Save Filter Values", icon = icon("fa-regular fa-pen-to-square")),
                    #br(),
                    datamods::filter_data_ui(ns("mp_filter"))
                    )
            ),
            column(
                width = 9,
                # shinyWidgets::progressBar(
                #     id = ns("pbar"), value = 100,
                #     total = 100, display_pct = TRUE
                # ),

                tags$h4("Preview Filter:"),
                #DTOutput(ns("table")),
                reactable::reactableOutput(outputId = ns("table")),
                br(),
                actionButton(ns("saveFilterButton"),
                             "Save Filter Values", 
                             icon = icon("fa-regular fa-pen-to-square"),
                             class = "btn-primary"),
                br(),
                tags$h4("Filtered data:"),
                verbatimTextOutput(outputId = ns("res_str"))
                # tags$b("mpse"),
                # verbatimTextOutput(outputId = ns("mpse"))
            )
        )

     )
    #)
    #return(res)
}

filter_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            
            mpse_filter <- reactiveValues(mpse = NULL)
            #savedFilterValues <- reactiveVal()
            
            observe({
                req(inherits(mpse, "MPSE"))
                #req(any(mpse@assays %>% names %in% "RareAbundance"))
                mpse_filter$mpse <<- mpse
                #print("here!!")
                #print(mpse@assays %>% names())
                #print(mpse_filter$mpse %>% class)
            })

            observe({
                    req(inherits(mpse, "MPSE"))
                    #req(any(mpse@assays %>% names() %in% "RareAbundance"))
                    #print("here!!2")
                    #print(mpse@assays %>% names())
                    metaData <- mp_extract_sample(mpse)
                    #Taxonomy <- (mpse %>% mp_extract_taxonomy)[, -1]
                    data <- reactive({
                        #data <- get(input$dataset)
                        metaData
                    })
                    
                    res_filter <- filter_data_server(
                        id = "mp_filter",
                        data = data,
                        name = reactive("metaData"),
                        defaults = reactive(NULL),
                        widget_char = "picker",
                        drop_ids = FALSE,
                        vars = reactive(NULL)
                    )
                    
                    mpse_filter$values <<- res_filter$values
                    mpse_filter$filtered <<- res_filter$filtered

                })
            
            observeEvent(input$saveFilterButton,{
                mpse_filter$mpse <<- mpse %>%
                    filter(Sample %in% (mpse_filter$filtered())[["Sample"]])
                data_check <- mpse_filter$values()
                
                output$res_str <- renderPrint({
                    str(data_check)
                })
                
                # output$mpse <- renderPrint({
                #     mpse_filter$mpse
                # })
                
            })

            observeEvent(mpse_filter$filtered, {
                
                #output$table <- renderDT(mpse_filter$filtered())
                output$table <- reactable::renderReactable({
                    reactable::reactable(mpse_filter$filtered())
                })
                
            })

            
            # observeEvent(mpse_filter$filtered, {
            #     updateProgressBar(
            #         session = session, id = ns("pbar"),
            #         value = nrow(mpse_filter$filtered), total = nrow(metaData)
            #     )
            # })

            return(mpse_filter)
        })
}



