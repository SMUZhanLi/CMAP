linebreaks <- function(n){HTML(strrep(br(), n))}
error_info <- "Invalid operation: Attempt to delete all data"

data_rarefy_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = 12,
                       title = "Data rarefying",
                       status = "primary",
                       collapsible = TRUE,
                       p("Rarefied species richness for community ecologists."),
                       materialSwitch(ns("rarefying"), 
                                      value = TRUE,
                                      label = "Data rarefying",
                                      status = "primary"),
                       actionButton(ns("feature"), "Submit"),
                       br(),
                       br(),
                       downloadButton(ns("downloadAbu"), "Abundance Table"),
                       downloadButton(ns("downloadTax"), "Taxonomy Table")
                   )
            ),
            column(9,
                   uiOutput(ns("overview"))
            )
        )
    )
    return(res)
}

data_rarefy_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns

            # res <- eventReactive(input$feature, {
            #     req(inherits(mpse(), "MPSE"))
            #     if(input$rarefying){
            #        res <- mpse() %>% mp_rrarefy
            #     }else{
            #         res <- mpse()
            #     }
            #     return(res)
            # })
            
            mpsum_int <- reactive({
                req(inherits(mpse(), "MPSE"))
                x <- mpse() %>%
                    mp_extract_assays(.abundance=Abundance) %>% colSums() %>% as.data.frame
                colnames(x) <- "OTU"
                x$Sample <- rownames(x)
                return(x)
            })
            
            mpsum_btn <- eventReactive(input$feature, {
                req(inherits(mpse(), "MPSE"))

                if(input$rarefying){
                    abun <- "RareAbundance"
                }else{
                    abun <- "Abundance"
                }
                x <- mpse() %>%
                    mp_extract_assays(.abundance=!!sym(abun)) %>% colSums() %>% as.data.frame
                colnames(x) <- "OTU"
                x$Sample <- rownames(x)
                return(x)
            })
            
            mpsum <- reactive({
                req(inherits(mpse(), "MPSE"))
            if(input$feature) {
                return(mpsum_btn())
            }else{
                return(mpsum_int())
            }
            })
            
            
            # #Render overview
            output$overview <- renderUI({
                req(inherits(mpse(), "MPSE"))
                shinydashboardPlus::box(
                    width = 12,
                    title = "Library size",
                    status = "success",
                    collapsible = TRUE,
                    div(
                        class = "overview_plot",
                        style = "height:600px; width:95%; overflow:scroll;",
                        plotOutput(ns("plot"), width = "95%", height = paste0(nrow(mpsum()) * 20, "px"))
                    )
                )
            })
            
            output$plot <- renderPlot(res = 90, {
                req(inherits(mpsum(), "data.frame"))
                x_end <- max(mpsum()$OTU) * 1.2
                p <- ggplot(mpsum(), aes(y = reorder(Sample, OTU), x = OTU)) +
                    geom_point(color = "#FD9347", size = 3) +
                    geom_text(aes(label = OTU), hjust = -0.2,  size = 5) +
                    ylab("Sample") +
                    xlab("Sequence Count") +
                    scale_x_continuous(limits = c(0, x_end)) +
                    theme_classic() +
                    theme(text = element_text(size = 16, family = "serif"))
                return(p)
            })
            
            output$downloadAbu <- downloadHandler(
                filename = function(){
                    if(input$rarefying){
                        "RareAbundance.csv"
                    }else{
                        "Abundance.csv"
                    }
                    },
               content = function(file){
                   req(inherits(mpse(), "MPSE"))
                   if(input$rarefying){
                       abun <- "RareAbundance"
                   }else{
                       abun <- "Abundance"
                   }
                   
                    table <- mpse() %>% mp_extract_assays(.abundance = !!sym(abun))
                    write.csv(table, 
                              file)
                })
            
            output$downloadTax <- downloadHandler(
                filename = function(){"Taxonomy_annotation.csv"},
                content = function(file){
                    req(inherits(mpse(), "MPSE"))
                    table <- mpse() %>% mp_extract_taxonomy
                    write.csv(table, 
                              file,
                              row.names = FALSE)
                })
            
            #return(res)
        })
}