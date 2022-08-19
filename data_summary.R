data_summary_ui <- function(id) {
  ns <- NS(id)
  res <- div(
    class = "tab-body",
    # shinydashboardPlus::box(
    # width = 12,
    # DT::dataTableOutput(ns("table"))
    # ),
    uiOutput(ns("summary")),
    uiOutput(ns("overview"))
  )
  return(res)
}

data_summary_mod <- function(id, mpse) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      #get MP summary
      mpsum <- reactive({
        req(inherits(mpse(), "MPSE"))
        mpse() %>%
          mp_stat_taxa(.abundance = Abundance, action = "get") %>%
          rename(Counts = TotalNumByAbundanceEachTaxonomy) %>%
          distinct(Sample, Counts) %>%
          arrange(Counts)
      })
      
      output$table <- DT::renderDataTable({
        mpse() %>% mp_extract_sample
      })
      
      #Render overview
      output$overview <- renderUI({
        req(inherits(mpsum(), "tbl"))
        shinydashboardPlus::box(
          width = 12,
          title = "Library size overview",
          status = "success",
          collapsible = TRUE,
          div(
            class = "overview_plot",
            style = "height:600px; width:95%; overflow:scroll;",
            plotOutput(ns("plot"), width = "95%", height = paste0(nrow(mpsum()) * 20, "px"))
          )
        )
      })
      
      output$summary <- renderUI({
        req(inherits(mpse(), "MPSE"))
        taxa <- mp_extract_taxonomy(mpse()) %>% names()
        summary_div <- tagList(
          div(
            class = "summary",
            style = "width: 40%; float: left;",
            tags$b("OTU number: "),
            tags$b("OTU annotation: "),
            tags$b("Total read counts: "),
            tags$b("Phylogenetic tree uploaded: "),
            tags$b("Number of samples in metadata: ")
          ),
          div(
            class = "summary",
            style = "width: 60%; float: left;",
            tags$b(nrow(mp_extract_feature(mpse()))),
            tags$b(paste0(taxa[-1], collapse = ";")),
            tags$b(sum(mpsum()[[2]])),
            tags$b(!is.null(input$treeda)),
            tags$b((nrow(mpsum())))
          )
        )
        shinydashboardPlus::box(
          width = 12,
          title = "Text summary",
          status = "success",
          collapsible = TRUE,
          summary_div
        )

      })
      
      #plot overview
      output$plot <- renderPlot(res = 90, {
        req(inherits(mpsum(), "tbl"))
        x_end <- max(mpsum()$Counts) * 1.2
        p <- ggplot(mpsum(), aes(y = reorder(Sample, Counts), x = Counts)) +
          geom_point(color = "#FD9347", size = 3) +
          # ggrepel::geom_text_repel(aes(label = Counts), size = 5) +
          geom_text(aes(label = Counts), hjust = -0.2,  size = 5) +
          ylab("Sample") +
          scale_x_continuous(limits = c(0, x_end)) +
          theme_classic() +
          theme(text = element_text(size = 16, family = "serif"))
        return(p)
      })
      
    })
}