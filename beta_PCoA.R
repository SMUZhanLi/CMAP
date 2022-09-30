beta_pcoa_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "PCoA Analysis",
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
                                  pickerInput(ns("dist_method"),
                                              "Distance method:",
                                              choices = dist_method,
                                              selected = "bray"
                                  )
                           )
                       ),
                       pickerInput(ns("group"), "Group:", NULL),
                       actionButton(ns("btn"), "Submit")
                   ),
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Setting Plot",
                       status = "warning",
                       collapsible = TRUE,
                       materialSwitch(ns("btn_adonis"), value = TRUE,label = "Adonis:",status = "primary"),
                       uiOutput(ns("box_order")),
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
                       plotOutput(ns("plot"), width = '900px', height = '600px'),
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
                }
                
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
            
            box_leves <- reactive({
                req(mp_pcoa())
                input$btn
                box_leves <- mp_extract_sample(mp_pcoa())[[input$group]] %>% unique
                return(box_leves)
            })
            
            #Modify color
            color_list <- reactive({
                req(mp_pcoa())
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
            
            output$box_order <- renderUI({
                req(mp_pcoa())
                orderInput(ns('items1'), 'Boxes order (Drag items below)', items = box_leves())
                
            })
            
            output$plot <- renderPlot({
                req(p_PCoA())
                p_PCoA()
            })
            
            output$color <- renderUI(
                #req(color_list)
                color_list()
            )
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("PCoA_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_PCoA())
                    ggsave(file, 
                           plot = p_PCoA(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
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
            

            
        }
    )
}