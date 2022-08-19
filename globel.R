library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(shinyjqui)
library(shinydashboardPlus)
#library(shinyBS)
library(DT)
#library(MicrobiotaProcess)
#library(phyloseq)
library(ggplot2)
#library(gghalves)
#library(dplyr)
#library(picante)
#library(ggside)
library(shinybusy)
#library(Maaslin2) 
#library(ggupset) 
#library(ggtree)
options(shiny.maxRequestSize = 30*1024^2)

cmap_theme <- theme(
    text = element_text(size = 18, family = "serif"),
    axis.text.x = element_text(size = 16, family = "serif"),
    axis.text.y = element_text(size = 16, family = "serif"),
    legend.text = element_text(size = 16, family = "serif")
)

calm_upload_plain <- function(otufilename, mapfilename, otutree = NULL) {
    mpseda <- mp_import_qiime(otufilename, mapfilename)
    return(mpseda)
}

calm_upload_biom <- function(BIOMfilename, mapfilename, otutree = NULL) {
    biomda <- biomformat::read_biom(BIOMfilename)
    mpseda <- as.MPSE(biomda)
    metada <- read.table(mapfilename, header = TRUE, sep = "\t",
        comment.char = "", check.names = FALSE, quote = ""
    )
    #y.by <- colnames(metada)[1]
    colnames(metada)[1] <- "Sample"

    ##get intersection between MP samples and meta samples
    mp_sample <- mpseda %>% mp_extract_sample
    meta_sample <- metada[,1]
    sample_intersect <- intersect(mp_sample[[1]], meta_sample)
    mpseda %<>% filter(Sample %in% sample_intersect)
    metada %<>% filter(Sample %in% sample_intersect)
    #mpseda %<>% left_join(metada, by = c("Sample" = y.by))
    mpseda %<>% left_join(metada, by = c("Sample" = "Sample"))
    if (!is.null(otutree)) {
        treeda <- ape::read.tree(otutree)
        otutree(mpseda) <- treeda
    }
    return(mpseda)
}
