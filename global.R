library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(shinyjqui)
library(shinydashboardPlus)
library(datamods)
#library(shinyBS)
library(DT)
#library(MicrobiotaProcess)
#library(phyloseq)
library(ggplot2)
library(cowplot)
#library(gghalves)
#library(dplyr)
#library(picante)
#library(ggside)
library(shinybusy)
#library(Maaslin2) 
#library(ggupset) 
#library(ggtree)
options(shiny.maxRequestSize = 30*1024^2)

s <- function(x) {
    if(is.null(x)) {
        return(x)
    }else {
        return(sym(x))
    }
}

# double_colors<- c("#00A087FF", "#3C5488FF")
# 
# pca_color <- list(scale_fill_manual(values = double_colors),
#                   scale_color_manual(values = double_colors))

cmap_theme <- theme(
    text = element_text(size = 18, family = "serif"),
    axis.text.x = element_text(size = 16, family = "serif"),
    axis.text.y = element_text(size = 16, family = "serif"),
    legend.text = element_text(size = 16, family = "serif")
)

anosim_theme <- theme(
    text = element_text(
        size = 8, 
        color = "black"),
    plot.title = element_text(
        size = 20,
        face = "bold",
        color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
        size = 12, 
        face = "bold",
        color="#1b2838"
    ),
    plot.title.position = "plot",# slightly different from default
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12),
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed")
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



cols <- toupper(c(
    "#fb8072","#80b1d3","#bebada","#fdb462","#b3de69","#fccde5","#FDBF6F","#A6CEE3",
    "#56B4E9","#B2DF8A","#FB9A99","#CAB2D6","#A9C4E2","#79C360","#FDB762","#9471B4",
    "#A4A4A4","#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6","#ffffcc","#e5d8bd",
    "#fddaec","#f2f2f2","#8dd3c7","#d9d9d9"))

Set2 <- RColorBrewer::brewer.pal(8,"Set2")
Dark2 <- RColorBrewer::brewer.pal(8,"Dark2")

cc <- function(length) {

    if(length < length(cols)) {
        return(cols[1:length])
    }else{
        cRP_cols <- colorRampPalette(cols)
        return(cRP_cols(length))
        #stop(paste0("Length out of limit:", length(cols)))
    }
    
}

cols2 <- function(length) {
    if(length > length(Set2)) {
        cRP_set2 <- colorRampPalette(Set2)
        return(cRP_set2(length))
    }else{
        return(Set2[1:length])
    }
    
}

pattle_drak2 <- function(length) {
    if(length > length(Dark2)) {
        cc <- colorRampPalette(Dark2)
        return(cc(length))
    }else{
        return(Dark2[1:length])
    }
    
}





