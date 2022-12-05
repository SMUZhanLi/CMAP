server <- function(input, output, session) {
    mpse <- upload_data_mod("upload")
    #mpse_rarefy <- data_rarefy_mod("rarefy", mpse)
    data_rarefy_mod("rarefy", mpse)
    mpse_filter <- filter_mod("filter", mpse())
    #mpse_filter <- filter_mod("filter", mpse_rarefy())
    observe({
      mpse_filter$mpse
      alpha_index_mod("a_index",mpse_filter$mpse)
      rare_curve_mod("rare", mpse_filter$mpse)
      beta_pca_mod("pca", mpse_filter$mpse)
      beta_pcoa_mod("pcoa", mpse_filter$mpse)
      beta_nmds_mod("nmds", mpse_filter$mpse)
      cca_mod("cca", mpse_filter$mpse)
      rda_mod("rda", mpse_filter$mpse)
      beta_hcluster_mod("hcluster", mpse_filter$mpse)
      #beta_adonis_mod("adonis", mpse_filter$mpse)
      beta_anosim_mod("anosim", mpse_filter$mpse)
      taxa_composition_mod("taxa_diff", mpse_filter$mpse)
      feature_composition_mod("feature_diff", mpse_filter$mpse)
      lefse_mod("lefse", mpse_filter$mpse)
      diff_clade_mod("diff_clade", mpse_filter$mpse)
      maaslin_mod("maaslin", mpse_filter$mpse)
    })
    
}

shinyServer(server)








