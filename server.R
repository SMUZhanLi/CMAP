server <- function(input, output, session) {
    mpse <- upload_data_mod("upload")
    data_summary_mod("summary", mpse)
    mpse_filter <- filter_data_mod("filter", mpse())
    observe({
      mpse_filter$mpse
      alpha_index_mod("a_index", mpse_filter$mpse)
      rare_curve_mod("rare", mpse_filter$mpse)
      beta_pca_mod("pca", mpse_filter$mpse)
      beta_pcoa_mod("pcoa", mpse_filter$mpse)
      beta_nmds_mod("nmds", mpse_filter$mpse)
      beta_hcluster_mod("hcluster", mpse_filter$mpse)
      beta_adonis_mod("adonis", mpse_filter$mpse)
      beta_anosim_mod("anosim", mpse_filter$mpse)
      taxa_composition_mod("taxa_diff", mpse_filter$mpse)
      feature_composition_mod("feature_diff", mpse_filter$mpse)
      lefse_mod("lefse", mpse_filter$mpse)
      diff_clade_mod("diff_clade", mpse_filter$mpse)
      maaslin_mod("maaslin", mpse_filter$mpse)
    })
    
}

shinyServer(server)








