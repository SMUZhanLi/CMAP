source("globel.R")
source("upload-data.R")
source("filter-data.R")
source("alpha-analysis.R")
source("beta-analysis.R")
source("taxa-composition-analysis.R")
source("taxa-diff-analysis.R")
source("maaslin.R")
source("analysis-overview.R")
source("data_summary.R")

header <- dashboardHeader(disable = T)


sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tab",
        menuItem(
            text = "分析总览",
            tabName = "overview"
        ),
        menuItem(
            text = "分析前准备",
            menuSubItem("数据上传", tabName = "upload"),
            menuSubItem("数据总览", tabName = "summary"),
            menuSubItem("数据筛选", tabName = "filter")
        ),
        menuItem(
            "Alpha多样性分析",
            menuSubItem("a多样性指数比较", tabName = "a_index"),
            menuSubItem("稀释曲线", tabName = "rare_curve")
        ),
        menuItem(
            "Beta多样性分析",
            menuSubItem("PCA分析", tabName = "pca"),
            menuSubItem("PCoA分析", tabName = "pcoa"),
            menuSubItem("NMDS分析", tabName = "nmds"),
            menuSubItem("Adonis分析", tabName = "adonis"),
            menuSubItem("Anosim分析", tabName = "anosim"),
            menuSubItem("样本层级聚类分析", tabName = "hcluster")
            
        ),
        menuItem(
            "群落组成分析",
            menuSubItem("菌群分析", tabName = "taxa_diff"),
            menuSubItem("单菌分析", tabName = "feature_diff")
            # menuSubItem("Venn/Upset图", tabName = "taxa_diff_venn"),
            # menuSubItem("Heatmap图", tabName = "taxa_diff_heatmap")
        ),
        menuItem(
            "环境因子分析",
            # menuSubItem("CCA分析", tabName = "cca"),
            menuSubItem("Maaslin2分析", tabName = "maaslin")
        ),
        menuItem(
            "差异物种分析",
            # menuSubItem("ANCOM分析", tabName = "ancom"),
            menuSubItem("LEfSe分析", tabName = "lefse")
        )
        # menuItem(
        #     "进化分析",
        #     # menuSubItem("ANCOM分析", tabName = "ancom"),
        #     menuSubItem("系统发育树", tabName = "tree")
        # ),
        # menuItem(
        #     "功能预测",
        #     # menuSubItem("ANCOM分析", tabName = "ancom"),
        #     menuSubItem("PICRUst", tabName = "PICRUst")
        # )
    )
)


body <- dashboardBody(
    # includeCSS("www/CMAP.css"),
    tabItems(
        tabItem("overview",  analysis_overview_ui()),
        tabItem("a_index", alpha_index_ui("a_index")),
        tabItem("rare_curve", rare_curve_ui("rare")),
        tabItem("filter", filter_data_ui("filter")),
        tabItem("summary", data_summary_ui("summary")),
        tabItem("upload", upload_data_ui("upload")),
        tabItem("pca", beta_pca_ui("pca")),
        tabItem("pcoa", beta_pcoa_ui("pcoa")),
        tabItem("nmds", beta_nmds_ui("nmds")),
        tabItem("hcluster", beta_hcluster_ui("hcluster")),
        tabItem("adonis", beta_adonis_ui("adonis")),
        tabItem("anosim", beta_anosim_ui("anosim")),
        tabItem("taxa_diff", taxa_composition_ui("taxa_diff")),
        tabItem("feature_diff", feature_composition_ui("feature_diff")),
        tabItem("lefse", lefse_ui("lefse")),
        tabItem("maaslin", maaslin_ui("maaslin"))
    )
)

ui <- fillPage(
    shinydashboardPlus::dashboardPage(
        scrollToTop = TRUE,
        title = "CMAP",
        header = header,
        sidebar = sidebar,
        body = body
    )
)


ui <- tagList(
    # useShinyjs(),
    includeCSS("www/CMAP.css"),
    add_busy_spinner(spin = "circle", position = "bottom-right"),
    tags$head(
        tags$meta(name = "referrer", content = "no-referrer")
    ),
    tags$image(src = "CMAP_header.png", style = "width: 100%;"),

    shinydashboardPlus::dashboardPage(
        scrollToTop = TRUE,
        title = "CMAP",
        header = header,
        sidebar = sidebar,
        body = body
    )
)


shinyUI(ui)
