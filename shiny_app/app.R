# =============================================================================
# ELIM-IP Dashboard v2: Interactive Shiny application
# =============================================================================
# Exploring Levers to Increase Mosquito-borne Illness Prevention (ELIM-IP)
# KAP survey and discrete choice experiment
# Publication-quality visualization for decision support
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(DT)
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(scico)
  library(viridis)
  library(forcats)
  library(stringr)
  library(sf)
  library(leaflet)
})

# =============================================================================
# THEME AND AESTHETICS
# =============================================================================

# Nature-style theme with Helvetica
theme_elim <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "Helvetica") +
    theme(
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = base_size, color = "gray40", hjust = 0),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1),
      legend.title = element_text(size = base_size),
      legend.text = element_text(size = base_size - 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      strip.text = element_text(size = base_size, face = "bold"),
      plot.margin = margin(10, 10, 10, 10)
    )
}

# Scientific color palettes
pal_domain <- c(
  knowledge = "#1B7837",
  attitudes = "#762A83",
  practices = "#D95F02",
  dce = "#1F78B4"
)

pal_regions <- scico(5, palette = "roma")

# =============================================================================
# DATA LOADING
# =============================================================================

app_data <- readRDS("data/app_data.rds")

# Extract components
individual <- app_data$individual
dept_stats <- app_data$dept_stats
dept_lookup <- app_data$lookups$dept
regional_stats <- app_data$regional_stats
regional_uda5_stats <- app_data$regional_uda5_stats
national_stats <- app_data$national_stats
variable_metadata <- app_data$metadata
activity_labels <- app_data$activity_labels
activity_order <- app_data$activity_order
dce_attributes <- app_data$dce_attributes
provenance <- app_data$provenance

# Geographic data
dept_geo <- app_data$geo$dept_boundaries

# Create choice vectors for UI
region_choices <- sort(unique(dept_lookup$region_clean))
uda5_choices <- c("North East", "North West", "Paris region", "South East", "South West")

# Variable choices by domain
knowledge_vars <- variable_metadata %>% 
  filter(domain == "Knowledge") %>% 
  pull(variable)

attitude_vars <- variable_metadata %>% 
  filter(domain == "Attitudes") %>% 
  pull(variable)

practice_vars <- variable_metadata %>% 
  filter(domain == "Practices") %>% 
  pull(variable)

dce_vars <- variable_metadata %>% 
  filter(domain == "DCE") %>% 
  pull(variable)

# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "ELIM-IP Dashboard",
    titleWidth = 280
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Knowledge", tabName = "knowledge", icon = icon("brain")),
      menuItem("Attitudes", tabName = "attitudes", icon = icon("heart")),
      menuItem("Practices", tabName = "practices", icon = icon("shield-virus")),
      menuItem("Geographic analysis", tabName = "geographic", icon = icon("map")),
      menuItem("Demographic profiles", tabName = "demographics", icon = icon("users")),
      menuItem("DCE preferences", tabName = "dce", icon = icon("balance-scale")),
      menuItem("Data explorer", tabName = "explorer", icon = icon("table")),
      menuItem("Methodology", tabName = "methods", icon = icon("book"))
    ),
    
    hr(),
    
    h4("Filters", style = "padding-left: 15px; color: white;"),
    
    pickerInput(
      inputId = "filter_region",
      label = "Region",
      choices = c("All regions" = "all", region_choices),
      selected = "all",
      multiple = FALSE,
      options = list(`live-search` = TRUE)
    ),
    
    pickerInput(
      inputId = "filter_uda5",
      label = "Macroregion (UDA5)",
      choices = c("All macroregions" = "all", uda5_choices),
      selected = "all",
      multiple = FALSE
    ),
    
    hr(),
    
    div(
      style = "padding: 10px;",
      downloadButton("download_data", "Download data", 
                     class = "btn-info btn-block")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@400;600&display=swap');
        
        .content-wrapper { background-color: #fafafa; }
        .box { border-top: 3px solid #2c3e50; }
        .box-header { font-weight: 600; }
        .box-title { font-size: 14px; }
        .small-box h3 { font-size: 26px; font-weight: 600; }
        .small-box p { font-size: 13px; }
        .info-box-number { font-size: 22px; font-weight: 600; }
        .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #2c3e50; }
        .dataTables_wrapper { font-size: 12px; }
        .leaflet-container { background: #fafafa; }
        
        .about-text { 
          font-size: 14px; 
          line-height: 1.6; 
          color: #333;
          background: #f8f9fa;
          padding: 15px;
          border-radius: 4px;
          border-left: 4px solid #2c3e50;
        }
        
        .metric-label { font-size: 12px; color: #666; }
        .metric-value { font-size: 24px; font-weight: 600; color: #2c3e50; }
        
        .dce-explainer {
          background: #f0f4f8;
          padding: 15px;
          border-radius: 4px;
          margin-bottom: 15px;
          font-size: 13px;
        }
      "))
    ),
    
    tabItems(
      # =========================================================================
      # OVERVIEW TAB
      # =========================================================================
      tabItem(
        tabName = "overview",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = FALSE,
            div(class = "about-text",
              HTML("Explore the results of the largest quasi-representative survey on <strong>mosquito-borne disease knowledge, 
                   attitudes, practices, and preferences</strong> in metropolitan France.")
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("vbox_n_respondents", width = 3),
          valueBoxOutput("vbox_n_depts", width = 3),
          valueBoxOutput("vbox_knowledge_score", width = 3),
          valueBoxOutput("vbox_practices_mean", width = 3)
        ),
        
        fluidRow(
          box(
            title = "KAP domain summary by macroregion",
            status = "primary",
            solidHeader = TRUE,
            width = 7,
            plotlyOutput("plot_kap_summary", height = "380px")
          ),
          
          box(
            title = "Sample distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 5,
            plotlyOutput("plot_sample_dist", height = "380px")
          )
        ),
        
        fluidRow(
          box(
            title = "Geographic coverage",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            leafletOutput("map_overview", height = "400px")
          )
        )
      ),
      
      # =========================================================================
      # KNOWLEDGE TAB
      # =========================================================================
      tabItem(
        tabName = "knowledge",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Knowledge indicators",
            
            fluidRow(
              column(4,
                selectInput("knowledge_var", "Select indicator:",
                            choices = setNames(knowledge_vars, 
                                               variable_metadata$label_en[match(knowledge_vars, variable_metadata$variable)]),
                            selected = "mbd_know")
              ),
              column(4,
                selectInput("knowledge_groupby", "Group by:",
                            choices = c("Region" = "region_clean", 
                                        "Region UDA5" = "region_uda5",
                                        "Département" = "dep_code"),
                            selected = "region_clean")
              ),
              column(4,
                checkboxInput("knowledge_show_ci", "Show 95% CI", value = TRUE)
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Distribution by geography",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("plot_knowledge_geo", height = "450px")
          ),
          
          box(
            title = "Summary statistics",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            tableOutput("table_knowledge_summary"),
            hr(),
            h5("Interpretation"),
            textOutput("text_knowledge_interp")
          )
        ),
        
        fluidRow(
          box(
            title = "Disease recognition",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("plot_disease_recognition", height = "280px")
          )
        )
      ),
      
      # =========================================================================
      # ATTITUDES TAB
      # =========================================================================
      tabItem(
        tabName = "attitudes",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Attitude indicators",
            
            fluidRow(
              column(4,
                selectInput("attitude_var", "Select indicator:",
                            choices = setNames(attitude_vars,
                                               variable_metadata$label_en[match(attitude_vars, variable_metadata$variable)]),
                            selected = "confiance")
              ),
              column(4,
                selectInput("attitude_groupby", "Group by:",
                            choices = c("Region" = "region_clean",
                                        "Region UDA5" = "region_uda5",
                                        "Age group" = "age_group",
                                        "Education" = "edu2"),
                            selected = "region_clean")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Attitude distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("plot_attitude_dist", height = "420px")
          ),
          
          box(
            title = "Trust vs threat perception",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotlyOutput("plot_trust_threat", height = "420px")
          )
        ),
        
        fluidRow(
          box(
            title = "Attitude correlations",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("plot_attitude_corr", height = "320px")
          )
        )
      ),
      
      # =========================================================================
      # PRACTICES TAB
      # =========================================================================
      tabItem(
        tabName = "practices",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Prevention practices",
            
            fluidRow(
              column(4,
                selectInput("practice_groupby", "Group by:",
                            choices = c("Region" = "region_clean",
                                        "Region UDA5" = "region_uda5",
                                        "Housing type" = "housing_type"),
                            selected = "region_clean")
              ),
              column(8,
                checkboxGroupInput("practice_activities", "Select activities:",
                                   choices = setNames(activity_order, activity_labels[activity_order]),
                                   selected = activity_order[1:3],
                                   inline = TRUE)
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Activity adoption rates",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("plot_activity_rates", height = "420px")
          ),
          
          box(
            title = "Number of activities",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotlyOutput("plot_activity_count", height = "420px")
          )
        ),
        
        fluidRow(
          box(
            title = "Practice co-occurrence",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("plot_practice_heatmap", height = "380px")
          )
        )
      ),
      
      # =========================================================================
      # GEOGRAPHIC TAB
      # =========================================================================
      tabItem(
        tabName = "geographic",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Geographic analysis controls",
            
            fluidRow(
              column(4,
                selectInput("geo_variable", "Variable to map:",
                            choices = setNames(
                              c(knowledge_vars, attitude_vars, "count_prev"),
                              variable_metadata$label_en[match(
                                c(knowledge_vars, attitude_vars, "count_prev"),
                                variable_metadata$variable)]
                            ),
                            selected = "count_prev")
              ),
              column(4,
                selectInput("geo_metric", "Metric:",
                            choices = c("Mean" = "mean",
                                        "Inverse-variance weighted mean" = "ivw_mean",
                                        "Sample-size weighted mean" = "ssw_mean"),
                            selected = "mean")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Choropleth map",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            leafletOutput("map_choropleth", height = "480px")
          ),
          
          box(
            title = "Rankings",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            h5("Highest values"),
            tableOutput("table_geo_top"),
            hr(),
            h5("Lowest values"),
            tableOutput("table_geo_bottom")
          )
        )
      ),
      
      # =========================================================================
      # DEMOGRAPHICS TAB
      # =========================================================================
      tabItem(
        tabName = "demographics",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Demographic profile analysis",
            
            fluidRow(
              column(4,
                selectInput("demo_outcome", "Outcome variable:",
                            choices = c("Prevention activities" = "count_prev",
                                        "Knowledge score" = "mbd_know",
                                        "Trust" = "confiance",
                                        "Threat perception" = "threat"),
                            selected = "count_prev")
              ),
              column(4,
                selectInput("demo_groupby", "Demographic factor:",
                            choices = c("Age group" = "age_group",
                                        "Gender" = "gender",
                                        "Education" = "edu2",
                                        "Urbanization" = "commune_type",
                                        "Socioeconomic status" = "CSP",
                                        "Housing type" = "housing_type"),
                            selected = "age_group")
              ),
              column(4,
                selectInput("demo_facet", "Facet by:",
                            choices = c("None" = "none",
                                        "Region" = "region_clean",
                                        "Region UDA5" = "region_uda5",
                                        "Gender" = "gender"),
                            selected = "none")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Demographic breakdown",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("plot_demo_breakdown", height = "420px")
          ),
          
          box(
            title = "Sample composition",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotlyOutput("plot_demo_composition", height = "420px")
          )
        ),
        
        fluidRow(
          box(
            title = "Age and gender interaction",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("plot_demo_interaction", height = "320px")
          )
        )
      ),
      
      # =========================================================================
      # DCE TAB
      # =========================================================================
      tabItem(
        tabName = "dce",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Discrete choice experiment: preference analysis",
            
            div(class = "dce-explainer",
              HTML("<strong>About the DCE:</strong> Respondents chose between hypothetical 
                   mosquito control programs that varied across four attributes. The 
                   coefficients shown represent the marginal utility (preference weight) 
                   each person assigns to each attribute level, estimated via a mixed 
                   logit model. More negative values indicate stronger disutility 
                   (aversion) to that attribute level relative to the reference.<br><br>
                   <strong>Reference levels:</strong> Government-led participation, 
                   once every 5 years frequency, no environmental threat, no side effects.")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "DCE attributes and levels",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("dt_dce_attributes", height = "280px")
          )
        ),
        
        fluidRow(
          box(
            title = "Average preference coefficients",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_dce_coefficients", height = "380px")
          ),
          
          box(
            title = "Regional variation in preferences",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_dce_regional", height = "380px")
          )
        ),
        
        fluidRow(
          box(
            title = "Preference heterogeneity across individuals",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("plot_dce_heterogeneity", height = "320px")
          )
        )
      ),
      
      # =========================================================================
      # DATA EXPLORER TAB
      # =========================================================================
      tabItem(
        tabName = "explorer",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Data explorer",
            
            tabsetPanel(
              tabPanel("Département summary",
                       br(),
                       DTOutput("dt_dept_summary")),
              tabPanel("Region summary",
                       br(),
                       DTOutput("dt_regional_summary")),
              tabPanel("Region UDA5 summary",
                       br(),
                       DTOutput("dt_uda5_summary"))
            )
          )
        )
      ),
      
      # =========================================================================
      # METHODOLOGY TAB
      # =========================================================================
      tabItem(
        tabName = "methods",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Methodology and data sources",
            
            h4("ELIM-IP project"),
            p(HTML("<strong>ELIM-IP</strong> (Exploring Levers to Increase Mosquito-borne 
                   Illness Prevention) is a research project combining a Knowledge, 
                   Attitudes, and Practices (KAP) survey with a Discrete Choice Experiment 
                   (DCE) to understand public preferences for mosquito control interventions 
                   in metropolitan France.")),
            p("The project was funded by an Institut Pasteur internal seed grant."),
            
            h4("Survey methodology"),
            tags$ul(
              tags$li("Cross-sectional survey of French metropolitan residents"),
              tags$li("Quota sampling stratified by region, age, gender, and socioeconomic status"),
              tags$li(sprintf("Total sample: %s respondents", format(provenance$n_observations, big.mark = ","))),
              tags$li(sprintf("Geographic coverage: %d départements across %d regions", 
                              provenance$n_departements, provenance$n_regions))
            ),
            
            h4("Statistical methods"),
            p("To address ecological bias when aggregating individual responses:"),
            tags$ul(
              tags$li("Standard errors computed as SE = SD / √n for each département"),
              tags$li("Inverse-variance weights: w = 1/SE² (larger samples with smaller variance receive more weight)"),
              tags$li("Regional and national estimates use inverse-variance weighted means: IVW Mean = Σ(mean × w) / Σw")
            ),
            
            h4("KAP framework"),
            tags$ul(
              tags$li(HTML("<strong>Knowledge:</strong> Recognition of mosquito-borne diseases, understanding of transmission")),
              tags$li(HTML("<strong>Attitudes:</strong> Trust in health authorities, perceived threat, fear of disease, acceptance of interventions")),
              tags$li(HTML("<strong>Practices:</strong> Adoption of prevention behaviors including source reduction, personal protection, and chemical control"))
            ),
            
            h4("Discrete choice experiment"),
            p("Individual-level preference coefficients were estimated using mixed multinomial 
              logit models. Coefficients represent the utility (or disutility) associated with 
              different prevention program attribute levels relative to reference levels."),
            
            h4("Citation"),
            p(HTML(paste0("<em>", provenance$citation, "</em>"))),
            
            h4("Data provenance"),
            tags$ul(
              tags$li(sprintf("Preparation date: %s", format(provenance$preparation_date, "%Y-%m-%d"))),
              tags$li(sprintf("R version: %s", provenance$r_version))
            )
          )
        )
      )
    )
  )
)
# =============================================================================
# SERVER DEFINITION
# =============================================================================

server <- function(input, output, session) {
  
  # ===========================================================================
  # REACTIVE DATA FILTERING
  # ===========================================================================
  
  filtered_individual <- reactive({
    data <- individual
    
    if (input$filter_region != "all") {
      data <- data %>% filter(region_clean == input$filter_region)
    }
    
    if (input$filter_uda5 != "all") {
      data <- data %>% filter(region_uda5 == input$filter_uda5)
    }
    
    data
  })
  
  filtered_dept_stats <- reactive({
    data <- dept_stats
    
    if (input$filter_region != "all") {
      data <- data %>% filter(region_clean == input$filter_region)
    }
    
    if (input$filter_uda5 != "all") {
      data <- data %>% filter(region_uda5 == input$filter_uda5)
    }
    
    data
  })
  
  # ===========================================================================
  # OVERVIEW TAB
  # ===========================================================================
  
  output$vbox_n_respondents <- renderValueBox({
    n <- nrow(filtered_individual())
    valueBox(
      format(n, big.mark = ","),
      "Respondents",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$vbox_n_depts <- renderValueBox({
    n <- n_distinct(filtered_individual()$dep_code)
    valueBox(
      n,
      "Départements",
      icon = icon("map-marker-alt"),
      color = "green"
    )
  })
  
  output$vbox_knowledge_score <- renderValueBox({
    score <- mean(filtered_individual()$mbd_know, na.rm = TRUE)
    max_score <- provenance$knowledge_max
    valueBox(
      sprintf("%.1f/%d", score, max_score),
      "Avg knowledge score",
      icon = icon("brain"),
      color = "purple"
    )
  })
  
  output$vbox_practices_mean <- renderValueBox({
    score <- mean(filtered_individual()$count_prev, na.rm = TRUE)
    max_score <- provenance$practices_max
    valueBox(
      sprintf("%.1f/%d", score, max_score),
      "Avg prevention activities",
      icon = icon("shield-virus"),
      color = "orange"
    )
  })
  
  output$plot_kap_summary <- renderPlotly({
    # Compute KAP scores by UDA5 macroregion
    kap_data <- filtered_individual() %>%
      group_by(region_uda5) %>%
      summarise(
        Knowledge = mean(mbd_know, na.rm = TRUE) / provenance$knowledge_max,
        Trust = mean(confiance, na.rm = TRUE) / 10,
        `Threat perception` = mean(threat, na.rm = TRUE) / 50,
        Practices = mean(count_prev, na.rm = TRUE) / provenance$practices_max,
        n = n(),
        .groups = "drop"
      ) %>%
      filter(!is.na(region_uda5)) %>%
      pivot_longer(cols = c(Knowledge, Trust, `Threat perception`, Practices),
                   names_to = "Domain", values_to = "Score")
    
    p <- ggplot(kap_data, aes(x = Domain, y = Score, fill = region_uda5,
                               text = paste0("Macroregion: ", region_uda5,
                                            "<br>Domain: ", Domain,
                                            "<br>Normalized score: ", sprintf("%.2f", Score)))) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.85) +
      scale_fill_scico_d(palette = "roma", name = "Macroregion") +
      scale_y_continuous(limits = c(0, 1), labels = percent_format()) +
      theme_elim() +
      labs(x = NULL, y = "Normalized score (0-100%)")
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center"))
  })
  
  output$plot_sample_dist <- renderPlotly({
    sample_dist <- filtered_individual() %>%
      filter(!is.na(region_uda5)) %>%
      count(region_uda5, name = "count") %>%
      mutate(
        region_uda5 = fct_reorder(region_uda5, count),
        pct = count / sum(count)
      )
    
    p <- ggplot(sample_dist, aes(x = region_uda5, y = count, fill = region_uda5,
                                  text = paste0("Macroregion: ", region_uda5,
                                               "<br>Respondents: ", format(count, big.mark = ","),
                                               "<br>Percentage: ", sprintf("%.1f%%", pct * 100)))) +
      geom_col(show.legend = FALSE, alpha = 0.85) +
      coord_flip() +
      scale_fill_scico_d(palette = "roma") +
      theme_elim() +
      labs(x = NULL, y = "Number of respondents")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$map_overview <- renderLeaflet({
    # Prepare département sample sizes
    dept_n <- filtered_individual() %>%
      count(dep_code, dep_name, region_uda5, name = "sample_n")
    
    if (!is.null(dept_geo)) {
      # Join sample data to geographic boundaries
      map_data <- dept_geo %>%
        left_join(dept_n, by = "dep_code") %>%
        filter(!is.na(sample_n))
      
      # Color palette
      pal <- colorNumeric(
        palette = scico(100, palette = "oslo", direction = -1),
        domain = map_data$sample_n
      )
      
      # Create labels
      labels <- sprintf(
        "<strong>%s</strong><br/>Sample size: %s",
        map_data$dep_name, format(map_data$sample_n, big.mark = ",")
      ) %>% lapply(htmltools::HTML)
      
      leaflet(map_data) %>%
        addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png",
                 attribution = '&copy; <a href="https://carto.com/">CARTO</a>') %>%
        setView(lng = 2.5, lat = 46.5, zoom = 5) %>%
        addPolygons(
          fillColor = ~pal(sample_n),
          weight = 1,
          opacity = 1,
          color = "white",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#333",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal,
          values = ~sample_n,
          title = "Sample size",
          position = "bottomright",
          opacity = 0.8
        )
    } else {
      # Fallback if no geo data
      leaflet() %>%
        addTiles() %>%
        setView(lng = 2.5, lat = 46.5, zoom = 5) %>%
        addControl(html = "<div style='background:white;padding:10px;'>Geographic boundaries not available</div>",
                   position = "topright")
    }
  })
  
  # ===========================================================================
  # KNOWLEDGE TAB
  # ===========================================================================
  
  output$plot_knowledge_geo <- renderPlotly({
    var <- input$knowledge_var
    groupby <- input$knowledge_groupby
    var_label <- variable_metadata$label_en[variable_metadata$variable == var]
    
    plot_data <- filtered_dept_stats() %>%
      filter(variable == var) %>%
      group_by(across(all_of(groupby))) %>%
      summarise(
        mean_val = mean(mean, na.rm = TRUE),
        se_val = sqrt(sum(se^2, na.rm = TRUE)) / n(),
        n = sum(n),
        .groups = "drop"
      ) %>%
      arrange(desc(mean_val)) %>%
      mutate(group_label = .data[[groupby]])
    
    # Create proper tooltip text
    group_label_pretty <- case_when(
      groupby == "region_clean" ~ "Region",
      groupby == "region_uda5" ~ "Macroregion",
      groupby == "dep_code" ~ "Département",
      TRUE ~ groupby
    )
    
    plot_data <- plot_data %>%
      mutate(tooltip_text = paste0(group_label_pretty, ": ", group_label,
                                   "<br>Mean: ", sprintf("%.2f", mean_val),
                                   "<br>95% CI: [", sprintf("%.2f", mean_val - 1.96*se_val),
                                   ", ", sprintf("%.2f", mean_val + 1.96*se_val), "]",
                                   "<br>N: ", format(n, big.mark = ",")))
    
    p <- ggplot(plot_data, aes(x = reorder(group_label, mean_val), y = mean_val,
                                text = tooltip_text)) +
      geom_col(fill = pal_domain["knowledge"], alpha = 0.85) +
      coord_flip() +
      theme_elim() +
      labs(x = NULL, y = var_label)
    
    if (input$knowledge_show_ci) {
      p <- p + geom_errorbar(aes(ymin = mean_val - 1.96*se_val, 
                                  ymax = mean_val + 1.96*se_val),
                              width = 0.2, color = "gray40", linewidth = 0.4)
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  output$table_knowledge_summary <- renderTable({
    var <- input$knowledge_var
    
    filtered_dept_stats() %>%
      filter(variable == var) %>%
      summarise(
        `N départements` = n(),
        `Total N` = sum(n),
        Mean = sprintf("%.2f", mean(mean, na.rm = TRUE)),
        SD = sprintf("%.2f", sd(mean, na.rm = TRUE)),
        Min = sprintf("%.2f", min(mean, na.rm = TRUE)),
        Max = sprintf("%.2f", max(mean, na.rm = TRUE))
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$text_knowledge_interp <- renderText({
    var <- input$knowledge_var
    meta <- variable_metadata %>% filter(variable == var)
    
    mean_val <- filtered_dept_stats() %>%
      filter(variable == var) %>%
      summarise(m = mean(mean, na.rm = TRUE)) %>%
      pull(m)
    
    sprintf("The average %s across selected areas is %.2f (scale: %d to %d). Higher values indicate better knowledge of mosquito-borne diseases.",
            tolower(meta$label_en), mean_val, meta$scale_min, meta$scale_max)
  })
  
  output$plot_disease_recognition <- renderPlotly({
    # Simplified disease recognition plot
    data <- filtered_individual()
    n_total <- nrow(data)
    
    # Count mentions for key diseases
    disease_counts <- tibble(
      disease = c("Dengue", "Chikungunya", "Zika", "Malaria (paludisme)"),
      count = c(
        sum(str_detect(paste(data$maladies1, data$maladies2, data$maladies3, 
                             data$maladies4, sep = " "), regex("dengue", ignore_case = TRUE)), na.rm = TRUE),
        sum(str_detect(paste(data$maladies1, data$maladies2, data$maladies3,
                             data$maladies4, sep = " "), regex("chikungunya", ignore_case = TRUE)), na.rm = TRUE),
        sum(str_detect(paste(data$maladies1, data$maladies2, data$maladies3,
                             data$maladies4, sep = " "), regex("zika", ignore_case = TRUE)), na.rm = TRUE),
        sum(str_detect(paste(data$maladies1, data$maladies2, data$maladies3,
                             data$maladies4, sep = " "), regex("paludisme|malaria", ignore_case = TRUE)), na.rm = TRUE)
      )
    ) %>%
      mutate(
        prop = count / n_total,
        tooltip_text = paste0("Disease: ", disease,
                             "<br>Recognition: ", sprintf("%.1f%%", prop * 100),
                             "<br>Count: ", format(count, big.mark = ","))
      )
    
    p <- ggplot(disease_counts, aes(x = reorder(disease, -prop), y = prop, fill = disease,
                                     text = tooltip_text)) +
      geom_col(show.legend = FALSE, alpha = 0.85) +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_scico_d(palette = "batlow") +
      theme_elim() +
      labs(x = NULL, y = "Recognition rate")
    
    ggplotly(p, tooltip = "text")
  })
  
  # ===========================================================================
  # ATTITUDES TAB
  # ===========================================================================
  
  output$plot_attitude_dist <- renderPlotly({
    var <- input$attitude_var
    groupby <- input$attitude_groupby
    var_label <- variable_metadata$label_en[variable_metadata$variable == var]
    
    group_label_pretty <- case_when(
      groupby == "region_clean" ~ "Region",
      groupby == "region_uda5" ~ "Macroregion",
      groupby == "age_group" ~ "Age group",
      groupby == "edu2" ~ "Education",
      TRUE ~ groupby
    )
    
    plot_data <- filtered_individual() %>%
      filter(!is.na(.data[[var]]) & !is.na(.data[[groupby]])) %>%
      group_by(across(all_of(groupby))) %>%
      summarise(
        mean_val = mean(.data[[var]], na.rm = TRUE),
        se_val = sd(.data[[var]], na.rm = TRUE) / sqrt(n()),
        n = n(),
        .groups = "drop"
      ) %>%
      filter(n >= 10) %>%
      mutate(
        group_label = .data[[groupby]],
        tooltip_text = paste0(group_label_pretty, ": ", group_label,
                             "<br>Mean: ", sprintf("%.2f", mean_val),
                             "<br>95% CI: [", sprintf("%.2f", mean_val - 1.96*se_val),
                             ", ", sprintf("%.2f", mean_val + 1.96*se_val), "]",
                             "<br>N: ", format(n, big.mark = ","))
      )
    
    p <- ggplot(plot_data, aes(x = reorder(group_label, mean_val), y = mean_val,
                                text = tooltip_text)) +
      geom_col(fill = pal_domain["attitudes"], alpha = 0.85) +
      geom_errorbar(aes(ymin = mean_val - 1.96*se_val,
                        ymax = mean_val + 1.96*se_val),
                    width = 0.2, color = "gray40", linewidth = 0.4) +
      coord_flip() +
      theme_elim() +
      labs(x = NULL, y = var_label)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_trust_threat <- renderPlotly({
    plot_data <- filtered_individual() %>%
      filter(!is.na(confiance) & !is.na(threat) & !is.na(region_uda5)) %>%
      group_by(region_uda5) %>%
      summarise(
        trust = mean(confiance, na.rm = TRUE),
        threat = mean(threat, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      mutate(tooltip_text = paste0("Macroregion: ", region_uda5,
                                   "<br>Trust: ", sprintf("%.1f", trust),
                                   "<br>Threat: ", sprintf("%.1f", threat),
                                   "<br>N: ", format(n, big.mark = ",")))
    
    p <- ggplot(plot_data, aes(x = trust, y = threat, color = region_uda5, size = n,
                                text = tooltip_text)) +
      geom_point(alpha = 0.8) +
      scale_color_scico_d(palette = "roma", name = "Macroregion") +
      scale_size_continuous(name = "Sample size", range = c(4, 12)) +
      theme_elim() +
      labs(x = "Trust in authorities (1-10)", y = "Perceived threat (0-50)")
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "v", x = 1.02, y = 0.5))
  })
  
  output$plot_attitude_corr <- renderPlotly({
    att_vars <- c("confiance", "threat", "disease_prob", "disease_fear")
    att_labels <- c("Trust", "Threat", "Disease prob.", "Disease fear")
    
    corr_data <- filtered_individual() %>%
      select(all_of(att_vars)) %>%
      cor(use = "pairwise.complete.obs")
    
    rownames(corr_data) <- att_labels
    colnames(corr_data) <- att_labels
    
    corr_long <- as.data.frame(corr_data) %>%
      tibble::rownames_to_column("var1") %>%
      pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
      mutate(tooltip_text = paste0(var1, " vs ", var2,
                                   "<br>Correlation: ", sprintf("%.2f", correlation)))
    
    p <- ggplot(corr_long, aes(x = var1, y = var2, fill = correlation, text = tooltip_text)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.2f", correlation)), color = "white", size = 3.5) +
      scale_fill_scico(palette = "vik", midpoint = 0, limits = c(-1, 1), name = "r") +
      theme_elim() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = NULL, y = NULL)
    
    ggplotly(p, tooltip = "text")
  })
  
  # ===========================================================================
  # PRACTICES TAB
  # ===========================================================================
  
  output$plot_activity_rates <- renderPlotly({
    activities <- input$practice_activities
    groupby <- input$practice_groupby
    
    if (length(activities) == 0) {
      return(plotly_empty() %>% layout(title = "Select at least one activity"))
    }
    
    group_label_pretty <- case_when(
      groupby == "region_clean" ~ "Region",
      groupby == "region_uda5" ~ "Macroregion",
      groupby == "housing_type" ~ "Housing type",
      TRUE ~ groupby
    )
    
    plot_data <- filtered_individual() %>%
      filter(!is.na(.data[[groupby]])) %>%
      group_by(across(all_of(groupby))) %>%
      summarise(
        across(all_of(activities), ~ mean(., na.rm = TRUE)),
        n = n(),
        .groups = "drop"
      ) %>%
      filter(n >= 10) %>%
      pivot_longer(all_of(activities), names_to = "activity", values_to = "proportion") %>%
      mutate(
        activity_label = activity_labels[activity],
        activity_label = factor(activity_label, levels = activity_labels[activity_order]),
        group_label = .data[[groupby]],
        tooltip_text = paste0(group_label_pretty, ": ", group_label,
                             "<br>Activity: ", activity_label,
                             "<br>Adoption rate: ", sprintf("%.1f%%", proportion * 100))
      )
    
    p <- ggplot(plot_data, aes(x = group_label, y = proportion, fill = activity_label,
                                text = tooltip_text)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.85) +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_scico_d(palette = "batlow", name = "Activity") +
      theme_elim() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = NULL, y = "Adoption rate")
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "v", x = 1.02, y = 0.5))
  })
  
  output$plot_activity_count <- renderPlotly({
    plot_data <- filtered_individual() %>%
      filter(!is.na(count_prev)) %>%
      count(count_prev, name = "freq") %>%
      mutate(
        prop = freq / sum(freq),
        tooltip_text = paste0("Activities: ", count_prev,
                             "<br>Count: ", format(freq, big.mark = ","),
                             "<br>Proportion: ", sprintf("%.1f%%", prop * 100))
      )
    
    p <- ggplot(plot_data, aes(x = factor(count_prev), y = prop, text = tooltip_text)) +
      geom_col(fill = pal_domain["practices"], alpha = 0.85) +
      scale_y_continuous(labels = percent_format()) +
      theme_elim() +
      labs(x = "Number of activities", y = "Proportion of respondents")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_practice_heatmap <- renderPlotly({
    # Co-occurrence matrix using correlation
    act_data <- filtered_individual() %>%
      select(all_of(activity_order)) %>%
      mutate(across(everything(), ~ as.numeric(. == 1 | . == "Yes")))
    
    # Use correlation instead of crossprod to avoid matrix errors
    cooccur <- cor(act_data, use = "pairwise.complete.obs")
    diag(cooccur) <- NA
    
    # Convert to data frame with proper labels
    rownames(cooccur) <- activity_labels[activity_order]
    colnames(cooccur) <- activity_labels[activity_order]
    
    cooccur_df <- as.data.frame(cooccur) %>%
      tibble::rownames_to_column("act1") %>%
      pivot_longer(-act1, names_to = "act2", values_to = "correlation") %>%
      mutate(
        act1 = factor(act1, levels = activity_labels[activity_order]),
        act2 = factor(act2, levels = activity_labels[activity_order]),
        tooltip_text = paste0(act1, "<br>vs ", act2,
                             "<br>Correlation: ", sprintf("%.2f", correlation))
      )
    
    p <- ggplot(cooccur_df, aes(x = act1, y = act2, fill = correlation, text = tooltip_text)) +
      geom_tile(color = "white", linewidth = 0.3) +
      scale_fill_scico(palette = "roma", na.value = "gray95", 
                       limits = c(-0.5, 0.5), name = "Correlation") +
      theme_elim() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
            axis.text.y = element_text(size = 9)) +
      labs(x = NULL, y = NULL)
    
    ggplotly(p, tooltip = "text")
  })
  
  # ===========================================================================
  # GEOGRAPHIC TAB
  # ===========================================================================
  
  output$map_choropleth <- renderLeaflet({
    var <- input$geo_variable
    metric <- input$geo_metric
    var_label <- variable_metadata$label_en[variable_metadata$variable == var]
    
    # Prepare data
    if (metric == "mean") {
      map_values <- filtered_dept_stats() %>%
        filter(variable == var) %>%
        select(dep_code, value = mean, n)
    } else {
      map_values <- filtered_dept_stats() %>%
        filter(variable == var) %>%
        left_join(
          regional_stats %>% 
            filter(variable == var) %>%
            select(region_clean, !!sym(metric)),
          by = "region_clean"
        ) %>%
        select(dep_code, value = mean, n)
    }
    
    if (!is.null(dept_geo)) {
      map_data <- dept_geo %>%
        left_join(map_values, by = "dep_code") %>%
        filter(!is.na(value))
      
      pal <- colorNumeric(
        palette = scico(100, palette = "roma"),
        domain = map_data$value
      )
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%s: %.2f<br/>Sample: %s",
        map_data$dep_name_geo, var_label, map_data$value, 
        format(map_data$n, big.mark = ",")
      ) %>% lapply(htmltools::HTML)
      
      leaflet(map_data) %>%
        addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png",
                 attribution = '&copy; CARTO') %>%
        setView(lng = 2.5, lat = 46.5, zoom = 5) %>%
        addPolygons(
          fillColor = ~pal(value),
          weight = 1,
          opacity = 1,
          color = "white",
          fillOpacity = 0.75,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#333",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal,
          values = ~value,
          title = var_label,
          position = "bottomright",
          opacity = 0.8
        )
    } else {
      leaflet() %>%
        addTiles() %>%
        setView(lng = 2.5, lat = 46.5, zoom = 5)
    }
  })
  
  output$table_geo_top <- renderTable({
    var <- input$geo_variable
    
    filtered_dept_stats() %>%
      filter(variable == var) %>%
      arrange(desc(mean)) %>%
      head(5) %>%
      select(Département = dep_name, N = n, Mean = mean) %>%
      mutate(Mean = sprintf("%.2f", Mean))
  }, striped = TRUE, hover = TRUE)
  
  output$table_geo_bottom <- renderTable({
    var <- input$geo_variable
    
    filtered_dept_stats() %>%
      filter(variable == var) %>%
      arrange(mean) %>%
      head(5) %>%
      select(Département = dep_name, N = n, Mean = mean) %>%
      mutate(Mean = sprintf("%.2f", Mean))
  }, striped = TRUE, hover = TRUE)
  
  # ===========================================================================
  # DEMOGRAPHICS TAB
  # ===========================================================================
  
  output$plot_demo_breakdown <- renderPlotly({
    outcome <- input$demo_outcome
    groupby <- input$demo_groupby
    facet <- input$demo_facet
    outcome_label <- names(which(c("count_prev" = "count_prev", "mbd_know" = "mbd_know",
                                    "confiance" = "confiance", "threat" = "threat") == outcome))
    
    group_label_pretty <- case_when(
      groupby == "age_group" ~ "Age group",
      groupby == "gender" ~ "Gender",
      groupby == "edu2" ~ "Education",
      groupby == "commune_type" ~ "Settlement type",
      groupby == "CSP" ~ "Socioeconomic status",
      groupby == "housing_type" ~ "Housing type",
      TRUE ~ groupby
    )
    
    plot_data <- filtered_individual() %>%
      filter(!is.na(.data[[outcome]]) & !is.na(.data[[groupby]])) %>%
      { if (facet != "none") filter(., !is.na(.data[[facet]])) else . } %>%
      group_by(across(c(all_of(groupby), if(facet != "none") all_of(facet) else NULL))) %>%
      summarise(
        mean_val = mean(.data[[outcome]], na.rm = TRUE),
        se_val = sd(.data[[outcome]], na.rm = TRUE) / sqrt(n()),
        n = n(),
        .groups = "drop"
      ) %>%
      filter(n >= 10) %>%
      mutate(
        group_label = .data[[groupby]],
        tooltip_text = paste0(group_label_pretty, ": ", group_label,
                             "<br>Mean: ", sprintf("%.2f", mean_val),
                             "<br>N: ", format(n, big.mark = ","))
      )
    
    p <- ggplot(plot_data, aes(x = group_label, y = mean_val, text = tooltip_text)) +
      geom_col(fill = pal_domain["practices"], alpha = 0.85) +
      geom_errorbar(aes(ymin = mean_val - 1.96*se_val,
                        ymax = mean_val + 1.96*se_val),
                    width = 0.2, color = "gray40", linewidth = 0.4) +
      theme_elim() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = NULL, y = variable_metadata$label_en[variable_metadata$variable == outcome])
    
    if (facet != "none") {
      p <- p + facet_wrap(as.formula(paste0("~", facet)), scales = "free_x")
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_demo_composition <- renderPlotly({
    groupby <- input$demo_groupby
    
    group_label_pretty <- case_when(
      groupby == "age_group" ~ "Age group",
      groupby == "gender" ~ "Gender",
      groupby == "edu2" ~ "Education",
      groupby == "commune_type" ~ "Settlement type",
      groupby == "CSP" ~ "Socioeconomic status",
      groupby == "housing_type" ~ "Housing type",
      TRUE ~ groupby
    )
    
    plot_data <- filtered_individual() %>%
      filter(!is.na(.data[[groupby]])) %>%
      count(.data[[groupby]], name = "freq") %>%
      mutate(
        prop = freq / sum(freq),
        group_label = .data[[groupby]],
        tooltip_text = paste0(group_label_pretty, ": ", group_label,
                             "<br>Count: ", format(freq, big.mark = ","),
                             "<br>Proportion: ", sprintf("%.1f%%", prop * 100))
      )
    
    p <- ggplot(plot_data, aes(x = group_label, y = prop, text = tooltip_text)) +
      geom_col(fill = "#2c3e50", alpha = 0.85) +
      scale_y_continuous(labels = percent_format()) +
      coord_flip() +
      theme_elim() +
      labs(x = NULL, y = "Proportion of sample")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_demo_interaction <- renderPlotly({
    outcome <- input$demo_outcome
    outcome_label <- variable_metadata$label_en[variable_metadata$variable == outcome]
    
    plot_data <- filtered_individual() %>%
      filter(!is.na(.data[[outcome]]) & !is.na(age_group) & !is.na(gender)) %>%
      group_by(age_group, gender) %>%
      summarise(
        mean_val = mean(.data[[outcome]], na.rm = TRUE),
        se_val = sd(.data[[outcome]], na.rm = TRUE) / sqrt(n()),
        n = n(),
        .groups = "drop"
      ) %>%
      filter(n >= 10) %>%
      mutate(tooltip_text = paste0("Age: ", age_group,
                                   "<br>Gender: ", gender,
                                   "<br>Mean: ", sprintf("%.2f", mean_val),
                                   "<br>N: ", format(n, big.mark = ",")))
    
    p <- ggplot(plot_data, aes(x = age_group, y = mean_val, color = gender, group = gender,
                                text = tooltip_text)) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = mean_val - 1.96*se_val,
                        ymax = mean_val + 1.96*se_val),
                    width = 0.15, linewidth = 0.4) +
      scale_color_manual(values = c("Woman" = "#762A83", "Man" = "#1B7837"), name = "Gender") +
      theme_elim() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Age group", y = outcome_label)
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.2, x = 0.5, xanchor = "center"))
  })
  
  # ===========================================================================
  # DCE TAB
  # ===========================================================================
  
  output$dt_dce_attributes <- renderDT({
    dce_attributes %>%
      select(Attribute = attribute, `Attribute description` = attribute_label,
             Level = level, `Level code` = level_code, Description = description) %>%
      datatable(
        options = list(
          pageLength = 12,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        class = 'compact stripe'
      )
  })
  
  output$plot_dce_coefficients <- renderPlotly({
    # Average coefficients from national stats
    dce_means <- national_stats %>%
      filter(variable %in% dce_vars) %>%
      mutate(
        level_label = case_when(
          variable == "b_part2_mean" ~ "Individual (self)",
          variable == "b_part3_mean" ~ "Individual (self + neighbors)",
          variable == "b_part4_mean" ~ "Community dependent",
          variable == "b_freq_mean" ~ "Monthly/weekly effort",
          variable == "b_env2_mean" ~ "Negative enviro. effect",
          variable == "b_env3_mean" ~ "Enviro. under evaluation",
          variable == "b_se2_mean" ~ "Mild side effects",
          variable == "b_se3_mean" ~ "Rare serious side effects",
          TRUE ~ variable
        ),
        attribute = case_when(
          str_detect(variable, "part") ~ "Participation",
          str_detect(variable, "freq") ~ "Frequency",
          str_detect(variable, "env") ~ "Environment",
          str_detect(variable, "se") ~ "Side effects",
          TRUE ~ "Other"
        ),
        tooltip_text = paste0("Attribute: ", attribute,
                             "<br>Level: ", level_label,
                             "<br>Coefficient: ", sprintf("%.3f", ivw_mean),
                             "<br>SE: ", sprintf("%.3f", ivw_se))
      )
    
    # Order by coefficient value
    dce_means <- dce_means %>%
      mutate(level_label = fct_reorder(level_label, ivw_mean))
    
    p <- ggplot(dce_means, aes(x = level_label, y = ivw_mean, fill = attribute,
                                text = tooltip_text)) +
      geom_col(alpha = 0.85) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_errorbar(aes(ymin = ivw_mean - 1.96*ivw_se, ymax = ivw_mean + 1.96*ivw_se),
                    width = 0.2, color = "gray30", linewidth = 0.4) +
      coord_flip() +
      scale_fill_scico_d(palette = "batlow", name = "Attribute") +
      theme_elim() +
      labs(x = NULL, y = "Preference coefficient (vs reference)",
           caption = "More negative = stronger aversion to that level")
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center"))
  })
  
  output$plot_dce_regional <- renderPlotly({
    # Regional variation in key DCE coefficients
    dce_regional <- app_data$dce_uda5_summary %>%
      select(region_uda5, ends_with("_mean_mean")) %>%
      pivot_longer(-region_uda5, names_to = "variable", values_to = "coefficient") %>%
      mutate(
        variable = str_replace(variable, "_mean_mean$", "_mean"),
        level_label = case_when(
          variable == "b_part2_mean" ~ "Individual responsibility",
          variable == "b_part4_mean" ~ "Community dependent",
          variable == "b_env2_mean" ~ "Negative enviro. effect",
          variable == "b_se3_mean" ~ "Rare serious side effects",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(level_label)) %>%
      mutate(tooltip_text = paste0("Macroregion: ", region_uda5,
                                   "<br>Attribute level: ", level_label,
                                   "<br>Coefficient: ", sprintf("%.3f", coefficient)))
    
    p <- ggplot(dce_regional, aes(x = region_uda5, y = coefficient, fill = level_label,
                                   text = tooltip_text)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.85) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      scale_fill_scico_d(palette = "batlow", name = "Attribute level") +
      theme_elim() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = NULL, y = "Preference coefficient")
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "v", x = 1.02, y = 0.5))
  })
  
  output$plot_dce_heterogeneity <- renderPlotly({
    # Distribution of individual coefficients for key variables
    dce_long <- filtered_individual() %>%
      select(b_part4_mean, b_env2_mean, b_se3_mean) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "coefficient") %>%
      mutate(
        level_label = case_when(
          variable == "b_part4_mean" ~ "Community dependent",
          variable == "b_env2_mean" ~ "Negative enviro. effect",
          variable == "b_se3_mean" ~ "Rare serious side effects"
        )
      )
    
    p <- ggplot(dce_long, aes(x = coefficient, fill = level_label)) +
      geom_histogram(bins = 40, alpha = 0.7, position = "identity") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
      facet_wrap(~level_label, scales = "free_y", ncol = 3) +
      scale_fill_scico_d(palette = "batlow") +
      theme_elim() +
      theme(legend.position = "none") +
      labs(x = "Individual coefficient", y = "Frequency",
           caption = "Distribution shows heterogeneity in preferences across individuals")
    
    ggplotly(p)
  })
  
  # ===========================================================================
  # DATA EXPLORER TAB
  # ===========================================================================
  
  output$dt_dept_summary <- renderDT({
    filtered_dept_stats() %>%
      filter(variable %in% c("mbd_know", "confiance", "threat", "count_prev")) %>%
      select(dep_code, dep_name, region_clean, region_uda5, variable, n, mean, se) %>%
      mutate(
        mean = round(mean, 3),
        se = round(se, 4)
      ) %>%
      pivot_wider(names_from = variable, values_from = c(mean, se)) %>%
      datatable(
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        filter = "top",
        rownames = FALSE,
        extensions = 'Buttons'
      )
  })
  
  output$dt_regional_summary <- renderDT({
    regional_stats %>%
      filter(variable %in% c("mbd_know", "confiance", "threat", "count_prev")) %>%
      mutate(
        ivw_mean = round(ivw_mean, 3),
        ivw_se = round(ivw_se, 4)
      ) %>%
      select(region_clean, variable, n_depts, total_n, ivw_mean, ivw_se) %>%
      pivot_wider(names_from = variable, values_from = c(ivw_mean, ivw_se)) %>%
      datatable(
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        filter = "top",
        rownames = FALSE,
        extensions = 'Buttons'
      )
  })
  
  output$dt_uda5_summary <- renderDT({
    regional_uda5_stats %>%
      filter(variable %in% c("mbd_know", "confiance", "threat", "count_prev")) %>%
      mutate(
        ivw_mean = round(ivw_mean, 3),
        ivw_se = round(ivw_se, 4)
      ) %>%
      select(region_uda5, variable, n_depts, total_n, ivw_mean, ivw_se) %>%
      pivot_wider(names_from = variable, values_from = c(ivw_mean, ivw_se)) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        filter = "top",
        rownames = FALSE,
        extensions = 'Buttons'
      )
  })
  
  # ===========================================================================
  # DOWNLOADS
  # ===========================================================================
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("elimip_dept_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      filtered_dept_stats() %>%
        select(dep_code, dep_name, region_clean, region_uda5, variable, n, mean, se) %>%
        write.csv(file, row.names = FALSE)
    }
  )
}

# =============================================================================
# RUN APPLICATION
# =============================================================================

shinyApp(ui = ui, server = server)
