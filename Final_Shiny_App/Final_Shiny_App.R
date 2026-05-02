library(shiny)
library(tidyverse)
library(janitor)
library(DT)

# ---- read data ----
data <- read.csv("wastewater.csv") %>%
  clean_names()

# ---- create variables ----
data <- data %>%
  mutate(
    total_organic_input = sludge_digested + food_scraps_digested,
    input_level = ntile(total_organic_input, 3),
    input_level = factor(input_level, labels = c("Low", "Medium", "High")),
    year  = as.factor(year),
    month = as.factor(month)
  )

var_labels <- c(
  rng_produced        = "RNG Produced",
  biogas_flared       = "Biogas Flared",
  total_organic_input = "Total Organic Input",
  rng_system_uptime   = "System Uptime (%)",
  input_level         = "Input Level",
  year                = "Year",
  month               = "Month"
)

# ---- CSS ----
css <- "
@import url('https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700;800&family=Source+Serif+4:ital,wght@0,400;1,400&family=IBM+Plex+Sans:wght@300;400;500;600&family=IBM+Plex+Mono:wght@400;500&display=swap');

:root {
  --navy:        #1b2a3b;
  --navy-light:  #243547;
  --gold:        #c9973a;
  --gold-light:  #f5e9d3;
  --gold-border: #dfc38a;
  --bg:          #f2f0eb;
  --bg-card:     #ffffff;
  --bg-sidebar:  #faf9f6;
  --border:      #e2ddd5;
  --text-dark:   #1b2a3b;
  --text-body:   #3d4f5e;
  --text-muted:  #7a8a96;
  --green:       #2d7d52;
  --red:         #c0392b;
  --shadow-sm:   0 1px 4px rgba(0,0,0,0.07);
  --shadow-md:   0 4px 16px rgba(0,0,0,0.10);
}

*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

html, body {
  background: var(--bg) !important;
  color: var(--text-body) !important;
  font-family: 'IBM Plex Sans', sans-serif !important;
  font-size: 14px; line-height: 1.6;
}

/* HERO */
.hero {
  background: var(--navy);
  padding: 34px 44px 28px;
  border-bottom: 4px solid var(--gold);
}
.hero-title {
  font-family: 'Playfair Display', serif !important;
  font-weight: 800; font-size: 2rem;
  color: #ffffff !important; line-height: 1.2; margin-bottom: 8px;
}
.hero-sub {
  font-size: 0.8rem; color: rgba(255,255,255,0.5); letter-spacing: 0.04em;
}

/* GOAL BOX */
.goal-box {
  background: var(--bg-card);
  border-left: 5px solid var(--gold);
  border-radius: 8px;
  padding: 18px 22px;
  margin: 26px 34px;
  font-family: 'Source Serif 4', serif;
  font-size: 0.95rem; line-height: 1.75;
  color: var(--text-body);
  box-shadow: var(--shadow-sm);
}
.goal-box strong { color: var(--text-dark); }

/* LAYOUT */
.container-fluid { padding: 0 !important; }
.row { margin: 0 !important; }

/* SIDEBAR */
.well {
  background: var(--bg-sidebar) !important;
  border: none !important;
  border-right: 1px solid var(--border) !important;
  border-radius: 0 !important;
  box-shadow: none !important;
  padding: 26px 20px !important;
  min-height: 100%;
}
.ctrl-title {
  font-family: 'Playfair Display', serif;
  font-size: 1.1rem; font-weight: 700;
  color: var(--text-dark);
  padding-bottom: 12px; margin-bottom: 20px;
  border-bottom: 1px solid var(--border);
}
.ctrl-section { margin-bottom: 24px; }
.ctrl-label {
  font-size: 0.63rem; font-weight: 600;
  letter-spacing: 0.13em; text-transform: uppercase;
  color: var(--text-muted); margin-bottom: 10px; display: block;
}
.radio label, .radio-inline label {
  font-size: 0.86rem !important; color: var(--text-body) !important;
  font-family: 'IBM Plex Sans', sans-serif !important; padding-left: 6px !important;
}
input[type='radio'] { accent-color: var(--navy); }
.checkbox label { font-size: 0.84rem !important; color: var(--text-body) !important; }
input[type='checkbox'] { accent-color: var(--navy); }

/* Selectize */
.selectize-control .selectize-input {
  background: var(--bg-card) !important;
  border: 1px solid var(--border) !important; border-radius: 5px !important;
  color: var(--text-dark) !important; font-family: 'IBM Plex Sans', sans-serif !important;
  font-size: 0.84rem !important; padding: 8px 10px !important;
  box-shadow: var(--shadow-sm) !important;
}
.selectize-control .selectize-input.focus {
  border-color: var(--gold) !important;
  box-shadow: 0 0 0 3px rgba(201,151,58,0.18) !important;
}
.selectize-dropdown {
  background: var(--bg-card) !important; border: 1px solid var(--border) !important;
  border-radius: 5px !important; box-shadow: var(--shadow-md) !important;
  font-family: 'IBM Plex Sans', sans-serif !important; font-size: 0.84rem !important;
}
.selectize-dropdown .option { color: var(--text-body) !important; padding: 8px 12px !important; }
.selectize-dropdown .option:hover,
.selectize-dropdown .option.active { background: var(--gold-light) !important; color: var(--navy) !important; }

/* RUN BUTTON */
.run-btn {
  width: 100%; padding: 13px;
  background: var(--navy); color: #fff;
  border: none; border-radius: 6px;
  font-family: 'IBM Plex Sans', sans-serif;
  font-size: 0.9rem; font-weight: 600; letter-spacing: 0.04em;
  cursor: pointer;
  display: flex; align-items: center; justify-content: center; gap: 8px;
  box-shadow: var(--shadow-sm);
  transition: background 0.18s, box-shadow 0.18s;
  margin-top: 6px;
}
.run-btn:hover { background: #243547; box-shadow: var(--shadow-md); }

/* MAIN */
.col-sm-8 { padding: 26px 30px !important; background: var(--bg); }

/* STAT CARDS */
.stat-row { display: flex; gap: 14px; margin-bottom: 22px; }
.stat-card {
  flex: 1; background: var(--bg-card);
  border: 1px solid var(--gold-border); border-radius: 8px;
  padding: 16px 16px 14px; box-shadow: var(--shadow-sm);
}
.stat-label {
  font-size: 0.61rem; font-weight: 600;
  letter-spacing: 0.13em; text-transform: uppercase;
  color: var(--text-muted); margin-bottom: 8px; line-height: 1.3;
}
.stat-value {
  font-family: 'Playfair Display', serif;
  font-size: 1.8rem; font-weight: 700; color: var(--text-dark);
  line-height: 1; margin-bottom: 5px;
}
.stat-sub { font-size: 0.71rem; color: var(--text-muted); line-height: 1.4; }
.stat-sig { color: var(--green); font-size: 0.73rem; font-weight: 500; }
.stat-ns  { color: var(--red);   font-size: 0.73rem; font-weight: 500; }

/* CHART CARD */
.chart-card {
  background: var(--bg-card); border: 1px solid var(--border);
  border-radius: 10px; box-shadow: var(--shadow-sm);
  margin-bottom: 20px; overflow: hidden;
}
.chart-card-header { padding: 15px 20px 12px; border-bottom: 1px solid var(--border); }
.chart-card-title {
  font-family: 'Playfair Display', serif;
  font-size: 1.08rem; font-weight: 700; color: var(--text-dark); margin-bottom: 3px;
}
.chart-card-desc {
  font-size: 0.77rem; color: var(--text-muted);
  font-family: 'Source Serif 4', serif; font-style: italic;
}
.chart-card-body { padding: 16px 16px 12px; }

/* TABLE */
.dataTables_wrapper { font-family: 'IBM Plex Sans', sans-serif !important; font-size: 0.82rem !important; }
table.dataTable { border-collapse: collapse !important; width: 100% !important; }
table.dataTable thead th {
  background: #f5f3ef !important; color: var(--text-muted) !important;
  border-bottom: 2px solid var(--border) !important; border-top: none !important;
  font-size: 0.67rem !important; font-weight: 600 !important;
  letter-spacing: 0.1em !important; text-transform: uppercase !important;
  padding: 9px 14px !important;
}
table.dataTable tbody tr { background: var(--bg-card) !important; }
table.dataTable tbody tr:nth-child(even) { background: #faf9f6 !important; }
table.dataTable tbody tr:hover { background: var(--gold-light) !important; }
table.dataTable tbody td { border-bottom: 1px solid #ede9e2 !important; padding: 8px 14px !important; color: var(--text-body) !important; }
.dataTables_info, .dataTables_filter label, .dataTables_length label { color: var(--text-muted) !important; font-size: 0.74rem !important; }
.dataTables_filter input, .dataTables_length select { border: 1px solid var(--border) !important; border-radius: 4px !important; padding: 3px 8px !important; }
.paginate_button { color: var(--text-muted) !important; border: none !important; background: transparent !important; }
.paginate_button.current { background: var(--gold-light) !important; color: var(--navy) !important; border: 1px solid var(--gold-border) !important; border-radius: 4px !important; }

/* ANOVA */
pre.shiny-text-output {
  background: #f8f6f1 !important; border: 1px solid var(--gold-border) !important;
  border-left: 4px solid var(--gold) !important; border-radius: 6px !important;
  color: #2a2a1e !important; font-family: 'IBM Plex Mono', monospace !important;
  font-size: 0.77rem !important; line-height: 1.85 !important;
  padding: 16px 18px !important; margin: 0 !important;
}

h2 { display: none !important; }
::-webkit-scrollbar { width: 5px; height: 5px; }
::-webkit-scrollbar-track { background: var(--bg); }
::-webkit-scrollbar-thumb { background: var(--border); border-radius: 3px; }
"

# ---- UI ----
ui <- fluidPage(
  tags$head(tags$style(HTML(css))),
  
  tags$div(class = "hero",
           tags$div(class = "hero-title", "RNG Production Analysis Dashboard"),
           tags$div(class = "hero-sub",
                    "Wastewater Biogas System  ·  Organic Inputs & Renewable Natural Gas  ·  Interactive Explorer")
  ),
  
  tags$div(class = "goal-box",
           tags$strong("Author: "), "Xinran Chen (Cornell MPH)", br(),
           
           tags$strong("Data Source: "),
           tags$a(
             href = "https://catalog.data.gov/dataset/wastewater-co-digestion-and-biogas-to-grid-performance-indicators",
             "NYC Wastewater Co-digestion and Biogas-to-Grid Performance Indicators",
             target = "_blank"
           ), br(),
           
           tags$strong("Methods: "),
           "The analysis combined descriptive statistics with inferential methods, including one-way ANOVA and simple linear regression, supported by visualizations such as boxplots, regression scatter plots with fitted linear trends, and correlation heatmaps.", br(),
           
           tags$strong("GitHub Repository: "),
           tags$a(
             href = "https://github.com/xrc577/VTPEH6270-project",
             "View full project code",
             target = "_blank"
           ), br(),
           
           tags$strong("Research Question: "),
           "Does the food scraps input level influence renewable natural gas (RNG) production?",
           br(), 
           tags$strong("Dashboard Goal: "),
           "This app explores the relationship between renewable natural gas (RNG) production and organic inputs, as well as other system-level variables. Use the controls to select a ",
           tags$strong("chart type"),
           ", choose an ",
           tags$strong("outcome variable"),
           ", and adjust the ",
           tags$strong("grouping factor"),
           ". Click ",
           tags$strong("Run Analysis"),
           " to update all visualizations and statistical outputs.", br(),
           
           tags$strong("AI Disclosure: "),
           "This project was developed with assistance from ChatGPT and Claude for coding and design support. All outputs were reviewed and validated by the author."
           
           
  ),
  
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      tags$div(class = "ctrl-title", "Controls"),
      
      tags$div(class = "ctrl-section",
               tags$span(class = "ctrl-label", "Chart Type"),
               radioButtons("chart_type", label = NULL,
                            choices = c(
                              "Box plot \u2013 distribution by group"     = "boxplot",
                              "Regression Plot" = "reg", 
                              "Bar chart \u2013 mean \u00b1 SD by group"  = "bar",
                              "Scatter \u2013 organic input vs. outcome"  = "scatter",
                              "Trend line \u2013 mean across groups"      = "trend",
                              "Heatmap – pattern across time" = "heatmap",
                              "Correlation Heatmap" = "corr"
                            ), selected = "boxplot"
               )
      ),
      
      tags$div(class = "ctrl-section",
               selectInput("yvar",
                           label = tags$span(class = "ctrl-label", "Outcome Variable (Y)"),
                           choices = c(
                             "RNG Produced"        = "rng_produced",
                             "Biogas Flared"       = "biogas_flared",
                             "Total Organic Input" = "total_organic_input",
                             "System Uptime"       = "rng_system_uptime"
                           )
               ),
               
     tags$div(class = "ctrl-section",
              tags$span(class = "ctrl-label", "Plot Type"),
              radioButtons("plot_type", label = NULL,
                            choices = c(
                            "Boxplot" = "box",
                            "Regression Plot" = "reg"
                           ), selected = "box"
                        )
               )
      ),
      
      tags$div(class = "ctrl-section",
               tags$span(class = "ctrl-label", "Grouping Factor"),
               radioButtons("xvar", label = NULL,
                            choices = c(
                              "Input Level (Low / Med / High)" = "input_level",
                              "Year"                           = "year",
                              "Month"                          = "month"
                            ), selected = "input_level"
               )
      ),
      
      tags$div(class = "ctrl-section",
               checkboxInput("show_points", "Overlay individual data points", TRUE)
      ),
      
      tags$button(
        class = "run-btn", id = "run_btn",
        onclick = "Shiny.setInputValue('run', Math.random())",
        "\u25B6  Run Analysis"
      )
    ),
    
    mainPanel(
      uiOutput("stat_cards"),
      
      tags$div(class = "chart-card",
               tags$div(class = "chart-card-header", uiOutput("chart_title")),
               tags$div(class = "chart-card-body", plotOutput("plot", height = "360px"))
      ),
      
      tags$div(class = "chart-card",
               tags$div(class = "chart-card-header",
                        tags$div(class = "chart-card-title", "Descriptive Statistics"),
                        tags$div(class = "chart-card-desc", "Mean, SD, min, max, and N per group.")
               ),
               tags$div(class = "chart-card-body", DTOutput("summary"))
      ),
      
      tags$div(class = "chart-card",
               tags$div(class = "chart-card-header",
                        tags$div(class = "chart-card-title", "One-Way ANOVA"),
                        tags$div(class = "chart-card-desc", "Tests whether group means differ significantly.")
               ),
               tags$div(class = "chart-card-body", verbatimTextOutput("anova"))
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  df_r <- eventReactive(input$run, { data }, ignoreNULL = FALSE)
  
  # Stat cards
  output$stat_cards <- renderUI({
    df <- df_r(); yv <- input$yvar; xv <- input$xvar
    n_obs  <- nrow(df)
    y_mean <- round(mean(df[[yv]], na.rm = TRUE), 1)
    y_sd   <- round(sd(df[[yv]],   na.rm = TRUE), 1)
    pval_txt <- tryCatch({
      if (length(unique(df[[xv]])) >= 2) {
        p <- summary(aov(df[[yv]] ~ as.factor(df[[xv]])))[[1]][1, "Pr(>F)"]
        if (p < 0.001) "< 0.001" else as.character(round(p, 3))
      } else "N/A"
    }, error = function(e) "N/A")
    is_sig     <- grepl("0\\.00|< 0", pval_txt)
    sig_class  <- if (is_sig) "stat-sig" else "stat-ns"
    sig_label  <- if (is_sig) "\u2713 Significant" else "\u2717 Not significant"
    
    tags$div(class = "stat-row",
             tags$div(class = "stat-card",
                      tags$div(class = "stat-label", "Sample Size"),
                      tags$div(class = "stat-value", format(n_obs, big.mark = ",")),
                      tags$div(class = "stat-sub", "total observations")
             ),
             tags$div(class = "stat-card",
                      tags$div(class = "stat-label", var_labels[yv]),
                      tags$div(class = "stat-value", format(y_mean, big.mark = ",")),
                      tags$div(class = "stat-sub", paste0("overall mean  \u00b7  SD = ", y_sd))
             ),
             tags$div(class = "stat-card",
                      tags$div(class = "stat-label", "ANOVA p-value"),
                      tags$div(class = "stat-value", pval_txt),
                      tags$div(class = sig_class, sig_label)
             )
    )
  })
  
  # Chart title
  output$chart_title <- renderUI({
    titles <- c(boxplot="Distribution by Group", bar="Mean \u00b1 SD by Group",
                scatter="Organic Input vs. Outcome", trend="Trend across Ordered Groups")
    descs  <- c(
      boxplot = "Box plots show median, IQR, and outliers per group.",
      bar     = "Bar height = group mean; error bars = \u00b11 SD.",
      scatter = "Each point = one observation. Line = linear fit with 95% CI.",
      trend   = "Connected means show how the outcome changes across groups. Band = 95% CI."
    )
    tags$div(
      tags$div(class = "chart-card-title", titles[input$chart_type]),
      tags$div(class = "chart-card-desc",  descs[input$chart_type])
    )
  })
  
  # Plot
  output$plot <- renderPlot({
    df <- df_r(); yv <- input$yvar; xv <- input$xvar; ct <- input$chart_type
    
    base_t <- theme_classic(base_family = "IBM Plex Sans") +
      theme(
        plot.background    = element_rect(fill="white", color=NA),
        panel.background   = element_rect(fill="white", color=NA),
        panel.border       = element_rect(color="#e2ddd5", fill=NA, linewidth=0.8),
        axis.line          = element_blank(),
        panel.grid.major.y = element_line(color="#ede9e2", linewidth=0.45),
        panel.grid.major.x = element_blank(),
        axis.text          = element_text(color="#3d4f5e", size=10),
        axis.title         = element_text(color="#1b2a3b", size=11, face="bold"),
        axis.ticks         = element_line(color="#e2ddd5"),
        plot.margin        = margin(14,18,14,18)
      )
    
    if (ct == "boxplot") {
      p <- ggplot(df, aes(.data[[xv]], .data[[yv]])) +
        geom_boxplot(fill="#ddeaf5", color="#1b2a3b", outlier.color="#c0392b",
                     outlier.shape=1, outlier.size=2.2, linewidth=0.6, width=0.5) +
        labs(x=var_labels[xv], y=var_labels[yv]) + base_t
      if (input$show_points)
        p <- p + geom_jitter(width=0.15, alpha=0.45, size=1.8, color="#c9973a")
  
    } else if (ct == "reg") {
      
      p <- ggplot(df, aes(x = total_organic_input, y = .data[[yv]])) +
        geom_point(color = "#1b2a3b", size = 2) +
        geom_smooth(method = "lm", se = FALSE, color = "#c0392b", linewidth = 1) +
        labs(
          title = "Regression: Organic Input vs Outcome",
          x = "Total Organic Input",
          y = var_labels[yv]
        ) +
        base_t
      
    } else if (ct == "bar") {
      s <- df %>% group_by(.data[[xv]]) %>%
        summarise(m=mean(.data[[yv]], na.rm=T), s=sd(.data[[yv]], na.rm=T), .groups="drop")
      p <- ggplot(s, aes(.data[[xv]], m)) +
        geom_col(fill="#1b2a3b", alpha=0.82, width=0.55) +
        geom_errorbar(aes(ymin=m-s, ymax=m+s), width=0.22, color="#c9973a", linewidth=0.9) +
        labs(x=var_labels[xv], y=paste0("Mean ", var_labels[yv])) + base_t
      
    } else if (ct == "scatter") {
      p <- ggplot(df, aes(total_organic_input, .data[[yv]])) +
        geom_point(alpha=0.4, size=2, color="#c9973a") +
        geom_smooth(method="lm", color="#1b2a3b", fill="#d4e8f0", alpha=0.25, linewidth=1) +
        labs(x="Total Organic Input", y=var_labels[yv]) + base_t
      
    } else if (ct == "heatmap") {
      heat_df <- df %>%
        group_by(year, month) %>%
        summarise(value = mean(.data[[yv]], na.rm = TRUE), .groups = "drop")
      
      p <- ggplot(heat_df, aes(month, year, fill = value)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "#f5e9d3", high = "#c9973a") +
        labs(x = "Month", y = "Year") +
        base_t +
        theme(panel.grid = element_blank())
    } else if (ct == "corr") {
      
      corr_df <- df %>%
        select(rng_produced, biogas_flared, total_organic_input, rng_system_uptime)
      
      corr_mat <- cor(corr_df, use = "complete.obs")
      
      corr_long <- as.data.frame(as.table(corr_mat))
      
      p <- ggplot(corr_long, aes(Var1, Var2, fill = Freq)) +
        geom_tile() +
        geom_text(aes(label = round(Freq, 2)), size = 4) +
        scale_fill_gradient2(low = "#c0392b", mid = "white", high = "#2d7d52") +
        base_t +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else {
      s <- df %>% mutate(xn=as.numeric(.data[[xv]])) %>%
        group_by(.data[[xv]], xn) %>%
        summarise(m=mean(.data[[yv]], na.rm=T),
                  se=sd(.data[[yv]], na.rm=T)/sqrt(n()), .groups="drop") %>% arrange(xn)
      p <- ggplot(s, aes(.data[[xv]], m, group=1)) +
        geom_ribbon(aes(ymin=m-1.96*se, ymax=m+1.96*se), fill="#c9973a", alpha=0.15) +
        geom_line(color="#1b2a3b", linewidth=1.2) +
        geom_point(size=5, color="#c9973a") + geom_point(size=2.5, color="white") +
        geom_text(aes(label=round(m,1)), vjust=-1.4, size=3.3,
                  family="IBM Plex Sans", fontface="bold", color="#1b2a3b") +
        labs(x=var_labels[xv], y=paste0("Mean ", var_labels[yv]),
             caption="Shaded band = 95% confidence interval") +
        base_t + theme(plot.caption=element_text(color="#7a8a96", size=8, hjust=0.5))
    }
    p
  }, bg="white")
  
  # Summary table
  output$summary <- renderDT({
    df <- df_r()
    df %>% group_by(.data[[input$xvar]]) %>%
      summarise(Mean=round(mean(.data[[input$yvar]], na.rm=T),1),
                SD  =round(sd(.data[[input$yvar]],   na.rm=T),1),
                Min =round(min(.data[[input$yvar]],   na.rm=T),1),
                Max =round(max(.data[[input$yvar]],   na.rm=T),1),
                N=n(), .groups="drop") %>%
      datatable(rownames=FALSE, options=list(pageLength=8, dom="tip"))
  })
  
  # ANOVA
  output$anova <- renderPrint({
    df <- df_r(); xv <- input$xvar; yv <- input$yvar
    if (length(unique(df[[xv]])) < 2) { cat("Not enough groups.\n"); return() }
    res <- summary(aov(df[[yv]] ~ as.factor(df[[xv]])))
    print(res)
    p <- res[[1]][1, "Pr(>F)"]
    cat("\n", strrep("-", 50), "\n", sep="")
    cat("Interpretation\n\n")
    if (p < 0.001)     cat("  Highly significant group differences  (p < 0.001)\n")
    else if (p < 0.05) cat("  Significant group differences  (p =", round(p,4), ")\n")
    else               cat("  No statistically significant differences in RNG production were observed across input levels (p = 0.139), suggesting that input level alone may not strongly influence output.  (p =", round(p,4), ")\n")
  })
}

shinyApp(ui = ui, server = server)