library(shiny)
library(finalprojectgroup12)
library(ggplot2)
library(DT)

# UI Interface
ui <- fluidPage(
  titlePanel("Multi-Path Stepwise Selection Explorer"),
  tags$head(
    tags$style(HTML("
      .btn-primary {
        background-color: #2c3e50;
        border-color: #2c3e50;
      }
      .btn-primary:hover {
        background-color: #1a252f;
        border-color: #1a252f;
      }
      .shiny-output-error { color: #e74c3c; }
      .shiny-output-error:before { content: '⚠️ '; }
      .well { margin-bottom: 15px; }
    "))
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # Data Settings
      wellPanel(
        h4("📊 Data Settings", style = "color: #2c3e50;"),
        selectInput("data_type", "Data Source:",
                    choices = c("Simulated Linear" = "linear_sim",
                                "Simulated Logistic" = "logistic_sim",
                                "mtcars (Linear)" = "mtcars",
                                "Upload CSV" = "upload"),
                    selected = "linear_sim"),

        # File upload UI (only shown when "Upload CSV" is selected)
        conditionalPanel(
          condition = "input.data_type == 'upload'",
          hr(),
          h5("File Upload", style = "color: #2c3e50;"),
          fileInput("file_upload", "Choose CSV File",
                    accept = c(".csv", ".txt"),
                    placeholder = "Select a CSV file"),

          # File options
          checkboxInput("header", "Header", TRUE),
          radioButtons("sep", "Separator",
                       choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                       selected = ","),
          radioButtons("quote", "Quote",
                       choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                       selected = '"'),

          # Variable selection
          uiOutput("response_var_ui"),
          uiOutput("predictor_vars_ui"),

          # Data preview button
          actionButton("preview_data", "Preview Data",
                       class = "btn-default btn-sm",
                       style = "width: 100%; margin-top: 10px;")
        ),

        helpText("Choose data for analysis")
      ),

      # Model Parameters
      wellPanel(
        h4("⚙️ Model Parameters", style = "color: #2c3e50;"),
        selectInput("family", "Model Family:",
                    choices = c("Linear Regression" = "gaussian",
                                "Logistic Regression" = "binomial"),
                    selected = "gaussian"),

        sliderInput("K", "Max Steps (K):",
                    min = 1, max = 20, value = 5, step = 1,
                    ticks = TRUE),
        helpText("Maximum number of forward selection steps"),

        sliderInput("delta", "Delta (AIC tolerance):",
                    min = 0, max = 5, value = 1, step = 0.1,
                    ticks = TRUE),
        helpText("Keep models within delta AIC units of the best"),

        numericInput("L", "Max Models per Level (L):",
                     value = 30, min = 10, max = 100, step = 5),
        helpText("Limit models kept at each step")
      ),

      # Stability Parameters
      wellPanel(
        h4("📈 Stability Parameters", style = "color: #2c3e50;"),
        sliderInput("B", "Number of Resamples (B):",
                    min = 10, max = 200, value = 30, step = 5,
                    ticks = TRUE),
        helpText("Bootstrap resamples for stability estimation")
      ),

      # Plausible Model Parameters
      wellPanel(
        h4("Plausible Model Filters", style = "color: #2c3e50;"),
        sliderInput("Delta_thresh", "AIC Window (Δ):",
                    min = 0, max = 10, value = 2, step = 0.5,
                    ticks = TRUE),
        helpText("Keep models within Δ AIC units of the best"),

        sliderInput("tau", "Min Average Stability (τ):",
                    min = 0, max = 1, value = 0.4, step = 0.05,
                    ticks = TRUE),
        helpText("Minimum average variable stability required")
      ),

      # Run Button
      actionButton("run", "Run Analysis",
                   class = "btn-primary btn-lg",
                   style = "width: 100%; margin-top: 20px;"),

      br(), br(),

      # GitHub Link
      wellPanel(
        h5("Package Info"),
        tags$a(href = "https://github.com/R-4-Data-Science/finalprojectgroup12",
               "GitHub Repository", target = "_blank",
               class = "btn btn-default btn-sm",
               style = "width: 100%;"),
        br(), br(),
        tags$code("finalprojectgroup12 v0.0.0.9000")
      )
    ),

    # Main Panel
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        type = "tabs",

        # Tab 1: Overview
        tabPanel("Overview",
                 h3("Multi-Path Stepwise Selection Package"),
                 p("This interactive application demonstrates the finalprojectgroup12 R package,
                   which implements a multi-path forward selection framework for model building,
                   stability estimation, and selection of plausible models based on AIC."),

                 hr(),

                 h4("Workflow"),
                 tags$ol(
                   tags$li(tags$b("Build Multiple Paths:"),
                           "Start from empty model, explore several near-best paths simultaneously"),
                   tags$li(tags$b("Estimate Stability:"),
                           "Use resampling to see which variables appear consistently"),
                   tags$li(tags$b("Select Plausible Models:"),
                           "Keep models with both good AIC and stable variables")
                 ),

                 br(),

                 h4("Key Features"),
                 fluidRow(
                   column(6,
                          tags$ul(
                            tags$li("Supports linear & logistic regression"),
                            tags$li("Explores multiple model paths (not just one)"),
                            tags$li("AIC-based selection with configurable thresholds"),
                            tags$li("Upload and analyze your own CSV data")
                          )
                   ),
                   column(6,
                          tags$ul(
                            tags$li("Bootstrap stability estimation"),
                            tags$li("Interactive parameter tuning"),
                            tags$li("Visualization of results"),
                            tags$li("Data preview and variable selection")
                          )
                   )
                 ),

                 hr(),

                 h4("Getting Started"),
                 tags$ol(
                   tags$li("Select your data source (simulated, mtcars, or upload your own CSV)"),
                   tags$li("If uploading CSV, select response and predictor variables"),
                   tags$li("Adjust parameters using the sliders on the left"),
                   tags$li("Click 'Run Analysis' to start"),
                   tags$li("Explore results in the different tabs")
                 ),

                 div(class = "alert alert-info",
                     tags$b("Tip:"),
                     "Start with default parameters to see how it works, then experiment!"),

                 conditionalPanel(
                   condition = "input.data_type == 'upload'",
                   div(class = "alert alert-warning",
                       tags$b("Note for CSV upload:"),
                       tags$ul(
                         tags$li("CSV files should have headers (optional)"),
                         tags$li("Response variable should be numeric for linear regression"),
                         tags$li("For logistic regression, response should be binary (0/1)"),
                         tags$li("Missing values are not supported in the analysis")
                       )
                   )
                 )
        ),

        # Tab 2: Data Preview (new tab for uploaded data)
        tabPanel("Data Preview",
                 h3("Data Preview"),
                 conditionalPanel(
                   condition = "input.data_type != 'upload'",
                   p("This tab shows uploaded data. Select 'Upload CSV' in Data Settings to use your own data.")
                 ),
                 conditionalPanel(
                   condition = "input.data_type == 'upload'",
                   fluidRow(
                     column(12,
                            h4("Uploaded Data Summary"),
                            verbatimTextOutput("data_summary"),
                            br(),
                            h4("Data Preview"),
                            DTOutput("data_preview_table"),
                            br(),
                            h4("Variable Information"),
                            DTOutput("variable_info")
                     )
                   )
                 )
        ),

        # Tab 3: Top Models
        tabPanel("Top Models",
                 h3("Top Models by AIC"),
                 p("Models ranked by Akaike Information Criterion (lower is better)."),

                 br(),

                 fluidRow(
                   column(8,
                          DTOutput("top_models_table")
                   ),
                   column(4,
                          wellPanel(
                            h5("Model Statistics"),
                            verbatimTextOutput("model_stats")
                          )
                   )
                 ),

                 br(),

                 h4("AIC vs Model Size"),
                 plotOutput("aic_plot", height = "400px"),

                 br(),

                 h4("Best Model Formula"),
                 verbatimTextOutput("best_model_formula")
        ),

        # Tab 4: Variable Stability
        tabPanel("Variable Stability",
                 h3("Variable Stability Analysis"),
                 p("Proportion of models containing each variable across bootstrap resamples."),

                 br(),

                 fluidRow(
                   column(6,
                          h4("Stability Scores"),
                          DTOutput("stability_table")
                   ),
                   column(6,
                          h4("Stability Distribution"),
                          plotOutput("stability_hist", height = "300px")
                   )
                 ),

                 br(),

                 h4("Top Variable Stability"),
                 plotOutput("stability_barplot", height = "400px")
        ),

        # Tab 5: Plausible Models
        tabPanel("Plausible Models",
                 h3("Plausible Model Selection"),
                 p("Models that are both:",
                   tags$ul(
                     tags$li("Within Δ AIC units of the best model"),
                     tags$li("Have average variable stability ≥ τ")
                   )),

                 br(),

                 fluidRow(
                   column(8,
                          DTOutput("plausible_table")
                   ),
                   column(4,
                          wellPanel(
                            h5("Selection Summary"),
                            verbatimTextOutput("plausible_stats")
                          )
                   )
                 ),

                 br(),

                 h4("Variable Frequency in Plausible Models"),
                 plotOutput("var_freq_plot", height = "400px")
        ),

        # Tab 6: Visualizations
        tabPanel("Visualizations",
                 h3("Model Comparison Visualizations"),

                 br(),

                 fluidRow(
                   column(6,
                          h4("Model Size vs AIC"),
                          plotOutput("size_aic_plot", height = "350px")
                   ),
                   column(6,
                          h4("Stability vs AIC Improvement"),
                          plotOutput("stability_improvement_plot", height = "350px")
                   )
                 ),

                 br(),

                 h4("Variable Correlation with Stability"),
                 plotOutput("var_correlation_plot", height = "400px")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {

  # Reactive value to store uploaded data
  uploaded_data <- reactiveVal(NULL)

  # Observe file upload
  observe({
    req(input$file_upload)

    tryCatch({
      df <- read.csv(
        input$file_upload$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )

      # Clean column names
      colnames(df) <- make.names(colnames(df))
      uploaded_data(df)

      showNotification("File uploaded successfully!", type = "success")

    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message),
                       type = "error", duration = 10)
      uploaded_data(NULL)
    })
  })

  # UI for response variable selection
  output$response_var_ui <- renderUI({
    req(input$data_type == "upload")
    req(uploaded_data())

    selectInput("response_var", "Response Variable:",
                choices = colnames(uploaded_data()),
                selected = colnames(uploaded_data())[1])
  })

  # UI for predictor variable selection
  output$predictor_vars_ui <- renderUI({
    req(input$data_type == "upload")
    req(uploaded_data())
    req(input$response_var)

    # All variables except response
    pred_choices <- setdiff(colnames(uploaded_data()), input$response_var)

    selectInput("predictor_vars", "Predictor Variables:",
                choices = pred_choices,
                selected = pred_choices,
                multiple = TRUE,
                selectize = TRUE)
  })

  # Update family based on response variable type for uploaded data
  observe({
    if (input$data_type == "upload" && !is.null(input$response_var)) {
      df <- uploaded_data()
      if (!is.null(df)) {
        response_vals <- df[[input$response_var]]

        # Check if response is binary for logistic regression
        if (is.numeric(response_vals) &&
            all(response_vals %in% c(0, 1, NA))) {
          # Binary response - suggest logistic
          updateSelectInput(session, "family",
                            selected = "binomial")
        } else if (is.numeric(response_vals)) {
          # Continuous response - suggest linear
          updateSelectInput(session, "family",
                            selected = "gaussian")
        }
      }
    } else if (input$data_type == "linear_sim") {
      updateSelectInput(session, "family", selected = "gaussian")
    } else if (input$data_type == "logistic_sim") {
      updateSelectInput(session, "family", selected = "binomial")
    }
  })

  # Data preview
  observeEvent(input$preview_data, {
    req(uploaded_data())

    # Switch to Data Preview tab
    updateTabsetPanel(session, "main_tabs", selected = "Data Preview")
  })

  # Data summary output
  output$data_summary <- renderPrint({
    req(input$data_type == "upload")
    req(uploaded_data())

    df <- uploaded_data()
    cat("Dataset Summary:\n")
    cat("────────────────\n")
    cat("Dimensions:", nrow(df), "rows ×", ncol(df), "columns\n")
    cat("Selected response variable:", input$response_var %||% "Not selected", "\n")
    cat("Selected predictor variables:",
        length(input$predictor_vars %||% character(0)), "variables\n")
    cat("\nColumn types:\n")
    print(sapply(df, class))
  })

  # Data preview table
  output$data_preview_table <- renderDT({
    req(input$data_type == "upload")
    req(uploaded_data())

    datatable(
      uploaded_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    )
  })

  # Variable information table
  output$variable_info <- renderDT({
    req(input$data_type == "upload")
    req(uploaded_data())

    df <- uploaded_data()

    var_info <- data.frame(
      Variable = colnames(df),
      Type = sapply(df, class),
      Missing = sapply(df, function(x) sum(is.na(x))),
      Unique = sapply(df, function(x) length(unique(x))),
      Mean = sapply(df, function(x) if(is.numeric(x)) mean(x, na.rm=TRUE) else NA),
      SD = sapply(df, function(x) if(is.numeric(x)) sd(x, na.rm=TRUE) else NA),
      Min = sapply(df, function(x) if(is.numeric(x)) min(x, na.rm=TRUE) else NA),
      Max = sapply(df, function(x) if(is.numeric(x)) max(x, na.rm=TRUE) else NA)
    )

    datatable(
      var_info,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })

  # Generate data
  data <- reactive({
    set.seed(123)  # For reproducibility

    if (input$data_type == "linear_sim") {
      n <- 100
      p <- 8
      X <- data.frame(matrix(rnorm(n*p), n, p))
      colnames(X) <- paste0("X", 1:p)
      # True model: y = 2 + 0.5*X1 + 0.3*X2 + noise
      y <- 2 + 0.5*X$X1 + 0.3*X$X2 + rnorm(n)
      list(X = X, y = y, type = "Linear Simulation")

    } else if (input$data_type == "logistic_sim") {
      n <- 100
      p <- 8
      X <- data.frame(matrix(rnorm(n*p), n, p))
      colnames(X) <- paste0("X", 1:p)
      # True model: logit(p) = 0.5*X1 - 0.3*X3
      logit <- 0.5*X$X1 - 0.3*X$X3
      p <- 1 / (1 + exp(-logit))
      y <- rbinom(n, 1, p)
      list(X = X, y = y, type = "Logistic Simulation")

    } else if (input$data_type == "mtcars") {
      X <- mtcars[, -1]  # Remove mpg column
      y <- mtcars$mpg
      list(X = X, y = y, type = "mtcars (Linear)")

    } else if (input$data_type == "upload") {
      req(uploaded_data())
      req(input$response_var)
      req(input$predictor_vars)

      df <- uploaded_data()

      # Extract response and predictors
      y <- df[[input$response_var]]
      X <- df[, input$predictor_vars, drop = FALSE]

      # Remove rows with missing values
      complete_cases <- complete.cases(X, y)
      y <- y[complete_cases]
      X <- X[complete_cases, , drop = FALSE]

      if (length(y) == 0 || ncol(X) == 0) {
        showNotification("No complete cases after removing missing values!",
                         type = "error", duration = 10)
        return(NULL)
      }

      list(
        X = X,
        y = y,
        type = paste("Uploaded Data:", input$file_upload$name),
        response_var = input$response_var
      )
    }
  })

  # Rest of your existing server code remains the same...
  # Run analysis
  results <- eventReactive(input$run, {
    dat <- data()
    if (is.null(dat)) return(NULL)

    withProgress(message = "Running multi-path analysis...", value = 0, {
      # Step 1: Multi-path selection
      incProgress(0.3, detail = "Building model paths...")
      forest <- tryCatch({
        build_paths(
          x = dat$X,
          y = dat$y,
          family = input$family,
          K = input$K,
          eps = 1e-6,
          delta = input$delta,
          L = input$L,
          keep_fits = FALSE,
          trace = FALSE
        )
      }, error = function(e) {
        showNotification(paste("Error in build_paths:", e$message),
                         type = "error", duration = 10)
        return(NULL)
      })

      if (is.null(forest)) return(NULL)

      # Step 2: Stability estimation
      incProgress(0.4, detail = "Estimating stability...")
      stable <- tryCatch({
        stability(
          x = dat$X,
          y = dat$y,
          B = input$B,
          resample = "bootstrap",
          build_args = list(
            family = input$family,
            K = input$K,
            eps = 1e-6,
            delta = input$delta,
            L = input$L,
            keep_fits = FALSE
          ),
          verbose = FALSE
        )
      }, error = function(e) {
        showNotification(paste("Error in stability:", e$message),
                         type = "error", duration = 10)
        return(NULL)
      })

      if (is.null(stable)) return(NULL)

      # Step 3: Select plausible models
      incProgress(0.2, detail = "Selecting plausible models...")
      plausible <- tryCatch({
        plausible_models(
          forest = forest,
          pi = stable$pi,
          Delta = input$Delta_thresh,
          tau = input$tau
        )
      }, error = function(e) {
        showNotification(paste("Error in plausible_models:", e$message),
                         type = "error", duration = 10)
        return(data.frame())
      })

      incProgress(0.1, detail = "Preparing results...")

      list(
        forest = forest,
        stable = stable,
        plausible = plausible,
        data_info = dat$type,
        n_models = forest$meta$n_models,
        best_aic = forest$meta$aic_min
      )
    })
  })

  # Model statistics
  output$model_stats <- renderPrint({
    res <- results()
    if (is.null(res)) return("No results yet")

    cat("Data:", res$data_info, "\n")
    cat("Family:", res$forest$meta$params$family, "\n")
    cat("Total models:", res$n_models, "\n")
    cat("Best AIC:", round(res$best_aic, 2), "\n")
    cat("Parameters used:\n")
    cat("  K =", res$forest$meta$params$K, "\n")
    cat("  delta =", res$forest$meta$params$delta, "\n")
    cat("  L =", res$forest$meta$params$L, "\n")
  })

  # Top models table
  output$top_models_table <- renderDT({
    res <- results()
    if (is.null(res)) return(data.frame())

    df <- res$forest$aic_by_model
    if (nrow(df) == 0) return(data.frame(Message = "No models found"))

    display_df <- data.frame(
      Rank = 1:nrow(df),
      AIC = round(df$aic, 2),
      Size = df$size,
      Variables = sapply(df$vars, function(v) {
        if (length(v) == 0) return("(Intercept only)")
        paste(v, collapse = ", ")
      }),
      Formula = sapply(df$vars, function(v) {
        if (length(v) == 0) return("y ~ 1")
        paste("y ~", paste(v, collapse = " + "))
      })
    )

    datatable(
      head(display_df, 15),
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 15),
        dom = 'Bfrtip',
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    )
  })

  # AIC plot
  output$aic_plot <- renderPlot({
    res <- results()
    if (is.null(res)) return(NULL)

    df <- res$forest$aic_by_model

    ggplot(df, aes(x = size, y = aic)) +
      geom_point(color = "steelblue", size = 3, alpha = 0.6) +
      geom_smooth(method = "loess", color = "darkred", se = TRUE, alpha = 0.2) +
      geom_point(data = df[which.min(df$aic), ],
                 aes(x = size, y = aic),
                 color = "red", size = 5, shape = 17) +
      labs(
        title = "AIC vs Model Size",
        subtitle = "Red triangle indicates best model (lowest AIC)",
        x = "Number of Predictors",
        y = "AIC"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray50")
      )
  })

  # Best model formula
  output$best_model_formula <- renderPrint({
    res <- results()
    if (is.null(res)) return("No results yet")

    df <- res$forest$aic_by_model
    best <- df[which.min(df$aic), ]

    if (length(best$vars[[1]]) == 0) {
      cat("Best model: y ~ 1 (Intercept only)\n")
      cat("AIC:", round(best$aic, 2), "\n")
    } else {
      cat("Best model formula:\n")
      cat("y ~", paste(best$vars[[1]], collapse = " + "), "\n")
      cat("\nAIC:", round(best$aic, 2), "\n")
      cat("Number of predictors:", best$size, "\n")
    }
  })

  # Stability table
  output$stability_table <- renderDT({
    res <- results()
    if (is.null(res)) return(data.frame())

    pi_sorted <- sort(res$stable$pi, decreasing = TRUE)

    df <- data.frame(
      Variable = names(pi_sorted),
      Stability = round(pi_sorted, 3),
      Category = cut(pi_sorted,
                     breaks = c(0, 0.3, 0.7, 1),
                     labels = c("Low", "Medium", "High"),
                     include.lowest = TRUE)
    )

    datatable(
      df,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip'
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Stability',
        background = styleColorBar(df$Stability, 'lightblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Category',
        backgroundColor = styleEqual(
          c("Low", "Medium", "High"),
          c("#FFCCCB", "#FFFACD", "#90EE90")
        )
      )
  })

  # Stability histogram
  output$stability_hist <- renderPlot({
    res <- results()
    if (is.null(res)) return(NULL)

    pi_sorted <- sort(res$stable$pi, decreasing = TRUE)

    ggplot(data.frame(Stability = pi_sorted), aes(x = Stability)) +
      geom_histogram(fill = "steelblue", color = "white", bins = 20, alpha = 0.7) +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
      labs(
        title = "Distribution of Stability Scores",
        x = "Stability (π)",
        y = "Frequency"
      ) +
      theme_minimal()
  })

  # Stability bar plot
  output$stability_barplot <- renderPlot({
    res <- results()
    if (is.null(res)) return(NULL)

    pi_sorted <- sort(res$stable$pi, decreasing = TRUE)
    n_show <- min(15, length(pi_sorted))

    df <- data.frame(
      Variable = factor(names(pi_sorted)[1:n_show],
                        levels = rev(names(pi_sorted)[1:n_show])),
      Stability = pi_sorted[1:n_show]
    )

    ggplot(df, aes(x = Stability, y = Variable, fill = Stability)) +
      geom_col(width = 0.7) +
      scale_fill_gradient(low = "#a6d4fa", high = "#0055aa", limits = c(0, 1)) +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.7) +
      labs(
        title = "Top 15 Most Stable Variables",
        subtitle = "Red line indicates 0.5 stability threshold",
        x = "Stability Score (π)",
        y = ""
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 11),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray50")
      ) +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))
  })

  # Plausible models table
  output$plausible_table <- renderDT({
    res <- results()
    if (is.null(res)) return(data.frame())

    plaus <- res$plausible

    if (nrow(plaus) == 0) {
      return(datatable(
        data.frame(Message = "No plausible models found with current thresholds (Δ and τ)"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }

    display_df <- data.frame(
      AIC = round(plaus$aic, 2),
      Size = plaus$size,
      Avg_Stability = round(plaus$avg_stability, 3),
      Variables = sapply(plaus$vars, function(v) {
        paste(v, collapse = ", ")
      })
    )

    datatable(
      display_df,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })

  # Plausible model statistics
  output$plausible_stats <- renderPrint({
    res <- results()
    if (is.null(res)) return("No results yet")

    plaus <- res$plausible

    cat("Plausible Model Selection:\n")
    cat("─────────────────────────\n")
    cat("Current thresholds:\n")
    cat("  Δ =", input$Delta_thresh, "(AIC window)\n")
    cat("  τ =", input$tau, "(min stability)\n\n")
    cat("Results:\n")
    cat("  Plausible models found:", nrow(plaus), "\n")

    if (nrow(plaus) > 0) {
      cat("  AIC range:", round(min(plaus$aic), 2), "-", round(max(plaus$aic), 2), "\n")
      cat("  Size range:", min(plaus$size), "-", max(plaus$size), "predictors\n")
      cat("  Avg stability range:", round(min(plaus$avg_stability), 3), "-",
          round(max(plaus$avg_stability), 3), "\n")
    }
  })

  # Variable frequency plot
  output$var_freq_plot <- renderPlot({
    res <- results()
    if (is.null(res)) return(NULL)

    plaus <- res$plausible
    if (nrow(plaus) == 0) return(NULL)

    all_vars <- table(unlist(plaus$vars))
    if (length(all_vars) == 0) return(NULL)

    df <- data.frame(
      Variable = names(all_vars),
      Frequency = as.numeric(all_vars)
    )
    df <- df[order(df$Frequency, decreasing = TRUE), ]
    df <- head(df, 10)

    ggplot(df, aes(x = reorder(Variable, Frequency), y = Frequency)) +
      geom_col(fill = "#ff6b6b", alpha = 0.8) +
      coord_flip() +
      geom_text(aes(label = Frequency), hjust = -0.2, size = 4) +
      labs(
        title = "Most Frequent Variables in Plausible Models",
        subtitle = paste("Total plausible models:", nrow(plaus)),
        x = "",
        y = "Frequency"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray50"),
        axis.text.y = element_text(size = 11)
      ) +
      ylim(0, max(df$Frequency) * 1.1)
  })

  # Model size vs AIC plot
  output$size_aic_plot <- renderPlot({
    res <- results()
    if (is.null(res)) return(NULL)

    df <- res$forest$aic_by_model
    plaus <- res$plausible

    p <- ggplot(df, aes(x = size, y = aic)) +
      geom_point(aes(color = "All Models"), alpha = 0.4, size = 2) +
      labs(
        title = "Model Comparison: Size vs AIC",
        x = "Model Size (Number of Predictors)",
        y = "AIC",
        color = "Model Type"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom"
      )

    if (nrow(plaus) > 0) {
      p <- p +
        geom_point(data = plaus,
                   aes(x = size, y = aic, color = "Plausible Models"),
                   size = 3, alpha = 0.8) +
        scale_color_manual(
          values = c("All Models" = "gray70", "Plausible Models" = "#2ecc71"),
          breaks = c("Plausible Models", "All Models")
        )
    } else {
      p <- p + scale_color_manual(values = c("All Models" = "gray70"))
    }

    p
  })

  # Stability vs AIC improvement plot
  output$stability_improvement_plot <- renderPlot({
    res <- results()
    if (is.null(res)) return(NULL)

    forest <- res$forest
    stable <- res$stable
    plaus <- res$plausible

    if (nrow(forest$aic_by_model) < 2) return(NULL)

    # Calculate average stability for each model
    avg_stab <- sapply(forest$aic_by_model$vars, function(v) {
      if (length(v) == 0) return(0)
      mean(stable$pi[v], na.rm = TRUE)
    })

    # Calculate AIC improvement relative to best model
    best_aic <- min(forest$aic_by_model$aic)
    aic_diff <- forest$aic_by_model$aic - best_aic

    df <- data.frame(
      Avg_Stability = avg_stab,
      AIC_Difference = aic_diff,
      Size = forest$aic_by_model$size,
      Is_Plausible = forest$aic_by_model$key %in% plaus$key
    )

    ggplot(df, aes(x = Avg_Stability, y = AIC_Difference,
                   size = Size, color = Is_Plausible)) +
      geom_point(alpha = 0.6) +
      scale_color_manual(
        values = c("TRUE" = "#2ecc71", "FALSE" = "#3498db"),
        labels = c("TRUE" = "Plausible", "FALSE" = "Other"),
        name = "Model Type"
      ) +
      scale_size_continuous(range = c(2, 6), name = "Model Size") +
      labs(
        title = "Stability vs AIC Improvement",
        subtitle = "Distance from best AIC vs average variable stability",
        x = "Average Variable Stability",
        y = "AIC Difference from Best"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray50"),
        legend.position = "right"
      )
  })

  # Variable correlation plot
  output$var_correlation_plot <- renderPlot({
    res <- results()
    if (is.null(res)) return(NULL)

    forest <- res$forest
    stable <- res$stable

    if (nrow(forest$aic_by_model) < 5) return(NULL)

    # Extract variable frequency and stability
    all_models <- forest$aic_by_model$vars
    var_freq <- table(unlist(all_models))

    df <- data.frame(
      Variable = names(var_freq),
      Frequency = as.numeric(var_freq),
      Stability = stable$pi[names(var_freq)]
    )

    if (nrow(df) < 2) return(NULL)

    ggplot(df, aes(x = Frequency, y = Stability)) +
      geom_point(size = 4, color = "#9b59b6", alpha = 0.7) +
      geom_smooth(method = "lm", color = "#e74c3c", se = TRUE, alpha = 0.2) +
      geom_text(aes(label = Variable), vjust = -0.5, size = 3.5, check_overlap = TRUE) +
      labs(
        title = "Variable Frequency vs Stability",
        subtitle = "Do frequently selected variables tend to be more stable?",
        x = "Frequency (number of models containing variable)",
        y = "Stability Score (π)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray50")
      )
  })

  # 添加一个额外的观察器来检查上传数据时的错误
  observe({
    if (input$data_type == "upload") {
      req(uploaded_data())
      req(input$response_var)

      df <- uploaded_data()
      y <- df[[input$response_var]]

      # 检查响应变量类型
      if (input$family == "binomial" && !all(y %in% c(0, 1, NA))) {
        showNotification(
          "Warning: For logistic regression, response variable should be binary (0/1).",
          type = "warning", duration = 8
        )
      }

      if (input$family == "gaussian" && !is.numeric(y)) {
        showNotification(
          "Warning: For linear regression, response variable should be numeric.",
          type = "warning", duration = 8
        )
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
