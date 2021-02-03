library(shiny)
library(readr)
library(here)
library(splitstackshape)
library(DT)
library(plotly)

df <- read_csv(file.path(here::here(), 'lab_4', 'data', 'bankruptcy_data.csv'))
names(df) <- make.names(names(df), unique = TRUE)
sampled_df <- splitstackshape::stratified(df, group = c('Bankrupt.'), size = 0.1)

ui <- fluidPage(
  titlePanel("Lab 4: Shiny, Logistic Regression, and Bankruptcy"),
  sidebarLayout(
    sidebarPanel(
      tags$b("Data:"),
      textInput("x", label = "Operating Profit Rate", 
                value = paste(sampled_df$operating.profit.rate, collapse = ', '), 
                placeholder = "Enter values separated by a comma with decimals as points, e.g. 0.9, 1.12, 13, etc."
                ),
      textInput("y", label = "Bankruptcy",
                value = paste(sampled_df$Bankrupt., collapse = ', '),
                placeholder = "Enter binary values separated by a comma, e.g. 0, 1, 1, etc."
                ),
    ),
    mainPanel = mainPanel(
      tags$b("Data:"),
      DT::dataTableOutput("df_tbl"),
      br(),
      uiOutput("data"),
      br(),
      tags$b("Compute parameters in R:"),
      verbatimTextOutput("summary"),
      br(),
      tags$b("Regression plot:"),
      uiOutput("results"),
      plotlyOutput("plot"),
      br(),
      tags$b("Interpretation:"),
      uiOutput("interpretation"),
      br(),
      br()
    )
  )
)

server <- function(input, output, session){
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  
  # Data output
  output$df <- DT::renderDataTable({
    y <- extract(input$y)
    x <- extract(input$x)
    DT::datatable(data.frame(x, y),
                  extensions = "Buttons",
                  options = list(
                    lengthChange = FALSE,
                    dom = "Blfrtip",
                  )
    )
  })
  
  output$data <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    if (anyNA(x) | length(x) < 2 | anyNA(y) | length(y) < 2) {
      "Invalid input or not enough observations"
    } else if (length(x) != length(y)) {
      "Number of observations must be equal for x and y"
    } else {
      withMathJax(
        paste0("\\(\\bar{x} =\\) ", round(mean(x), 3)),
        br(),
        paste0("\\(\\bar{y} =\\) ", round(mean(y), 3)),
        br(),
        paste0("\\(n =\\) ", length(x))
      )
    }
  })
  
  output$summary <- renderPrint({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- glm(y ~ x, family = "binomial")
    summary(fit)
  })
  
  output$results <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- glm(y ~ x, family = "binomial")
    nullmod <- glm(y~1, family="binomial")
    mfR2 <- 1-logLik(fit)/logLik(nullmod)
    withMathJax(
      paste0(
        "Adj. McFadden's \\( R^2 = \\) ", round(mfR2, 3),
        ", \\( \\beta_0 = \\) ", round(fit$coefficients[[1]], 3),
        ", \\( \\beta_1 = \\) ", round(fit$coefficients[[2]], 3),
        ", P-value ", "\\( = \\) ", signif(summary(fit)$coef[2, 4], 3)
      )
    )
  })
  
  output$interpretation <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
      withMathJax(
        paste0("For a (hypothetical) value of ", input$xlab, " = 0, the mean of ", input$ylab, " = ", round(fit$coef[[1]], 3), "."),
        br(),
        paste0("For an increase of one unit of ", input$xlab, ", ", input$ylab, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
      )
    } else if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] >= 0.05) {
      withMathJax(
        paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
        br(),
        paste0("For a (hypothetical) value of ", input$xlab, " = 0, the mean of ", input$ylab, " = ", round(fit$coef[[1]], 3), "."),
        br(),
        paste0("\\( \\beta_1 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[2, 4], 3), ") so there is no significant relationship between ", input$xlab, " and ", input$ylab, ".")
      )
    } else if (summary(fit)$coefficients[1, 4] >= 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
      withMathJax(
        paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
        br(),
        paste0("\\( \\beta_0 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[1, 4], 3), ") so when ", input$xlab, " = 0, the mean of ", input$ylab, " is not significantly different from 0."),
        br(),
        paste0("For an increase of one unit of ", input$xlab, ", ", input$ylab, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
      )
    } else {
      withMathJax(
        paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
        br(),
        paste0("\\( \\beta_0 \\)", " and ", "\\( \\beta_1 \\)", " are not significantly different from 0 (p-values = ", round(summary(fit)$coefficients[1, 4], 3), " and ", round(summary(fit)$coefficients[2, 4], 3), ", respectively) so the mean of ", input$ylab, " is not significantly different from 0.")
      )
    }
  })
  
  output$plot <- renderPlotly({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    dat <- data.frame(x, y)
    p <- ggplot(dat, aes(x = x, y = y)) +
      geom_point() +
      stat_smooth(method = "glm", method.args = list(family = "binomial")) +
      ylab(input$ylab) +
      xlab(input$xlab) +
      theme_minimal()
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)