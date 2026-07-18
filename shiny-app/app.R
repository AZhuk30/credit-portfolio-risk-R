# =============================================================================
# Credit Portfolio Risk Dashboard — Shiny app
# Author: Alexander Zhuk
#
# Three tabs:
#   1. Portfolio   — the loan book and its assumptions
#   2. Loss & Capital — Monte Carlo loss distribution with a LIVE rho slider
#   3. Stress Test — baseline vs severely adverse overlay
#
# Design choice: simulations run live at 2,000 draws (fast enough for a
# slider) with a fixed seed, so the app is responsive AND reproducible.
#
# Files needed in the same folder: app.R (this file) + portfolio.csv
# Run locally:  install.packages("shiny")  then click "Run App" in RStudio
# Deploy:       install.packages("rsconnect"), set up shinyapps.io account,
#               then rsconnect::deployApp()
# =============================================================================

library(shiny)

# ---- Model code (self-contained so the deployed app has no dependencies) ----
portfolio <- read.csv("portfolio.csv")

pd_table <- data.frame(
  rating = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"),
  pd     = c(0.0001, 0.0002, 0.0006, 0.0018, 0.0075, 0.0380, 0.2500)
)

simulate_losses <- function(pd_vec, lgd_vec, ead_vec,
                            n_sims = 2000, rho = 0.20, seed = 123) {
  set.seed(seed)
  thresh <- qnorm(pd_vec)
  n <- length(pd_vec)
  losses <- numeric(n_sims)
  for (s in 1:n_sims) {
    M <- rnorm(1)
    e <- rnorm(n)
    A <- sqrt(rho) * M + sqrt(1 - rho) * e
    d <- A < thresh
    losses[s] <- sum(ead_vec[d] * lgd_vec[d])
  }
  losses
}

apply_stress <- function(pf, notches = 1, lgd_add = 0.15) {
  idx <- match(pf$rating, pd_table$rating)
  idx_s <- pmin(idx + notches, nrow(pd_table))
  pf$pd_stressed  <- pd_table$pd[idx_s]
  pf$lgd_stressed <- pmin(pf$lgd + lgd_add, 1.0)
  pf
}
portfolio <- apply_stress(portfolio)

# =============================================================================
# UI
# =============================================================================
ui <- fluidPage(
  titlePanel("Credit Portfolio Risk Model — Alexander Zhuk"),
  p("Synthetic 500-loan corporate book | One-factor Gaussian copula |",
    "Monte Carlo economic capital at the 99.9% standard"),

  tabsetPanel(

    # ---- Tab 1: Portfolio ---------------------------------------------------
    tabPanel("Portfolio",
      br(),
      fluidRow(
        column(4, h4("Book summary"), tableOutput("book_summary")),
        column(8, h4("Exposure & expected loss by rating"),
                  plotOutput("rating_bars", height = "300px"))
      ),
      h4("Assumptions"),
      p("PDs: stylized long-run S&P-style default rates. LGD by collateral segment",
        "(secured CRE 25-40% up to unsecured software 75-90%). EAD = drawn +",
        "75% CCF on undrawn revolver commitments."),
      h4("Loan-level detail (first 15)"),
      tableOutput("loan_table")
    ),

    # ---- Tab 2: Loss & Capital ----------------------------------------------
    tabPanel("Loss & Capital",
      br(),
      sidebarLayout(
        sidebarPanel(width = 3,
          sliderInput("rho", "Asset correlation (rho)",
                      min = 0.05, max = 0.50, value = 0.20, step = 0.05),
          helpText("Drag the slider: expected loss barely moves,",
                   "but the tail - and capital - explodes.",
                   "Correlation doesn't change average losses;",
                   "it changes how losses cluster."),
          tableOutput("capital_summary")
        ),
        mainPanel(width = 9,
          plotOutput("loss_hist", height = "420px")
        )
      )
    ),

    # ---- Tab 3: Stress Test -------------------------------------------------
    tabPanel("Stress Test",
      br(),
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("scenario", "Scenario",
                      choices = c("Baseline", "Severely adverse")),
          helpText("Severely adverse: all ratings migrate down one notch,",
                   "LGD +15 points, correlation 0.20 to 0.35."),
          tableOutput("stress_table")
        ),
        mainPanel(width = 9,
          plotOutput("stress_plot", height = "420px"),
          p(em("Note: this compares economic capital requirements under two",
               "parameterizations. Regulatory DFAST instead tests whether",
               "current capital absorbs stressed losses over nine quarters."))
        )
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output) {

  # ---- Tab 1 ----------------------------------------------------------------
  output$book_summary <- renderTable({
    data.frame(
      Metric = c("Loans", "Total EAD", "Expected loss", "EL % of EAD"),
      Value  = c(nrow(portfolio),
                 paste0("$", format(round(sum(portfolio$ead)/1e6), big.mark = ","), "M"),
                 paste0("$", format(round(sum(portfolio$expected_loss)/1e6, 1)), "M"),
                 sprintf("%.2f%%", 100*sum(portfolio$expected_loss)/sum(portfolio$ead)))
    )
  }, colnames = FALSE)

  output$rating_bars <- renderPlot({
    br <- aggregate(cbind(ead, expected_loss) ~ rating, portfolio, sum)
    br <- br[match(pd_table$rating, br$rating), ]
    bp <- barplot(br$ead/1e6, names.arg = br$rating, col = "steelblue",
                  ylab = "EAD ($M)", main = "Exposure by rating (EL rate labeled, bps)")
    text(bp, br$ead/1e6, labels = round(1e4*br$expected_loss/br$ead), pos = 3, cex = 0.9)
  })

  output$loan_table <- renderTable({
    head(portfolio[, c("loan_id","rating","segment","facility","ead","pd","lgd","expected_loss")], 15)
  })

  # ---- Tab 2 (reactive: reruns when the slider moves) -----------------------
  losses_reactive <- reactive({
    simulate_losses(portfolio$pd, portfolio$lgd, portfolio$ead,
                    n_sims = 2000, rho = input$rho, seed = 123)
  })

  output$loss_hist <- renderPlot({
    l <- losses_reactive()
    el <- mean(l); v999 <- quantile(l, 0.999)
    hist(l/1e6, breaks = 50, col = "steelblue", border = "white",
         main = sprintf("Loss distribution (rho = %.2f)", input$rho),
         xlab = "Loss ($ millions)", xlim = c(0, 220))
    abline(v = el/1e6, col = "darkgreen", lwd = 3)
    abline(v = v999/1e6, col = "red", lwd = 3)
    legend("topright",
           legend = c(sprintf("EL: $%.1fM", el/1e6),
                      sprintf("99.9%% VaR: $%.1fM", v999/1e6)),
           col = c("darkgreen","red"), lwd = 3, bty = "n")
  })

  output$capital_summary <- renderTable({
    l <- losses_reactive()
    el <- mean(l); v999 <- quantile(l, 0.999)
    data.frame(
      Metric = c("Expected loss", "99.9% VaR", "Economic capital", "Capital % EAD"),
      Value  = c(sprintf("$%.1fM", el/1e6),
                 sprintf("$%.1fM", v999/1e6),
                 sprintf("$%.1fM", (v999-el)/1e6),
                 sprintf("%.2f%%", 100*(v999-el)/sum(portfolio$ead)))
    )
  }, colnames = FALSE)

  # ---- Tab 3 ----------------------------------------------------------------
  stress_losses <- reactive({
    if (input$scenario == "Baseline") {
      simulate_losses(portfolio$pd, portfolio$lgd, portfolio$ead,
                      n_sims = 2000, rho = 0.20, seed = 123)
    } else {
      simulate_losses(portfolio$pd_stressed, portfolio$lgd_stressed, portfolio$ead,
                      n_sims = 2000, rho = 0.35, seed = 123)
    }
  })

  output$stress_plot <- renderPlot({
    l <- stress_losses()
    el <- mean(l); v999 <- quantile(l, 0.999)
    colr <- if (input$scenario == "Baseline") "steelblue" else "indianred3"
    hist(l/1e6, breaks = 50, col = colr, border = "white",
         main = paste("Loss distribution -", input$scenario),
         xlab = "Loss ($ millions)", xlim = c(0, 400))
    abline(v = el/1e6, col = "darkgreen", lwd = 3)
    abline(v = v999/1e6, col = "red", lwd = 3)
    legend("topright",
           legend = c(sprintf("EL: $%.1fM", el/1e6),
                      sprintf("99.9%% VaR: $%.1fM", v999/1e6)),
           col = c("darkgreen","red"), lwd = 3, bty = "n")
  })

  output$stress_table <- renderTable({
    l <- stress_losses()
    el <- mean(l); v999 <- quantile(l, 0.999)
    data.frame(
      Metric = c("Expected loss", "99.9% VaR", "Economic capital"),
      Value  = c(sprintf("$%.1fM", el/1e6),
                 sprintf("$%.1fM", v999/1e6),
                 sprintf("$%.1fM", (v999-el)/1e6))
    )
  }, colnames = FALSE)
}

shinyApp(ui = ui, server = server)
