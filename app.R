
# Laundromat Investment Analysis Shiny App
# Author: AI Assistant
# Date: 2025-06-12

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyauthr)
library(DT)
library(plotly)
library(dplyr)
library(readr)
library(FinCal)
library(scales)
library(shinyWidgets)

# Authentication setup - simple password protection
user_base <- data.frame(
  user = "mcf",
  password = "Ayala420!",  # Simple password as requested
  stringsAsFactors = FALSE
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "MCF Expansion"),

  dashboardSidebar(
    # Password input for simple authentication
    conditionalPanel(
      condition = "output.authenticated == false",
      tags$div(
        style = "padding: 20px;",
        h3("Authentication Required"),
        textInput("username", "Username:", value = ""),
        passwordInput("password", "Password:", value = ""),
        actionButton("loginBtn", "Login", class = "btn-primary"),
        br(), br()
      )
    ),

    # Main navigation menu (only shown when authenticated)
    conditionalPanel(
      condition = "output.authenticated == true",
      sidebarMenu(
        menuItem("Investment Analysis", tabName = "analysis", icon = icon("calculator"))
      )
    )
  ),

  dashboardBody(
    # Authentication status output
    verbatimTextOutput("auth_status", placeholder = TRUE),

    # Main content (only shown when authenticated)
    conditionalPanel(
      condition = "output.authenticated == true",

      tabItems(
        # Investment Analysis Tab
        tabItem(tabName = "analysis",
          fluidRow(
            box(
              title = "Investment Parameters", status = "primary", solidHeader = TRUE, width = 6,

              # Purchase Price Slider
              sliderInput("purchase_price",
                         "Purchase Price (€):",
                         min = 10000, max = 200000, value = 60000, step = 1000,
                         pre = "€", sep = ","),

              # Expected Annual Sales
              numericInput("annual_sales",
                          "(A+B) Expected Annual Sales (€):",
                          value = 50000, min = 25000, max = 200000, step = 1000),

              # Down Payment Percentage
              sliderInput("down_payment_pct",
                         "Down Payment (%):",
                         min = 10, max = 100, value = 30, step = 5, post = "%"),

              # Interest Rate
              sliderInput("interest_rate",
                         "Loan Interest Rate (%):",
                         min = 0, max = 20, value = 6, step = 0.05, post = "%"),

              # Loan Term
              sliderInput("loan_term",
                         "Loan Term (years):",
                         min = 1, max = 25, value = 5, step = 1),

              h3("Include tax impact?"),
              shinyWidgets::switchInput(
                inputId = "tax_toggle",
                label = "Yes",
                value = FALSE
              )
            ),

            box(
              title = "Operating Costs", status = "primary", solidHeader = TRUE, width = 6,

              # Rent
              sliderInput("monthly_rent",
                         "(C) Monthly Rent (€):",
                         min = 300, max = 3000, value = 800, step = 20,
                         pre = "€", sep = ","),

              # Utilities var
              sliderInput("monthly_utilities",
                         "(E+F) Monthly Variable (€):",
                         min = 100, max = 3000, value = 1000, step = 50,
                         pre = "€", sep = ","),

              # fixed costs
              sliderInput("monthly_fixed",
                          "(G+H) Monthly Fixed Costs (€):",
                          min = 100, max = 3000, value = 1000, step = 50,
                          pre = "€", sep = ","),

              # Maintenance
              sliderInput("annual_maintenance",
                         "(I) Annual Maintenance (€):",
                         min = 0, max = 30000, value = 2500, step = 200,
                         pre = "€", sep = ","),

              # corp overhead
              sliderInput("annual_corp",
                          "(J) Annual Corp Costs (€):",
                          min = 0, max = 5000, value = 100, step = 50,
                          pre = "€", sep = ","),

              # Equipment Depreciation
              sliderInput("annual_depreciation",
                         "(O) Annual Equipment Depreciation (€):",
                         min = 0, max = 30000, value = 3000, step = 500,
                         pre = "€", sep = ",")
            )
          ),

          fluidRow(
            box(
              title = "Analysis Assumptions", status = "primary", solidHeader = TRUE, width = 12,

              sliderInput("discount_rate",
                         "Discount Rate for NPV (%):",
                         min = 1, max = 15, value = 5, step = 0.5, post = "%"),

              sliderInput("analysis_period",
                         "Analysis Period (years):",
                         min = 2, max = 20, value = 5, step = 1),

              sliderInput("terminal_value",
                          "Valor terminal:",
                          min = 0, max = 250000, value = 10000, step = 1000)
            ),
              box(title = "Analyze", status = "primary", solidHeader = TRUE, width = 12,
              actionButton("calculate", "Calculate Investment Metrics",
                          class = "btn-success", style = "margin-top: 10px;")
            )
          ),

          fluidRow(
            box(
              title = "Investment Metrics", status = "warning", solidHeader = TRUE, width = 6,
              DT::dataTableOutput("metrics_table")
            ),

            box(
              title = "Cash Flow Analysis", status = "warning", solidHeader = TRUE, width = 6,
              plotlyOutput("cashflow_plot")
            )
          ),

          fluidRow(
            box(
              title = "Key Assumptions Used", status = "warning", solidHeader = TRUE, width = 12,
              verbatimTextOutput("assumptions_summary")
            )
          )

        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {

  # Authentication logic
  values <- reactiveValues(authenticated = FALSE)

  # Check authentication status
  output$authenticated <- reactive({
    return(values$authenticated)
  })
  outputOptions(output, "authenticated", suspendWhenHidden = FALSE)

  # Login logic
  observeEvent(input$loginBtn, {
    if (input$username == user_base$user && input$password == user_base$password) {
      values$authenticated <- TRUE
      showNotification("Login successful!", type = "message")
    } else {
      showNotification("Invalid username or password!", type = "error")
    }
  })

  # Auth status display
  output$auth_status <- renderText({
    if (values$authenticated) {
      return(paste0("Logged in: ",Sys.time() ))
    } else {
      return("")
    }
  })



  output$pl_preview <- DT::renderDataTable({
    req(pl_data())
    DT::datatable(head(pl_data(), 100), options = list(scrollX = TRUE))
  })

  # Calculate investment metrics
  investment_results <- eventReactive(input$calculate, {

    # Extract inputs
    purchase_price <- input$purchase_price
    annual_sales <- input$annual_sales
    down_payment <- purchase_price * (input$down_payment_pct / 100)
    loan_amount <- purchase_price - down_payment
    interest_rate <- input$interest_rate / 100
    loan_term <- input$loan_term
    discount_rate <- input$discount_rate / 100
    analysis_period <- input$analysis_period

    # Calculate annual operating costs
    annual_rent <- input$monthly_rent * 12
    annual_utilities <- input$monthly_utilities * 12
    annual_maintenance <- input$annual_maintenance
    annual_fixed <- input$monthly_fixed * 12
    annual_depreciation <- input$annual_depreciation
    annual_corp <- input$annual_corp
    terminal_value_input <- input$terminal_value

    total_annual_costs <- annual_rent + annual_utilities + annual_maintenance +
                         annual_fixed + annual_depreciation + annual_corp

    # Calculate monthly loan payment
    if (loan_amount > 0) {
      monthly_rate <- interest_rate / 12
      num_payments <- loan_term * 12
      monthly_payment <- loan_amount * (monthly_rate * (1 + monthly_rate)^num_payments) /
                        ((1 + monthly_rate)^num_payments - 1)
      annual_debt_service <- monthly_payment * 12
    } else {
      monthly_payment <- 0
      annual_debt_service <- 0
    }

    # Calculate annual cash flows
    irpf_tax <- (annual_sales*(1-0.21) - total_annual_costs*(1-0.21))*0.25
    iva_net <- (annual_sales*(0.21) - (total_annual_costs-annual_depreciation)*(0.21))

    annual_net_income <- annual_sales - total_annual_costs - ifelse(input$tax_toggle, iva_net+irpf_tax, 0)
    annual_cash_flow <- annual_net_income - annual_debt_service

    # Create cash flow vector for NPV and IRR calculations
    cash_flows <- c(-down_payment)  # Initial investment (negative)

    for (i in 1:analysis_period) {
      cash_flows <- c(cash_flows, annual_cash_flow)
    }

    # Add terminal value (simplified as remaining loan balance reduction)
    if (loan_term > analysis_period) {
      remaining_balance <- loan_amount * ((1 + monthly_rate)^(num_payments) - (1 + monthly_rate)^(analysis_period * 12)) /
                          ((1 + monthly_rate)^(num_payments) - 1)
      terminal_value <- terminal_value_input - remaining_balance
    } else {
      terminal_value <- terminal_value_input
    }

    cash_flows[length(cash_flows)] <- cash_flows[length(cash_flows)] + terminal_value

    # Calculate NPV
    npv <- sum(cash_flows / (1 + discount_rate)^(0:(length(cash_flows)-1)))

    # Calculate IRR (using iterative method)
    irr_calc <- function(rate) {
      sum(cash_flows / (1 + rate)^(0:(length(cash_flows)-1)))
    }

    # Find IRR using optimization
    irr_result <- tryCatch({
      uniroot(irr_calc, interval = c(-0.99, 5))$root
    }, error = function(e) {
      NA
    })

    # Calculate other metrics
    total_cash_invested <- down_payment
    annual_return <- annual_cash_flow
    cash_on_cash_return <- ifelse(total_cash_invested > 0, annual_return / total_cash_invested, 0)

    # Payback period
    cumulative_cash_flow <- cumsum(cash_flows)
    payback_period <- ifelse(any(cumulative_cash_flow >= down_payment),
                            which(cumulative_cash_flow >= down_payment)[1],
                            NA)

    # ROI calculation
    total_profit <- sum(cash_flows[-1])  # Exclude initial investment
    roi <- ifelse(down_payment > 0, (total_profit / down_payment) * 100, 0)

    # Create results
    results <- list(
      npv = npv,
      irr = ifelse(is.na(irr_result), "Unable to calculate", paste0(round(irr_result * 100, 2), "%")),
      cash_on_cash = paste0(round(cash_on_cash_return * 100, 2), "%"),
      payback_period = ifelse(is.na(payback_period), "Never", paste(payback_period, "years")),
      roi = paste0(round(roi, 2), "%"),
      annual_cash_flow = annual_cash_flow,
      annual_net_income = annual_net_income,
      debt_service = annual_debt_service,
      cash_flows = cash_flows,
      assumptions = list(
        purchase_price = purchase_price,
        down_payment = down_payment,
        loan_amount = loan_amount,
        interest_rate = paste0(round(interest_rate * 100, 2), "%"),
        loan_term = paste(loan_term, "years"),
        discount_rate = paste0(round(discount_rate * 100, 2), "%"),
        analysis_period = paste(analysis_period, "years"),
        annual_sales = annual_sales,
        total_annual_costs = total_annual_costs,
        annual_rent = annual_rent,
        annual_utilities = annual_utilities,
        annual_maintenance = annual_maintenance,
        annual_fixed = annual_fixed,
        annual_corp = annual_corp,
        annual_depreciation = annual_depreciation,
        terminal_value = terminal_value_input
      )
    )

    return(results)
  })

  # Render metrics table
  output$metrics_table <- DT::renderDataTable({
    req(investment_results())

    results <- investment_results()

    metrics_df <- data.frame(
      Metric = c("Net Present Value (NPV)",
                 "Internal Rate of Return (IRR)",
                 "Cash-on-Cash Return",
                 "Payback Period",
                 "Return on Investment (ROI)",
                 "Annual Cash Flow",
                 "Annual Net Income",
                 "Annual Debt Service"),
      Value = c(paste0("€", format(round(results$npv), big.mark = ",")),
                results$irr,
                results$cash_on_cash,
                results$payback_period,
                results$roi,
                paste0("€", format(round(results$annual_cash_flow), big.mark = ",")),
                paste0("€", format(round(results$annual_net_income), big.mark = ",")),
                paste0("€", format(round(results$debt_service), big.mark = ","))),
      stringsAsFactors = FALSE
    )

    DT::datatable(metrics_df, options = list(dom = 't', pageLength = -1), rownames = FALSE)
  })

  # Render cash flow plot
  output$cashflow_plot <- renderPlotly({
    req(investment_results())

    results <- investment_results()
    years <- 0:(length(results$cash_flows) - 1)

    plot_data <- data.frame(
      Year = years,
      Cash_Flow = results$cash_flows
    )

    p <- plot_ly(plot_data, x = ~Year, y = ~Cash_Flow, type = 'bar',
                 marker = list(color = ifelse(plot_data$Cash_Flow >= 0, 'green', 'red'))) %>%
      layout(title = "Projected Cash Flows by Year",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Cash Flow (€)"),
             hovermode = 'x')

    return(p)
  })

  # Render assumptions summary
  output$assumptions_summary <- renderText({
    req(investment_results())

    assumptions <- investment_results()$assumptions

    paste(
      "INVESTMENT ASSUMPTIONS USED IN ANALYSIS:",
      "=========================================",
      "",
      "=== PURCHASE DETAILS:",
      paste("• Purchase Price:", format(assumptions$purchase_price, big.mark = ","), "€"),
      paste("• Down Payment:", format(assumptions$down_payment, big.mark = ","), "€"),
      paste("• Loan Amount:", format(assumptions$loan_amount, big.mark = ","), "€"),
      paste("• Interest Rate:", assumptions$interest_rate),
      paste("• Loan Term:", assumptions$loan_term),
      "",
      "=== REVENUE & COSTS:",
      paste("• Expected Annual Sales:", format(assumptions$annual_sales, big.mark = ","), "€"),
      paste("• Total Annual Operating Costs:", format(assumptions$total_annual_costs, big.mark = ","), "€"),
      paste("  - Annual Rent:", format(assumptions$annual_rent, big.mark = ","), paste0("€ (", format(round(assumptions$annual_rent/assumptions$annual_sales*100,1), big.mark = ","), "%)")),
      paste("  - Annual Utilities Variable:", format(assumptions$annual_utilities, big.mark = ","), paste0("€ (", format(round(assumptions$annual_utilities/assumptions$annual_sales*100,1), big.mark = ","), "%)")),
      paste("  - Annual Maintenance:", format(assumptions$annual_maintenance, big.mark = ","), paste0("€ (", format(round(assumptions$annual_maintenance/assumptions$annual_sales*100,1), big.mark = ","), "%)")),
      paste("  - Annual Fixed:", format(assumptions$annual_fixed, big.mark = ","), paste0("€ (", format(round(assumptions$annual_fixed/assumptions$annual_sales*100,1), big.mark = ","), "%)")),
      paste("  - Annual Corp Costs:", format(assumptions$annual_corp, big.mark = ","), paste0("€ (", format(round(assumptions$annual_corp/assumptions$annual_sales*100,1), big.mark = ","), "%)")),
      paste("  - Annual Depreciation:", format(assumptions$annual_depreciation, big.mark = ","), paste0("€ (", format(round(assumptions$annual_depreciation/assumptions$annual_sales*100,1), big.mark = ","), "%)")),
      "",
      "=== MARGINS:",
      paste0("EBITDA ",format((1-(assumptions$total_annual_costs-assumptions$annual_depreciation)/assumptions$annual_sales)*100,
                                    big.mark = ".",digits=1,nsmall=1), "%"),
      paste0("Net Income ",format((1-(assumptions$total_annual_costs)/assumptions$annual_sales)*100,
                              big.mark = ".",digits=1,nsmall=1), "%"),
      "",
      "=== ANALYSIS PARAMETERS:",
      paste("• Discount Rate (NPV):", assumptions$discount_rate),
      paste("• Analysis Period:", assumptions$analysis_period),
      paste("• Taxes:", ifelse(input$tax_toggle," yes", " no" )),
      paste("• Terminal Value", paste0(format(assumptions$terminal_value,big.mark = ","), "€" )),
      "",
      "Note: This analysis assumes consistent annual performance and does not account for",
      "inflation, market changes, or unexpected events. Results should be used as a guide",
      "alongside professional financial advice.",
      sep = "
      "
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
