/**
 * Main application logic: auth, tab switching, scenarios, event wiring, rendering.
 */

(function () {
  "use strict";

  /* ===========================
   *  Predefined Scenarios
   * =========================== */
  const SCENARIOS = {
    Default: {
      purchase_price: 60000, annual_sales: 50000, down_payment_pct: 100,
      interest_rate: 5, loan_term: 5, monthly_rent: 800, monthly_utilities: 1000,
      monthly_fixed: 1000, annual_maintenance: 2500, annual_corp: 100,
      annual_depreciation: 3000, discount_rate: 6, analysis_period: 5, terminal_value: 25000
    },
    Usera: {
      purchase_price: 81000, annual_sales: 78000, down_payment_pct: 100,
      interest_rate: 5, loan_term: 5, monthly_rent: 650, monthly_utilities: 1100,
      monthly_fixed: 400, annual_maintenance: 2000, annual_corp: 300,
      annual_depreciation: 2000, discount_rate: 6, analysis_period: 5, terminal_value: 23000
    },
    Hortaleza: {
      purchase_price: 55000, annual_sales: 60000, down_payment_pct: 100,
      interest_rate: 5, loan_term: 5, monthly_rent: 1030, monthly_utilities: 1000,
      monthly_fixed: 500, annual_maintenance: 1600, annual_corp: 400,
      annual_depreciation: 1800, discount_rate: 6, analysis_period: 5, terminal_value: 17000
    }
  };

  /* ===========================
   *  DOM References
   * =========================== */
  const $ = (id) => document.getElementById(id);

  // Auth
  const loginOverlay = $("login-overlay");
  const appEl        = $("app");
  const loginBtn     = $("login-btn");
  const loginError   = $("login-error");
  const authStatus   = $("auth-status");

  // Tabs
  const navLinks    = document.querySelectorAll(".nav-link");
  const tabSections = document.querySelectorAll(".tab-content");

  // Scenario
  const scenarioSelect = $("scenario_name");
  const loadScenario   = $("load_scenario");

  // Action buttons
  const calcBtn      = $("calculate");
  const recalcBtn    = $("recalculate_cashflow");
  const downloadBtn  = $("download_json");

  // All slider IDs that need live value display
  const SLIDERS_EUR = [
    "purchase_price", "monthly_rent", "monthly_utilities", "monthly_fixed",
    "annual_maintenance", "annual_corp", "annual_depreciation", "terminal_value"
  ];
  const SLIDERS_PCT = ["down_payment_pct", "interest_rate", "discount_rate", "inflation_rate", "price_increase_rate"];
  const SLIDERS_PLAIN = ["loan_term", "analysis_period"];

  // Stored results for download / cross-tab
  let lastResults = null;

  /* ===========================
   *  Authentication
   * =========================== */
  loginBtn.addEventListener("click", doLogin);
  $("password").addEventListener("keydown", (e) => { if (e.key === "Enter") doLogin(); });

  function doLogin() {
    const u = $("username").value.trim();
    const p = $("password").value;
    if (u === "mcf" && p === "Ayala420!") {
      loginOverlay.classList.add("hidden");
      appEl.classList.remove("hidden");
      authStatus.textContent = "Logged in: " + new Date().toLocaleString();
    } else {
      loginError.classList.remove("hidden");
    }
  }

  /* ===========================
   *  Tab Switching
   * =========================== */
  navLinks.forEach((link) => {
    link.addEventListener("click", (e) => {
      e.preventDefault();
      const target = link.dataset.tab;
      navLinks.forEach((l) => l.classList.remove("active"));
      link.classList.add("active");
      tabSections.forEach((s) => {
        s.classList.toggle("active", s.id === "tab-" + target);
      });
    });
  });

  /* ===========================
   *  Slider Value Display
   * =========================== */
  function fmtSliderEur(v) {
    return "€" + Number(v).toLocaleString("en-US");
  }

  function updateSliderDisplay(id) {
    const el = $(id);
    const valSpan = $(id + "_val");
    if (!el || !valSpan) return;
    const v = el.value;
    if (SLIDERS_EUR.includes(id)) valSpan.textContent = fmtSliderEur(v);
    else if (SLIDERS_PCT.includes(id)) valSpan.textContent = v + "%";
    else valSpan.textContent = v;
  }

  // Wire up all sliders
  [...SLIDERS_EUR, ...SLIDERS_PCT, ...SLIDERS_PLAIN].forEach((id) => {
    const el = $(id);
    if (el) {
      el.addEventListener("input", () => updateSliderDisplay(id));
      updateSliderDisplay(id); // init
    }
  });

  /* ===========================
   *  Scenario Loader
   * =========================== */
  loadScenario.addEventListener("click", () => {
    const name = scenarioSelect.value;
    const s = SCENARIOS[name];
    if (!s) return;

    Object.keys(s).forEach((key) => {
      const el = $(key);
      if (el) {
        el.value = s[key];
        updateSliderDisplay(key);
      }
    });
  });

  /* ===========================
   *  Gather Inputs
   * =========================== */
  function gatherInputs() {
    return {
      purchase_price:     Number($("purchase_price").value),
      annual_sales:       Number($("annual_sales").value),
      down_payment_pct:   Number($("down_payment_pct").value),
      interest_rate:      Number($("interest_rate").value),
      loan_term:          Number($("loan_term").value),
      tax_toggle:         $("tax_toggle").checked,
      monthly_rent:       Number($("monthly_rent").value),
      monthly_utilities:  Number($("monthly_utilities").value),
      monthly_fixed:      Number($("monthly_fixed").value),
      annual_maintenance: Number($("annual_maintenance").value),
      annual_corp:        Number($("annual_corp").value),
      annual_depreciation:Number($("annual_depreciation").value),
      discount_rate:      Number($("discount_rate").value),
      analysis_period:    Number($("analysis_period").value),
      terminal_value:     Number($("terminal_value").value),
      inflation_rate:     Number($("inflation_rate").value),
      price_increase_rate:Number($("price_increase_rate").value)
    };
  }

  /* ===========================
   *  Calculate & Render
   * =========================== */
  function runCalculation() {
    const inp = gatherInputs();
    const results = Calculations.calculateInvestment(inp);
    lastResults = results;

    // Metrics table
    renderMetricsTable(results);

    // Yearly cash flow chart
    Charts.renderYearlyCashFlow("cashflow_plot", results.cashFlows);

    // Assumptions text
    renderAssumptions(results);

    // Also compute monthly
    runMonthlyCashFlow(results);
  }

  function runMonthlyCashFlow(results) {
    if (!results) results = lastResults;
    if (!results) return;

    const inp = gatherInputs();
    const monthly = Calculations.calculateMonthlyCashFlow(results, inp);
    Charts.renderMonthlyCashFlow("monthly_cashflow_plot", monthly.labels, monthly.cashFlowVec);
  }

  calcBtn.addEventListener("click", runCalculation);
  recalcBtn.addEventListener("click", () => runMonthlyCashFlow(null));

  /* ===========================
   *  Metrics Table
   * =========================== */
  function renderMetricsTable(r) {
    const fmt = Calculations.fmtEur;
    const irrStr = r.irr === null ? "Unable to calculate" : (r.irr * 100).toFixed(2) + "%";
    const payback = isNaN(r.paybackPeriod) || !isFinite(r.paybackPeriod)
      ? "Never"
      : r.paybackPeriod.toFixed(2) + " years";

    const rows = [
      ["Net Present Value (NPV)", fmt(r.npv)],
      ["Internal Rate of Return (IRR)", irrStr],
      ["Cash-on-Cash Return", (r.cashOnCash * 100).toFixed(2) + "%"],
      ["Payback Period", payback],
      ["Return on Investment (ROI)", r.roi.toFixed(2) + "%"],
      ["Annual Cash Flow", fmt(r.annualCashFlow)],
      ["Annual Net Income", fmt(r.annualNetIncome)],
      ["Annual Debt Service", fmt(r.debtService)]
    ];

    const tbody = $("metrics_table").querySelector("tbody");
    tbody.innerHTML = rows.map(([m, v]) =>
      `<tr><td>${m}</td><td>${v}</td></tr>`
    ).join("");
  }

  /* ===========================
   *  Assumptions Summary
   * =========================== */
  function renderAssumptions(r) {
    const a = r.assumptions;
    const f = (n) => Math.round(n).toLocaleString("en-US");
    const pctOf = (n) => (n / a.annual_sales * 100).toFixed(1);
    const ebitdaPct = ((1 - (a.total_annual_costs - a.annual_depreciation) / a.annual_sales) * 100).toFixed(1);
    const niPct = ((1 - a.total_annual_costs / a.annual_sales) * 100).toFixed(1);

    const text = [
      "INVESTMENT ASSUMPTIONS USED IN ANALYSIS:",
      "=========================================",
      "",
      "=== PURCHASE DETAILS:",
      `• Purchase Price: ${f(a.purchase_price)} €`,
      `• Down Payment: ${f(a.down_payment)} €`,
      `• Loan Amount: ${f(a.loan_amount)} €`,
      `• Interest Rate: ${a.interest_rate.toFixed(2)}%`,
      `• Loan Term: ${a.loan_term} years`,
      "",
      "=== REVENUE & COSTS:",
      `• Expected Annual Sales: ${f(a.annual_sales)} €`,
      `• Total Annual Operating Costs: ${f(a.total_annual_costs)} €`,
      `  - Annual Rent: ${f(a.annual_rent)} € (${pctOf(a.annual_rent)}%)`,
      `  - Annual Utilities Variable: ${f(a.annual_utilities)} € (${pctOf(a.annual_utilities)}%)`,
      `  - Annual Maintenance: ${f(a.annual_maintenance)} € (${pctOf(a.annual_maintenance)}%)`,
      `  - Annual Fixed: ${f(a.annual_fixed)} € (${pctOf(a.annual_fixed)}%)`,
      `  - Annual Corp Costs: ${f(a.annual_corp)} € (${pctOf(a.annual_corp)}%)`,
      `  - Annual Depreciation: ${f(a.annual_depreciation)} € (${pctOf(a.annual_depreciation)}%)`,
      "",
      "=== MARGINS:",
      `EBITDA ${ebitdaPct}%`,
      `Net Income ${niPct}%`,
      "",
      "=== ANALYSIS PARAMETERS:",
      `• Discount Rate (NPV): ${a.discount_rate.toFixed(2)}%`,
      `• Analysis Period: ${a.analysis_period} years`,
      `• Taxes: ${a.tax_toggle ? " yes" : " no"}`,
      `• Terminal Value ${f(a.terminal_value)}€`,
      "",
      "Note: This analysis assumes consistent annual performance and does not account for",
      "inflation, market changes, or unexpected events. Results should be used as a guide",
      "alongside professional financial advice."
    ].join("\n");

    $("assumptions_summary").textContent = text;
  }

  /* ===========================
   *  JSON Download
   * =========================== */
  downloadBtn.addEventListener("click", () => {
    if (!lastResults) return;

    // Build export matching the R structure
    const r = lastResults;
    const a = r.assumptions;
    const exportObj = {
      analysis: {
        npv: r.npv,
        irr: r.irr === null ? "Unable to calculate" : (r.irr * 100).toFixed(2) + "%",
        cash_on_cash: (r.cashOnCash * 100).toFixed(2) + "%",
        payback_period: (isNaN(r.paybackPeriod) || !isFinite(r.paybackPeriod))
          ? "Never" : r.paybackPeriod.toFixed(2) + " years",
        roi: r.roi.toFixed(2) + "%",
        annual_cash_flow: r.annualCashFlow,
        annual_net_income: r.annualNetIncome,
        debt_service: r.debtService,
        cash_flows: r.cashFlows,
        assumptions: {
          purchase_price: a.purchase_price,
          down_payment: a.down_payment,
          loan_amount: a.loan_amount,
          interest_rate: a.interest_rate.toFixed(2) + "%",
          loan_term: a.loan_term + " years",
          discount_rate: a.discount_rate.toFixed(2) + "%",
          analysis_period: a.analysis_period + " years",
          annual_sales: a.annual_sales,
          total_annual_costs: a.total_annual_costs,
          annual_rent: a.annual_rent,
          annual_utilities: a.annual_utilities,
          annual_maintenance: a.annual_maintenance,
          annual_fixed: a.annual_fixed,
          annual_corp: a.annual_corp,
          annual_depreciation: a.annual_depreciation,
          terminal_value: a.terminal_value
        }
      }
    };

    const blob = new Blob([JSON.stringify(exportObj, null, 2)], { type: "application/json" });
    const url = URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.href = url;
    link.download = "investment_data.json";
    link.click();
    URL.revokeObjectURL(url);
  });

})();
