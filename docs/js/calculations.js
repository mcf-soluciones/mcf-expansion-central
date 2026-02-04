/**
 * Financial calculation engine — ported from app.R server logic.
 */

const Calculations = (() => {

  /**
   * Main investment analysis.
   * @param {Object} inp – all input values keyed by their HTML id
   * @returns {Object} results
   */
  function calculateInvestment(inp) {
    const purchasePrice    = inp.purchase_price;
    const annualSales      = inp.annual_sales;
    const downPaymentPct   = inp.down_payment_pct;
    const interestRatePct  = inp.interest_rate;
    const loanTerm         = inp.loan_term;
    const discountRatePct  = inp.discount_rate;
    const analysisPeriod   = inp.analysis_period;
    const monthlyRent      = inp.monthly_rent;
    const monthlyUtilities = inp.monthly_utilities;
    const monthlyFixed     = inp.monthly_fixed;
    const annualMaint      = inp.annual_maintenance;
    const annualCorp       = inp.annual_corp;
    const annualDepr       = inp.annual_depreciation;
    const terminalValueIn  = inp.terminal_value;
    const taxToggle        = inp.tax_toggle;
    const salesIncreasePct = inp.sales_increase_rate || 0;
    const rentIncreasePct  = inp.rent_increase_rate || 0;

    const downPayment = purchasePrice * (downPaymentPct / 100);
    const loanAmount  = purchasePrice - downPayment;
    const interestRate = interestRatePct / 100;
    const discountRate = discountRatePct / 100;
    const salesGrowth  = salesIncreasePct / 100;
    const rentGrowth   = rentIncreasePct / 100;

    // Base annual operating costs (year 1)
    const annualRentTotal  = monthlyRent * 12;
    const annualUtilTotal  = monthlyUtilities * 12;
    const annualFixedTotal = monthlyFixed * 12;
    const totalAnnualCosts = annualRentTotal + annualUtilTotal + annualMaint +
                             annualFixedTotal + annualDepr + annualCorp;

    // Loan amortisation (handle 0% interest rate)
    let monthlyPayment = 0;
    let annualDebtService = 0;
    let monthlyRate = 0;
    let numPayments = 0;

    if (loanAmount > 0) {
      monthlyRate = interestRate / 12;
      numPayments = loanTerm * 12;
      if (interestRate === 0) {
        monthlyPayment = loanAmount / numPayments;
      } else {
        monthlyPayment = loanAmount *
          (monthlyRate * Math.pow(1 + monthlyRate, numPayments)) /
          (Math.pow(1 + monthlyRate, numPayments) - 1);
      }
      annualDebtService = monthlyPayment * 12;
    }

    // Build per-year cash flows with YoY growth
    const cashFlows = [-downPayment];
    let firstYearCashFlow = 0;
    let firstYearNetIncome = 0;

    for (let yr = 1; yr <= analysisPeriod; yr++) {
      const yrSales = annualSales * Math.pow(1 + salesGrowth, yr - 1);
      const yrRent  = annualRentTotal * Math.pow(1 + rentGrowth, yr - 1);
      const yrCosts = yrRent + annualUtilTotal + annualMaint +
                      annualFixedTotal + annualDepr + annualCorp;

      // Tax (Spanish IVA 21% + IRPF 25%)
      const irpfTax = (yrSales * (1 - 0.21) - yrCosts * (1 - 0.21)) * 0.25;
      const ivaNet  = (yrSales * 0.21) - (yrCosts - annualDepr) * 0.21;
      const taxImpact = taxToggle ? (ivaNet + irpfTax) : 0;

      const netIncome = yrSales - yrCosts - taxImpact;
      const cf = netIncome - annualDebtService;
      cashFlows.push(cf);

      if (yr === 1) {
        firstYearCashFlow = cf;
        firstYearNetIncome = netIncome;
      }
    }

    // Terminal value (subtract remaining loan balance if loan extends beyond analysis)
    let terminalValue = terminalValueIn;
    if (loanAmount > 0 && loanTerm > analysisPeriod) {
      if (interestRate === 0) {
        const paidMonths = analysisPeriod * 12;
        const remainingBalance = loanAmount - monthlyPayment * paidMonths;
        terminalValue = terminalValueIn - Math.max(0, remainingBalance);
      } else {
        const remainingBalance = loanAmount *
          (Math.pow(1 + monthlyRate, numPayments) - Math.pow(1 + monthlyRate, analysisPeriod * 12)) /
          (Math.pow(1 + monthlyRate, numPayments) - 1);
        terminalValue = terminalValueIn - remainingBalance;
      }
    }
    cashFlows[cashFlows.length - 1] += terminalValue;

    // NPV
    let npv = 0;
    for (let i = 0; i < cashFlows.length; i++) {
      npv += cashFlows[i] / Math.pow(1 + discountRate, i);
    }

    // IRR (bisection method replicating R's uniroot on [-0.99, 5])
    const irr = computeIRR(cashFlows);

    // Other metrics (use year-1 figures for cash-on-cash and payback)
    const cashOnCash = downPayment > 0 ? (firstYearCashFlow / downPayment) : 0;
    const paybackPeriod = firstYearCashFlow > 0 ? (downPayment / firstYearCashFlow) : NaN;
    const totalProfit = cashFlows.slice(1).reduce((s, v) => s + v, 0);
    const roi = downPayment > 0 ? (totalProfit / downPayment) * 100 : 0;

    return {
      npv,
      irr,
      cashOnCash,
      paybackPeriod,
      roi,
      annualCashFlow: firstYearCashFlow,
      annualNetIncome: firstYearNetIncome,
      debtService: annualDebtService,
      cashFlows,
      assumptions: {
        purchase_price: purchasePrice,
        down_payment: downPayment,
        loan_amount: loanAmount,
        interest_rate: interestRatePct,
        loan_term: loanTerm,
        discount_rate: discountRatePct,
        analysis_period: analysisPeriod,
        annual_sales: annualSales,
        total_annual_costs: totalAnnualCosts,
        annual_rent: annualRentTotal,
        annual_utilities: annualUtilTotal,
        annual_maintenance: annualMaint,
        annual_fixed: annualFixedTotal,
        annual_corp: annualCorp,
        annual_depreciation: annualDepr,
        terminal_value: terminalValueIn,
        tax_toggle: taxToggle,
        sales_increase_rate: salesIncreasePct,
        rent_increase_rate: rentIncreasePct
      }
    };
  }

  /**
   * Monthly cash flow projection.
   */
  function calculateMonthlyCashFlow(results, inp) {
    const a = results.assumptions;
    const analysisPeriodYears = a.analysis_period;
    const totalMonths = analysisPeriodYears * 12;
    const inflationRate     = inp.inflation_rate / 100;
    const priceIncreaseRate = inp.price_increase_rate / 100;

    // Seasonal proportions (sum ≈ 1)
    const salesProp = [0.13, 0.12, 0.11, 0.10, 0.09, 0.03, 0.02, 0.01, 0.02, 0.10, 0.13, 0.14];
    const utilProp  = [0.12, 0.12, 0.12, 0.10, 0.08, 0.03, 0.02, 0.02, 0.02, 0.11, 0.12, 0.14];

    const baseMonthlySales = salesProp.map(p => a.annual_sales * p);
    const baseMonthlyRent  = a.annual_rent / 12;
    const baseMontlyUtil   = utilProp.map(p => a.annual_utilities * p);
    const baseMonthlyMaint = a.annual_maintenance / 12;
    const baseMonthlyFixed = a.annual_fixed / 12;
    const baseMonthlyCorp  = a.annual_corp / 12;
    const monthlyDebtService = results.debtService / 12;

    const cashFlowVec = [];
    const labels = [];
    let currentYear = 1;

    // Month label helper
    const monthNames = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
    const startYear = 2025;

    for (let m = 1; m <= totalMonths; m++) {
      if (m > 1 && (m - 1) % 12 === 0) {
        currentYear++;
      }
      const mi = (m - 1) % 12; // 0-indexed month within year

      const growthSales = Math.pow(1 + priceIncreaseRate, currentYear - 1);
      const growthCosts = Math.pow(1 + inflationRate, currentYear - 1);

      const sales = baseMonthlySales[mi] * growthSales;
      const rent  = baseMonthlyRent * growthCosts;
      const util  = baseMontlyUtil[mi] * growthCosts;
      const maint = baseMonthlyMaint * growthCosts;
      const fixed = baseMonthlyFixed * growthCosts;
      const corp  = baseMonthlyCorp * growthCosts;

      const totalCosts = rent + util + maint + fixed + corp;
      const cf = sales - totalCosts - monthlyDebtService;

      cashFlowVec.push(cf);

      const calYear = startYear + Math.floor((m - 1) / 12);
      labels.push(monthNames[mi] + " " + calYear);
    }

    return { labels, cashFlowVec };
  }

  /* ---- IRR via bisection (mirrors R uniroot on [-0.99, 5]) ---- */
  function computeIRR(cashFlows) {
    // Guard: if all cash flows are non-positive or non-negative, no IRR exists
    const hasPos = cashFlows.some(v => v > 0);
    const hasNeg = cashFlows.some(v => v < 0);
    if (!hasPos || !hasNeg) return null;

    function npvAtRate(rate) {
      let s = 0;
      for (let i = 0; i < cashFlows.length; i++) {
        s += cashFlows[i] / Math.pow(1 + rate, i);
      }
      return s;
    }

    let lo = -0.99, hi = 5;
    let fLo = npvAtRate(lo);
    let fHi = npvAtRate(hi);

    // Handle NaN/Infinity from extreme discount rates
    if (!isFinite(fLo) || !isFinite(fHi)) return null;

    // If same sign at both ends, no root in interval
    if (fLo * fHi > 0) return null;

    for (let iter = 0; iter < 200; iter++) {
      const mid = (lo + hi) / 2;
      const fMid = npvAtRate(mid);
      if (!isFinite(fMid)) return null;
      if (Math.abs(fMid) < 1e-8) return mid;
      if (fLo * fMid < 0) { hi = mid; }
      else { lo = mid; fLo = fMid; }
    }
    return (lo + hi) / 2;
  }

  /* ---- Formatting helpers ---- */
  function fmtEur(n) {
    return "€" + Math.round(n).toLocaleString("en-US");
  }

  function fmtPct(n) {
    return (n * 100).toFixed(2) + "%";
  }

  return { calculateInvestment, calculateMonthlyCashFlow, fmtEur, fmtPct };
})();
