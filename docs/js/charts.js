/**
 * Plotly chart rendering.
 */

const Charts = (() => {

  /**
   * Yearly cash flow bar chart (Tab 1).
   */
  function renderYearlyCashFlow(containerId, cashFlows) {
    const years = cashFlows.map((_, i) => i);
    const colors = cashFlows.map(v => v >= 0 ? "#00a65a" : "#dd4b39");

    const data = [{
      x: years,
      y: cashFlows,
      type: "bar",
      marker: { color: colors },
      hovertemplate: "Year %{x}<br>€%{y:,.0f}<extra></extra>"
    }];

    const layout = {
      title: "Projected Cash Flows by Year",
      xaxis: { title: "Year", dtick: 1 },
      yaxis: { title: "Cash Flow (€)", tickformat: ",d", tickprefix: "€" },
      hovermode: "x",
      margin: { t: 50, b: 50, l: 70, r: 30 },
      paper_bgcolor: "rgba(0,0,0,0)",
      plot_bgcolor: "rgba(0,0,0,0)"
    };

    Plotly.newPlot(containerId, data, layout, { responsive: true });
  }

  /**
   * Monthly cash flow bar chart (Tab 2).
   */
  function renderMonthlyCashFlow(containerId, labels, cashFlowVec) {
    const colors = cashFlowVec.map(v => v >= 0 ? "#00a65a" : "#dd4b39");

    const data = [{
      x: labels,
      y: cashFlowVec,
      type: "bar",
      marker: { color: colors },
      hovertemplate: "%{x}<br>€%{y:,.0f}<extra></extra>"
    }];

    const layout = {
      title: "Projected Monthly Cash Flow",
      xaxis: { title: "Month", tickangle: -45 },
      yaxis: { title: "Cash Flow (€)", tickformat: ",d", tickprefix: "€" },
      hovermode: "x",
      margin: { t: 50, b: 100, l: 70, r: 30 },
      height: 600,
      paper_bgcolor: "rgba(0,0,0,0)",
      plot_bgcolor: "rgba(0,0,0,0)"
    };

    Plotly.newPlot(containerId, data, layout, { responsive: true });
  }

  return { renderYearlyCashFlow, renderMonthlyCashFlow };
})();
