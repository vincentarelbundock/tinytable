// TinyTable JavaScript Framework
// Function factory for creating table-specific operations
window.TinyTable = {
  createTableFunctions: function (tableId) {
    const spans = []; // {top, left, rowspan, colspan}

    function resolveToAnchor(i, j) {
      // find if (i,j) is inside any span; return that span's anchor if so
      for (const s of spans) {
        if (
          i >= s.top && i < s.top + s.rowspan &&
          j >= s.left && j < s.left + s.colspan
        ) {
          return { i: s.top, j: s.left };
        }
      }
      return { i, j };
    }

    return {
      styleCell: function (i, j, css_id) {
        // normalize possible string indices
        i = +i; j = +j;

        const table = document.getElementById(tableId);
        const { i: ai, j: aj } = resolveToAnchor(i, j);
        const cell = table.querySelector(`[data-row="${ai}"][data-col="${aj}"]`);
        if (!cell) return; // silently skip if not found (e.g., removed)
        cell.classList.add(css_id);
      },

      spanCell: function (i, j, rowspan, colspan) {
        i = +i; j = +j;

        const table = document.getElementById(tableId);
        const targetCell = table.querySelector(`[data-row="${i}"][data-col="${j}"]`);
        if (!targetCell) {
          console.warn(`Cell at (${i}, ${j}) not found.`);
          return;
        }

        // record span so future styleCell calls map to the anchor
        spans.push({ top: i, left: j, rowspan, colspan });

        // remove covered cells (except anchor)
        const cellsToRemove = [];
        for (let r = 0; r < rowspan; r++) {
          for (let c = 0; c < colspan; c++) {
            if (r === 0 && c === 0) continue;
            const cell = table.querySelector(`[data-row="${i + r}"][data-col="${j + c}"]`);
            if (cell) cellsToRemove.push(cell);
          }
        }
        cellsToRemove.forEach(cell => cell.remove());

        // set HTML attributes for clarity/serialization
        targetCell.setAttribute('rowspan', rowspan);
        targetCell.setAttribute('colspan', colspan);
      }
    };
  }
};

// =============================================================================
// TABULATOR CUSTOM FORMATTERS
// =============================================================================

// Custom SVG sparkline formatter
function tinytable_sparkline(cell, p = {}) {
  const values = cell.getValue() || [];
  if (!values.length) return "";
  const w = p.width ?? 120, h = p.height ?? 30, pad = 2;
  const color = p.color ?? "currentColor", sw = p.strokeWidth ?? 1.5;
  const fill = p.fillArea ?? false;
  const n = values.length, min = Math.min(...values), max = Math.max(...values);
  const span = (max - min) || 1;
  const x = i => pad + (i * (w - 2 * pad)) / ((n - 1) || 1);
  const y = v => h - pad - ((v - min) * (h - 2 * pad)) / span;
  let d = `M ${x(0)},${y(values[0])}`;
  for (let i = 1; i < n; i++) d += ` L ${x(i)},${y(values[i])}`;
  const NS = "http://www.w3.org/2000/svg";
  const svg = document.createElementNS(NS, "svg");
  svg.setAttribute("viewBox", `0 0 ${w} ${h}`);
  svg.setAttribute("width", w);
  svg.setAttribute("height", h);
  const path = document.createElementNS(NS, "path");
  if (fill) {
    path.setAttribute("d", d + ` L ${x(n - 1)},${h - pad} L ${x(0)},${h - pad} Z`);
    path.setAttribute("fill", color);
    path.setAttribute("fill-opacity", "0.3");
  } else {
    path.setAttribute("d", d);
    path.setAttribute("fill", "none");
  }
  path.setAttribute("stroke", color);
  path.setAttribute("stroke-width", sw);
  svg.appendChild(path);
  if (!fill) {
    const dot = document.createElementNS(NS, "circle");
    dot.setAttribute("cx", x(n - 1));
    dot.setAttribute("cy", y(values[n - 1]));
    dot.setAttribute("r", 2.2);
    dot.setAttribute("fill", color);
    svg.appendChild(dot);
  }
  return svg;
}

// Custom histogram formatter using canvas
function tinytable_histogram(cell, p = {}) {
  const data = cell.getValue() || [];
  const color = p.color || "black";
  const width = p.width || 120;
  const height = p.height || 30;

  if (!data || data.length === 0) return "";

  // Calculate histogram bins
  const numBins = Math.min(10, Math.floor(data.length / 2));
  const min = Math.min(...data);
  const max = Math.max(...data);
  const binWidth = (max - min) / numBins;

  const bins = new Array(numBins).fill(0);
  for (let val of data) {
    let binIndex = Math.floor((val - min) / binWidth);
    if (binIndex === numBins) binIndex--; // Edge case for max value
    bins[binIndex]++;
  }

  const maxCount = Math.max(...bins);

  // Create canvas
  const canvas = document.createElement("canvas");
  canvas.width = width;
  canvas.height = height;
  const ctx = canvas.getContext("2d");

  // Draw histogram
  ctx.fillStyle = color;
  const barWidth = width / numBins;

  for (let i = 0; i < numBins; i++) {
    const barHeight = (bins[i] / maxCount) * height;
    ctx.fillRect(
      i * barWidth,
      height - barHeight,
      barWidth - 1,
      barHeight
    );
  }

  return canvas;
}
