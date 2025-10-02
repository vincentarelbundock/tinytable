// TinyTable JavaScript Framework
// Function factory for creating table-specific operations
window.TinyTable = {
  // Helper to create Tabulator instance with Luxon dependency
  createTabulator: function(elementId, config) {
    const { DateTime } = window.luxon || {};
    return new Tabulator(elementId, {
      dependencies: { DateTime },
      ...config
    });
  },

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
// TABULATOR CUSTOM FORMATTERS (revised)
// =============================================================================

// SVG sparkline
function tinytable_sparkline(cell, p = {}) {
  let values = cell.getValue() ?? [];
  if (!Array.isArray(values)) values = [values];
  // coerce to finite numbers & strip NaNs
  values = values.map(Number).filter(Number.isFinite);
  const n = values.length;
  if (n === 0) return "";

  const w = p.width ?? 120, h = p.height ?? 30, pad = p.pad ?? 2;
  const color = p.color ?? "currentColor", sw = p.strokeWidth ?? 1.5;
  const fill = p.fillArea ?? false;
  const showDots = p.showDots ?? false;
  const dotR = p.dotRadius ?? 1.75;
  const ariaLabel = p.ariaLabel ?? `sparkline with ${n} points`;

  // single pass for min/max
  let min = Infinity, max = -Infinity;
  for (let v of values) { if (v < min) min = v; if (v > max) max = v; }
  const span = (max - min) || 1;

  const x = i => pad + (i * (w - 2 * pad)) / Math.max(n - 1, 1);
  const y = v => h - pad - ((v - min) * (h - 2 * pad)) / span;

  let d = `M ${x(0)},${y(values[0])}`;
  for (let i = 1; i < n; i++) d += ` L ${x(i)},${y(values[i])}`;

  const NS = "http://www.w3.org/2000/svg";
  const svg = document.createElementNS(NS, "svg");
  svg.setAttribute("viewBox", `0 0 ${w} ${h}`);
  svg.setAttribute("width", w);
  svg.setAttribute("height", h);
  svg.setAttribute("role", "img");
  svg.setAttribute("aria-label", ariaLabel);

  const path = document.createElementNS(NS, "path");
  path.setAttribute("stroke", color);
  path.setAttribute("stroke-width", sw);
  path.setAttribute("vector-effect", "non-scaling-stroke");

  if (fill) {
    const area = document.createElementNS(NS, "path");
    area.setAttribute("d", d + ` L ${x(n - 1)},${h - pad} L ${x(0)},${h - pad} Z`);
    area.setAttribute("fill", color);
    area.setAttribute("fill-opacity", p.fillOpacity ?? 0.25);
    svg.appendChild(area);
    path.setAttribute("d", d);
    path.setAttribute("fill", "none");
  } else {
    path.setAttribute("d", d);
    path.setAttribute("fill", "none");
  }
  svg.appendChild(path);

  if (showDots) {
    const dot = (i) => {
      const c = document.createElementNS(NS, "circle");
      c.setAttribute("cx", x(i));
      c.setAttribute("cy", y(values[i]));
      c.setAttribute("r", dotR);
      c.setAttribute("fill", color);
      return c;
    };
    if (p.dots === "end") svg.appendChild(dot(n - 1));
    else if (p.dots === "minmax") {
      const iMin = values.indexOf(min), iMax = values.indexOf(max);
      svg.appendChild(dot(iMin)); svg.appendChild(dot(iMax));
    } else if (p.dots === true) {
      for (let i = 0; i < n; i++) svg.appendChild(dot(i));
    }
  }

  if (p.title) {
    const title = document.createElementNS(NS, "title");
    title.textContent = p.title;
    svg.appendChild(title);
  }

  return svg;
}

// Canvas histogram
function tinytable_histogram(cell, p = {}) {
  let data = cell.getValue() ?? [];
  if (!Array.isArray(data)) data = [data];
  data = data.map(Number).filter(Number.isFinite);
  const n = data.length;
  if (n === 0) return "";

  const cssWidth = p.width ?? 120;
  const cssHeight = p.height ?? 30;
  const color = p.color ?? "currentColor";
  const gap = p.gap ?? 1;         // px gap between bars
  const pad = p.pad ?? 0;         // inner padding
  const ariaLabel = p.ariaLabel ?? `histogram with ${n} observations`;

  // robust bins: Sturges as default; allow override
  const k = Math.max(1, Math.min(64, p.bins ?? Math.ceil(Math.log2(n) + 1)));

  // stats
  let min = Infinity, max = -Infinity;
  for (let v of data) { if (v < min) min = v; if (v > max) max = v; }

  // zero-range safeguard -> single centered bar
  const range = max - min;
  const binWidth = range > 0 ? range / k : 1;

  const bins = new Array(k).fill(0);
  if (range === 0) {
    // put all mass in the middle bin
    bins[Math.floor(k / 2)] = n;
  } else {
    for (let v of data) {
      let idx = Math.floor((v - min) / binWidth);
      if (idx >= k) idx = k - 1; // clamp max edge
      bins[idx]++;
    }
  }
  const maxCount = Math.max(...bins, 1);

  // HiDPI / Retina crispness
  const dpr = window.devicePixelRatio || 1;
  const canvas = document.createElement("canvas");
  canvas.width = Math.round(cssWidth * dpr);
  canvas.height = Math.round(cssHeight * dpr);
  canvas.style.width = cssWidth + "px";
  canvas.style.height = cssHeight + "px";
  canvas.setAttribute("role", "img");
  canvas.setAttribute("aria-label", ariaLabel);

  const ctx = canvas.getContext("2d");
  ctx.scale(dpr, dpr);
  ctx.clearRect(0, 0, cssWidth, cssHeight);
  ctx.fillStyle = color;

  const innerW = cssWidth - pad * 2;
  const innerH = cssHeight - pad * 2;
  const barW = innerW / k;

  for (let i = 0; i < k; i++) {
    const h = (bins[i] / maxCount) * innerH;
    const x = pad + i * barW;
    const y = cssHeight - pad - h;
    // 0.5 translate helps crisp vertical edges on some displays
    ctx.fillRect(Math.round(x) + 0.5, Math.floor(y), Math.max(0, barW - gap), Math.ceil(h));
  }

  if (p.title) canvas.title = p.title;
  return canvas;
}

// Unified sorter that uses hidden rank field
function tinytable_rank_sorter(a, b, aRow, bRow, column, dir, sorterParams) {
  const rankField = sorterParams.rankField;
  const aVal = aRow.getData()[rankField] || 0;
  const bVal = bRow.getData()[rankField] || 0;
  return aVal - bVal;
}
