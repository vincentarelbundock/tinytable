// TinyTable JavaScript Framework
// Function factory for creating table-specific operations
window.TinyTable = {
  createTableFunctions: function (tableId) {
    const spans = []; // {top, left, rowspan, colspan}

    function resolveToAnchor(i, j) {
      // find if (i,j) is inside any span; return that span’s anchor if so
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
