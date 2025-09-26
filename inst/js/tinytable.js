// TinyTable JavaScript Framework
// Function factory for creating table-specific operations

window.TinyTable = {
    createTableFunctions: function(tableId) {
        return {
            styleCell: function(i, j, css_id) {
                var table = document.getElementById(tableId);
                var cell = table.querySelector(`[data-row="${i}"][data-col="${j}"]`);
                if (cell) {
                    console.log(`Styling cell at (${i}, ${j}) with class ${css_id}`);
                    cell.classList.add(css_id);
                } else {
                    console.warn(`Cell at (${i}, ${j}) not found.`);
                }
            },
            spanCell: function(i, j, rowspan, colspan) {
                var table = document.getElementById(tableId);
                const targetCell = table.querySelector(`[data-row="${i}"][data-col="${j}"]`);
                if (!targetCell) {
                    console.warn(`Cell at (${i}, ${j}) not found.`);
                }

                // Get all cells that need to be removed
                const cellsToRemove = [];
                for (let r = 0; r < rowspan; r++) {
                    for (let c = 0; c < colspan; c++) {
                        if (r === 0 && c === 0) continue; // Skip the target cell
                        const cell = table.querySelector(`[data-row="${i + r}"][data-col="${j + c}"]`);
                        if (cell) {
                            cellsToRemove.push(cell);
                        }
                    }
                }

                // Remove all cells
                cellsToRemove.forEach(cell => cell.remove());

                // Set rowspan and colspan of the target cell if it exists
                if (targetCell) {
                    targetCell.rowSpan = rowspan;
                    targetCell.colSpan = colspan;
                }
            }
        };
    }
};