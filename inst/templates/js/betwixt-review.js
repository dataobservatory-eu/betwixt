(() => {
  const rows = [...document.querySelectorAll("tbody tr")];

  function outcome(row) {
    const cells = [...row.querySelectorAll(".semantic-cell")];

    if (cells.some(cell => cell.dataset.qualification === "reject")) {
      return "reject";
    }

    if (cells.some(cell => cell.dataset.qualification === "defer")) {
      return "defer";
    }

    return "accept";
  }

  function paint(row) {
    row.dataset.outcome = outcome(row);

    const mark = row.querySelector(".finalise-mark");

    if (row.dataset.finalised !== "true") {
      mark.textContent = "";
      return;
    }

    if (row.dataset.outcome === "reject") {
      mark.textContent = "×";
    } else if (row.dataset.outcome === "defer") {
      mark.textContent = "○";
    } else {
      mark.textContent = "✓";
    }
  }

  function summary() {
    const finalised = rows.filter(
      row => row.dataset.finalised === "true"
    ).length;

    const deferred = document.querySelectorAll(
      '.semantic-cell[data-qualification="defer"]'
    ).length;

    const rejected = document.querySelectorAll(
      '.semantic-cell[data-qualification="reject"]'
    ).length;

    document.getElementById("summary").textContent =
      `${finalised} finalised · ${deferred} deferred cells · ` +
      `${rejected} rejected cells`;
  }

  function qualify(cell, state) {
    cell.dataset.qualification = state;
    paint(cell.closest("tr"));
    summary();
  }

  rows.forEach(row => {
    row.querySelectorAll("[data-qualify]").forEach(button => {
      button.addEventListener("click", () => {
        const cell = button.closest(".semantic-cell");
        const qualification = button.dataset.qualify;
        const current = cell.dataset.qualification;

        qualify(
          cell,
          current === qualification ? "none" : qualification
        );
      });
    });

    row.querySelectorAll(".candidate-select").forEach(select => {
      const cell = select.closest(".semantic-cell");
      const writeIn = cell.querySelector(".write-in");

      const updateWriteIn = () => {
        const show = select.value === "__other__";

        writeIn.style.display = show ? "block" : "none";

        if (!show) {
          writeIn.value = "";
        }

        if (show) {
          writeIn.focus();
        }
      };

      select.addEventListener("change", updateWriteIn);
      updateWriteIn();
    });

    const check = row.querySelector(".finalise-check");

    check.addEventListener("change", () => {
      row.dataset.finalised = check.checked ? "true" : "false";
      paint(row);
      summary();
    });

    const create = row.querySelector(".create-item");

    if (create) {
      create.addEventListener("click", () => {
        const cell = create.closest(".semantic-cell");
        const input = cell.querySelector(".subject-input");

        cell.innerHTML =
          `<a class="entity-link" href="#">` +
          `fuds:QNEW${row.dataset.row}</a>` +
          `<div style="margin-top:6px;font-size:12px;color:#666">` +
          `Prototype registration for “${input.value}”</div>` +
          `<div class="qualify">` +
          `<button data-qualify="defer">Defer</button>` +
          `<button data-qualify="reject">Reject</button>` +
          `</div>`;

        cell.querySelectorAll("[data-qualify]").forEach(button => {
          button.addEventListener("click", () => {
            const qualification = button.dataset.qualify;
            const current = cell.dataset.qualification;

            qualify(
              cell,
              current === qualification ? "none" : qualification
            );
          });
        });
      });
    }
  });

  rows.forEach(paint);
  summary();

  const startedAt = document.getElementById("review-started-at");

  if (startedAt && !startedAt.value) {
    startedAt.value = new Date().toISOString();
  }

  function persistStateIntoClone(clone) {
    const sourceInputs = [...document.querySelectorAll("input")];
    const clonedInputs = [...clone.querySelectorAll("input")];

    sourceInputs.forEach((source, index) => {
      const target = clonedInputs[index];

      if (!target) {
        return;
      }

      target.setAttribute("value", source.value);

      if (source.type === "checkbox") {
        if (source.checked) {
          target.setAttribute("checked", "");
        } else {
          target.removeAttribute("checked");
        }
      }
    });

    const sourceTextareas = [...document.querySelectorAll("textarea")];
    const clonedTextareas = [...clone.querySelectorAll("textarea")];

    sourceTextareas.forEach((source, index) => {
      if (clonedTextareas[index]) {
        clonedTextareas[index].textContent = source.value;
      }
    });

    const sourceSelects = [...document.querySelectorAll("select")];
    const clonedSelects = [...clone.querySelectorAll("select")];

    sourceSelects.forEach((source, index) => {
      const target = clonedSelects[index];

      if (!target) {
        return;
      }

      [...target.options].forEach(option => {
        if (option.value === source.value) {
          option.setAttribute("selected", "");
        } else {
          option.removeAttribute("selected");
        }
      });
    });

    const sourceCells = [
      ...document.querySelectorAll(".semantic-cell")
    ];
    const clonedCells = [
      ...clone.querySelectorAll(".semantic-cell")
    ];

    sourceCells.forEach((source, index) => {
      if (clonedCells[index]) {
        clonedCells[index].dataset.qualification =
          source.dataset.qualification;
      }
    });

    const sourceRows = [...document.querySelectorAll("tbody tr")];
    const clonedRows = [...clone.querySelectorAll("tbody tr")];

    sourceRows.forEach((source, index) => {
      const target = clonedRows[index];

      if (!target) {
        return;
      }

      target.dataset.finalised =
        source.dataset.finalised || "false";

      target.dataset.outcome =
        source.dataset.outcome || "accept";
    });
  }

  function saveReview(finaliseReview) {
    const now = new Date().toISOString();
    const sequenceInput =
      document.getElementById("review-sequence");

    const filenameStem =
      document.getElementById("filename-stem").value.trim() ||
      "betwixt-review";

    let sequence = Number(sequenceInput.value || 0);

    // The first browser save starts review sequence 1.
    if (sequence === 0) {
      sequence = 1;
      sequenceInput.value = sequence;
    }

    document.getElementById("review-last-saved-at").value = now;

    document.getElementById("review-status").value =
      finaliseReview ? "finalised" : "in-progress";

    if (finaliseReview) {
      document.getElementById("review-ended-at").value = now;
    }

    // Preserve the updated state in the downloaded HTML.
    const clone = document.documentElement.cloneNode(true);
    persistStateIntoClone(clone);

    // Draft and finalised files belong to the same review sequence.
    const stem = `${filenameStem}_${sequence}`;
    const suffix = finaliseReview ? "-finalised" : "-draft";
    const filename = `${stem}${suffix}.html`;

    const blob = new Blob(
      ["<!doctype html>\n" + clone.outerHTML],
      { type: "text/html;charset=utf-8" }
    );

    const url = URL.createObjectURL(blob);
    const link = document.createElement("a");

    link.href = url;
    link.download = filename;

    document.body.appendChild(link);
    link.click();
    link.remove();

    URL.revokeObjectURL(url);

    document.getElementById("save-status").textContent =
      (finaliseReview
        ? "Finalised review saved at "
        : "Draft saved at ") + now;
  }

  document
    .getElementById("save-draft")
    .addEventListener("click", () => saveReview(false));

  document
    .getElementById("save-final")
    .addEventListener("click", () => saveReview(true));
})();

