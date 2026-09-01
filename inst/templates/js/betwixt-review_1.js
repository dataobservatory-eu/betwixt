(() => {
  const rows = [...document.querySelectorAll("tbody tr")];
  const wrap = document.querySelector(".table-wrap");
  const scrollControl = document.getElementById("review-scroll-range");

  /*
   * There is only one real horizontal scroll position:
   * table-wrap.scrollLeft.
   *
   * The sticky top range control mirrors and controls that value.
   */
  function updateScrollControl() {
    if (!wrap || !scrollControl) return;

    const max = Math.max(0, wrap.scrollWidth - wrap.clientWidth);

    scrollControl.max = String(max);
    scrollControl.value = String(Math.min(wrap.scrollLeft, max));
    scrollControl.disabled = max === 0;
  }

  if (wrap && scrollControl) {
    scrollControl.min = "0";
    scrollControl.step = "1";

    updateScrollControl();

    scrollControl.addEventListener("input", () => {
      wrap.scrollLeft = Number(scrollControl.value);
    });

    wrap.addEventListener(
      "scroll",
      () => {
        scrollControl.value = String(wrap.scrollLeft);
      },
      { passive: true }
    );

    addEventListener("resize", updateScrollControl);
  }

  function outcome(row) {
    const cells = [...row.querySelectorAll(".semantic-cell")];

    if (cells.some(c => c.dataset.qualification === "reject")) {
      return "reject";
    }

    if (cells.some(c => c.dataset.qualification === "defer")) {
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

    mark.textContent =
      row.dataset.outcome === "reject"
        ? "×"
        : row.dataset.outcome === "defer"
          ? "○"
          : "✓";
  }

  function summary() {
    const f = rows.filter(r => r.dataset.finalised === "true").length;

    const d = document.querySelectorAll(
      '.semantic-cell[data-qualification="defer"]'
    ).length;

    const x = document.querySelectorAll(
      '.semantic-cell[data-qualification="reject"]'
    ).length;

    document.getElementById("summary").textContent =
      `${f} finalised · ${d} deferred cells · ${x} rejected cells`;
  }

  function qualify(cell, state) {
    cell.dataset.qualification = state;
    paint(cell.closest("tr"));
    summary();
  }

  rows.forEach(row => {
    row.querySelectorAll("[data-qualify]").forEach(b =>
      b.addEventListener("click", () => {
        const cell = b.closest(".semantic-cell");
        const q = b.dataset.qualify;

        qualify(
          cell,
          cell.dataset.qualification === q ? "none" : q
        );
      })
    );

    const heritage = row.querySelector(".heritage-select");
    const hw = row.querySelector(".heritage-write-in");

    heritage.addEventListener("change", () => {
      hw.style.display =
        heritage.value === "__other__" ? "block" : "none";

      if (heritage.value === "__unknown__") {
        qualify(
          heritage.closest(".semantic-cell"),
          "defer"
        );
      } else if (heritage.value === "__none__") {
        qualify(
          heritage.closest(".semantic-cell"),
          "reject"
        );
      } else {
        qualify(
          heritage.closest(".semantic-cell"),
          "none"
        );
      }

      if (heritage.value === "__other__") {
        hw.focus();
      }
    });

    const inst = row.querySelector(".instance-select");
    const iw = inst.parentElement.querySelector(".write-in");

    inst.addEventListener("change", () => {
      iw.style.display =
        inst.value === "__other__" ? "block" : "none";

      if (inst.value === "__other__") {
        iw.focus();
      }
    });

    const check = row.querySelector(".finalise-check");

    check.addEventListener("change", () => {
      row.dataset.finalised =
        check.checked ? "true" : "false";

      paint(row);
      summary();
    });

    const create = row.querySelector(".create-item");

    if (create) {
      create.addEventListener("click", () => {
        const cell = create.closest(".subject");
        const input = cell.querySelector(".subject-input");

        cell.innerHTML =
          `<a class="entity-link" href="#">fuds:QNEW${row.dataset.row}</a>` +
          `<div style="margin-top:6px;font-size:12px;color:#666">` +
          `Prototype registration for “${input.value}”` +
          `</div>` +
          `<div class="qualify">` +
          `<button data-qualify="defer">Defer</button>` +
          `<button data-qualify="reject">Reject</button>` +
          `</div>`;

        cell.querySelectorAll("[data-qualify]").forEach(b =>
          b.addEventListener("click", () => {
            const q = b.dataset.qualify;

            qualify(
              cell,
              cell.dataset.qualification === q
                ? "none"
                : q
            );
          })
        );
      });
    }
  });

  rows.forEach(paint);
  summary();

  const startedAt =
    document.getElementById("review-started-at");

  if (startedAt && !startedAt.value) {
    startedAt.value = new Date().toISOString();
  }

  function persistStateIntoClone(clone) {
    const si = [...document.querySelectorAll("input")];
    const di = [...clone.querySelectorAll("input")];

    si.forEach((x, i) => {
      const y = di[i];

      if (!y) return;

      y.setAttribute("value", x.value);

      if (x.type === "checkbox") {
        if (x.checked) {
          y.setAttribute("checked", "");
        } else {
          y.removeAttribute("checked");
        }
      }
    });

    const st = [...document.querySelectorAll("textarea")];
    const dt = [...clone.querySelectorAll("textarea")];

    st.forEach((x, i) => {
      if (dt[i]) {
        dt[i].textContent = x.value;
      }
    });

    const ss = [...document.querySelectorAll("select")];
    const ds = [...clone.querySelectorAll("select")];

    ss.forEach((x, i) => {
      if (!ds[i]) return;

      [...ds[i].options].forEach(o =>
        o.value === x.value
          ? o.setAttribute("selected", "")
          : o.removeAttribute("selected")
      );
    });

    const sc = [
      ...document.querySelectorAll(".semantic-cell")
    ];

    const dc = [
      ...clone.querySelectorAll(".semantic-cell")
    ];

    sc.forEach((x, i) => {
      if (dc[i]) {
        dc[i].dataset.qualification =
          x.dataset.qualification;
      }
    });

    const sr = [
      ...document.querySelectorAll("tbody tr")
    ];

    const dr = [
      ...clone.querySelectorAll("tbody tr")
    ];

    sr.forEach((x, i) => {
      if (!dr[i]) return;

      dr[i].dataset.finalised =
        x.dataset.finalised || "false";

      dr[i].dataset.outcome =
        x.dataset.outcome || "accept";
    });
  }

  function saveReview(finaliseReview) {
    const now = new Date().toISOString();

    document.getElementById(
      "review-last-saved-at"
    ).value = now;

    document.getElementById(
      "review-status"
    ).value = finaliseReview
      ? "finalised"
      : "in-progress";

    if (finaliseReview) {
      document.getElementById(
        "review-ended-at"
      ).value = now;
    }

    const clone =
      document.documentElement.cloneNode(true);

    persistStateIntoClone(clone);

    const suffix = finaliseReview
      ? "-betwixt-finalised.html"
      : "-betwixt-draft.html";

    const blob = new Blob(
      ["<!doctype html>\n" + clone.outerHTML],
      { type: "text/html;charset=utf-8" }
    );

    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");

    a.href = url;
    a.download = "delini_wide" + suffix;

    document.body.appendChild(a);
    a.click();
    a.remove();

    URL.revokeObjectURL(url);

    document.getElementById(
      "save-status"
    ).textContent =
      (finaliseReview
        ? "Finalised review saved at "
        : "Draft saved at ") + now;
  }

  document
    .getElementById("save-draft")
    .addEventListener(
      "click",
      () => saveReview(false)
    );

  document
    .getElementById("save-final")
    .addEventListener(
      "click",
      () => saveReview(true)
    );
})();
