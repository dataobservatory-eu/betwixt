/*
 * Review provenance
 */

const startedAt =
  document.getElementById("review-started-at");

if (!startedAt.value) {
  startedAt.value = new Date().toISOString();
}

const sourceFilenameField =
  document.getElementById("review-source-filename");

if (!sourceFilenameField.value) {
  const pathname = window.location.pathname;

  const sourceFilename =
    decodeURIComponent(
      pathname.substring(pathname.lastIndexOf("/") + 1)
    ) || "review.html";

  sourceFilenameField.value = sourceFilename;
}


/*
 * Review interaction
 */

document
  .querySelectorAll(".review-cell")
  .forEach(cell => {
    const select = cell.querySelector(".candidate-select");
    const state = cell.querySelector(".review-state");
    const otherProposal = cell.querySelector(".other-proposal");
    const rangeLink = cell.querySelector(".range-link");
    const defer = cell.querySelector(".defer-action");
    const reject = cell.querySelector(".reject-action");

    function clearStateClasses() {
      cell.classList.remove(
        "is-modified",
        "is-deferred",
        "is-rejected"
      );
    }

    function updateRangeLink() {
      const option = select.options[select.selectedIndex];
      const url = option ? option.dataset.url : "";

      if (url) {
        rangeLink.href = url;
        rangeLink.style.display = "inline-block";
      } else {
        rangeLink.removeAttribute("href");
        rangeLink.style.display = "none";
      }
    }

    function updateOtherProposal() {
      if (select.value === "__other__") {
        otherProposal.style.display = "block";
      } else {
        otherProposal.style.display = "none";
      }
    }

    defer.addEventListener("click", function () {
      clearStateClasses();
      state.value = "defer";
      cell.classList.add("is-deferred");
    });

    reject.addEventListener("click", function () {
      clearStateClasses();
      state.value = "reject";
      cell.classList.add("is-rejected");
    });

    select.addEventListener("change", function () {
      state.value = "accept";
      clearStateClasses();
      cell.classList.add("is-modified");
      updateOtherProposal();
      updateRangeLink();

      if (this.value === "__other__") {
        const input = otherProposal.querySelector("input");
        if (input) input.focus();
      }
    });

    updateOtherProposal();
    updateRangeLink();
  });


/*
 * Open original evidence source
 */

function openSource(url) {
  window.open(
    url,
    "betwixt-source",
    "width=1100,height=800,resizable=yes,scrollbars=yes"
  );
}

document
  .querySelectorAll(".view-source")
  .forEach(button => {
    button.addEventListener("click", function () {
      openSource(this.dataset.url);
    });
  });

document
  .querySelectorAll(".review-thumbnail")
  .forEach(image => {
    image.addEventListener("click", function () {
      openSource(this.dataset.url);
    });
  });


/*
 * Save reviewed HTML
 */

function reviewedFilename(sourceFilename) {
  if (sourceFilename.toLowerCase().endsWith(".html")) {
    return sourceFilename.slice(0, -5) +
      "-betwixt-reviewed.html";
  }

  if (sourceFilename.toLowerCase().endsWith(".htm")) {
    return sourceFilename.slice(0, -4) +
      "-betwixt-reviewed.html";
  }

  return sourceFilename + "-betwixt-reviewed.html";
}

document
  .getElementById("save-review")
  .addEventListener("click", saveReview);

function saveReview() {
  document
    .getElementById("review-saved-at")
    .value = new Date().toISOString();

  const sourceFilename =
    document
      .getElementById("review-source-filename")
      .value;

  const outputFilename =
    reviewedFilename(sourceFilename);

  document
    .getElementById("review-saved-filename")
    .value = outputFilename;

  const clone =
    document.documentElement.cloneNode(true);

  const originalInputs =
    document.querySelectorAll("input");

  const clonedInputs =
    clone.querySelectorAll("input");

  originalInputs.forEach((input, index) => {
    const clonedInput = clonedInputs[index];

    clonedInput.setAttribute("value", input.value);

    if (input.type === "radio") {
      if (input.checked) {
        clonedInput.setAttribute("checked", "");
      } else {
        clonedInput.removeAttribute("checked");
      }
    }
  });

  const originalTextareas =
    document.querySelectorAll("textarea");

  const clonedTextareas =
    clone.querySelectorAll("textarea");

  originalTextareas.forEach((textarea, index) => {
    clonedTextareas[index].textContent = textarea.value;
  });

  const originalSelects =
    document.querySelectorAll("select");

  const clonedSelects =
    clone.querySelectorAll("select");

  originalSelects.forEach((select, index) => {
    const clonedSelect = clonedSelects[index];

    Array.from(clonedSelect.options)
      .forEach(option => {
        if (option.value === select.value) {
          option.setAttribute("selected", "");
        } else {
          option.removeAttribute("selected");
        }
      });

    if (select.disabled) {
      clonedSelect.setAttribute("disabled", "");
    } else {
      clonedSelect.removeAttribute("disabled");
    }
  });

  /*
   * Persist review-cell state and proposal visibility.
   */

  const originalCells =
    document.querySelectorAll(".review-cell");

  const clonedCells =
    clone.querySelectorAll(".review-cell");

  originalCells.forEach((cell, index) => {
    clonedCells[index].className = cell.className;
  });

  const originalProposals =
    document.querySelectorAll(".other-proposal");

  const clonedProposals =
    clone.querySelectorAll(".other-proposal");

  originalProposals.forEach((proposal, index) => {
    clonedProposals[index].style.display =
      proposal.style.display;
  });

  const originalRangeLinks =
    document.querySelectorAll(".range-link");

  const clonedRangeLinks =
    clone.querySelectorAll(".range-link");

  originalRangeLinks.forEach((link, index) => {
    clonedRangeLinks[index].style.display =
      link.style.display;

    if (link.getAttribute("href")) {
      clonedRangeLinks[index].setAttribute(
        "href",
        link.getAttribute("href")
      );
    } else {
      clonedRangeLinks[index].removeAttribute("href");
    }
  });

  const html =
    "<!DOCTYPE html>\n" +
    clone.outerHTML;

  const blob =
    new Blob(
      [html],
      { type: "text/html;charset=utf-8" }
    );

  const url =
    URL.createObjectURL(blob);

  const download =
    document.createElement("a");

  download.href = url;
  download.download = outputFilename;

  document.body.appendChild(download);
  download.click();
  document.body.removeChild(download);

  URL.revokeObjectURL(url);
}
