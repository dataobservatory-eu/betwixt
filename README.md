
<!-- README.md is generated from README.Rmd. Please edit that file -->

# betwixt

<!-- badges: start -->

[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![devel-version](https://img.shields.io/badge/devel%20version-0.0.4-blue.svg)](https://github.com/dataobservatory-eu/fscontext)
[![dataobservatory](https://img.shields.io/badge/ecosystem-dataobservatory.eu-3EA135.svg)](https://dataobservatory.eu/)

<!-- badges: end -->

Betwixt is a lightweight framework for representing semantic assertions
as scoped claims and rendering them for human review. It provides a
pragmatic review layer between observations and semantic objects,
allowing candidate claims to be stabilised through reproducible review
workflows.

Betwixt defines a portable tidy claim schema. Implementations reuse
their native tabular environments, such as tibble, pandas, or SQLite,
while review is rendered through standard HTML/CSS and returned as tidy
CSV.

## Installation

You can install the development version of betwixt from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("dataobservatory-eu/betwixt")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(betwixt)
ad_gdp_2024 <- claim(
  scope = "country=AD;year=2023",
  subject = "country",
  predicate = "GDP",
  value = "3.73 billion EUR"
)

print(ad_gdp_2024)
#> <claim_df>
#> Claims: 1
#> Scopes: 1
#> Scope: country=AD;year=2023
#> 
#> # A tibble: 1 × 4
#>   scope                subject predicate value           
#>   <chr>                <chr>   <chr>     <chr>           
#> 1 country=AD;year=2023 country GDP       3.73 billion EUR
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
betwixt_render(ad_gdp_2024)
#> [1] "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n  <meta charset=\"utf-8\">\n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\n  <title>Betwixt Review</title>\n\n  <style>\n* {\n  box-sizing: border-box;\n}\n\nbody {\n  margin: 0;\n  font-family: system-ui, -apple-system, BlinkMacSystemFont, \"Segoe UI\",\n    sans-serif;\n  color: #222;\n  background: #fff;\n}\n\n.review {\n  width: 100%;\n  padding: 1.5rem;\n}\n\n.review-header {\n  margin-bottom: 1.5rem;\n}\n\n.review-header h1 {\n  margin: 0 0 0.5rem;\n  font-size: 1.6rem;\n}\n\n.review-description {\n  max-width: 60rem;\n  margin: 0;\n  line-height: 1.5;\n}\n\n.table-scroll-top,\n.table-scroll {\n  width: 100%;\n  overflow-x: auto;\n}\n\n.table-scroll-top {\n  height: 18px;\n  margin-bottom: 0.25rem;\n  overflow-y: hidden;\n}\n\n.table-scroll-top-content {\n  height: 1px;\n}\n\n.review-table {\n  width: max-content;\n  min-width: 100%;\n  border-collapse: collapse;\n  border-spacing: 0;\n}\n\n.review-table th,\n.review-table td {\n  padding: 0.6rem;\n  border: 1px solid #d7d7d7;\n  vertical-align: top;\n  text-align: left;\n}\n\n.review-table th {\n  position: sticky;\n  top: 0;\n  z-index: 2;\n  background: #f5f5f5;\n  font-weight: 600;\n}\n\n.review-table .row-number {\n  position: sticky;\n  left: 0;\n  z-index: 3;\n  width: 3rem;\n  min-width: 3rem;\n  text-align: right;\n  background: #f5f5f5;\n}\n\n.review-table td.row-number {\n  font-variant-numeric: tabular-nums;\n  font-weight: 600;\n}\n\n.evidence-column,\n.evidence-cell {\n  min-width: 9rem;\n}\n\n.evidence-cell {\n  text-align: center;\n}\n\n.evidence-media {\n  display: block;\n  width: auto;\n  max-width: 120px;\n  max-height: 120px;\n  margin: 0 auto 0.5rem;\n  object-fit: contain;\n}\n\n.evidence-linked {\n  cursor: pointer;\n}\n\n.view-source {\n  white-space: nowrap;\n}\n\n.description-column,\n.description-cell {\n  min-width: 13rem;\n}\n\n.assertion-column,\n.review-cell {\n  min-width: 11rem;\n}\n\n.context-column,\n.context-cell {\n  min-width: 11rem;\n}\n\n.description-cell input,\n.description-cell textarea,\n.review-cell input,\n.review-cell select {\n  width: 100%;\n  font: inherit;\n}\n\n.description-cell textarea {\n  min-height: 5rem;\n  resize: vertical;\n}\n\n.candidate-control {\n  display: flex;\n  align-items: center;\n  gap: 0.4rem;\n}\n\n.candidate-control select,\n.candidate-control input {\n  flex: 1 1 auto;\n  min-width: 8rem;\n}\n\n.range-link {\n  flex: 0 0 auto;\n  text-decoration: none;\n  font-size: 1.1rem;\n}\n\n.other-proposal {\n  margin-top: 0.5rem;\n}\n\n.review-actions {\n  display: flex;\n  gap: 0.4rem;\n  margin-top: 0.6rem;\n}\n\n.review-actions button {\n  font: inherit;\n}\n\n.review-cell[data-review-state=\"defer\"] {\n  background: #f7f7f7;\n}\n\n.review-cell[data-review-state=\"reject\"] {\n  background: #eeeeee;\n}\n\n.review-footer {\n  margin-top: 1rem;\n}\n\n.save-review {\n  padding: 0.55rem 1rem;\n  font: inherit;\n}\n\n[hidden] {\n  display: none !important;\n}\n  </style>\n</head>\n\n<body>\n  <main class=\"review\">\n    <header class=\"review-header\">\n      <h1>Betwixt Review</h1>\n\n      <p class=\"review-description\">Please review the following claims.</p>\n    </header>\n\n    <form id=\"betwixt-review\">\n      <input type=\"hidden\" name=\"betwixt_id\" value=\"P1234\">\n      <input type=\"hidden\"\n             name=\"betwixt_generated_at\"\n             value=\"2026-08-18T14:59:56+0200\">\n\n      <div class=\"table-scroll-top\" aria-hidden=\"true\">\n        <div class=\"table-scroll-top-content\"></div>\n      </div>\n\n      <div class=\"table-scroll\">\n        <table class=\"review-table\">\n          <thead>\n            <tr>\n              <th class=\"row-number\">#</th>\n\n\n\n\n            </tr>\n          </thead>\n\n          <tbody>\n            <tr data-row-number=\"1\">\n              <td class=\"row-number\">1</td>\n\n\n\n\n            </tr>\n          </tbody>\n        </table>\n      </div>\n\n      <div class=\"review-footer\">\n        <button type=\"submit\" class=\"save-review\">\n          Save review\n        </button>\n      </div>\n    </form>\n  </main>\n\n  <script>\n/*\n * Review provenance\n */\n\nconst startedAt =\n  document.getElementById(\"review-started-at\");\n\nif (!startedAt.value) {\n  startedAt.value = new Date().toISOString();\n}\n\nconst sourceFilenameField =\n  document.getElementById(\"review-source-filename\");\n\nif (!sourceFilenameField.value) {\n  const pathname = window.location.pathname;\n\n  const sourceFilename =\n    decodeURIComponent(\n      pathname.substring(pathname.lastIndexOf(\"/\") + 1)\n    ) || \"review.html\";\n\n  sourceFilenameField.value = sourceFilename;\n}\n\n\n/*\n * Review interaction\n */\n\ndocument\n  .querySelectorAll(\".review-cell\")\n  .forEach(cell => {\n    const select = cell.querySelector(\".candidate-select\");\n    const state = cell.querySelector(\".review-state\");\n    const otherProposal = cell.querySelector(\".other-proposal\");\n    const rangeLink = cell.querySelector(\".range-link\");\n    const defer = cell.querySelector(\".defer-action\");\n    const reject = cell.querySelector(\".reject-action\");\n\n    function clearStateClasses() {\n      cell.classList.remove(\n        \"is-modified\",\n        \"is-deferred\",\n        \"is-rejected\"\n      );\n    }\n\n    function updateRangeLink() {\n      const option = select.options[select.selectedIndex];\n      const url = option ? option.dataset.url : \"\";\n\n      if (url) {\n        rangeLink.href = url;\n        rangeLink.style.display = \"inline-block\";\n      } else {\n        rangeLink.removeAttribute(\"href\");\n        rangeLink.style.display = \"none\";\n      }\n    }\n\n    function updateOtherProposal() {\n      if (select.value === \"__other__\") {\n        otherProposal.style.display = \"block\";\n      } else {\n        otherProposal.style.display = \"none\";\n      }\n    }\n\n    defer.addEventListener(\"click\", function () {\n      clearStateClasses();\n      state.value = \"defer\";\n      cell.classList.add(\"is-deferred\");\n    });\n\n    reject.addEventListener(\"click\", function () {\n      clearStateClasses();\n      state.value = \"reject\";\n      cell.classList.add(\"is-rejected\");\n    });\n\n    select.addEventListener(\"change\", function () {\n      state.value = \"accept\";\n      clearStateClasses();\n      cell.classList.add(\"is-modified\");\n      updateOtherProposal();\n      updateRangeLink();\n\n      if (this.value === \"__other__\") {\n        const input = otherProposal.querySelector(\"input\");\n        if (input) input.focus();\n      }\n    });\n\n    updateOtherProposal();\n    updateRangeLink();\n  });\n\n\n/*\n * Open original evidence source\n */\n\nfunction openSource(url) {\n  window.open(\n    url,\n    \"betwixt-source\",\n    \"width=1100,height=800,resizable=yes,scrollbars=yes\"\n  );\n}\n\ndocument\n  .querySelectorAll(\".view-source\")\n  .forEach(button => {\n    button.addEventListener(\"click\", function () {\n      openSource(this.dataset.url);\n    });\n  });\n\ndocument\n  .querySelectorAll(\".review-thumbnail\")\n  .forEach(image => {\n    image.addEventListener(\"click\", function () {\n      openSource(this.dataset.url);\n    });\n  });\n\n\n/*\n * Save reviewed HTML\n */\n\nfunction reviewedFilename(sourceFilename) {\n  if (sourceFilename.toLowerCase().endsWith(\".html\")) {\n    return sourceFilename.slice(0, -5) +\n      \"-betwixt-reviewed.html\";\n  }\n\n  if (sourceFilename.toLowerCase().endsWith(\".htm\")) {\n    return sourceFilename.slice(0, -4) +\n      \"-betwixt-reviewed.html\";\n  }\n\n  return sourceFilename + \"-betwixt-reviewed.html\";\n}\n\ndocument\n  .getElementById(\"save-review\")\n  .addEventListener(\"click\", saveReview);\n\nfunction saveReview() {\n  document\n    .getElementById(\"review-saved-at\")\n    .value = new Date().toISOString();\n\n  const sourceFilename =\n    document\n      .getElementById(\"review-source-filename\")\n      .value;\n\n  const outputFilename =\n    reviewedFilename(sourceFilename);\n\n  document\n    .getElementById(\"review-saved-filename\")\n    .value = outputFilename;\n\n  const clone =\n    document.documentElement.cloneNode(true);\n\n  const originalInputs =\n    document.querySelectorAll(\"input\");\n\n  const clonedInputs =\n    clone.querySelectorAll(\"input\");\n\n  originalInputs.forEach((input, index) => {\n    const clonedInput = clonedInputs[index];\n\n    clonedInput.setAttribute(\"value\", input.value);\n\n    if (input.type === \"radio\") {\n      if (input.checked) {\n        clonedInput.setAttribute(\"checked\", \"\");\n      } else {\n        clonedInput.removeAttribute(\"checked\");\n      }\n    }\n  });\n\n  const originalTextareas =\n    document.querySelectorAll(\"textarea\");\n\n  const clonedTextareas =\n    clone.querySelectorAll(\"textarea\");\n\n  originalTextareas.forEach((textarea, index) => {\n    clonedTextareas[index].textContent = textarea.value;\n  });\n\n  const originalSelects =\n    document.querySelectorAll(\"select\");\n\n  const clonedSelects =\n    clone.querySelectorAll(\"select\");\n\n  originalSelects.forEach((select, index) => {\n    const clonedSelect = clonedSelects[index];\n\n    Array.from(clonedSelect.options)\n      .forEach(option => {\n        if (option.value === select.value) {\n          option.setAttribute(\"selected\", \"\");\n        } else {\n          option.removeAttribute(\"selected\");\n        }\n      });\n\n    if (select.disabled) {\n      clonedSelect.setAttribute(\"disabled\", \"\");\n    } else {\n      clonedSelect.removeAttribute(\"disabled\");\n    }\n  });\n\n  /*\n   * Persist review-cell state and proposal visibility.\n   */\n\n  const originalCells =\n    document.querySelectorAll(\".review-cell\");\n\n  const clonedCells =\n    clone.querySelectorAll(\".review-cell\");\n\n  originalCells.forEach((cell, index) => {\n    clonedCells[index].className = cell.className;\n  });\n\n  const originalProposals =\n    document.querySelectorAll(\".other-proposal\");\n\n  const clonedProposals =\n    clone.querySelectorAll(\".other-proposal\");\n\n  originalProposals.forEach((proposal, index) => {\n    clonedProposals[index].style.display =\n      proposal.style.display;\n  });\n\n  const originalRangeLinks =\n    document.querySelectorAll(\".range-link\");\n\n  const clonedRangeLinks =\n    clone.querySelectorAll(\".range-link\");\n\n  originalRangeLinks.forEach((link, index) => {\n    clonedRangeLinks[index].style.display =\n      link.style.display;\n\n    if (link.getAttribute(\"href\")) {\n      clonedRangeLinks[index].setAttribute(\n        \"href\",\n        link.getAttribute(\"href\")\n      );\n    } else {\n      clonedRangeLinks[index].removeAttribute(\"href\");\n    }\n  });\n\n  const html =\n    \"<!DOCTYPE html>\\n\" +\n    clone.outerHTML;\n\n  const blob =\n    new Blob(\n      [html],\n      { type: \"text/html;charset=utf-8\" }\n    );\n\n  const url =\n    URL.createObjectURL(blob);\n\n  const download =\n    document.createElement(\"a\");\n\n  download.href = url;\n  download.download = outputFilename;\n\n  document.body.appendChild(download);\n  download.click();\n  document.body.removeChild(download);\n\n  URL.revokeObjectURL(url);\n}\n  </script>\n</body>\n</html>"
```

## Vignettes

Betwixt implemeents the following workflow:

    observation provenance
             ↓
    candidate claim
             ↓
    review provenance
             ↓
    reviewed claim

It is organised around three complementary vignettes.

### 1. Preparing Data for Human Review

Introduces scoped claims, semantic stabilisation, contextual
inheritance, and the conceptual foundations of Betwixt.

### 2. R Reference Implementation

Shows how `claim_df` objects are created, manipulated, rendered, and
integrated with the R ecosystem.

### 3. Python Minimal Implementation

Demonstrates that the Betwixt model is implementation-independent and
can be realised using pandas and Mustache-compatible tooling.
