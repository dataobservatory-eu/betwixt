
<!-- README.md is generated from README.Rmd. Please edit that file -->

# betwixt

<!-- badges: start -->

[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![devel-version](https://img.shields.io/badge/devel%20version-0.0.4-blue.svg)](https://github.com/dataobservatory-eu/betwixt)
[![dataobservatory](https://img.shields.io/badge/ecosystem-dataobservatory.eu-3EA135.svg)](https://dataobservatory.eu/)

<!-- badges: end -->

Betwixt is a lightweight framework for constructing, representing, and
reviewing candidate semantic claims in tabular data. It provides a
pragmatic review layer in which candidate knowledge can be organised
into bounded review tasks and presented for human judgement.

Betwixt keeps candidate semantic structure separate from its
presentation for review. Candidate data can be projected into
human-reviewable forms, while review decisions, states, and provenance
remain explicit and reproducible.

## Installation

You can install the development version of betwixt from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("dataobservatory-eu/betwixt")
```

## Example

The `delini` dataset provides a small cultural heritage example
containing candidate semantic assertions prepared for human review:

``` r
library(betwixt)

data("delini")
delini
#> # A tibble: 5 × 15
#>   row_number evidence_url      evidence_text label description col_1 col_1_range
#>        <int> <chr>             <chr>         <chr> <chr>       <chr> <chr>      
#> 1          1 https://betwixt.… P7101565      Deli… the farmho… fuds… <NA>       
#> 2          2 https://betwixt.… P7101561      tabl… a tablet-w… tabl… <NA>       
#> 3          3 https://betwixt.… P7101556      bed … a bed in t… bed … <NA>       
#> 4          4 https://betwixt.… P7101590      reco… a record c… reco… <NA>       
#> 5          5 https://betwixt.… P7101623      reco… a floor pl… reco… <NA>       
#> # ℹ 8 more variables: col_1_definition <chr>, col_2 <chr>, col_2_range <chr>,
#> #   col_2_definition <chr>, col_3 <chr>, col_3_range <chr>,
#> #   col_3_definition <chr>, context_1 <chr>
```

``` r
betwixt_render(
  delini,
  cols = c(
    col_1 = "Subject",
    col_2 = "instance of",
    col_3 = "heritage of",
    context_1 = "held by"
  ),
  subheadings = c(
    col_2 = "wdt:P31",
    col_3 = "controlled range"
  ),
  title = "Delini semantic review",
  description = "Review the proposed semantic assertions.",
  project_id = "delini",
  filename_stem = "delini-wide",
  sequence = 0L,
  path = tempdir()
)
```

The resulting standalone HTML review presents the evidence and
descriptive context alongside the candidate semantic assertions and
records the resulting review state and provenance.

## Vignettes

Betwixt implements the following workflow:

    candidate knowledge
            ↓
    bounded review task
            ↓
    review projection
            ↓
    human review
            ↓
    review state

It is organised around complementary vignettes.

### 1. Betwixt Implementation

Introduces scoped claims, semantic stabilisation, contextual
inheritance, and the conceptual foundations of Betwixt.

### 2. Review Layouts and Semantic Projections

Introduces the Delini example and shows how candidate semantic material
can be represented as wide, dual-wide, long, and dual-long projections.
The current browser-based review implementation uses the wide
projection.

### 3. R Reference Implementation

Describes the R implementation and the construction and processing of
candidate semantic data.

### 4. Python Minimal Implementation

Planned vignette demonstrating how the underlying approach can be
implemented outside R using ordinary tabular and web technologies.
