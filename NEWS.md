# betwixt 0.0.9

## Semantic assertion model

* Formalised `btx:Assertion` as the intermediate semantic representation used by Betwixt, with lexical `subject`, `predicate`, and `value` components that remain independent of any particular domain ontology.

* Added canonical assertion coordinates through `btx:rowId` and `btx:assertionId`, allowing assertions to retain their position in Betwixt tabular and long-form projections where those coordinates are available.

* Added `btx:alignedWith` for expressing bounded correspondence between reviewed assertions without asserting global identity or semantic equivalence.

* Extended the Betwixt mapping model with explicit assertion roles for subject, predicate, and value mappings, supporting projection of lexical Betwixt terms to RDF resources.

## Input representation and review projection

* Generalised review inputs around `input_url`, `input_media_url`, `input_label`, and `input_description`, allowing review inputs to be represented independently from the semantic assertions being reviewed.

* Added optional `input_predicate` and `input_predicate_range` support for cases where the relationship between a review input and the represented subject is itself reviewable.

* Improved candidate construction so that optional input fields can be omitted while retaining support for descriptive input labels and descriptions when supplied.

* Added canonical long-form assertion projection with `row_id`, `assertion_id`, `component`, `subject`, `predicate`, and `value`, distinguishing assertion components from mapping roles.

## RDF serialisation

* Updated RDF serialisation to use the stabilised Betwixt vocabulary, including `btx:rowId`, optional `btx:assertionId`, and assertion-level review status.

* Preserved the distinction between canonical assertion coordinates and serializer-generated RDF resource identifiers, avoiding the introduction of synthetic semantic identifiers during serialisation.

# betwixt 0.0.8

## Review representation, provenance, and RDF

* Added the Betwixt vocabulary for representing intermediate semantic assertions, review statuses, mappings, and provenance without requiring adoption of a domain ontology.

* Improved provenance handling throughout the review round trip, keeping candidate preparation and human review as distinct activities and preserving data-manager, reviewer, project, software, and timestamp metadata.

* Added `project_review_wide()` for representing candidate, reviewed, and status states as aligned tabular planes while preserving row-scoped context.

* Added `project_review_long()` for projecting reviews into atomic subject–predicate–value assertions with assertion-level review status, inherited context, and provenance.

* Added RDF serialisation of candidate and reviewed states using the Betwixt vocabulary and PROV-O, preserving the derivation of reviewed datasets from their candidate state.

* Added `serialise_review()` as the canonical RDF serialisation interface, with `serialize_review()` provided as an American-English alias.


# betwixt 0.0.7

## Candidate-data contract and portability

* Formalised the Betwixt candidate-data contract for programmatically and externally created candidate datasets. Candidate data can be prepared in spreadsheets, statistical applications, databases, or other programming environments and validated before entering the review workflow.

* Added `create_candidate_template()` for creating empty, conforming candidate tables suitable for manual or external data preparation.

* Added the exported `validate_candidate_dataset()` to check required structure, column types, candidate metadata, row identifiers, and evidence URLs before rendering.

* Generalised candidate assertions so that `_range` and `_definition` metadata are independently optional. Candidate values may therefore be associated with a controlled range, a semantic definition, or both.

* Generalised display-only context columns through the `context_` naming convention, allowing meaningful names such as `context_year` and `context_unit` to survive spreadsheet and other tabular-data round trips.

* Improved candidate preparation and rendering for typed candidate values and datasets in which optional evidence and descriptive fields are absent.

## Public API naming

* Regularised function names around explicit action verbs and consistent snake-case naming.

* Renamed `candidate_dataset()` to `create_candidate_dataset()` and `candidate_range()` to `add_candidate_range()`, aligning them with `add_candidate_column()` and the new `create_candidate_template()`.

* Renamed `betwixt_render()` to `render_review()` to describe the operation and resulting artefact more directly.

* Standardised `is_claim_df()` as the canonical claim-data predicate while retaining the deprecated `is.claim_df()` alias for compatibility.

* Retained British `serialise_review()` as the canonical serialisation function and added `serialize_review()` as a direct spelling alias.

## Examples and documentation

* Added `small_countries_dataset`, a compact Eurostat GDP example for Iceland and Malta demonstrating semantic definitions, evidence resources, statistical candidate values, and observational context outside the cultural-heritage domain.

* Added the **Working with Externally Created Candidate Datasets** vignette, demonstrating the portable workflow: create or download → edit → read → validate → render → review.

* Updated candidate-data, rendering, reference, and vignette documentation to reflect the revised public API and portable candidate-data contract.
# betwixt 0.0.6

## Review round trip

* Added `read_review()` to reconstruct standalone Betwixt review files. It returns review `metadata` and `provenance` together with separate `candidate` and `reviewed` data frames, preserving the distinction between the state presented for review and the state resulting from reviewer intervention.

* Standalone review files now preserve machine-readable candidate values alongside current reviewed values, assertion qualifications, row finalisation and outcomes, reviewer comments, and review lifecycle metadata. Saved draft and finalised HTML files can therefore be reopened and reconstructed without treating the rendered interface as the semantic representation itself.

* Review lifecycle metadata now distinguishes the original candidate artefact from subsequent saved review states, including filenames, review sequence, reviewer identity, and ISO 8601 UTC timestamps for review creation, saving, and completion.

* Improved support for evidence in candidate datasets. `evidence_media_url` identifies media presented directly in the review interface, while `evidence_url` identifies evidence resources that can be opened separately. Both fields support multiple resources.

* Added optional primary and alternative descriptive information through `label`, `description`, `alternative_label`, and `alternative_description`, kept distinct from candidate semantic assertions.

* Improved wide review rendering and persistence, including editable semantic values, assertion-level qualification, row-level finalisation and outcomes, optional row and review comments, and creation controls for unresolved semantic entities.

* Added and expanded the MuIS garment example to document candidate construction, browser review, draft saving, finalisation, and reconstruction of candidate and reviewed states.

* Clarified the review-state contract throughout the documentation: finalising a Betwixt review completes the review artefact but does not itself promote reviewed assertions into a subsequent stabilised semantic state or apply them to an external knowledge system.

# betwixt 0.0.5

* Added separate `evidence_media_url` and `evidence_url` support, including
  multiple evidence resources per observation.
* Added optional `alternative_label` and `alternative_description` fields for
  translations and other alternative human-readable descriptions.
* Added optional row-level reviewer comments and review-level comments to
  standalone reviews.
* Extended saved review artefacts to preserve edited descriptions, alternative
  descriptions, reviewer comments, semantic qualifications, and review state.
* Added the MuIS garment example demonstrating multilingual candidate
  construction and the candidate-to-draft-to-finalised review lifecycle.
* Added the **Creating Candidate Datasets** vignette, providing a worked guide
  to constructing and rendering candidate datasets from ordinary source data.
  
# betwixt 0.0.4

* Added wide human review projection and standalone browser-based review
  rendering.
* Added candidate dataset construction with `candidate_dataset()`,
  `add_candidate_column()`, and `candidate_range()`.
* Added controlled and open candidate ranges, semantic definitions, and
  optional reviewable evidence relations.
* Added review metadata, review sequence tracking, and draft and finalised
  review persistence to saved review artefacts.
* Added the Delini cultural heritage reference dataset and semantic projection
  examples.
* Added long and dual-long candidate projections as examples for semantic and
  review-task design.

# betwixt 0.0.3

* Added long and wide human review projections.
* Added controlled and open review ranges.
* Added review provenance to saved review artefacts.
* Added the Delini Farmstead reference review dataset.
