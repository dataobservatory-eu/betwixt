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
