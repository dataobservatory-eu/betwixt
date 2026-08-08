#!/bin/bash

# Usage: sh list.sh "documentation and license" .gitignore _pkgdown.yml LICENSE LICENSE.md
# Usage: sh list.sh "fix unit tests" tests/testthat/test-document_survey_item.R


MESSAGE=$1
shift # Remove the message from the list
FILES=$@ # The remaining arguments are your files

FAKE_DATE="2026-06-18 12:09:23"

# Apply Time Travel
export GIT_AUTHOR_DATE="$FAKE_DATE"
export GIT_COMMITTER_DATE="$FAKE_DATE"

echo "Starting recovery commit for date: $FAKE_DATE"

for FILE in $FILES
do
if [ -f "$FILE" ]; then
git add "$FILE"
echo "Staged: $FILE"
else
  echo "Error: $FILE not found. Skipping."
fi
done

# Check if anything was actually staged before committing
if git diff --cached --quiet; then
echo "No valid files were staged. Commit aborted."
else
  git commit -m "$MESSAGE"
echo "Successfully committed to $FAKE_DATE"
fi

# Clean up
unset GIT_AUTHOR_DATE
unset GIT_COMMITTER_DATE
