#!/usr/bin/env Rscript

# Lint only the lines added or modified by the current pull request.
#
# Runs lintr on each changed R file, then keeps only the lints whose line
# falls on a line this PR added or modified (parsed from the PR's diff
# hunks). This supports adopting or tightening lint rules incrementally:
# new and edited code must comply, while untouched legacy code in the same
# file is not flagged. Exits non-zero if any lint remains on a changed line.

suppressPackageStartupMessages(library(lintr))

repo <- Sys.getenv("GITHUB_REPOSITORY")
pr <- Sys.getenv("PR_NUMBER")
if (repo == "" || pr == "") {
  stop("GITHUB_REPOSITORY and PR_NUMBER must both be set.")
}

# The set of new-file line numbers that a unified-diff patch adds or modifies.
added_lines <- function(patch) {
  if (is.null(patch) || is.na(patch)) {
    return(integer(0))
  }
  out <- integer(0)
  new_line <- NA_integer_
  for (line in strsplit(patch, "\n", fixed = TRUE)[[1]]) {
    marker <- substr(line, 1, 1)
    if (grepl("^(diff |index |\\+\\+\\+ |--- )", line)) {
      # File-header lines (present in a full `git diff`, absent from the
      # GitHub API `patch` field); never a code line.
      next
    } else if (startsWith(line, "@@")) {
      # Hunk header: @@ -old,n +new,n @@ ; take the new-file start line.
      new_line <- as.integer(regmatches(line, regexec("\\+([0-9]+)", line))[[1]][2])
    } else if (marker == "+") {
      out <- c(out, new_line)
      new_line <- new_line + 1L
    } else if (marker == "-") {
      # Deleted line: absent from the new file, so no counter change.
    } else if (marker == "\\") {
      # "\ No newline at end of file": not a real line.
    } else {
      # Context line: present in the new file.
      new_line <- new_line + 1L
    }
  }
  out
}

files <- gh::gh(sprintf("GET /repos/%s/pulls/%s/files", repo, pr), .limit = Inf)

changed <- list()
for (f in files) {
  path <- f$filename
  if (!grepl("[.][Rr]$", path)) next
  if (identical(f$status, "removed")) next
  if (!file.exists(path)) next
  lines <- added_lines(f$patch)
  if (length(lines) > 0) {
    changed[[path]] <- lines
  }
}

if (length(changed) == 0) {
  cat("No changed R lines to lint.\n")
  quit(status = 0)
}

lints <- list()
for (path in names(changed)) {
  on_changed <- Filter(
    function(l) l$line_number %in% changed[[path]],
    lint(path)
  )
  lints <- c(lints, on_changed)
}

if (length(lints) == 0) {
  cat("No lints on changed lines.\n")
  quit(status = 0)
}

print(structure(lints, class = "lints"))
quit(status = 1)
