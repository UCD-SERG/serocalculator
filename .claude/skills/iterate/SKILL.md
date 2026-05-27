---
name: iterate
description: Drive a pull request to a clean review verdict by looping request-review → address every finding → re-request-review until there are zero flagged items. Use when asked to "iterate until clean", "address the review comments", "@claude review again and fix what it finds", or after opening a PR you want carried all the way to mergeable. Handles the @claude bot reviewer and human reviewers.
user-invocable: true
allowed-tools:
  - Bash
  - Read
  - Edit
  - Write
---

# iterate

Carry a PR to a genuinely clean review verdict, not "ready with a couple of
nits." The loop: request a review, address **every** flagged item, re-request,
repeat — until the reviewer returns no findings under any heading.

## When this fires

- "iterate until clean", "address the review comments", "keep going until the
  reviewer's happy".
- "@claude review again" / iterative review loops on a PR.
- Right after opening a PR you've been asked to take all the way to mergeable.

## The loop

For each round:

1. **Claim the PR** (first round only). Post a brief comment so a parallel
   `@claude` CI run or another person doesn't start a colliding session:
   `gh pr comment <N> --body "Claude Code CLI (local session) is working on
   this — paws off until I'm done."` Skip if your most recent comment already
   says so.

2. **Sync with main.** If main has moved ahead of the PR branch, merge it in
   *before* triggering review, so the reviewer evaluates against current main:
   ```bash
   git fetch origin main
   git log --oneline ..origin/main | head   # any commits? merge them in
   git merge origin/main
   ```
   Resolve conflicts, run the project's pre-commit checks, commit, then push.
   Don't rebase/squash a published branch — a merge commit matches GitHub's
   "Update branch" button.

3. **Request the review.**
   - `@claude` bot reviewer: post `@claude review` on the PR (or the repo's
     equivalent trigger).
   - Human reviewer: use [[request-pr-review]] (request `d-morrison`). Note a
     self-authored PR can't request its own author — surface the 422, don't
     swallow it.

4. **Wait for the review to land, then read the LATEST one.** Don't trust an
   earlier cached verdict — a newer review may have landed since (bot, human,
   or re-trigger). Poll the workflow run / the review comment until it's
   complete, then read the most recent reviewer comment in full:
   ```bash
   gh pr view <N> --json comments \
     --jq '[.comments[] | select(.author.login=="claude")] | last | .body'
   ```
   `gh pr checks` going green is about CI state, **not** the review verdict —
   always parse the latest review body for findings.

5. **Address every flagged item — regardless of severity label.** "Not a
   blocker", "minor", "nit", "optional", "consider", "if you want" are for the
   user's prioritization, not a pass for the implementer. For each item,
   exactly one of:
   - **Fix it in this PR** (the default — most nits are 1–3 lines), or
   - **Defer to a tracked issue** via [[defer-issue]] — only when the fix
     genuinely expands scope (new feature, broader refactor, separate
     concern), then reference the issue in a PR comment so it isn't lost.

6. **Push the fixes** (sync main again first if it moved). Post a short
   comment summarizing what you addressed and how (fixed vs. deferred + issue
   link).

7. **Re-request review (back to step 3) and repeat** until the verdict
   contains **zero** flagged items under any heading — no "non-blocking",
   "minor observation", "could improve", etc. "Looks good" / "no findings" /
   "approved" with no follow-on bullets is the bar.

## The bar for "clean"

Don't stop at, or report, "ready to merge with one minor nit noted" /
"harmless as-is" / "can address if you want." That hedging just pushes triage
back to the user. Keep going until there's nothing flagged.

## Asymptotic-noise guard

If after **3–4 rounds** the reviewer keeps generating *new* nits each cycle
(it's chasing diminishing returns rather than converging), stop and surface
that to the user: summarize the open items and ask whether to keep going or
accept the current state. Don't loop forever.

## On clean

- If you claimed the PR, post a closing/unclaim comment so it's free for the
  next person.
- Report the final verdict and the round count. Don't merge unless the user
  asked — opening the merge is their call.

## Driving many PRs at once

When iterating a batch, give each PR its own [[loop]]-style cadence or process
them in series, but keep the per-PR rules above intact (claim, sync, address
*every* item, re-request, latest-review-only). Report a per-PR status table at
the end; link each PR number to its URL.
