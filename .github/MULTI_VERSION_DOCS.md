# Multi-version documentation

The documentation site publishes several versions side by side on the
`gh-pages` branch:

| What was built | Where it lands |
| --- | --- |
| A pull request | `/pr-preview/pr<number>/` |
| A push to `main` | `/dev/` |
| A published release | `/latest-tag/`, plus an archived `/vX.Y.Z/` copy |

The site root (`/`) is a redirect landing page into whichever of `/dev/` or
`/latest-tag/` was deployed most recently, and a "Versions" dropdown in the
navbar links between them.

## How it is built

[`.github/workflows/docs.yaml`](workflows/docs.yaml) declares when to build and
what to pass. Everything else -- rendering with
[altdoc](https://altdoc.etiennebacher.com/), generating the version dropdown
and the landing page, and deploying each version -- lives in
[`d-morrison/gha`](https://github.com/d-morrison/gha)'s
`altdoc-multiversion-docs` reusable workflow. Its
[reference page](https://d-morrison.github.io/gha/reference/altdoc-multiversion-docs.html)
documents every input.

The navbar's "Versions" block in
[`altdoc/quarto_website.yml`](../altdoc/quarto_website.yml) is a placeholder;
the workflow rewrites it at build time with the real version list.

## One-time repository setup

Settings -> Pages -> Build and deployment -> Source = "Deploy from a branch",
branch `gh-pages` / `(root)`.

## Deploying by hand

Run the **Docs** workflow from the Actions tab. Choose `dev` to rebuild
`/dev/`, or `stable` plus the latest release tag to rebuild `/latest-tag/`.
The workflow rejects a `stable` run whose tag is not the latest published
release, since that run also rewrites the root landing page.

## History

Before the altdoc migration this site was built by `pkgdown` and
[`insightsengineering/r-pkgdown-multiversion`](https://github.com/insightsengineering/r-pkgdown-multiversion),
which published each branch and tag under its own name -- so the development
docs lived at `/main/` and PR previews at `/preview/pr<number>/`. Links written
against those paths no longer resolve.
