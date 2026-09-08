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

## Which URL to cite

**Cite a `/vX.Y.Z/` URL in anything that goes to print.**
Those directories are archived copies of one release and are never rebuilt,
so a link written against them survives any later restructuring of the site:

```
https://ucd-serg.github.io/serocalculator/v1.4.1/articles/enteric_fever_example.html
```

The other paths are all moving targets, and each fails differently:

| Path | Why not to cite it |
| --- | --- |
| `/articles/...`, `/reference/...` | Unversioned. Served only by the redirect below, and pointed at whatever `/latest-tag/` currently is. |
| `/latest-tag/...` | Advances on every release, so its content changes under a fixed URL. |
| `/dev/...` | Unreleased behaviour, and rebuilt on every push to `main`. |

## Redirects for pre-migration links

The migration changed the path shape as well as the version prefix, because
altdoc and pkgdown lay a site out differently:

| pkgdown (`/latest-tag/`, `/vX.Y.Z/`) | altdoc (`/dev/`) |
| --- | --- |
| `reference/index.html` | `reference.html` |
| `reference/<topic>.html` | `man/<topic>.html` |
| `articles/<name>.html` | `vignettes/<source path>.html` |

That last row is a path, not a name, and the difference bites.
altdoc mirrors the source tree, so an article at
`vignettes/articles/enteric_fever_example.Rmd` publishes to
`/dev/vignettes/articles/enteric_fever_example.html`,
while `vignettes/methodology.qmd` publishes to
`/dev/vignettes/methodology.html` -- one directory up.
pkgdown flattened both to `articles/<name>.html`.

So a pre-migration deep link cannot be re-prefixed onto `/dev/` at all:
the 404 page rewrites only the first path segment,
and no first-segment rewrite can turn `articles/methodology.html` into
`vignettes/methodology.html`.
It can be re-prefixed onto a pkgdown-shaped build, which is what the
`legacy-paths` input in [`workflows/docs.yaml`](workflows/docs.yaml) does:

| Requested | Served |
| --- | --- |
| `/reference/index.html` | `/v1.4.1/reference/index.html` |
| `/articles/<name>.html` | `/v1.4.1/articles/<name>.html` |
| `/news/index.html` | `/v1.4.1/news/index.html` |
| `/main/<anything>` | `/v1.4.1/<anything>` |

**The target is `/v1.4.1/` rather than `/latest-tag/` on purpose.**
`/latest-tag/` is pkgdown-shaped only because it still holds the v1.4.1
build.
This workflow rebuilds `/latest-tag/` with altdoc on every published
release, so the first release after these redirects shipped would have
turned all of them into 404s, silently.
A `/vX.Y.Z/` directory is archived and never rebuilt, so it keeps its shape
for good -- and v1.4.1 is the version those links were written against
anyway.

Redirection needs JavaScript, and the HTTP status stays `404` --
a browser follows it, `curl` reports the 404 page.
That is worth remembering when checking these by hand.

**The loop rule is about targets, not about what exists.**
No key may be the *first segment* of any target, because the first segment
is all the rewrite matches on.
`v1.4.1` is absent from the key column for exactly that reason.
A key naming a directory that also exists under `/v1.4.1/` is fine:
`reference`, `articles` and `news` all do, and none of them loops, because
each rewritten URL's first segment is `v1.4.1`, which is not a key.

## History

Before the altdoc migration this site was built by `pkgdown` and
[`insightsengineering/r-pkgdown-multiversion`](https://github.com/insightsengineering/r-pkgdown-multiversion),
which published each branch and tag under its own name -- so the development
docs lived at `/main/` and PR previews at `/preview/pr<number>/`. Links written
against those paths are redirected as described above.
