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
| `articles/<name>.html` | `vignettes/articles/<name>.html` |

So a pre-migration deep link cannot simply be re-prefixed onto `/dev/` --
the tail does not exist there.
It can be re-prefixed onto `/latest-tag/`,
which is still the pkgdown-shaped v1.4.1 build,
and that is what the `legacy-paths` input in
[`workflows/docs.yaml`](workflows/docs.yaml) does.
It generates a site-root `404.html` that rewrites the first path segment,
deep links included:

| Requested | Served |
| --- | --- |
| `/reference/index.html` | `/latest-tag/reference/index.html` |
| `/articles/<name>.html` | `/latest-tag/articles/<name>.html` |
| `/news/index.html` | `/latest-tag/news/index.html` |
| `/main/<anything>` | `/latest-tag/<anything>` |

Redirection needs JavaScript, and the HTTP status stays `404` --
a browser follows it, `curl` reports the 404 page.
That is worth remembering when checking these by hand.

When adding an entry, do not name a segment that also exists under
`/latest-tag/`: the rewritten URL would match the same rule again and loop.

## History

Before the altdoc migration this site was built by `pkgdown` and
[`insightsengineering/r-pkgdown-multiversion`](https://github.com/insightsengineering/r-pkgdown-multiversion),
which published each branch and tag under its own name -- so the development
docs lived at `/main/` and PR previews at `/preview/pr<number>/`. Links written
against those paths are redirected as described above.
