"""Generate the root multiversion landing-page redirect for GitHub Pages.

This mirrors the r-pkgdown-multiversion pattern: keep docs in versioned
subdirectories and use the root page as a redirect entrypoint.

Ported from d-morrison/rpt's `.github/scripts/generate_multiversion_landing_page.py`
(see UCD-SERG/serocalculator#504). A generalized copy of this same script is
being contributed to d-morrison/gha as a reusable composite action
(d-morrison/gha#284); once that merges and ships at a released tag, this
repo can drop this file and call the composite instead (see
d-morrison/rpt#169 for the parallel migration on the source template). Called
by this repo's docs.yaml workflow after the docs subdirectory (dev /
latest-tag / vX.Y.Z) for this build has been decided.
"""

import os
import pathlib
import sys


def _base_url():
    configured_url = os.environ.get("DOCS_BASE_URL", "").strip()
    if configured_url:
        return configured_url.rstrip("/") + "/"

    repository = os.environ.get("GITHUB_REPOSITORY", "").strip()
    if "/" in repository:
        owner, repo = repository.split("/", 1)
        if owner and repo:
            return f"https://{owner}.github.io/{repo}/"

    owner = os.environ.get("GITHUB_REPOSITORY_OWNER", "").strip()
    repo = os.environ.get("GITHUB_EVENT_REPOSITORY_NAME", "").strip()
    if owner and repo:
        return f"https://{owner}.github.io/{repo}/"

    print(
        "Could not derive the docs base URL: set the base-url input, or run "
        "this action where GITHUB_REPOSITORY (or "
        "GITHUB_REPOSITORY_OWNER + GITHUB_EVENT_REPOSITORY_NAME) is set.",
        file=sys.stderr,
    )
    sys.exit(1)


def main():
    target = os.environ["LANDING_TARGET"].strip("/")
    if not target:
        print("LANDING_TARGET must not be empty", file=sys.stderr)
        sys.exit(1)

    output_dir = pathlib.Path(os.environ.get("OUTPUT_DIR", "site-root"))
    output_dir.mkdir(parents=True, exist_ok=True)

    url = f"{_base_url()}{target}/"
    repo_name = os.environ.get("GITHUB_EVENT_REPOSITORY_NAME") or os.environ.get(
        "GITHUB_REPOSITORY", ""
    ).split("/")[-1]
    html = (
        "<!DOCTYPE html>\n"
        '<meta charset="utf-8">\n'
        f"<title>{repo_name}</title>\n"
        f'<meta http-equiv="refresh" content="0; URL={url}">\n'
        f'<link rel="canonical" href="{url}">\n'
    )

    (output_dir / "index.html").write_text(html, encoding="utf-8")
    (output_dir / ".nojekyll").write_text("", encoding="utf-8")

    print(f"Generated root redirect to: {url}")


if __name__ == "__main__":
    main()
