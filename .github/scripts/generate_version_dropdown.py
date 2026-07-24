"""Generate a Quarto navbar version dropdown for multiversion altdoc sites.

Reads the current dev version from DESCRIPTION and all release tags from git,
then rewrites the "Versions" menu block in an altdoc `quarto_website.yml` so
that:
  - The current stable release is labeled "vX.Y.Z (stable)" -> /latest-tag/ URL
  - The development build is labeled "<version> (dev)"      -> /dev/ URL
  - A separator followed by each older tag links to /vX.Y.Z/ (previous versions)

Ported from d-morrison/rpt's `.github/scripts/generate_version_dropdown.py`
(see UCD-SERG/serocalculator#504). A generalized copy of this same script is
being contributed to d-morrison/gha as a reusable composite action
(d-morrison/gha#284); once that merges and ships at a released tag, this
repo can drop this file and call the composite instead (see
d-morrison/rpt#169 for the parallel migration on the source template).
Called by this repo's docs.yaml workflow before `render_docs()`.
"""

import subprocess
import re
import sys
import os
from pathlib import Path

REPO_DIR = Path(os.environ.get("DOCS_SOURCE_DIR", ".")).resolve()
DEFAULT_BRANCH = os.environ.get("DEFAULT_BRANCH", "main").strip() or "main"
QUARTO_CONFIG_PATH = (
    os.environ.get("QUARTO_CONFIG_PATH", "altdoc/quarto_website.yml").strip()
    or "altdoc/quarto_website.yml"
)

# --- Helpers ---


def _parse_version(content):
    """Return the Version field from DESCRIPTION file content, or None."""
    for line in content.splitlines():
        m = re.match(r"^Version:\s+(.+)", line)
        if m:
            return m.group(1).strip()
    return None


def _run_git(*args):
    """Run a git command in REPO_DIR and return the CompletedProcess result.

    Runs with cwd=REPO_DIR rather than the process's own working directory --
    DOCS_SOURCE_DIR can point at a linked worktree checked out for a stable-tag
    release build (same repo, so this wouldn't matter) or, in a test fixture,
    an entirely separate repo (where it does matter: without an explicit cwd,
    `git tag`/`git show` would silently resolve against whatever repo the
    calling step happens to be in instead).

    Calls sys.exit(1) with a descriptive message if the git executable
    cannot be launched (e.g. git not installed or permission denied).
    """
    try:
        return subprocess.run(
            ["git", *args], capture_output=True, text=True, cwd=REPO_DIR
        )
    except OSError as e:
        print(f"Failed to execute git command: {e}", file=sys.stderr)
        sys.exit(1)


def _docs_base_url():
    """Return the GitHub Pages base URL used for docs links.

    Fails fast (rather than falling back to a hard-coded repo) when none of
    the usual GitHub Actions context is available -- see
    avoid-hardcoding-external-data / fail-fast in the d-morrison/ai-config
    corpus: a reusable action must derive this from the caller's own
    context, not from whichever repo the action happened to be written for.
    """
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


BASE_URL = _docs_base_url()

# --- Gather version information ---

# Get the development version from DESCRIPTION.
# On release builds the checked-out DESCRIPTION contains the release version
# (e.g. "1.0.0"), not the dev version.  Read origin/<default-branch>:DESCRIPTION
# instead so the /dev/ link is always labeled with the actual development
# version.  Fall back to the local file if the git command fails (e.g. no
# remote, or running against a fixture with no origin/<default-branch>).
dev_version = None

result = _run_git("show", f"origin/{DEFAULT_BRANCH}:DESCRIPTION")
if result.returncode == 0:
    dev_version = _parse_version(result.stdout)

if dev_version is None:
    # Fallback: read local DESCRIPTION (works for non-release builds)
    try:
        with open(REPO_DIR / "DESCRIPTION") as f:
            dev_version = _parse_version(f.read())
    except OSError as e:
        print(f"Could not open DESCRIPTION: {e}", file=sys.stderr)
        sys.exit(1)

if dev_version is None:
    print("Could not find Version field in DESCRIPTION", file=sys.stderr)
    sys.exit(1)

# Get release tags (vX.Y.Z only; tags with a dev component like v1.0.0.9000
# are excluded because they are not final releases)
release_tags = []
release_query_succeeded = False

gh_token = os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN")
github_repository = os.environ.get("GITHUB_REPOSITORY")
if gh_token and github_repository:
    try:
        result = subprocess.run(
            [
                "gh",
                "api",
                f"repos/{github_repository}/releases",
                "--paginate",
                "--jq",
                '.[] | select(.draft == false and .prerelease == false) | .tag_name',
            ],
            capture_output=True,
            text=True,
            env={**os.environ, "GH_TOKEN": gh_token},
        )
    except OSError as e:
        # `gh` missing/unrunnable is the same "can't use the API" case as a
        # failed API call below -- fall through to the `git tag` fallback
        # rather than aborting, since that fallback exists precisely to cover
        # environments where the API path doesn't work.
        print(f"Could not execute gh command ({e}); falling back to git tag", file=sys.stderr)
        result = None
    if result is not None and result.returncode == 0:
        release_query_succeeded = True
        all_tags = result.stdout.strip().split("\n") if result.stdout.strip() else []
        release_tags = [t for t in all_tags if re.match(r"^v\d+\.\d+\.\d+$", t)]

if not release_query_succeeded and not release_tags:
    result = _run_git("tag", "--sort=-version:refname")
    if result.returncode != 0:
        print(f"git tag command failed:\n{result.stderr}", file=sys.stderr)
        sys.exit(1)
    all_tags = result.stdout.strip().split("\n") if result.stdout.strip() else []
    release_tags = [t for t in all_tags if re.match(r"^v\d+\.\d+\.\d+$", t)]


def _semver_key(tag):
    return tuple(int(part) for part in tag.lstrip("v").split("."))


release_tags = sorted(set(release_tags), key=_semver_key, reverse=True)
latest_tag = release_tags[0] if release_tags else None
prev_tags = release_tags[1:] if len(release_tags) > 1 else []

# --- Locate and replace the Versions block in quarto_website.yml ---

quarto_config_file = REPO_DIR / QUARTO_CONFIG_PATH
try:
    with open(quarto_config_file) as f:
        lines = f.readlines()
except OSError as e:
    print(f"Could not open {quarto_config_file}: {e}", file=sys.stderr)
    sys.exit(1)

# Find the "- text: Versions" line in the navbar configuration
start_idx = None
for i, line in enumerate(lines):
    if re.match(r'\s+- text: ["\']?Versions["\']?\s*$', line):
        start_idx = i
        break
if start_idx is None:
    print(
        f'Could not find "- text: Versions" entry in {quarto_config_file} '
        "navbar configuration",
        file=sys.stderr,
    )
    sys.exit(1)

indent = len(lines[start_idx]) - len(lines[start_idx].lstrip())
base = " " * indent        # same indent level as "- text: Versions"
mi = " " * (indent + 2)    # menu: key
ii = " " * (indent + 4)    # individual menu items

# Find the end of the Versions block (first non-blank line at same or lower indent)
end_idx = start_idx + 1
while end_idx < len(lines):
    line = lines[end_idx]
    if line.strip() == "":
        end_idx += 1
        continue
    if len(line) - len(line.lstrip()) <= indent:
        break
    end_idx += 1

# --- Build the new Versions block ---

new_block = [
    f"{base}- text: Versions\n",
    f"{mi}menu:\n",
]

if latest_tag:
    new_block += [
        f'{ii}- text: "{latest_tag} (stable)"\n',
        f"{ii}  href: {BASE_URL}latest-tag/\n",
    ]

if dev_version:
    new_block += [
        f'{ii}- text: "{dev_version} (dev)"\n',
        f"{ii}  href: {BASE_URL}dev/\n",
    ]

if prev_tags:
    # "---" renders as a visual divider in the Quarto navbar dropdown
    new_block.append(f'{ii}- text: "---"\n')
    for tag in prev_tags:
        new_block += [
            f'{ii}- text: "{tag}"\n',
            f"{ii}  href: {BASE_URL}{tag}/\n",
        ]

# Write the updated file
new_lines = lines[:start_idx] + new_block + lines[end_idx:]
try:
    with open(quarto_config_file, "w") as f:
        f.writelines(new_lines)
except OSError as e:
    print(f"Could not write to {quarto_config_file}: {e}", file=sys.stderr)
    sys.exit(1)

print("Updated Versions dropdown:")
print("".join(new_block))

github_output = os.environ.get("GITHUB_OUTPUT")
if github_output:
    with open(github_output, "a") as f:
        f.write(f"latest-tag={latest_tag or ''}\n")
        f.write(f"dev-version={dev_version or ''}\n")
