# Multi-Version Documentation Setup

This repository uses multi-version pkgdown documentation with a version dropdown menu, based on the [insightsengineering/tern](https://github.com/insightsengineering/tern) setup.

## Features

### Version Dropdown Menu
Users can switch between different versions of the documentation using a dropdown menu in the navbar:
- **main**: Development version (latest commits on main branch)
- **latest-tag**: Most recent release version
- **v\*.\*.\***: Version-tagged releases (e.g., v1.0.0, v1.1.0)

### Default Landing Page
The default page at https://ucd-serg.github.io/serocalculator/ shows the **latest-tag** (most recent release) documentation.

### PR Previews
Pull requests automatically generate preview documentation at:
- `https://ucd-serg.github.io/serocalculator/preview/pr<number>/`
- Preview link is posted as a comment on the PR
- Previews are automatically cleaned up when PRs are closed

## How It Works

### Workflow: `.github/workflows/docs.yaml`

The workflow has four jobs:

1. **docs**: Builds the pkgdown site
   - Runs on: PRs, pushes to main, version tags
   - Deploys to version-specific directories on gh-pages

2. **multi-version-docs**: Creates version dropdown
   - Runs on: pushes to main, workflow_dispatch
   - Uses `insightsengineering/r-pkgdown-multiversion@v3`
   - Generates navigation between versions

3. **upload-release-assets**: Upload pkgdown.zip
   - Runs on: version tags only
   - Uploads documentation to GitHub Releases

4. **cleanup-pr-preview**: Clean up PR previews
   - Runs on: PR close events
   - Removes preview directories from gh-pages

### Configuration

#### `pkgdown/_pkgdown.yml`
```yaml
template:
  bootstrap: 5  # Required for multi-version action

url: https://ucd-serg.github.io/serocalculator/  # Required

search:
  exclude: ['preview/']  # Exclude PR previews from search
```

#### `DESCRIPTION`
```
URL: https://github.com/UCD-SERG/serocalculator,
    https://ucd-serg.github.io/serocalculator/
```

### Version Filtering

Versions shown in the dropdown are controlled by:

**refs-order**: `"main latest-tag"`
- Determines the order of versions in the dropdown

**branches-or-tags-to-list**: `'^main$|^latest-tag$|^v([0-9]+\\.)?([0-9]+\\.)?([0-9]+)$'`
- Regex pattern matching versions to include
- Matches: `main`, `latest-tag`, and semver tags like `v1.0.0`

## Creating a New Version

To create a new versioned documentation:

1. **Tag a release**:
   ```bash
   git tag v1.0.0
   git push origin v1.0.0
   ```

2. **Workflow runs automatically**:
   - Builds pkgdown site for v1.0.0
   - Deploys to `gh-pages/v1.0.0/`
   - Updates version dropdown to include v1.0.0
   - Uploads pkgdown.zip to the GitHub release

3. **Version appears in dropdown**:
   - Users can now select v1.0.0 from the versions menu
   - If it's the latest tag, it becomes the default landing page

## Triggering Builds

### Automatic Triggers
- **PR opened/updated**: Builds preview documentation
- **Push to main**: Rebuilds main version docs
- **Version tag pushed**: Builds new version docs
- **PR closed**: Cleans up preview

### Manual Trigger
Use the Actions tab to manually trigger the workflow via `workflow_dispatch`.

## Troubleshooting

### Workflow requires approval
First-time workflows using external actions need manual approval:
1. Go to repository Actions tab
2. Find the workflow run
3. Click "Approve and run"
4. Subsequent runs will execute automatically

### Version not appearing in dropdown
Check that:
1. Documentation was successfully built for that version
2. Version name matches the regex pattern
3. Version is listed in `refs-order` or matches `branches-or-tags-to-list`

### Default landing page not updating
Ensure:
1. `default-landing-page: "latest-tag"` is set in workflow
2. Latest tag has documentation built
3. Multi-version-docs job completed successfully

## Comparison to Original Setup

### Before (pkgdown.yaml)
- Single version (main branch only)
- PR previews only
- No version switching

### After (docs.yaml)
- Multiple versions (main + all releases)
- Version dropdown menu
- PR previews maintained
- Default to latest release
- Version-specific documentation URLs

## Resources

- [insightsengineering/r-pkgdown-multiversion](https://github.com/insightsengineering/r-pkgdown-multiversion)
- [insightsengineering/tern](https://github.com/insightsengineering/tern) (reference implementation)
- [pkgdown documentation](https://pkgdown.r-lib.org/)
