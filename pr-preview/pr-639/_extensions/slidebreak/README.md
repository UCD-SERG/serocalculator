# Slidebreak Shortcode Extension

A Quarto shortcode extension that inserts an untitled slide break in presentation formats while having no effect in document formats.

## Installation

This extension is included in the repository. It will be automatically available when rendering Quarto documents in this project.

## When to Use

Use this extension when you need to create presentations (RevealJS, PowerPoint, or Beamer) from Quarto documents and want to insert slide breaks without titles. This is particularly useful for separating content into distinct slides during presentations.

**Note**: This extension only affects presentation formats. When rendering to HTML or DOCX for documentation, the shortcode has no effect.

## Usage

Add the extension to your document's YAML frontmatter:

```yaml
---
title: "My Document"
filters:
  - slidebreak
---
```

Then use the shortcode in your document:

```markdown
## First Section

Some content here.

{{< slidebreak >}}

## Second Section

More content here.
```

## Behavior

- **All slide deck formats (RevealJS, PowerPoint, Beamer)**: Inserts a slide break (`---`), creating a new slide without a title
- **HTML and DOCX formats**: Does nothing (the shortcode is silently ignored)

## Example

```markdown
## Introduction

Welcome to the presentation!

{{< slidebreak >}}

## Main Content

Here's the main content on a new slide.
```

In any slide format (RevealJS, PowerPoint, or Beamer), this will create:
1. A slide titled "Introduction" with content
2. A new blank/untitled slide (from the slidebreak)
3. A slide titled "Main Content"

In HTML or DOCX, the slidebreak has no effect, and content flows normally.
