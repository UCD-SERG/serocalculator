-- slidebreak.lua
-- A Quarto shortcode that inserts a slide break in all slide deck formats
-- (revealjs, pptx, beamer) but does nothing in docx and html formats

function slidebreak()
  -- Get the current output format
  -- See: https://quarto.org/docs/extensions/lua-api.html#format-detection
  local format = quarto.doc.is_format
  
  -- Insert slide break for all slide/presentation formats (revealjs, pptx, beamer)
  if format("revealjs") or format("pptx") or format("beamer") then
    -- Use HorizontalRule which creates a slide separator in presentation formats
    return pandoc.HorizontalRule()
  end
  
  -- Return empty for html and docx formats (and any other non-presentation format)
  return pandoc.Null()
end

-- Return the shortcode handler
return {
  ['slidebreak'] = slidebreak
}
