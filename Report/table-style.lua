-- CONFIGURATION
-- 1. The Table Style (Borders, Shading, Header Row look)
local table_grid_style = "TFL" 

-- 2. The Paragraph Style (Font, Size, Latin/Asian settings)
local table_text_style = "Body Text" 

function Table(el)
  -- 1. Apply Table Grid Style (Borders)
  if el.attr then
    el.attr.attributes['custom-style'] = table_grid_style
  else
    -- Safety check: if table has no attr, create it
    el.attr = pandoc.Attr("", {}, {["custom-style"] = table_grid_style})
  end

  -- 2. Apply Content Style (Font) using walk_block
  return pandoc.walk_block(el, {
    
    -- Handle existing Paragraphs
    Para = function(b)
      if b.attr then
        b.attr.attributes['custom-style'] = table_text_style
      else
        -- Safety: Create attr if missing
        b.attr = pandoc.Attr("", {}, {["custom-style"] = table_text_style})
      end
      return b
    end,

    -- Handle Plain text (The part that was crashing)
    Plain = function(b)
      local p = pandoc.Para(b.content)
      
      -- THE FIX: Explicitly create a new Attribute object
      -- We do not assume p.attr exists; we assign it fresh.
      p.attr = pandoc.Attr("", {}, {["custom-style"] = table_text_style})
      
      return p
    end
  })
end