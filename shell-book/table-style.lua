-- fix-breaks.lua

-- Helper function: Scans a list of Inlines (text segments)
-- and swaps "Line One<<BR>>Line Two" into:
-- [Str "Line One", LineBreak, Str "Line Two"]
local function split_text_on_break(inlines)
  local new_inlines = {}
  
  for _, inline in ipairs(inlines) do
    -- Only look at String elements containing our placeholder
    if inline.t == "Str" and inline.text:find("<<BR>>") then
      local current_text = inline.text
      
      while true do
        local start_idx, end_idx = current_text:find("<<BR>>")
        
        -- If no more <<BR>>, stop
        if not start_idx then break end
        
        -- 1. Push text before the break
        if start_idx > 1 then
          table.insert(new_inlines, pandoc.Str(current_text:sub(1, start_idx - 1)))
        end
        
        -- 2. Push the actual Word Line Break
        table.insert(new_inlines, pandoc.LineBreak())
        
        -- 3. Advance the string position
        current_text = current_text:sub(end_idx + 1)
      end
      
      -- Push remaining text after the last break
      if #current_text > 0 then
        table.insert(new_inlines, pandoc.Str(current_text))
      end
      
    else
      -- Keep other elements (like Bold, Italic) unchanged
      table.insert(new_inlines, inline)
    end
  end
  
  return new_inlines
end

-- Helper function: Process a Cell (list of blocks)
local function process_cell_content(blocks)
  for _, b in ipairs(blocks) do
    -- Only process Paragraphs (Para) and Plain text (Plain)
    if b.t == "Para" or b.t == "Plain" then
      b.content = split_text_on_break(b.content)
    end
  end
  return blocks
end

-- Main Function: Iterate through the table manually to avoid crashes
function Table(el)
  
  -- 1. Header Rows
  for _, row in ipairs(el.head.rows) do
    for _, cell in ipairs(row.cells) do
      cell.content = process_cell_content(cell.content)
    end
  end

  -- 2. Body Rows
  for _, body in ipairs(el.bodies) do
    for _, row in ipairs(body.rows) do
      for _, cell in ipairs(row.cells) do
        cell.content = process_cell_content(cell.content)
      end
    end
  end

  -- 3. Footer Rows
  for _, row in ipairs(el.foot.rows) do
    for _, cell in ipairs(row.cells) do
      cell.content = process_cell_content(cell.content)
    end
  end

  return el
end