-- fix-breaks.lua

function Table(el)
  -- Use Pandoc's built-in walker. 
  -- This is safer than manual loops because it handles the structure for us.
  return pandoc.walk_block(el, {
    
    -- We only look for Text Strings (Str)
    Str = function(s)
      -- Check if the text contains our placeholder
      if s.text and s.text:find("<<BR>>") then
        
        local new_inlines = {}
        local current_text = s.text
        
        -- Loop to find all instances of <<BR>>
        while true do
          local start_idx, end_idx = current_text:find("<<BR>>")
          
          -- Stop if no more placeholders
          if not start_idx then break end
          
          -- 1. Keep text before the marker
          if start_idx > 1 then
            table.insert(new_inlines, pandoc.Str(current_text:sub(1, start_idx - 1)))
          end
          
          -- 2. Insert the Line Break
          table.insert(new_inlines, pandoc.LineBreak())
          
          -- 3. Advance past the marker
          current_text = current_text:sub(end_idx + 1)
        end
        
        -- Add any remaining text
        if #current_text > 0 then
          table.insert(new_inlines, pandoc.Str(current_text))
        end
        
        -- Return the new list of elements to replace the single Str
        return new_inlines
      end
    end
  })
end