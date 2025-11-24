-- Counter to ensure every bookmark has a unique internal numerical ID (required by Word)
local bookmark_counter = 0

function Div(el)
  -- 1. Check if this is your specific List Style and has an ID (#baseline)
  if el.attributes['custom-style'] == 'SHELL-16-01-列表' and el.identifier ~= "" then
    
    local bookmark_name = el.identifier
    bookmark_counter = bookmark_counter + 1
    local internal_id = bookmark_counter

    -- 2. Define OpenXML for the start and end of a bookmark
    local xml_start = string.format(
      '<w:bookmarkStart w:id="%d" w:name="%s"/>', 
      internal_id, bookmark_name
    )
    local xml_end = string.format(
      '<w:bookmarkEnd w:id="%d"/>', 
      internal_id
    )

    -- 3. Inject the bookmark INSIDE the paragraph text.
    -- We can't wrap the Div itself; we must wrap the *text* inside the Div.
    
    -- Ensure the Div contains blocks
    if #el.content > 0 then
      local first_block = el.content[1]
      
      -- If the content is a Paragraph or Plain text
      if first_block.t == "Para" or first_block.t == "Plain" then
        -- Insert bookmark Start at the very beginning of the text
        table.insert(first_block.content, 1, pandoc.RawInline('openxml', xml_start))
        
        -- Insert bookmark End at the very end of the text
        table.insert(first_block.content, pandoc.RawInline('openxml', xml_end))
        
        -- Remove the ID from the Div so Pandoc doesn't get confused
        el.identifier = ""
        
        return el
      end
    end
  end
end

-- 4. The Linker: Converts [?](#baseline) or !baseline into a Word REF field
function Str(el)
  -- Check for syntax like !baseline
  local id = el.text:match("^!(.+)")
  if id then
    local field_code = ' REF ' .. id .. ' \\h \\r '
    local xml = '<w:fldSimple w:instr="' .. field_code .. '"/>'
    return pandoc.RawInline('openxml', xml)
  end
end

function Link(el)
  -- Check for syntax like [?](#baseline)
  if el.target:match("^#") and pandoc.utils.stringify(el.content) == "?" then
    local id = el.target:sub(2)
    local field_code = ' REF ' .. id .. ' \\h \\r '
    local xml = '<w:fldSimple w:instr="' .. field_code .. '"/>'
    return pandoc.RawInline('openxml', xml)
  end
end