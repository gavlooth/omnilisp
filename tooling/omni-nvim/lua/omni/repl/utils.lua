local M = {}

function M.trim(text)
  return (text:gsub("^%s+", ""):gsub("%s+$", ""))
end

function M.ends_with_newline(text)
  return text:sub(-1) == "\n"
end

function M.strip_ansi(text)
  return text:gsub("\27%[[0-9;?]*[ -/]*[@-~]", "")
end

function M.truncate_text(text, max_length)
  local ok, normalized = pcall(tostring, text)
  if not ok then
    normalized = ""
  end
  normalized = normalized:gsub("[\r\n]+", " ")
  normalized = normalized:gsub("^%s+", ""):gsub("%s+$", "")
  if max_length and max_length > 0 and #normalized > max_length then
    local limit = max_length - 3
    if limit <= 0 then
      return "..."
    end
    return normalized:sub(1, limit) .. "..."
  end
  return normalized
end

function M.stringify_eval_value(value)
  local ok, inspected = pcall(vim.inspect, value)
  if ok then
    return inspected
  end
  return tostring(value)
end

return M
