vim.api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
  pattern = "*#*",
  callback = function()
    -- Get the raw filename
    local full_filename = vim.fn.expand("%:t")
    -- Remove anything after (and including) '#'
    local trimmed = full_filename:match("^[^#]+")
    -- Determine filetype using trimmed filename
    local filetype = vim.filetype.match({ filename = trimmed })
    if filetype then
      vim.cmd.set("filetype=" .. filetype)
    end
  end,
})
