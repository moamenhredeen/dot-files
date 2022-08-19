local M = {}

M.openNotesFile = function()
	local command = string.format(":e ~/notes/%s.%s", os.date("%Y-%m-%d"), "md")
	vim.cmd(command)
end

return M
