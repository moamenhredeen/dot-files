local M = {}

M.greet = function()
	print 'hello moamen'
end


M.openNotesFile = function()
	-- vim.cmd(":e ~/notes/")
	local command = string.format(":e ~/notes/%s.%s", os.date("%Y-%B-%A"), "md")
	vim.cmd(command)
end

return M
