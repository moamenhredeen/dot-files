local ok, toggleterm = pcall(require, 'toggleterm')
if not ok then
	print('toggleterm not found')
	return
end

toggleterm.setup({
	size = function(term)
		if term.direction == "horizontal" then
			return 15
		elseif term.direction == "vertical" then
			return vim.o.columns * 0.4
		end
	end
})
