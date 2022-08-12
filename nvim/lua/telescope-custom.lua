
local M = {}

function M.edit_dotfiles()
	require('telescope.builtin').find_files({
		cwd = '~/.config/nvim/',
		prompt_title = '~ dotfiles ~',
	}
)
end

return M

