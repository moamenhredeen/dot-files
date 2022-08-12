
require('telescope').setup {
	defaults = {
		prompt_prefix = '>>> ',
		layout_strategy = 'horizontal',
		path_display = 'shorten',
    layout_config = {
				horizontal = {
					width = 0.9,
					preview_cutoff = 10,
					prompt_position = "bottom"
				},
    },
  },
}

