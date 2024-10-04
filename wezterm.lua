
local wezterm = require('wezterm')

local config = wezterm.config_builder()

config.color_scheme = 'Everforest Light (Gogh)'
config.default_prog = { 'pwsh.exe' }

return config
