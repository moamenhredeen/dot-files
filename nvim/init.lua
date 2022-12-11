
local configuration_steps = {
-- 'install-packer',
'install-plugins',
'configure-plugins',
'customize-ui',
'register-autocmds',
'map-keys',
}

for _, step in ipairs(configuration_steps) do
	require(step)
end

