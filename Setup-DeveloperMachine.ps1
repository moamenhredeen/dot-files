<#
    .SYNOPSIS
    Setup and configre windows deveveloper laptop.

    .DESCRIPTION
    This script install the nessecery tools for development.

    .PARAMETER Yes
    do not prompt the user while installing the tools.
    
    .PARAMETER Verbose
    Protocol the configuration process to the console.

    .INPUTS
    None. You can't pipe objects to Setup-DeveloperMachine.ps1.

    .OUTPUTS
    None. Setup-DeveloperMachine.ps1 doesn't generate any output.

    .EXAMPLE
    PS> .\Setup-DeveloperMachine.ps1

    .EXAMPLE
    PS> .\Setup-DeveloperMachine.ps1 -doNotAsk

    .EXAMPLE
    PS> .\Setup-DeveloperMachine.ps1 -doNotAsk -verbose
#>

param (
    [switch]$Yes, 
    [switch]$Verbose
)

# ******************************************************** 
# Utils Functions
# ------------

function log {
    param (
        [Parameter(Mandatory=$true)]
        [string] $message
    )
    if($Verbose){
        Write-Host -NoNewline -ForegroundColor Green "[$(Get-Date -Format 'dd.mm.yyyy hh:mm:ss')] "
        Write-Host " $message"
    }
}

function _winget_package_install {
    param (
        # make paramter manditory
        [string] $package_id
    )
    
    log("install package $package_id")
    if(!$Yes){
        $userInput = Read-Host "Install $package_id [Y/n]"
        if($userInput.Trim().Equals("n") || $userInput.Trim().Equals("")){
            return
        }
    }
    winget install -e --id $package_id
}

# ******************************************************** 
# Intall Tools
# ------------

# Powershell Core (the new powershell)
_winget_package_install("Microsoft.PowerShell")

# Windows Terminal
_winget_package_install("Microsoft.WindowsTerminal")

# Powertoys
_winget_package_install("Microsoft.PowerToys")

# Firefox
_winget_package_install("Mozilla.Firefox")

# 7-Zip
_winget_package_install("7zip.7zip")

# KeePassXC
_winget_package_install("KeePassXCTeam.KeePassXC")

# Git
_winget_package_install("Git.Git")

# Neovim
_winget_package_install("Neovim.Neovim")

# JetBrains Toolbox
_winget_package_install("JetBrains.Toolbox")

# Visual Studio Community Edition 
_winget_package_install("Microsoft.VisualStudio.2022.Community")

# Visual Studio Code (the open source binaries codium)
_winget_package_install("VSCodium.VSCodium")

# ripgrep (unix grep rewritten in rust)
_winget_package_install("BurntSushi.ripgrep.MSVC")

# fd (unix find rewritten in rust ) 
_winget_package_install("sharkdp.fd")

# pdf xchange (the best pdf editor for winodws)
_winget_package_install("TrackerSoftware.PDF-XChangePRO")

# pdf and epub reader (lightweight, fast and OSS)
_winget_package_install("SumatraPDF.SumatraPDF")