# Run from any directory. Does not activate or change the user's pipx environment.
[CmdletBinding()]
param(
    [string]$Python = "python"
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

if ($env:OS -ne "Windows_NT") {
    throw "This build targets Windows x64. Run it on Windows."
}

$projectDir = $PSScriptRoot
$buildPython = Join-Path $projectDir ".venv-build\Scripts\python.exe"

function Invoke-Checked {
    param([string]$Program, [string[]]$Arguments)
    & $Program @Arguments
    if ($LASTEXITCODE -ne 0) {
        throw "Command failed (exit $LASTEXITCODE): $Program $Arguments"
    }
}

if (-not (Test-Path -LiteralPath $buildPython)) {
    Invoke-Checked $Python @("-m", "venv", (Join-Path $projectDir ".venv-build"))
}

Invoke-Checked $buildPython @("-c", "import sys, platform; assert sys.version_info >= (3, 10) and platform.machine().lower() in ('amd64', 'x86_64') and sys.maxsize > 2**32, 'Python 3.10+ x64 required'")
Invoke-Checked $buildPython @("-m", "pip", "install", "--only-binary=:all:", "-r", (Join-Path $projectDir "requirements-build.txt"))
Invoke-Checked $buildPython @("-m", "pip", "install", "--no-deps", "--no-build-isolation", $projectDir)
Invoke-Checked $buildPython @((Join-Path $projectDir "packaging\prepare_ffmpeg.py"))
Invoke-Checked $buildPython @("-m", "PyInstaller", "--noconfirm", "--clean", "--distpath", (Join-Path $projectDir "dist"), "--workpath", (Join-Path $projectDir ".build\pyinstaller"), (Join-Path $projectDir "rb-ffmpeg.spec"))

Write-Host "Executable: $(Join-Path $projectDir 'dist\rb-ffmpeg.exe')"

