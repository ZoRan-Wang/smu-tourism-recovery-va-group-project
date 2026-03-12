param(
  [int]$QuartoPort = 4321,
  [int]$ShinyPort = 3838,
  [switch]$DryRun
)

function Resolve-RScript {
  $cmd = Get-Command Rscript -ErrorAction SilentlyContinue
  if ($cmd) {
    return $cmd.Source
  }

  if ($env:R_HOME) {
    $candidate = Join-Path $env:R_HOME "bin\Rscript.exe"
    if (Test-Path $candidate) {
      return $candidate
    }
  }

  $registryKeys = @(
    "HKLM:\SOFTWARE\R-core\R",
    "HKLM:\SOFTWARE\WOW6432Node\R-core\R",
    "HKCU:\SOFTWARE\R-core\R"
  )

  foreach ($key in $registryKeys) {
    if (Test-Path $key) {
      $installPath = (Get-ItemProperty -Path $key -ErrorAction SilentlyContinue).InstallPath
      if ($installPath) {
        $candidate = Join-Path $installPath "bin\Rscript.exe"
        if (Test-Path $candidate) {
          return $candidate
        }
      }
    }
  }

  return $null
}

$root = Split-Path -Parent $PSScriptRoot
$rscript = Resolve-RScript

if (-not $rscript) {
  Write-Error "Rscript.exe not found. Install R or add Rscript to PATH."
  exit 1
}

if ($DryRun) {
  Write-Host "Root: $root"
  Write-Host "Rscript: $rscript"
  Write-Host "Quarto command: quarto preview --port $QuartoPort"
  Write-Host "Shiny command: `"$rscript`" run_app.R $ShinyPort"
  exit 0
}

Start-Process powershell -ArgumentList @(
  "-NoExit",
  "-Command",
  "Set-Location '$root'; quarto preview --port $QuartoPort"
)

Start-Process powershell -ArgumentList @(
  "-NoExit",
  "-Command",
  "Set-Location '$root'; & '$rscript' run_app.R $ShinyPort"
)

Write-Host "Started Quarto preview on port $QuartoPort and Shiny on port $ShinyPort."
