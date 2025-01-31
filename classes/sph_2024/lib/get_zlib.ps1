$version="1.3.1"
$folder="zlib-$version"
$file=$folder+".zip"

if (Test-Path $file) {
  Write-Host "Zlib already downloaded"
} else {
  Write-Host "Downloading Zlib"
  $url="https://github.com/madler/zlib/archive/refs/tags/v$version.zip"
  Invoke-WebRequest -Uri $url -OutFile $file
}

if (Test-Path $folder) {
  Write-Host "Zlib already extracted"
} else {
  Write-Host "Extracting Zlib"
  Expand-Archive -Path $file -DestinationPath .
}

