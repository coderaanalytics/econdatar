Write-Output "Downloading package from GitHub.com"
Write-Output ""

$zipFilePath = $home + "\Documents\econdatar.zip"

$WebClient = New-Object System.Net.WebClient
$WebClient.DownloadFile("https://github.com/coderaanalytics/econdatar/archive/refs/heads/master.zip", $zipFilePath)

Write-Output "Unzipping source code"
Write-Output ""

$unzipDir = $home + "\Documents"

Expand-Archive $zipFilePath -DestinationPath $unzipDir

$packageFolder0 = $unzipDir + "\econdatar-master"
$packageFolder1 = $unzipDir + "\econdatar"

Rename-Item $packageFolder0 $packageFolder1

Write-Output "Setting up directories..."
Write-Output ""

$rDir = @(Get-ChildItem -Path "C:\Program Files\R" | Sort-Object -Descending)
$rVersion = [regex]::match($rDir[0].name,"\d\.\d").Groups[0].Value
$libPath0 = $home + "\Documents\R\win-library\" + $rVersion
$libPath1 = $libPath0.replace("\","/")
$rProfilePath =  $unzipDir + "\.Rprofile"

If (Test-Path -Path $libPath0) {
  @("R_LIBS_USER=""" + $libPath1 + """") | Out-File -FilePath $rProfilePath -Append -Encoding ascii
  "options(repos=structure(c(CRAN='https://cran.mirror.ac.za')))" | Out-File -FilePath $rProfilePath -Append -Encoding ascii
} Else {
  New-Item -Path $libPath0 -ItemType Directory
  @(".libPaths('" + $libPath1 + "')") | Out-File -FilePath $rProfilePath -Append -Encoding ascii
  "options(repos=structure(c(CRAN='https://cran.mirror.ac.za')))" | Out-File -FilePath $rProfilePath -Append -Encoding ascii
}

Write-Output ""
Write-Output "Installing EconDataR package..."
Write-Output ""

$rBinDir = $rDir[0].Fullname + "\bin\Rscript.exe"
$rscriptDir = @(Get-Item -Path $rBinDir)
$packageFolder2 = $packageFolder1.replace("\","/")

$installCommand = "install.packages('" + $packageFolder2 + "',repos=NULL,type='source')"

Start-Process $rscriptDir[0].Fullname -ArgumentList "-e", "install.packages('httr')" -NoNewWindow -Wait
Start-Process $rscriptDir[0].Fullname -ArgumentList "-e", "install.packages('jsonline')" -NoNewWindow -Wait
Start-Process $rscriptDir[0].Fullname -ArgumentList "-e", "install.packages('xml2')" -NoNewWindow -Wait
Start-Process $rscriptDir[0].Fullname -ArgumentList "-e", $installCommand -NoNewWindow -Wait

Write-Output ""
Write-Output "Cleaning up"
Write-Output ""

Remove-Item $zipFilePath
Remove-Item -Recurse -Force $packageFolder1

Write-Output "Done!"
Write-Output ""
Write-Output "Press any key to exit..."

[Console]::ReadKey()
