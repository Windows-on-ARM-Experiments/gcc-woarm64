param (
    [Parameter(Mandatory = $false)]
    [string]$Token = $env:PAT_ACTION,

    [Parameter(Mandatory = $false)]
    [string]$Owner = "Windows-on-ARM-Experiments",

    [Parameter(Mandatory = $false)]
    [string]$Repository = "mingw-woarm64-build",

    [Parameter(Mandatory = $false)]
    [string]$Workflow = "advanced.yml",

    [Parameter(Mandatory = $false)]
    [string]$Branch = "woarm64"
)

$ErrorActionPreference = "Stop"

$Headers = @{
    "Authorization" = "Bearer $Token"
    "Content-Type"  = "application/json"
    "Accept"        = "application/vnd.github.v3+json"
}

$Inputs = @{
    gcc_branch = $Branch
}

$Body = @{
    ref = "main"
    inputs = $Inputs
} | ConvertTo-Json

$Response = Invoke-WebRequest `
    -Uri "https://api.github.com/repos/$Owner/$Repository/actions/workflows/$Workflow/dispatches" `
    -Method POST `
    -Headers $Headers `
    -Body $Body
if ($Response.StatusCode -eq 204) {
    Write-Host "Workflow dispatch triggered successfully."
}
else {
    Write-Host "Failed to trigger workflow dispatch. Status code: $($Response.StatusCode)"
    Write-Host "Response body: $($Response.Content)"
}
