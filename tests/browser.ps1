param([string]$Expression = 'document.title', [string]$Screenshot = '', [string]$ExpressionFile = '', [int]$Width=0, [int]$Height=1100, [switch]$Reload, [switch]$Trace, [int]$TimeoutMs=20000)
$ErrorActionPreference = 'Stop'
if ($ExpressionFile) { $Expression = [IO.File]::ReadAllText((Join-Path (Get-Location) $ExpressionFile)) }
$pages = Invoke-RestMethod 'http://127.0.0.1:9224/json/list'
$target = $pages | Where-Object { $_.type -eq 'page' } | Select-Object -First 1
$socket = [Net.WebSockets.ClientWebSocket]::new()
$cancel = [Threading.CancellationToken]::None
$script:cdpEvents = @()
$null = $socket.ConnectAsync([Uri]$target.webSocketDebuggerUrl,$cancel).GetAwaiter().GetResult()
function Invoke-Cdp([int]$id,[string]$method,$parameters) {
  $json = @{ id=$id; method=$method; params=$parameters } | ConvertTo-Json -Depth 30 -Compress
  $bytes = [Text.Encoding]::UTF8.GetBytes($json)
  $socket.SendAsync([ArraySegment[byte]]::new($bytes),[Net.WebSockets.WebSocketMessageType]::Text,$true,$cancel).GetAwaiter().GetResult()
  do {
    $buffer = [byte[]]::new(1048576)
    $stream = [IO.MemoryStream]::new()
    do {
      $received = $socket.ReceiveAsync([ArraySegment[byte]]::new($buffer),$cancel).GetAwaiter().GetResult()
      $stream.Write($buffer,0,$received.Count)
    } while (-not $received.EndOfMessage)
    $reply = [Text.Encoding]::UTF8.GetString($stream.ToArray()) | ConvertFrom-Json
    if ($reply.method -in @('Runtime.exceptionThrown','Runtime.consoleAPICalled','Log.entryAdded')) { $script:cdpEvents += $reply }
    $stream.Dispose()
  } while ($reply.id -ne $id)
  $reply
}
if ($Width -gt 0) { $null=Invoke-Cdp 99 'Emulation.setDeviceMetricsOverride' @{width=$Width;height=$Height;deviceScaleFactor=1;mobile=$false} }
if ($Width -gt 0) { Start-Sleep -Milliseconds 800 }
if ($Trace) { $null=Invoke-Cdp 95 'Runtime.enable' @{}; $null=Invoke-Cdp 96 'Log.enable' @{} }
if ($Reload) {
  $null=Invoke-Cdp 97 'Page.addScriptToEvaluateOnNewDocument' @{source='window.__v2Errors=[];window.addEventListener("error",e=>window.__v2Errors.push({message:e.message,file:e.filename,line:e.lineno}));window.addEventListener("unhandledrejection",e=>window.__v2Errors.push({message:String(e.reason),stack:e.reason?.stack}));'}
  $null=Invoke-Cdp 98 'Page.reload' @{ignoreCache=$true}
  Write-Output 'Page reloaded; errors will be recorded in window.__v2Errors.'
  $socket.Dispose()
  exit
}
if ($Screenshot) {
  $result = Invoke-Cdp 1 'Page.captureScreenshot' @{format='png';captureBeyondViewport=$false}
  [IO.File]::WriteAllBytes((Join-Path (Get-Location) $Screenshot),[Convert]::FromBase64String($result.result.data))
  Write-Output "Screenshot: $Screenshot"
} else {
  $result = Invoke-Cdp 1 'Runtime.evaluate' @{expression=$Expression;returnByValue=$true;awaitPromise=$true;timeout=$TimeoutMs}
  $result.result | ConvertTo-Json -Depth 20 -Compress
}
$socket.Dispose()
if ($Trace) { $script:cdpEvents | ConvertTo-Json -Depth 20 -Compress }
