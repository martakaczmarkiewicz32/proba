Param()
$out = 'line_changes_report.txt'
if (Test-Path $out) { Remove-Item $out }
$lines = git log --pretty=format:'--%an' --numstat
$current = ''
$hash = @{}
foreach ($l in $lines) {
    if ($l -like '--*') {
        $current = $l.Substring(2).Trim()
        if (-not $hash.ContainsKey($current)) { $hash[$current] = @{added=0; deleted=0} }
    } elseif ($l -match '^[0-9\-]+') {
        $parts = $l -split '\s+'
        $a = 0; $d = 0
        if ($parts[0] -match '^[0-9]+$') { $a = [int]$parts[0] }
        if ($parts[1] -match '^[0-9]+$') { $d = [int]$parts[1] }
        $hash[$current].added += $a
        $hash[$current].deleted += $d
    }
}
$total = 0
foreach ($k in $hash.Keys) { $total += ($hash[$k].added + $hash[$k].deleted) }
Add-Content $out "Total lines changed: $total"
foreach ($k in $hash.Keys | Sort-Object) {
    $a = $hash[$k].added; $d = $hash[$k].deleted; $t = $a + $d
    if ($total -gt 0) { $p = [math]::Round(100*($t/$total),2) } else { $p = 0 }
    Add-Content $out "$k - Added:$a Deleted:$d Total:$t Percent:$p%"
}
Get-Content $out
