$ErrorActionPreference='Stop'
$base='http://192.168.1.235:8790'
Add-Type -AssemblyName System.Net.Http
$handler=New-Object System.Net.Http.HttpClientHandler
$handler.UseProxy=$false
$http=New-Object System.Net.Http.HttpClient($handler)
$http.Timeout=[TimeSpan]::FromSeconds(3)
function Report($path,$data) {
 $body=$null;$response=$null
 try {
  $body=New-Object System.Net.Http.StringContent(($data|ConvertTo-Json -Compress),[Text.Encoding]::UTF8,'application/json')
  $response=$http.PostAsync(($base+$path),$body).GetAwaiter().GetResult()
  $response.EnsureSuccessStatusCode() | Out-Null
 }catch{}finally{if($response){$response.Dispose()};if($body){$body.Dispose()}}
}
$p=New-Object System.IO.Ports.SerialPort 'COM3',115200,'None',8,'Two'
$p.WriteTimeout=1000; $p.DtrEnable=$true; $p.RtsEnable=$true
[byte[]]$zero=New-Object byte[] 70; $zero[0]=126; $zero[1]=6; $zero[2]=65; $zero[69]=231
$voices=New-Object 'System.Collections.Generic.List[object]'
$clock=[Diagnostics.Stopwatch]::StartNew()
$pending=$null;$nextFrame=0.0
function End-Voice($voice,$result='ok') {
 Report '/ack' @{id=$voice.cmd.id;result=$result;fadeFrames=$voice.frames}
}
try {
 $p.Open();$p.Write($zero,0,70)
 Write-Host 'CultureHub DMX v4 - local note envelopes / persistent USB + HTTP'
 while($true){
  $new=$null
  if(-not $pending){$pending=$http.GetStringAsync(($base+'/next?wait=1'))}
  if($pending.IsCompleted){
   try{$raw=$pending.GetAwaiter().GetResult();$cmd=$raw|ConvertFrom-Json}
   catch{$p.Write($zero,0,70);foreach($v in $voices){End-Voice $v 'Network interrupted'};$voices.Clear();$cmd=$null;Report '/event' @{color='off';level=0}}
   $pending=$null
   if($cmd.id){
    try{
     if(($cmd.address -lt 1 -or $cmd.address -gt 59) -or $cmd.channel -lt 0 -or $cmd.channel -gt 6 -or $cmd.level -lt 0 -or $cmd.level -gt 128 -or $cmd.duration -le 0 -or $cmd.duration -gt 3600){throw 'Invalid DMX command'}
     [byte[]]$frame=$zero.Clone();$addresses=@([int]$cmd.address);if($cmd.all -eq $true){$addresses=$cmd.addresses}
     foreach($a in $addresses){
      if($a -lt 1 -or $a -gt 59){throw 'Invalid address'}
      if($cmd.color -eq 'rgb'){
       if($cmd.rgb.Count -ne 3){throw 'Invalid RGB'}
       for($i=0;$i -lt 3;$i++){if($cmd.rgb[$i] -lt 0 -or $cmd.rgb[$i] -gt 128){throw 'Invalid RGB'};$frame[4+[int]$a+$i]=[byte]$cmd.rgb[$i]}
      }else{$frame[4+[int]$a+[int]$cmd.channel]=[byte]$cmd.level}
     }
     $attack=0.0;$decay=0.0
     if($cmd.envelope){$attack=[double]$cmd.envelope.attack;$decay=[double]$cmd.envelope.decay;if($attack -lt 0 -or $attack -gt 3600 -or $decay -lt 0 -or $decay -gt 3600){throw 'Invalid envelope'}}
     if(-not $cmd.envelope -or $cmd.level -eq 0){foreach($v in $voices){End-Voice $v};$voices.Clear()}
     $new=[pscustomobject]@{cmd=$cmd;frame=$frame;started=$clock.Elapsed.TotalSeconds;duration=[double]$cmd.duration;attack=$attack;decay=$decay;frames=0}
     $voices.Add($new);$nextFrame=0
    }catch{Report '/ack' @{id=$cmd.id;result=$_.Exception.Message}}
   }
  }
  $now=$clock.Elapsed.TotalSeconds
  if($voices.Count -gt 0 -and $now -ge $nextFrame){
   [byte[]]$out=$zero.Clone()
   for($j=$voices.Count-1;$j -ge 0;$j--){
    $v=$voices[$j];$elapsed=$now-$v.started
    if($elapsed -ge $v.duration){End-Voice $v;$voices.RemoveAt($j);continue}
    $gain=1.0
    if($v.attack -gt 0 -and $elapsed -lt $v.attack){$gain=$elapsed/$v.attack}
    if($v.decay -gt 0){$decayStart=[Math]::Max(0,$v.duration-$v.decay);if($elapsed -gt $decayStart){$gain*=[Math]::Max(0,1-($elapsed-$decayStart)/$v.decay)}}
    for($k=5;$k -lt 69;$k++){$value=[byte][Math]::Round($v.frame[$k]*$gain);if($value -gt $out[$k]){$out[$k]=$value}}
    $v.frames++
   }
   $p.Write($out,0,70);$nextFrame=$now+.025
   if($new){Report '/event' @{address=$new.cmd.address;color=$new.cmd.color;level=$new.cmd.level;rgb=$new.cmd.rgb;id=$new.cmd.id}}
   if($voices.Count -eq 0){Report '/event' @{color='off';level=0}}
  }
  Start-Sleep -Milliseconds 2
 }
}finally{if($p.IsOpen){$p.Write($zero,0,70);$p.Close()};foreach($v in $voices){End-Voice $v 'Bridge stopped'};Report '/event' @{color='off';level=0}}
