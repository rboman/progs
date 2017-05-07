<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>

<head>
  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
  <title>CPU converter v0 (RoBo)</title>
</head>

<body>
  <p>Enter your time in second :</p>
  <form name="form1" method="post" action="">
    <?php
$val=0;
if(!empty($_POST['cpu']))
sscanf($_POST['cpu'],"%d",$val);
echo "<input type=\"text\" name=\"cpu\" value=\"".$val."\">";
?>
      <input type="submit" name="Submit" value="=>">
      <?php

if(!empty($_POST['cpu']))
{
    $cpu = $_POST['cpu'];
    
    $cpu_j = (int)($cpu / (24*3600));
    $cpu_h = (int)(($cpu - $cpu_j*(24*3600)) / 3600);
    $cpu_m = (int)(($cpu - $cpu_j*(24*3600) - $cpu_h*3600) / 60);
    $cpu_s = (int)($cpu - $cpu_j*(24*3600) - $cpu_h*3600 - $cpu_m*60);
    echo $cpu_j." day(s) ".$cpu_h." hour(s) ".$cpu_m." min ".$cpu_s." seconds<br>";
    
}
?>
  </form>
</body>

</html>