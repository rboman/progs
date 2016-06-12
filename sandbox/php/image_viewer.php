<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<title>Document sans titre</title>
</head>

<body>

<?php
 $dirname = $_GET['dir'];
 #echo getcwd();
 $dh = opendir($dirname) or die("couldn't open directory");
 echo "<h1>Fichiers dans " . $dirname . "</h1>"; 
 while (!(($file = readdir($dh)) === false ) ) 
 {
     if((strstr("$dirname/$file",".jpg") || 
	     strstr("$dirname/$file",".JPG"))  && !strstr("$dirname/$file","TN_"))
	 {
      echo "<a href=" . $dirname ."/" . $file . ">";
	  echo "<img src=\"" . "$dirname/$file" . "\" width=\"100\">";
	  echo "</a>";
	 echo " (" . (int)(filesize("$dirname/$file")/1024) . " Ko)";
	 echo " modified on " . date("D d M Y g:i A", filemtime("$dirname/$file"));
	 echo "<br>";
     }
}
closedir($dh);


$date_array = getdate(); 
echo "<p>date: ".$date_array['mon']."/".$date_array['mday']."/".$date_array['year']."<p>";

?>


</body>
</html>
