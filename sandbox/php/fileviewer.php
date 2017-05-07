<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>

<head>
  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
  <title>Document sans titre</title>
</head>

<body>

  <?php
#$dirname = $_GET['dir'];

echo "<table width=\"100%\"  border=\"0\">";
echo " <tr>";
echo "   <th scope=\"col\">Fichier</th>";
echo "   <th scope=\"col\">Taille</th>";
echo " </tr>";


$dh = opendir('.') or die("couldn't open directory");
echo "<h1>PDF</h1>";
while (!(($file = readdir($dh)) === false ) )
{
    if( strstr("$file",".pdf") ||
    strstr("$file",".PDF")     )
    {
        echo "<tr>";
        echo "<td>";
        echo "<a href=\"" . $file . "\">";
        echo "$file";
        echo "</a>";
        echo "</td>";
        echo "<td>";
        echo (int)(filesize("$file")/1024) . " Ko";
        echo "</td>";
        echo "</tr>";
    }
}
closedir($dh);

echo "</table>";

?>


    <p>&nbsp;</p>
    <p>&nbsp;</p>
</body>

</html>