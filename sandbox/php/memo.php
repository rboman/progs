<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<title>Document sans titre</title>
</head>

<body>

<?php

#gettype($var) =  recupere le type
#booleens = true/false
#settype($var, 'double')       # cast
#$newtest = (integer)$test;    # cast par copie
#"hello"."world"               # . = concatenation
# $x === 4                     # =true si == et meme type
#define("CONSTANT_NAME", 42);  # pas de "$" pour y acceder apres!!
#
# if, switch, while, do/while, for  = idem au C
# (a)? b:c ;   = idem au C
#
#function addNums($firstnum, $secondnum) {
#$result = $firstnum + $secondnum;
#return $result;
#}
#global $var1, $var2     # accede a $var1 et $var2 en defors du bloc de la fct
#static = idem au C
#args par defaut = idem au C
#function addFive(&$num) { # passage par ref
#$num += 5;
#}
#create_function('$txt', 'return "&quot;$txt&quot;";'));
#
#$rainbow = array("red", "orange", "yellow", "green", "blue", "indigo", "violet");
#$rainbow[] = "red";
#$rainbow[] = "orange";
#
#$character = array(
#"name" => "Bob",
#"occupation" => "superhero",
#"age" => 30,
#"special power" => "x-ray vision"
#);
#$character['occupation'];

#count($tab) and sizeof($tab)                       # taille 
#while (list($key, $val) = each($characater)) {     # iterateurs
#     echo "$key has a value of $val <br>";
#}
#foreach($characater as $c) {
#     echo "The value is $c <br>";
#}
#reset($character);                                 # reset du ptr d'iteration (!)
# array_push(), array_pop()                         # push_back(), pop_back() au C++
# array_shift(), array_unshift()                    # push_front(), pop_front() au C++
#$newArray = array_merge($array1, $array2);
#$keysArray = array_keys($existingArray);
#$valuesArray = array_values($existingArray);
#
#class myCar {
#     var $color = "silver";
#     var $make = "Mazda";
#     var $model = "Protege5";
#    function myCar($n) {          # constructeur
#         $this->make = $n;
#    }
#    function setModel($n) {
#         $this->model = $n;
#    }
#}
# $car =  new myCar();
# echo "I drive a ".$car -> color." ".$car -> make." ".$car -> model;
# derivation = "extends"
#
# printf("This is my number: %d", 55 );   # idem au C
# strlen 
# strstr($str, $sub)                 # $sub est-il une partie de $str? 
# strpos($str, $sub)
# substr($str, $pos, $length), substr_replace(...) # fonctionne avec des arrays (!) 
# while (is_string($word)) {...}
# strtok($test, $delims);            # coupe une string
# trim(), ltrim(), rtrim()           # vire les espaces en trop (tous, a gauche et a droite)
# strip_tags()                       # vire le code html 
# strtoupper
# nl2br()                            # convertit \n en <br>
# wordwrap()                         # coupe une string
#
#$start_date = "2003-08-12";
#$date_array = explode("-", $start_date);
#// $date[0] == "2003"
#// $date[1] == "08"
#// $date[2] == "12"

#time()
#$date_array = getdate(); // no argument passed so today's date will be used
# foreach ($date_array as $key => $val) {
#     echo "$key = $val<br>";
# }

# ---

#<form action="listing9.4.php" method="POST">

#<input type="text">       : entree 1 ligne   
#<input type="submit">     : bouton envoi   
#<textarea>    : entree plusiuers lignes
#<select> / <option>     : selection / option de selection

# parametre <   name="" > : nom de la variable - mettre un [] si array
# parametre <   name="" value=""> : nom du champ dans l'array name

#$_SERVER[PHP_SELF]   # nom du script courant. 

#<input type="hidden" name="num_tries" value="<?php echo $num_tries >"> # permet de sauver un variable

#file upload: => remplit _FILES
#<form action="listing9.14.php" enctype="multipart/form-data" method="POST">
# <input type="file"> ...


# fct pre definies
#------------------
# print() = echo()
# function_exists($func);      # teste l'existence de la fct
# empty($str);                 # teste la chaine vide
# isset($_POST[pipo]))         # teste lsi l'array $_POST contient un champ pipo
#  

#sessions
#--------
# ecrire session_start() dans chaque doc.
# => $_SESSION est un tableau global ou on peut ranger n'importe quoi.
# unset($_SESSION[test]); # vire la clef test


# files
# ------

# $pipo = include("fichier")   # "fichier" fait un "return valeur;" (! include_path dans php.ini)
# include_once("fichier")      # utile pour libs 
# require ~= include
# file_exists($file)
# is_file($file); is_dir($dir);
# is_readable(), is_writable, ...
# fileatime(), filemtime(), ...
# touch()
# unlink($file)     # = remove file
# ($fp = fopen("test.txt", "w")) or die ("Couldn't open file, sorry");
# $line = fgets($fp, 1024);   fread(), fgetc(), ...
# flock()   # bloque le fichier pour 1 utilisateur
# opendir(), readdir()
#
# popen() => idem avec pipes
# exec(), system()         
#
#

?>


<?php

$conn = mysql_connect("localhost", "webuser", "odie");
#echo $conn;

mysql_select_db("robo",$conn);

# ajoute l'entree --
if( isset($_POST['nom']) && isset($_POST['email']))
{
    $sql = "INSERT INTO address values ('', '".$_POST['nom']."', '".$_POST['email']."')";
	echo $sql;
	$result = mysql_query($sql, $conn) or die(mysql_error());
	if ($result) {
      echo "record ".$_POST['nom']." added!<br>";
     } else {
     echo "problem adding ".$_POST['nom']."!<br>";
     }

echo "$result<br>";
echo $_POST;
echo "<br>";
}

# lit la DB ---
$sql = "SELECT * FROM address";
$result = mysql_query($sql, $conn) or die(mysql_error());
$number_of_rows = mysql_num_rows($result);
echo "Number of records = ".$number_of_rows."<br/>";

 while ($newArray = mysql_fetch_array($result)) {
     // give a name to the fields
    $id  = $newArray['id'];
    $nameField = $newArray['nom'];
    $emailField = $newArray['email'];
     //echo the results onscreen
    echo "The email of $nameField (id=$id) is $emailField<br>";
 }


echo "closing connexion<br/>";


mysql_close($conn);



?>



<form name="form1" method="post" action="memo.php">
  Nom 
  <input name="nom" type="text" id="nom"> 
  e-mail 
  <input name="email" type="text" id="email">
  <input type="submit" name="Submit" value="Ajouter">
</form>
<p>&nbsp;</p>
</body>
</html>

