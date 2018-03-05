

<?php
$inipath = php_ini_loaded_file();

if ($inipath) {
    echo 'Loaded php.ini: ' . $inipath;
} else {
    echo 'A php.ini file is not loaded';
}

?>

<br/>

<?php
    $function_name = "ldap_connect";
    if ( function_exists($function_name) ) {
        echo "$function_name is enabled";
    }
    else {
        echo "$function_name is not enabled";
    }
?>

