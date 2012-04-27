<?php

/* This file is used by the Zabbix PHP web frontend.
 * It is pre-filled with the information asked during
 * installation of the zabbix-server-* package.
 */

global $DB;

$DB["TYPE"]      = "pgsql";
$DB["SERVER"]    = "localhost";
$DB["PORT"]      = "0";
$DB["DATABASE"]  = "zabbix";
$DB["USER"]      = "zabbix";
$DB["PASSWORD"]  = "@@ZABBIX_DB_PASSWORD@@";
$ZBX_SERVER      = "127.0.0.1";
$ZBX_SERVER_PORT = "10051";

$IMAGE_FORMAT_DEFAULT    = IMAGE_FORMAT_PNG;


/* dbconfig-common uses the database types (e.g. "sqlite3")
 * in lowercase. But Zabbix expects these in uppercase.
 */
## dont remove this!
## This is a work-around for dbconfig-common
if($DB["TYPE"] == "mysql") 
	$DB["TYPE"] = "MYSQL";

if($DB["TYPE"] == "pgsql")
	$DB["TYPE"] = "POSTGRESQL";

if($DB["TYPE"] == "sqlite3")
	$DB["TYPE"] = "SQLITE3";
##
?>
