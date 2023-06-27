<?php session_start();
      if (!isset($_SESSION['count'])) $_SESSION['count'] = 0;
?>
<!DOCTYPE html>
<html>
  <head>

  </head>
  <body>

    <h1>dynamic hello!</h1>
    <ul>
      <li> item 1 </li>
      <li> item 2 </li>
      <li> item 3 </li>
      <li> item 4 </li>
      <li> views: <?php
	  $n = $_SESSION['count'];
	  echo (strval($n));
	  $_SESSION['count'] = $n + 1;
      ?>
      </li>
    </ul>

    <a href="."> escape from this file </a>

    <br>

    request:
      <?php echo (var_dump($_SERVER)); ?>

  </body>
</html>
