<?php

$arr = [];
$arr[] = 0;
$arr[] = 1;
var_dump($arr);

$map = array(
    "A" => 1,
    "B" => 2
);

foreach ($map as $k => $v) {
    echo "key: " . $k . ", " . "value: " . $v . "\n";
}
