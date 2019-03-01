<?php

$time = new \DateTime("+1 hour");
var_dump($time);
var_dump($time->getTimezone());

$interval = new \DateInterval('P1D');
var_dump($interval);

// AFRICA = 1 ;
// AMERICA = 2 ;
// ANTARCTICA = 4 ;
// ARCTIC = 8 ;
// ASIA = 16 ;
// ATLANTIC = 32 ;
// AUSTRALIA = 64 ;
// EUROPE = 128 ;
// INDIAN = 256 ;
// PACIFIC = 512 ;
// UTC = 1024 ;
// ALL = 2047 ;
// ALL_WITH_BC = 4095 ;
// PER_COUNTRY = 4096 ;
