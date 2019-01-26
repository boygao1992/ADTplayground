<?php

// line comment 1
# line comment 2
/*
  block comment
*/

// NOTE var_dump for value inspection
var_dump(NULL); // NULL
var_dump(true); // bool(true)
var_dump(false); // bool(false)
var_dump(0); // int(0)
var_dump(0.0); // float(0)
var_dump("'"); // string(0)
var_dump('"'); // string(0)
var_dump(array()); // array(0) {}
var_dump([]); // array(0) {} NOTE syntactic sugar for array
var_dump(["a", "b", "c"]);
/* array(3) {
    [0] => string(1) "a"
    [1] => string(1) "b"
    [2] => string(1) "c"
   }
 */
var_dump(["a" => 1, "b" => 2, "c" => 3]);
/* array(3) {
     ["a"] => int(1)
     ["b"] => int(2)
     ["c"] => int(3)
   }
 */
var_dump(new stdClass); // object(stdClass)#1 (0) {} NOTE empty object of stdClass
var_dump((object) []); // object(stdClass)#1 (0) {} NOTE cast an empty array into an empty object of stdClass
var_dump((object) new class {}); // object(class@anonymous)#1 (0) {} NOTE empty object of anonymous class
// TODO missing `resource` type

// NOTE gettype to inspect value's type
echo gettype(NULL); // NULL
echo gettype(true); // boolean
echo gettype(0); // integer
echo gettype(0.0); // double
echo gettype(""); // string
echo gettype([]); // array
echo gettype(new stdClass); // object
// TODO missing `resource` type

/* empty

   http://php.net/manual/en/function.empty.php

- "" (an empty string)
- 0 (0 as an integer)
- 0.0 (0 as a float)
- "0" (0 as a string)
- NULL
- FALSE
- array() (an empty array)

 */

// NOTE difference between `echo` and `print`:
// `print` executes `echo` and returns an integer `1`
$unit = print "anything\n";
echo $unit, "\n";

// String concatenation
echo "a"."b"."c"."\n";
// NOTE `echo` is a special construct that concatenate a list of strings into one.
echo "a", "b", "c", "\n";
// NOTE `echo` coerce other types into String.
echo 1, 2, 3, "\n";

/* Callable */

// function
function hello ($name){
    echo "hello, ", $name, "!\n";
}
hello("world");
call_user_func("hello", "world2"); // NOTE call by name

// function object: objects implementing __invoke method can be called as functions
class Hello {
    public function __invoke($name) {
        echo "Hello, ", $name, "!\n";
    }
}
$hello = new Hello;
$hello("World");
call_user_func($hello, "World 2");

// static method
class EOW {
    public static function hello ($name) {
        echo "hello, ", $name, "!\n";
    }
}
EOW::hello("world 3"); // NOTE static method access by `::`
call_user_func(["EOW", "hello"], "world 4");

// private method
class FirstClass {
    private $name;

    // NOTE private const is not allowed; const is always public and static
    const default_name = "wenbo";

    public function __construct($name = NULL) { // NOTE provide default value
        if (is_null($name)) {
            $this->name = $this::default_name;
        } else {
            $this->name = $name;
        }
    }

    public function hello() {
        echo "hello, ", $this->name, "!\n";
    }
}
$first_instance = new FirstClass("world 5");
$first_instance->hello(); // NOTE private property access by `->`
$first_instance->{"hello"}(); // NOTE property access by name

$second_instance = new FirstClass();
$second_instance->hello();

echo json_encode($first_instance), "\n"; // {}, NOTE private properties will not be serialized

/* Object */

$arr0 = ["a", "b", "c"];
echo $arr0[0], "\n"; // NOTE array index access by []
$arr1 = ["a" => 0, "b" => 1, "c" => 2];
echo $arr1["a"], "\n";

// NOTE construct an anonymous object by casting an array of key-value pairs
$obj0 = (object) ["a" => 0, "b" => 1, "c" => 2];
echo $obj0->b, "\n";
echo $obj0->{"c"}, "\n";

// NOTE construct an anonymous object by mutate public properties
$obj1 = new stdClass;
$obj1->a = 0;
$obj1->b = 1;
$obj1->c = 2;
echo json_encode($obj1), "\n"; // {"a": 0, "b": 1, "c": 2}

/* Magic Methods

   http://php.net/manual/en/language.oop5.magic.php#language.oop5.magic

   __construct
   __destruct
   __call
   __callStatic
   __get
   __set
   __isset
   __unset
   __sleep
   __wakeup
   __toString
   __invoke
   __set_state NOTE php doesn't have a unified naming convention across versions
   __clone
   __debugInfo
 */

/* Abstract Class, Interface, Trait

   http://php.net/manual/en/language.oop5.abstract.php
   http://php.net/manual/en/language.oop5.interfaces.php
   http://php.net/manual/en/language.oop5.traits.php
 */

/* Eq

   (==)
   (===)

   http://php.net/manual/en/language.oop5.object-comparison.php
 */

/* Functor
 */

$arr2 = [0,1,2];
$arr3 = array_map( // NOTE map
    function ($item) {
        return $item * 2;
    },
    $arr2
);
array_walk( // NOTE forEach
    $arr3,
    function ($item, $index) {
        echo $index, " : ",$item, "\n";
    }
);

$sum = array_reduce(
    $arr3,
    function ($acc, $x) {
        return $acc + $x;
    },
    0
);
echo $sum, "\n";

foreach($arr3 as $key => $value) {
    echo $key, " : ", $value, "\n";
}
