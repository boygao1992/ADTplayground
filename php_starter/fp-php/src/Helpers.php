<?php
declare(strict_types=1);

namespace FPPHP;

use FPPHP\Predicates;

class Helpers
{
    public static function curry(callable $fn): callable
    {
        $refl = new ReflectionFunction($fn);
        $num_of_args = $refl->getNumberOfParameters();
        $apply_curry = function ( array $args1 )
            use ($fn, $num_of_args, &$apply_curry)
            {
                return function ( ...$args2 )
                    use ($fn, $num_of_args, &$apply_curry, $args1)
                    {
                        $args = array_merge($args1, $args2);
                        return count($args) >= $num_of_args
                                             ? call_user_func_array($fn, $args)
                                             : $apply_curry($args);
                    };
            };
        return $apply_curry([]);
    }

    // Category p => compose :: p b c -> p a b -> p a c
    public static function compose(callable $a, callable $b): callable
    {
        return function ($x) {
            return $b($a($x));
        };
    }
}
