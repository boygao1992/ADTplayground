<?php
namespace FPPHP;

class Predicates
{
    public static function isFunction($fn): bool {
        return is_callable($fn);
    }
}
