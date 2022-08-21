<?php
/**
 * The SplStack class provides the main functionalities of a stack.
 * It is integrated into the PHP SPL library which is usable in any PHP application.
 *
 * @see https://www.php.net/manual/en/class.splstack.php
 */
$stack = new SplStack();

$stack->push(4);
$stack->push(5);
$stack->push(9);

echo $stack->pop(), PHP_EOL;    // 9 - Last in first out
echo $stack->count(), PHP_EOL;  // 2 - Elements in the stack
echo $stack->top(), PHP_EOL;    // 5 - End of the doubly linked list
echo $stack->bottom(), PHP_EOL; // 4 - Begin of the doubly linked list

// Implementation of own Stack

interface StackInterface
{
    public function push($value);
    public function pop();
    public function top();
    public function bottom();
    public function count();
}

class Stack implements StackInterface
{
    private $stack = [];

    public function push($value)
    {
        $this->stack[] = $value;
    }

    public function pop()
    {
        return array_pop($this->stack);
    }

    public function top()
    {
        return end($this->stack);
    }

    public function bottom()
    {
        return reset($this->stack);
    }

    public function count()
    {
        return count($this->stack);
    }
}

$stack = new Stack();

$stack->push(4);
$stack->push(5);
$stack->push(9);

echo $stack->pop(), PHP_EOL;    // 9 - Last in first out
echo $stack->count(), PHP_EOL;  // 2 - Elements in the stack
echo $stack->top(), PHP_EOL;    // 5 - End of the array
echo $stack->bottom(), PHP_EOL; // 4 - Begin of the array
