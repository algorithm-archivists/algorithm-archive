<?php

/**
 * @template T
 */
interface IStack
{
    /**
     * Removes the last element from the stack and returns it.
     * @return T | null
     */
    public function pop();

    /**
     * Adds an element at the end of the stack and returns the new size.
     * @param T $element
     */
    public function push($element): int;

    /**
     * Returns the length of the stack.
     */
    public function size(): int;

    /**
     * Returns the first element of the stack.
     * @return T
     */
    public function top();
}

/**
 * @template T
 * @implements IStack<T>
 */
class Stack implements IStack
{
    /**
     * @var array<T> $elements
     */
    private $elements = [];

    public function pop()
    {
        return array_pop($this->elements);
    }

    public function push($element): int
    {
        array_push($this->elements, $element);
        return $this->size();
    }

    public function size(): int
    {
        return count($this->elements);
    }

    public function top()
    {
        return $this->elements[0];
    }
}

function example_stack(): void
{
    /**
     * @var Stack<int> $int_stack
     */
    $int_stack = new Stack();

    $int_stack->push(4);
    $int_stack->push(5);
    $int_stack->push(7);

    echo $int_stack->pop() . "\n"; // 7
    echo $int_stack->size() . "\n"; // 2
    echo $int_stack->top() . "\n"; // 4
}

example_stack();

