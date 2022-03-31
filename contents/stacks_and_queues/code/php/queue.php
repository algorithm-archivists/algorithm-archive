<?php

/**
 * @template T
 */
interface IQueue
{
    /**
     * Removes the first element from the queue and returns it.
     * @return T | null
     */
    public function dequeue();

    /**
     * Adds an element at the end of the queue and returns the new size.
     * @param T $element
     */
    public function enqueue($element): int;

    /**
     * Returns the length of the queue.
     */
    public function size(): int;

    /**
     * Returns the first element of the queue.
     * @return T
     */
    public function front();
}

/**
 * @template T
 * @implements IQueue<T>
 */
class Queue implements IQueue
{
    /**
     * @var array<T> $elements
     */
    private $elements = [];

    public function dequeue()
    {
        return array_shift($this->elements);
    }

    public function enqueue($element): int
    {
        array_push($this->elements, $element);
        return $this->size();
    }

    public function size(): int
    {
        return count($this->elements);
    }

    public function front()
    {
        return $this->elements[0];
    }
}

function example_queue(): void
{
    /**
     * @var Queue<int> $int_queue
     */
    $int_queue = new Queue();

    $int_queue->enqueue(4);
    $int_queue->enqueue(5);
    $int_queue->enqueue(7);

    echo $int_queue->dequeue() . "\n"; // 4
    echo $int_queue->size() . "\n"; // 2
    echo $int_queue->front() . "\n"; // 5
}

example_queue();
