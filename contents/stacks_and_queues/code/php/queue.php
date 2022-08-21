<?php
/**
 * The SplQueue class provides the main functionalities of a queue.
 * It is integrated into the PHP SPL library which is usable in any PHP application.
 *
 * Note: top() reads from end of the doubly linked list
 *       and will not return the first element. Use bottom() for that.
 *
 * @see https://www.php.net/manual/en/class.splqueue.php
 */
$queue = new SplQueue();

$queue->enqueue(4);
$queue->enqueue(5);
$queue->enqueue(9);

echo $queue->dequeue(), PHP_EOL; // 4 - First in first out
echo $queue->count(), PHP_EOL;   // 2 - Elements in the queue
echo $queue->top(), PHP_EOL;     // 9 - End of the doubly linked list
echo $queue->bottom(), PHP_EOL;  // 5 - Begin of the doubly linked list

// Implementation of own Queue

interface QueueInterface
{
    public function enqueue($value);
    public function dequeue();
    public function front();
    public function count();
}

class Queue implements QueueInterface
{
    private $queue = [];

    public function enqueue($value)
    {
        $this->queue[] = $value;
    }

    public function dequeue()
    {
        return array_shift($this->queue);
    }

    public function front()
    {
        return reset($this->queue);
    }

    public function count()
    {
        return count($this->queue);
    }
}

$queue = new Queue();

$queue->enqueue(4);
$queue->enqueue(5);
$queue->enqueue(9);

echo $queue->dequeue(), PHP_EOL; // 4 - First in first out
echo $queue->count(), PHP_EOL;   // 2 - Elements in the queue
echo $queue->front(), PHP_EOL;   // 5 - Begin of the array
