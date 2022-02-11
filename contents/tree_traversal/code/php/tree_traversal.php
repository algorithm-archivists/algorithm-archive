<?php
declare(strict_types=1);

class Tree implements JsonSerializable
{
    private $id;
    private $children = [];

    public function __construct(int $id, array $children = [])
    {
        $this->id = $id;
        $this->children = $children;
    }

    public function getId(): int
    {
        return $this->id;
    }

    public function getChildren(): array
    {
        return $this->children;
    }

    public function addChild(Tree $child): void
    {
        $this->children[] = $child;
    }

    public function jsonSerialize(): array
    {
        return [
            'id' => $this->id,
            'children' => $this->children,
        ];
    }
}

class TreeTraversal
{
    public static function DFSRecursive(Tree $tree): void
    {
        echo $tree->getId() . ' ';
        foreach ($tree->getChildren() as $child) {
            static::DFSRecursive($child);
        }
    }

    public static function DFSRecursivePostorder(Tree $tree): void
    {
        foreach ($tree->getChildren() as $child) {
            static::DFSRecursivePostorder($child);
        }
        echo $tree->getId() . ' ';
    }

    public static function DFSRecursiveInorderBinary(Tree $tree): void
    {
        switch (count($tree->getChildren())) {
            case 2:
                static::DFSRecursiveInorderBinary($tree->getChildren()[0]);
                echo $tree->getId() . ' ';
                static::DFSRecursiveInorderBinary($tree->getChildren()[1]);
                break;
            case 1:
                static::DFSRecursiveInorderBinary($tree->getChildren()[0]);
                echo $tree->getId() . ' ';
                break;
            case 0:
                echo $tree->getId() . ' ';
                break;
            default:
                throw new InvalidArgumentException('Not a binary tree!');
                break;
        }
    }

    public static function DFSStack(Tree $tree): void
    {
        $stack = [$tree];
        $temp = null;

        while (null !== ($temp = array_pop($stack))) {
            echo $temp->getId() . ' ';
            foreach ($temp->getChildren() as $child) {
                $stack[] = $child;
            }
        }
    }

    public static function DFSQueue(Tree $tree): void
    {
        $stack = [$tree];
        $temp = null;

        while (null !== ($temp = array_shift($stack))) {
            echo $temp->getId() . ' ';
            foreach ($temp->getChildren() as $child) {
                $stack[] = $child;
            }
        }
    }
}

function generate_tree(int $numOfRows, int $numOfChildren): Tree
{
    $node = new Tree($numOfRows);

    if ($numOfRows > 0) {
        for ($i = 0; $i < $numOfChildren; $i++) {
            $child = generate_tree($numOfRows - 1, $numOfChildren);
            $node->addChild($child);
        }
    }

    return $node;
}

$node = generate_tree(2, 3);

echo '[#]' . PHP_EOL . 'Recursive DFS:' . PHP_EOL;
TreeTraversal::DFSRecursive($node);
echo PHP_EOL;

echo '[#]' . PHP_EOL . 'Recursive Postorder DFS:' . PHP_EOL;
TreeTraversal::DFSRecursivePostorder($node);
echo PHP_EOL;

echo '[#]' . PHP_EOL . 'Stack-based DFS:' . PHP_EOL;
TreeTraversal::DFSStack($node);
echo PHP_EOL;

echo '[#]' . PHP_EOL . 'Queue-based BFS:' . PHP_EOL;
TreeTraversal::DFSQueue($node);
echo PHP_EOL;

// If you want to try to run binary order on a non-binary tree,
// comment out the generation of the new tree below.
// If you do that, an exception will be thrown
$node = generate_tree(3, 2);
echo '[#]' . PHP_EOL . 'Recursive Inorder DFS for Binary Tree:' . PHP_EOL;
TreeTraversal::DFSRecursiveInorderBinary($node);
echo PHP_EOL;
