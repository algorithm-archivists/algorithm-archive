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
        if ($tree->getId()) {
            echo $tree->getId() . PHP_EOL;
        }
        foreach ($tree->getChildren() as $child) {
            static::DFSRecursive($child);
        }
    }

    public static function DFSRecursivePostorder(Tree $tree): void
    {
        foreach ($tree->getChildren() as $child) {
            static::DFSRecursivePostorder($child);
        }
        echo $tree->getId() . PHP_EOL;
    }

    public static function DFSRecursiveInorderBinary(Tree $tree): void
    {
        switch (count($tree->getChildren())) {
            case 2:
                static::DFSRecursiveInorderBinary($tree->getChildren()[0]);
                echo $tree->getId() . PHP_EOL;
                static::DFSRecursiveInorderBinary($tree->getChildren()[1]);
                break;
            case 1:
                static::DFSRecursiveInorderBinary($tree->getChildren()[0]);
                echo $tree->getId() . PHP_EOL;
                break;
            case 0:
                echo $tree->getId() . PHP_EOL;
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
            echo $temp->getId() . PHP_EOL;
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
            echo $temp->getId() . PHP_EOL;
            foreach ($temp->getChildren() as $child) {
                $stack[] = $child;
            }
        }
    }
}

function generate_tree(int $num_of_rows, int $num_of_children, int $id = -1): Tree
{
    if ($id === -1) {
        $id = 1;
    }
    $node = new Tree($id);

    if ($num_of_rows > 1) {
        for ($i = 0; $i < $num_of_children; $i++) {
            $child = generate_tree($num_of_rows - 1, $num_of_children, $id * 10 + $i + 1);
            $node->addChild($child);
        }
    }

    return $node;
}

$node = generate_tree(3, 3);

echo 'DFS Recursive:' . PHP_EOL;
TreeTraversal::DFSRecursive($node);

echo 'DFS Recursive Postorder:' . PHP_EOL;
TreeTraversal::DFSRecursivePostorder($node);

echo 'DFS Stack:' . PHP_EOL;
TreeTraversal::DFSStack($node);

echo 'DFS Queue:' . PHP_EOL;
TreeTraversal::DFSQueue($node);

// If you want try binary order non-binary tree
// Comment generation of new tree bellow
// If you do that, exception will be thrown
$node = generate_tree(3, 2);
echo 'DFS Recursive Inorder Binary:' . PHP_EOL;
TreeTraversal::DFSRecursiveInorderBinary($node);
