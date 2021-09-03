use std::collections::VecDeque;

#[derive(Debug)]
struct Node {
    children: Vec<Node>,
    value: u64,
}

fn dfs_recursive(n: &Node) {
    print!("{} ", n.value);

    for child in &n.children {
        dfs_recursive(child);
    }
}

fn dfs_recursive_postorder(n: &Node) {
    for child in &n.children {
        dfs_recursive_postorder(child);
    }

    print!("{} ", n.value);
}

fn dfs_recursive_inorder_btree(n: &Node) {
    match &n.children[..] {
        [left, right] => {
            dfs_recursive_inorder_btree(left);
            print!("{} ", n.value);
            dfs_recursive_inorder_btree(right);
        }
        [left] => {
            dfs_recursive_inorder_btree(left);
            print!("{} ", n.value);
        }
        [] => print!("{} ", n.value),
        _ => print!("This is not a binary tree. "),
    }
}

fn dfs_stack(n: &Node) {
    let mut stack = vec![n];

    while let Some(current) = stack.pop() {
        print!("{} ", current.value);
        stack.extend(&current.children);
    }
}

fn bfs_queue(n: &Node) {
    let mut queue = VecDeque::new();
    queue.push_back(n);

    while let Some(current) = queue.pop_front() {
        print!("{} ", current.value);
        queue.extend(&current.children);
    }
}

fn create_tree(num_row: u64, num_child: u64) -> Node {
    if num_row == 0 {
        return Node {
            children: vec![],
            value: 0,
        };
    }

    let children = (0..num_child)
        .map(|_| create_tree(num_row - 1, num_child))
        .collect();

    Node {
        children,
        value: num_row,
    }
}

fn main() {
    let root = create_tree(2, 3);

    println!("[#] Recursive DFS:");
    dfs_recursive(&root);
    println!();

    println!("[#] Recursive Postorder DFS:");
    dfs_recursive_postorder(&root);
    println!();

    println!("[#] Stack-based DFS:");
    dfs_stack(&root);
    println!();

    println!("[#] Queue-based BFS:");
    bfs_queue(&root);
    println!();

    println!("[#] Recursive Inorder DFS for Binary Tree:");
    let root_binary = create_tree(3, 2);
    dfs_recursive_inorder_btree(&root_binary);
    println!();
}
