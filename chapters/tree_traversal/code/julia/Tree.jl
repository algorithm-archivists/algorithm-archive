using DataStructures

struct Node
    children::Vector{Node}
    ID::Int64
    Node(ID::Int64) = new(Vector{Node}(), ID)
end

function DFS_recursive(n::Node)
    # Here we are doing something...
    println(n.ID)

    for child in n.children
        DFS_recursive(child)
    end
end

function DFS_recursive_postorder(n::Node)

    for child in n.children
        DFS_recursive_postorder(child)
    end

    # Here we are doing something...
    println(n.ID)
end

# This assumes only 2 children
function DFS_recursive_inorder_btree(n::Node)

    if (length(n.children) > 2)
        println("Not a binary tree!")
        exit(1)
    end

    if (length(n.children) > 0)
        DFS_recursive_inorder_btree(n.children[0])
        println(n.ID)
        DFS_recursive_inorder_btree(n.children[1])
    else
        println(n.ID)
    end
end

function DFS_stack(n::Node)
    s = Stack(Node)
    push!(s, n)

    while(length(s) > 0)
        println(top(s).ID)
        temp = pop!(s)
        for child in temp.children
            push!(s, child)
        end
    end
end

function BFS_queue(n::Node)
    q = Queue(Node)
    enqueue!(q, n)

    while(length(q) > 0)
        println(front(q).ID)
        temp = dequeue!(q)
        for child in temp.children
            enqueue!(q, child)
        end
    end
end

# function to create a simple, balanced tree
function create_tree(num_row::Int64, num_child::Int64)
    ret = Node(num_row)
    if (num_row == 0)
        return ret
    end

    for i = 1:num_child
        child = create_tree(num_row - 1, num_child)
        push!(ret.children, child)
    end

    return ret
end

function main()

    println("Creating Tree")
    root = create_tree(2,3)

    println("Using recursive DFS:")
    DFS_recursive(root);

    println("Using stack-based DFS:")
    DFS_stack(root);

    println("Using queue-based BFS:")
    BFS_queue(root);
end

main()
