using DataStructures, Printf

struct Node
    children::Vector{Node}
    ID::Int64
    Node(ID::Int64) = new(Vector{Node}(), ID)
end

function DFS_recursive(n::Node)
    # Here we are doing something...
    @printf("%i ", n.ID)

    for child in n.children
        DFS_recursive(child)
    end
end

function DFS_recursive_postorder(n::Node)

    for child in n.children
        DFS_recursive_postorder(child)
    end

    # Here we are doing something...
    @printf("%i ", n.ID)
end

# This assumes only 2 children, but accounts for other possibilities
function DFS_recursive_inorder_btree(n::Node)

    if (length(n.children) == 2)
        DFS_recursive_inorder_btree(n.children[1])
        @printf("%i ", n.ID)
        DFS_recursive_inorder_btree(n.children[2])
    elseif (length(n.children) == 1)
        DFS_recursive_inorder_btree(n.children[1])
        @printf("%i ", n.ID)
    elseif (length(n.children) == 0)
        @printf("%i ", n.ID)
    else
        println("Not a binary tree!")
    end
end

function DFS_stack(n::Node)
    s = Stack{Node}()
    push!(s, n)

    while(length(s) > 0)
        @printf("%i ", top(s).ID)
        temp = pop!(s)
        for child in temp.children
            push!(s, child)
        end
    end
end

function BFS_queue(n::Node)
    q = Queue{Node}()
    enqueue!(q, n)

    while(length(q) > 0)
        @printf("%i ", first(q).ID)
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
    root = create_tree(2, 3)

    println("[#] Recursive DFS:")
    DFS_recursive(root);
    println()

    println("[#] Recursive Postorder DFS:")
    DFS_recursive_postorder(root);
    println()

    println("[#] Stack-based DFS:")
    DFS_stack(root);
    println()

    println("[#] Queue-based BFS:")
    BFS_queue(root);
    println()

    root_binary = create_tree(3,2)
    println("[#] Recursive Inorder DFS for Binary Tree:")
    DFS_recursive_inorder_btree(root_binary)
    println()
end

main()
