main()

%% Functions

function root = create_tree()
    node = @(k,v) containers.Map(k,v);

    node2  =  node(2, {{}});  node3 =  node(3, {{}});  node4 =  node(4, {{}});
    node6  =  node(6, {{}});  node7 =  node(7, {{}});  node8 =  node(8, {{}});
    node10 = node(10, {{}}); node11 = node(11, {{}}); node12 = node(12, {{}});

    node1  = node(1,  {node2,  node3,  node4});
    node5  = node(5,  {node6,  node7,  node8});
    node9  = node(9, {node10, node11, node12});

    root   = node(0,  {node1,  node5,  node9});
end

function root = create_btree()
    node = @(k,v) containers.Map(k,v);

    node2  =  node(2, {{}});  node3 =  node(3, {{}});
    node5  =  node(5, {{}});  node6 =  node(6, {{}});

    node1  = node(1,  {node2,  node3});
    node4  = node(4,  {node5,  node6});

    root   = node(0,  {node1,  node4});
end

function DFS_recursive(n)
    
    cell_index = @(a, b) a{b};
    ID = cell_index(keys(n), 1);
    
    fprintf('%u ', ID);
    
    children = cell_index(values(n), 1);
    for i = children
        child = i{1};
        if ~isempty(child)
            DFS_recursive(child);
        end
    end
end

function DFS_recursive_postorder(n)
    
    cell_index = @(a, b) a{b};
    
    children = cell_index(values(n), 1);
    for i = children
        child = i{1};
        if ~isempty(child)
            DFS_recursive_postorder(child);
        end
    end
    
    ID = cell_index(keys(n), 1);
    fprintf('%u ', ID);
    
end

function DFS_recursive_inorder_btree(n)
    
    cell_index = @(a, b) a{b};
    ID = cell_index(keys(n), 1);
    children = cell_index(values(n), 1);
    
    if length(children) == 2
        DFS_recursive_inorder_btree(children{1})
        fprintf('%u ', ID)
        DFS_recursive_inorder_btree(children{2})
    elseif length(children) == 1
        if ~isempty(children{1})
            DFS_recursive_inorder_btree(children{1})
        end
        fprintf('%u ', ID)
    else
        fprintf("Not a binary tree!")
    end
end

function DFS_stack(n)

    cell_index = @(a, b) a{b};
    node_stack = {n};
    
    while ~isempty(node_stack)
    
        parent = node_stack{end};
        node_stack(end) = [];
        
        ID = cell_index(keys(parent), 1);
        fprintf('%u ', ID);
        
        children = cell_index(values(parent), 1);
        
        for i = flip(children)
            child = i{1};
            if ~isempty(child)
                node_stack = {node_stack{:} child};
            end
        end
    end
end

function BFS_queue(n)

    cell_index = @(a, b) a{b};
    node_queue = {n};
    
    while ~isempty(node_queue)
        next_nodes = {};
        for parent_cell = node_queue
            parent = parent_cell{1};
            ID = cell_index(keys(parent), 1);
            fprintf('%u ', ID);
            children = cell_index(values(parent), 1);
            for i = children
                child = i{1};
                if ~isempty(child)
                    next_nodes = {next_nodes{:}, child};
                end
            end
        end
        node_queue = next_nodes;
    end
end

function main()
    root  = create_tree();
    rootb = create_btree();
    
    fprintf('\nDFS Recursive\n')
    DFS_recursive(root)
    
    fprintf('\nDFS Recursive Postorder\n')
    DFS_recursive_postorder(root)
    
    fprintf('\nDFS Recursive Inorder Binary Tree\n')
    DFS_recursive_inorder_btree(rootb)
    
    fprintf('\nDFS Stack\n')
    DFS_stack(root)

    fprintf('\nBFS Queue\n')
    BFS_queue(root) 
    fprintf('\n')
end