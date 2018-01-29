function create_tree(n, num_row, num_child){
    n.ID = num_row;
    n.children = [];
        if(num_row == 0){
                return;
        }

        for(var i = 0; i < num_child; ++i){
                var child = new Object();
                create_tree(child, num_row - 1, num_child);
                n.children.push(child);
        }
};

function DFS_recursive(n){
    console.log(n.ID);
    if(n.children.length == 0){
        return;
    }
        for(var i = 0; i < n.children.length; ++i){
                DFS_recursive(n.children[i]);
        }
}

function DFS_stack(n){
    var s = [];
    s.push(n);
    var temp;
    
    while(s.length > 0){
        temp = s.pop();
        console.log(temp.ID);
        for(var i = 0; i < temp.children.length; ++i){
                if(temp.children.length == 0){
                break;
            }
                s.push(temp.children[i]);
        }
    }
}

function BFS_queue(n){
    var q = [];
    q.push(n);
    var temp;
    
    while(q.length > 0){
        temp = q.shift();
        console.log(temp.ID);
        for(var i = 0; i < temp.children.length; ++i){
                if(temp.children.length == 0){
                break;
            }
                q.push(temp.children[i]);
        }
    }
}

var root = new Object();
create_tree(root, 3, 3);
//DFS_recursive(root);
//DFS_stack(root);
BFS_queue(root);
