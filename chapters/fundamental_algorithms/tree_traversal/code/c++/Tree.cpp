struct node{
    std::vector<node> children;
    int ID;
};

void DFS_recursive(const node& n){

    // Here we are doing something...
    std::cout << n.ID << '\n';

    for (int i = 0; i < n.children.size(); ++i){
        DFS_recursive(n.children[i]);
    }
}

void DFS_recursive_postorder(const node& n){

    for (int i = 0; i < n.children.size(); ++i){
        DFS_recursive_postorder(n.children[i]);
    }

    // Here we are doing something...
    std::cout << n.ID << '\n';
}


// This assumes only 2 children
void DFS_recursive_inorder_btree(const node& n){

    if (n.children.size() > 2){
        std::cout << "Not binary tree!" << '\n';
        exit(1);
    }

    if (n.children.size() > 0){
        DFS_recursive_inorder_btree(n.children[0]);
        std::cout << n.ID << '\n';
        DFS_recursive_inorder_btree(n.children[1]);
    }
    else{
        std::cout << n.ID << '\n';
    }

}

void DFS_stack(const node& n){
    std::stack<node> s;
    s.push(n);
    node temp;

    while(s.size() > 0){
        std::cout << s.top().ID << '\n';
        temp = s.top();
        s.pop();
        for (int i = 0; i < temp.children.size(); ++i){
            s.push(temp.children[i]);
        }
    }
}

void BFS_queue(const node& n){
    std::queue<node> q;
    q.push(n);
    node temp;

    while(q.size() > 0){
        std::cout << q.front().ID << '\n';
        temp = q.front();
        q.pop();
        for (int i = 0; i < temp.children.size(); ++i){
            q.push(temp.children[i]);
        }
    }
}

