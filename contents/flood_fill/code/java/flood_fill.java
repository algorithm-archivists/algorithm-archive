package code.java;

import java.util.*;
class Point{
    int x;
    int y;
    public Point(int x,int y){
        this.x = x;
        this.y = y;
    }
}

public class flood_fill {
    public boolean inbound(int[][] grid, Point p){
        int row = grid.length, col = grid[0].length;
        if (p.x < 0 || p.x >= row || p.y < 0 || p.y >= col) return false;
        return true;
    }

    public List<Point> find_neighbors(int[][] grid, Point p, int old_val, int new_val){
        //north, south, east, west neighbors
        List<Point> possible_neighbors = new ArrayList<>();
        List<Point> neighbors = new ArrayList<>();
        possible_neighbors.add(new Point(p.x,p.y + 1));
        possible_neighbors.add(new Point(p.x,p.y - 1));
        possible_neighbors.add(new Point(p.x - 1, p.y));
        possible_neighbors.add(new Point(p.x + 1, p.y));
        //exclude the neighbors that go out of bounds and should not be colored
        for (Point possible_neighbor : possible_neighbors){
            if(inbound(grid,possible_neighbor) && grid[possible_neighbor.x][possible_neighbor.y] == old_val){
                neighbors.add(possible_neighbor);
            }
        }
        return neighbors;
    }

    public void stack_fill(int[][] grid, Point p, int old_val, int new_val){
        if (old_val == new_val) return;
        Stack<Point> stack = new Stack<>();
        stack.add(p);
        while (!stack.empty()){
            Point cur = stack.pop();
            grid[cur.x][cur.y] = new_val;
            for (Point neighbor : find_neighbors(grid, cur, old_val, new_val)){
                stack.add(neighbor);
            }
        }
    }

    public void recursive_fill(int[][] grid, Point p, int old_val, int new_val){
        if (old_val == new_val) return;
        grid[p.x][p.y] = new_val;
        for (Point neighbor : find_neighbors(grid, p, old_val, new_val)){
            recursive_fill(grid, neighbor, old_val, new_val);
        }
    }

    public void queue_fill(int[][] grid, Point p, int old_val, int new_val){
        if (old_val == new_val) return;
        Queue<Point> queue = new ArrayDeque<>();
        queue.add(p);
        while (!queue.isEmpty()){
            Point cur = queue.remove();
            for (Point neighbor : find_neighbors(grid, cur, old_val, new_val)){
                grid[neighbor.x][neighbor.y] = new_val;
                queue.add(neighbor);
            }
        }
    }
}

