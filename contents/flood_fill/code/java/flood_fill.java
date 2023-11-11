package code.java;
import java.util.*;
public class flood_fill {
        static class Point{
            int x;
            int y;
            public Point(int x,int y){
                this.x = x;
                this.y = y;
            }
        }
        public static boolean inbound(int[][] grid, Point p){
            int row = grid.length, col = grid[0].length;
            return (p.x < 0 || p.x >= row || p.y < 0 || p.y >= col);
        }
        public static List<Point> find_neighbors(int[][] grid, Point p, int old_val, int new_val){
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
        public static void stack_fill(int[][] grid, Point p, int old_val, int new_val){
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
        public static void recursive_fill(int[][] grid, Point p, int old_val, int new_val){
            if (old_val == new_val) return;
            grid[p.x][p.y] = new_val;
            for (Point neighbor : find_neighbors(grid, p, old_val, new_val)){
                recursive_fill(grid, neighbor, old_val, new_val);
            }
        }
        public static void queue_fill(int[][] grid, Point p, int old_val, int new_val){
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
        public static boolean isEqual(int[][] grid, int[][] solution){
            for(int i = 0; i < grid.length; i++){
                for(int j = 0; j < grid[0].length; j++) {
                    if(grid[i][j] != solution[i][j]) return false;
                }
            }
            return true;
        }
        public static void main(String[] args) {
            Point start = new Point(0,0);
            int[][] grid = {
                {0, 0, 1, 0, 0},
                {0, 0, 1, 0, 0},
                {0, 0, 1, 0, 0},
                {0, 0, 1, 0, 0},
                {0, 0, 1, 0, 0}};

            int[][] solution = {
                {1, 1, 1, 0, 0},
                {1, 1, 1, 0, 0},
                {1, 1, 1, 0, 0},
                {1, 1, 1, 0, 0},
                {1, 1, 1, 0, 0}};

           queue_fill(grid, start, 0, 1);
           if(isEqual(grid, solution)) System.out.println("test pass");
           stack_fill(grid, start, 0, 1);
           if(isEqual(grid, solution)) System.out.println("test pass");
           recursive_fill(grid, start, 0, 1);
           if(isEqual(grid, solution)) System.out.println("test pass");
        }
}


