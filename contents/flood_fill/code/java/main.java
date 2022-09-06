package code.java;

public class main {
    public static void main(String[] args) {
        flood_fill f = new flood_fill();
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

        f.recursive_fill(grid, start,0,1);
        assert(grid.clone() == solution);
        f.queue_fill(grid, start, 0, 1);
        assert(grid.clone() == solution);
        f.stack_fill(grid, start, 0, 1);
        assert(grid.clone() == solution);

    }
}
