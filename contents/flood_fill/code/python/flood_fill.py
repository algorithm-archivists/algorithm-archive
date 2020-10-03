from collections import namedtuple
from queue import Queue
import numpy as np

Point = namedtuple("Point", "x y")

def inbounds(canvas_shape, p):
    return min(p) >= 0 and p.x < canvas_shape[0] and p.y < canvas_shape[1]

def color(canvas, p, new_val):
    canvas[p] = new_val

def find_neighbors(canvas, p, old_val, new_val):
    # north, south, east, west neighbors
    possible_neighbors = [
        Point(p.x, p.y+1),
        Point(p.x+1, p.y),
        Point(p.x-1, p.y),
        Point(p.x, p.y-1)
    ]

    # exclude the neighbors that go out of bounds and should not be colored
    neighbors = []
    for possible_neighbor in possible_neighbors:
        if inbounds(canvas.shape, possible_neighbor):
            if canvas[possible_neighbor] == old_val:
                neighbors.append(possible_neighbor)
    return neighbors

def stack_fill(canvas, p, old_val, new_val):
    if old_val == new_val:
        return

    stack = [p]

    while stack:
        cur_loc = stack.pop()
        color(canvas, cur_loc, new_val)
        stack += find_neighbors(canvas, cur_loc, old_val, new_val)

def queue_fill(canvas, p, old_val, new_val):
    if old_val == new_val:
        return

    q = Queue()
    q.put(p)

    color(canvas, p, new_val)

    while not q.empty():
        cur_loc = q.get()
        neighbors = find_neighbors(canvas, cur_loc, old_val, new_val)

        for neighbor in neighbors:
            color(canvas, neighbor, new_val)
            q.put(neighbor)

def recursive_fill(canvas, p, old_val, new_val):
    if old_val == new_val:
        return

    color(canvas, p, new_val)

    neighbors = find_neighbors(canvas, p, old_val, new_val)
    for neighbor in neighbors:
        recursive_fill(canvas, neighbor, old_val, new_val)

if __name__ == "__main__":
    TestResults = namedtuple('TestResults', 'passes failures')
    pass_count = failure_count = 0

    grid = np.zeros((5, 5))
    grid[2,:] = 1
    solution_grid = np.zeros((5, 5))
    solution_grid[:3,] = 1

    starting_location = Point(0, 0)


    # The following is manual unit testing of the function
    recursive_fill(grid, starting_location, 0, 1)
    try:
        assert (grid == solution_grid).all()
    except AssertionError:
        print('F', end='')
        failure_count += 1
    else:
        print('.', end='')
        pass_count += 1

    # Resetting the grid, if everything went well.
    grid[:2,] = 0

    stack_fill(grid, starting_location, 0, 1)
    try:
        assert (grid == solution_grid).all()
    except AssertionError:
        print('F', end='')
        failure_count += 1
    else:
        print('.', end='')
        pass_count += 1

    grid[:2,] = 0

    queue_fill(grid, starting_location, 0, 1)
    try:
        assert (grid == solution_grid).all()
    except AssertionError:
        print('F', end='')
        failure_count += 1
    else:
        print('.', end='')
        pass_count += 1

    print('')
    print(TestResults(pass_count, failure_count))
