from collections import namedtuple
from queue import Queue
import numpy as np

Point = namedtuple("Point", "x y")

def inbounds(canvas_shape, p):
    return min(p) >= 0 and p.x < canvas_shape[0] and p.y < canvas_shape[1]

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
        canvas[cur_loc] = new_val
        stack += find_neighbors(canvas, cur_loc, old_val, new_val)

def queue_fill(canvas, p, old_val, new_val):
    if old_val == new_val:
        return

    q = Queue()
    q.put(p)

    canvas[p] = new_val

    while not q.empty():
        cur_loc = q.get()
        neighbors = find_neighbors(canvas, cur_loc, old_val, new_val)

        for neighbor in neighbors:
            canvas[neighbor] = new_val
            q.put(neighbor)

def recursive_fill(canvas, p, old_val, new_val):
    if old_val == new_val:
        return

    canvas[p] = new_val

    neighbors = find_neighbors(canvas, p, old_val, new_val)
    for neighbor in neighbors:
        recursive_fill(canvas, neighbor, old_val, new_val)

def main():
    grid = np.zeros((5, 5))
    grid[2,:] = 1
    
    answer = np.zeros((5, 5))
    answer[:3,] = 1

    c0 = grid.copy()
    c1 = grid.copy()
    c2 = grid.copy()

    start_loc = Point(0, 0)

    recursive_fill(c0, start_loc, 0, 1)
    queue_fill(c1, start_loc, 0, 1)
    stack_fill(c2, start_loc, 0, 1)

    assert (c0 == answer).all()
    assert (c1 == answer).all()
    assert (c2 == answer).all()

    print("Tests Passed")

if __name__ == "__main__":
    main()
