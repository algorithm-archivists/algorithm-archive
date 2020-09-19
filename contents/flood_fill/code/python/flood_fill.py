from collections import namedtuple
from queue import Queue

Point = namedtuple("Point", "x y")
Canvas = namedtuple("Canvas", "max_x max_y data")

def inbounds(canvas, p):
    if p.x < 0 or p.y < 0 or p.x >= canvas.max_x or p.y >= canvas.max_y:
        return False
    return True

def color(canvas, p, old_val, new_val):
    canvas.data[p.x][p.y] = new_val

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
        if inbounds(canvas, possible_neighbor):
            if canvas.data[possible_neighbor.x][possible_neighbor.y] == old_val:
                neighbors.append(possible_neighbor)
    return neighbors

def stack_fill(canvas, p, old_val, new_val):
    if old_val == new_val:
        return

    S = list()
    S.append(p)

    while len(S) > 0:
        cur_loc = S.pop()
        if canvas.data[cur_loc.x][cur_loc.y] == old_val:
            color(canvas, cur_loc, old_val, new_val)
            S+= find_neighbors(canvas, cur_loc, old_val, new_val)

def queue_fill(canvas, p, old_val, new_val):
    if old_val == new_val:
        return

    Q = Queue()
    Q.put(p)

    color(canvas, p, old_val, new_val)

    while not Q.empty():
        cur_loc = Q.get()
        neighbors = find_neighbors(canvas, cur_loc, old_val, new_val)

        for neighbor in neighbors:
            color(canvas, neighbor, old_val, new_val)
            Q.put(neighbor)

def recursive_fill(canvas, p, old_val, new_val):
    if old_val == new_val:
        return

    color(canvas, p, old_val, new_val)

    neighbors = find_neighbors(canvas, p, old_val, new_val)
    for neighbor in neighbors:
        recursive_fill(canvas, neighbor, old_val, new_val)

def main():
    grid = [
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [1, 1, 1, 1, 1],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0]
    ]
    answer = [
        [1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0]
    ]

    c0 = Canvas(5, 5, grid)
    c1 = Canvas(5, 5, grid)
    c2 = Canvas(5, 5, grid)

    start_loc = Point(0, 0)

    recursive_fill(c0, start_loc, 0, 1)
    queue_fill(c1, start_loc, 0, 1)
    stack_fill(c2, start_loc, 0, 1)

    assert c0.data == answer
    assert c1.data == answer
    assert c2.data == answer

    print("Tests Passed")

if __name__ == "__main__":
    main() 
