import sys
from itertools import product

class Graph:
    def __init__(self, vertices, edges):
        self.vertices = vertices
        self.edges = {v: {} for v in self.vertices}

        for x, y, delta in edges:
            self.edges[x][y] = delta
            self.edges[y][x] = delta

    def route(self, start):
        unvisited = {x for x in self.vertices if x != start}
        distances = {k: 0 if k == start else sys.maxsize for k in self.vertices}
        paths = {k: [] if k == start else None for k in self.vertices}

        current = start
        while len(unvisited) > 0:
            for n, d in self.edges[current].items():
                if distances[current] + d < distances[n]:
                    distances[n] = distances[current] + d
                    paths[n] = paths[current] + ([current] if current != start else [])

            next_node = None
            for n in unvisited:
                if distances[n] < sys.maxsize and (next_node is None or distances[n] < distances[next_node]):
                    next_node = n

            if next_node is not None:
                unvisited.remove(next_node)
                current = next_node
            else:
                break

        for n, d in distances.items():
            if d == sys.maxsize:
                distances[n] = None
                paths[n] = None

        return distances, paths


def main():
    v = [1, 2, 3, 4, 5, 6]
    e = [(1, 2, 7), (1, 3, 9), (1, 6, 14), (2, 3, 10), (2, 4, 15), (3, 4, 11), (3, 6, 2), (4, 5, 6), (5, 6, 9)]
    g = Graph(v, e)

    for x in v:
        d, p = g.route(x)
        print('Starting at {}:'.format(x))
        for y in v:
            if d[y]:
                print('\t-> {} with distance {} via path {}'.format(y, d[y], p[y]))
            else:
                print('\t-> {} does not exist'.format(y))

if __name__ == '__main__':
    main()
