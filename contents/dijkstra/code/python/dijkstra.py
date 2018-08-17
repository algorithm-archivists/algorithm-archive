import sys
from itertools import product

class Graph:
    def __init__(self, vertices, edges):
        self.vertices = vertices
        self.edges = {v: {} for v in self.vertices}

        for x, y, dist in edges:
            self.edges[x][y] = dist
            self.edges[y][x] = dist

    def route_from(self, start):
        current = start
        unvisited = {x for x in self.vertices if x != start}
        distances = {k: 0 if k == start else sys.maxsize for k in self.vertices}
        paths = {k: [] if k == start else None for k in self.vertices}

        while len(unvisited) > 0:
            # Check all edges incident to our current vertex, looking for improved paths to neighbors
            for n, d in self.edges[current].items():
                if distances[current] + d < distances[n]:
                    distances[n] = distances[current] + d
                    paths[n] = paths[current] + ([current] if current != start else [])

            # Find the next vertex to iterate on.  This should be a vertex for which we've found a path, but
            #   whose path is a shortest length of that to all the yet-unvisited vertices.
            next_node = None
            for n in unvisited:
                if distances[n] < sys.maxsize and (next_node is None or distances[n] < distances[next_node]):
                    next_node = n

            if next_node is not None:
                unvisited.remove(next_node)
                current = next_node
            else:
                # This break handles the scenario in which our graph is disconnected, and we've found optimal paths
                #   to all vertices in the component we began our search in.
                break

        # Clean-up our output to better indicate which vertices are disconnected from our start vertex
        for n, d in distances.items():
            if d == sys.maxsize:
                distances[n] = None
                paths[n] = None

        return distances, paths


def main():
    vertices = list(range(1, 24)) # 1-23
    edges = [
        (1, 2, 5), (2, 3, 7), (3, 4, 1), (3, 11, 5), (3, 12, 7), (4, 5, 4), (4, 11, 5), (5, 6, 6), (5, 14, 20), (6, 7, 7), (6, 8, 11), (6, 14, 11),
        (8, 9, 4), (8, 15, 2), (9, 10, 5), (10, 11, 12), (10, 12, 17), (12, 13, 10), (12, 16, 2), (13, 14, 5), (14, 15, 10), (15, 16, 9), (17, 18, 1),
        (17, 20, 6), (18, 19, 7), (18, 21, 10), (19, 22, 4), (20, 21, 5), (21, 22, 4)
    ]
    g = Graph(vertices, edges)

    for start in vertices:
        distances, paths = g.route_from(start)
        print('Starting at {}:'.format(start))
        for end in vertices:
            if distances[end] is not None:
                print('\t-> {} with distance {} via path {}'.format(end, distances[end], paths[end]))
            else:
                print('\t-> {} does not exist'.format(end))

if __name__ == '__main__':
    main()
