from collections import namedtuple
from random import randint
from functools import reduce

Point = namedtuple('Point', ['x', 'y'])

def jarvis_march(gift):
    convex_hull = [min(gift, key=lambda point: point.x)]
    point_on_hull = reduce(next_hull_point(convex_hull[0]), gift)
    while point_on_hull != convex_hull[0]:
        convex_hull.append(point_on_hull)
        point_on_hull = reduce(next_hull_point(point_on_hull), gift)
    return convex_hull


def next_hull_point(current):
    def is_left_of(next_, candidate):
        if next_ in (None, current):
            return candidate
        return candidate if left_of(current, next_, candidate) else next_
    return is_left_of


def left_of(a, b, p):
    return (b.x - a.x) * (p.y - a.y) > (p.x - a.x) * (b.y - a.y)


if __name__ == '__main__':
    min_, max_ = -50, 50
    gift = [Point(randint(min_, max_), randint(min_, max_)) for _ in range(50)]
    print(jarvis_march(gift))
