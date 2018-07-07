from collections import namedtuple
from random import randint
from math import atan2

Point = namedtuple('Point', ['x', 'y'])

def graham_scan(gift):
    gift.sort(key=angle_sort(min(gift, key=lambda point: point.y)))
    m, i = 1, 2
    while i < len(gift):
        while ccw(gift[m - 1], gift[m], gift[i]) <= 0:
            if m > 1:
                m -= 1
            elif i == len(gift):
                break
            else:
                i += 1
        m += 1
        gift[i], gift[m] = gift[m], gift[i]
        i += 1
    return gift[:m+1]


def angle_sort(base):
    def sorter(point):
        return atan2(point.y - base.y, point.x - base.x)
    return sorter


def ccw(a, b, c):
    return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)


if __name__ == '__main__':
    min_, max_ = -55, 55
    gift = [Point(randint(min_, max_), randint(min_, max_)) for _ in range(50)]
    print(graham_scan(gift))
