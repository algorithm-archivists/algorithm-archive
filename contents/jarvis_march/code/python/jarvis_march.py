# Is the turn counter clockwise?
def ccw(p1, p2, p3):
    return (p3[1] - p1[1]) * (p2[0] - p1[0]) \
        >= (p2[1] - p1[1]) * (p3[0] - p1[0])


def jarvis_march(gift):
    n = len(gift)  # Number of points in list
    point_on_hull = min(gift)  # leftmost point in gift
    hull = [point_on_hull]  # leftmost point guaranteed to be in hull

    while True:
        # Candidate for next point in hull
        endpoint = gift[0]
        for j in range(1, n):
            if endpoint == point_on_hull \
               or not ccw(gift[j], hull[-1], endpoint):
                endpoint = gift[j]

        point_on_hull = endpoint

        # Check if we have completely wrapped gift
        if hull[0] == endpoint:
            break
        else:
            hull.append(point_on_hull)

    return hull


def main():
    test_gift = [
        (-5, 2), (5, 7), (-6, -12), (-14, -14), (9, 9),
        (-1, -1), (-10, 11), (-6, 15), (-6, -8), (15, -9),
        (7, -7), (-2, -9), (6, -5), (0, 14), (2, 8)
    ]
    hull = jarvis_march(test_gift)

    print("The points in the hull are:")
    for point in hull:
        print(point)


if __name__ == "__main__":
    main()
