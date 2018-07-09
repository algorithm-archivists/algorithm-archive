from math import atan2


#Is the turn counter clockwise?
def counter_Clockwise(p1, p2, p3):
    return (p3[1]-p1[1])*(p2[0]-p1[0]) >= (p2[1]-p1[1])*(p3[0]-p1[0])


#Find the polar angle of a point relative to a reference point
def polar_Angle(ref, point):
    return atan2(point[1]-ref[0],point[0]-ref[0])


#Sort the list of point by their polar angle
def sort_By_Polar(ref, points):
    return [x for _,x in sorted(zip(polar_Angles(ref,points),points))]


def graham_Scan(gift):
    start = min(gift, key=lambda p: (p[1],p[0])) #Must be in hull
    gift.remove(start)

    S = sorted(gift,key=lambda point: polar_Angle(start,point))
    hull = [start,S[0],S[1]]

    #Remove points from hull that make the hull concave
    for pt in S[2:]:
        while not counter_Clockwise(hull[-2],hull[-1],pt):
            del hull[-1]
        hull.append(pt)

    return hull


def main():
    test_Gift = [(-5,2),(5,7),(-6,-12),(-14,-14),(9,9),
                (-1,-1),(-10,11),(-6,15),(-6,-8),(15,-9),
                (7,-7),(-2,-9),(6,-5),(0,14),(2,8)]
    hull = graham_Scan(test_Gift)

    print("The points in the hull are:")
    for point in hull:
        print(point)

        
main()
