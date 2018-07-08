from math import atan2

#Is the turn counter clockwise?
def CCW(p1, p2, p3):
    return (p3[1]-p1[1])*(p2[0]-p1[0]) >= (p2[1]-p1[1])*(p3[0]-p1[0])


#Find the point with least y-value. If tie, use point with least x-value.
def minYPoint(points):
    minYPt = points[0]
    
    for point in points[1:]:
        if point[1] < minYPt[1]:
            minYPt = point
        elif point[1] == minYPt[1] and point[0]<minYPt[0]:
            minYPt = point
            
    return minYPt


#Find the polar angle of each point in list relative to a reference point
def polarAngles(ref, points):
    return [atan2(point[1]-ref[0],point[0]-ref[0]) for point in points]


#Sort the list of point by their polar angle
def sortByPolar(ref, points):
    return [x for _,x in sorted(zip(polarAngles(ref,points),points))]


def grahamScan(gift):
    start = minYPoint(gift) #Must be in hull
    gift.remove(start)

    S = sortByPolar(start,gift)
    hull = [start,S[0],S[1]]

    #Remove points from hull that make the hull concave
    for pt in S[2:]:
        while not CCW(hull[-2],hull[-1],pt):
            del hull[-1]
        hull.append(pt)

    return hull


def main():
    testGift = [(-5,2),(5,7),(-6,-12),(-14,-14),(9,9),
                (-1,-1),(-10,11),(-6,15),(-6,-8),(15,-9),
                (7,-7),(-2,-9),(6,-5),(0,14),(2,8)]
    hull = grahamScan(testGift)

    print("The points in the hull are:")
    for point in hull:
        print(point)

main()
