# Is the turn counter clockwise?
def CCW(p1, p2, p3):
    return (p3[1]-p1[1])*(p2[0]-p1[0]) >= (p2[1]-p1[1])*(p3[0]-p1[0])


#Find the leftmost point in the list
def leftmost(gift):
    leftmost = gift[0]
    for point in gift[1:]:
        if point[0]<leftmost[0]: leftmost=point
    return leftmost


#Are two points the same point?
def equal(p1,p2):
    return p1[0]==p2[0] and p1[1]==p2[1]


def jarvisMarch(gift):
    n = len(gift)  #Number of points in list
    hull = [(None,None)] * n #The hull, initially empty points
    pointOnHull = leftmost(gift) #leftmost point is guranteeded to be in hull
    
    i = 0
    while True:
        hull[i] = pointOnHull
        endpoint = gift[0]
        for j in range(1,n):
            if equal(endpoint,pointOnHull) or not CCW(gift[j],hull[i],endpoint):
                endpoint = gift[j]
        i+=1
        pointOnHull = endpoint
        if equal(hull[0],endpoint):
            break
        
    for i in range(n):
        if hull[-1] == (None,None):
            del hull[-1]

    hull.append(hull[0])
    return hull
