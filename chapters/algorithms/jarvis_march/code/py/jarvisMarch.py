# Is the turn counter clockwise?
def CCW(p1, p2, p3):
    return (p3[1]-p1[1])*(p2[0]-p1[0]) >= (p2[1]-p1[1])*(p3[0]-p1[0])


def jarvisMarch(gift):
    n = len(gift)  #Number of points in list
    pointOnHull = min(gift) #leftmost point in gift
    hull = [pointOnHull] #leftmost point guaranteed to be in hull
    
    while True:
        endpoint = gift[0]  #Candidate for next point in hull
        for j in range(1,n):
            if endpoint==pointOnHull or not CCW(gift[j],hull[-1],endpoint):
                endpoint = gift[j]
                
        pointOnHull = endpoint        
                
        #Check if we have completely wrapped gift
        if hull[0]==endpoint:
            break
        else:
            hull.append(pointOnHull)
        
    return hull


def main():
    testGift = [(-5, 2), (5, 7), (-6, -12), (-14, -14), (9, 9), 
                (-1, -1), (-10, 11), (-6, 15), (-6, -8), (15, -9),
                (7, -7), (-2, -9), (6, -5), (0, 14), (2, 8)]
    hull = jarvisMarch(testGift)
    
    print("The points in the hull are:")
    for point in hull:
        print(point)

main()
