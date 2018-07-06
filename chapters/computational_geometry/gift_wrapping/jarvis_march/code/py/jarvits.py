# Is the turn counter clockwise?
def CCW(p1, p2, p3):
    return (p3[1]-p1[1])*(p2[0]-p1[0]) >= (p2[1]-p1[1])*(p3[0]-p1[0])


#Find the leftmost point in the list
def leftmost(S):
    leftmost = S[0]
    for s in S[1:]:
        if s[0]<leftmost[0]: leftmost=s
    return leftmost


#Are two points the same point?
def equal(p1,p2):
    return p1[0]==p2[0] and p1[1]==p2[1]


def jarvisMarch(S):
    n = len(S)  #Number of points in list
    P = [(None,None)] * n #The hull, initially empty points
    pointOnHull = leftmost(S) #leftmost point is guranteeded to be in hull
    
    i = 0
    while True:
        P[i] = pointOnHull
        endpoint = S[0]
        for j in range(1,n):
            if equal(endpoint,pointOnHull) or not CCW(S[j],P[i],endpoint):
                endpoint = S[j]
        i+=1
        pointOnHull = endpoint
        if equal(P[0],endpoint):
            break
        
    for i in range(n):
        if P[-1] == (None,None):
            del P[-1]

    P.append(P[0])
    return P
