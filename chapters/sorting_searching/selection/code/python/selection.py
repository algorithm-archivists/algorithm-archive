def selectionList(lst):
	for i in range(0, len(lst)):
		if i < len(lst):
			lst[i] = i
	return lst
t = [1,0,3,2,6,4,5,9,7,8]
print(selectionList(t))
