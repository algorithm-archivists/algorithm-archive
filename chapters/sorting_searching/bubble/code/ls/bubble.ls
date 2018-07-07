# function sorts given array (with numbers) in place
bubbleSort = (arr) !->
    # the sorting makes sense only for 2 or more elements...
    if arr.length < 2
        return
    # iterate the array in reverse order,
    # reducing the surface index on each iteration
    for surface from arr.length - 1 to 1 by -1
        # iterate array normally and
        # push the "bubble" to the surface
        for i from 0 to surface
            # get next element's index
            j = i + 1
            # array may be sorted in ascending or descending order,
            # which is determined by the way values are compared
            if arr[i] > arr[j]
                # values are moved up by swapping
                tmp    = arr[i]
                arr[i] = arr[j]
                arr[j] = tmp

