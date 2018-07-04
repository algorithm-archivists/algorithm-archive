# create a function
# which sorts given array (with numbers) in place
bubbleSort = (arr) !->
    # determine ending (0-based) index of the array
    if (end = arr.length - 1) < 1
        # the sorting makes sense only for 2
        # or more elements...
        return
    # iterate the array in reverse order,
    # reducing the surface index on each iteration
    for surface from end to 1 by -1
        # iterate array normally and
        # push the "bubble" to the surface
        for i from 0 to surface
            # get next element's index
            j = i + 1
            # array may be sorted in ascending or descending order,
            # which is determined by the way values are compared
            if arr[i] > arr[j]
                # values are moved up by swapping,
                # temporary variable is used to do the swap
                tmp    = arr[i]
                arr[i] = arr[j]
                arr[j] = tmp

