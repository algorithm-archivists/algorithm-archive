using DataStructures
function gaussian_elimination(A::Array{Float64,2})

    rows = size(A,1)
    cols = size(A,2)

    # Row index
    row = 1

    # Main loop going through all columns
    for col = 1:(cols-1)

        # Step 1: finding the maximum element for each column
        max_index = indmax(abs.(A[row:end,col])) + row-1

        # Check to make sure matrix is good!
        if (A[max_index, col] == 0)
            println("matrix is singular!")
            continue
        end

        # Step 2: swap row with highest value for that column to the top
        temp_vector = A[max_index, :]
        A[max_index, :] = A[row, :]
        A[row, :] = temp_vector

        # Loop for all remaining rows
        for i = (row+1):rows

            # Step 3: finding fraction
            fraction = A[i,col]/A[row,col]

            # loop through all columns for that row
            for j = (col+1):cols

                 # Step 4: re-evaluate each element
                 A[i,j] -= A[row,j]*fraction

            end

            # Step 5: Set lower elements to 0
            A[i,col] = 0
        end
        row += 1
    end
end

function back_substitution(A::Array{Float64,2})

    rows = size(A,1)
    cols = size(A,2)


    # Creating a stack with pivot locations
    s = Stack(Int64)
    row = 1
    for col = 1:cols-1
        if A[row, col] != 0
            push!(s, col)
            row += 1
        end
    end 

    # After this, we know what row to start on (r-1)
    # to go back through the matrix
    row -= 1
    while (length(s) > 0)
        col = pop!(s)

        # dividing row by pivot and leaving pivot as 1
        for i = cols:-1:col
            A[row,i] /= A[row,col]
        end

        # subtracting value from above row and setting values above pivot to 0
        for i = 1:row-1
            for j = cols:-1:col
                A[i,j] -= A[i,col]*A[row,j]
            end
        end
        row -= 1
    end

    return A
end

function main()
    A = [2. 3 4 6;
         1 2 3 4;
         3 -4 0 10]

    gaussian_elimination(A)
    println(A)
    soln = back_substitution(A)

    println(A)

end

main()
