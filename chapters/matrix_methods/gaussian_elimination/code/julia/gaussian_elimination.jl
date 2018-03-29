function gaussian_elimination(A::Array{Float64,2})

    rows = size(A,1)
    cols = size(A,2)

    # Main loop going through all columns
    for k = 1:min(rows,cols)

        # Step 1: finding the maximum element for each column
        max_index = indmax(abs.(A[k:end,k])) + k-1

        # Check to make sure matrix is good!
        if (A[max_index, k] == 0)
            println("matrix is singular! End!")
            exit(0)
        end

        # Step 2: swap row with highest value for that column to the top
        temp_vector = A[max_index, :]
        A[max_index, :] = A[k, :]
        A[k, :] = temp_vector
        #println(A)

        # Loop for all remaining rows
        for i = (k+1):rows

            # Step 3: finding fraction
            fraction = A[i,k]/A[k,k]

            # loop through all columns for that row
            for j = (k+1):cols

                 # Step 4: re-evaluate each element
                 A[i,j] -= A[k,j]*fraction

            end

            # Step 5: Set lower elements to 0
            A[i,k] = 0
        end
    end
end

function back_substitution(A::Array{Float64,2})

    rows = size(A,1)
    cols = size(A,2)

    # Creating the solution Vector 
    soln = Vector{Float64}(rows)

    # initialize the final element
    soln[rows] = A[rows, cols] / A[rows, cols-1]

    for i = (rows - 1):-1:1
        sum = 0.0
        for j = rows:-1:i
            sum += soln[j]*A[i,j]
        end
        soln[i] = (A[i, cols] - sum) / A[i, i]
    end

    return soln
end

function main()
    A = [2. 3 4 6;
         1 2 3 4;
         3 -4 0 10]

    gaussian_elimination(A)
    println(A)
    soln = back_substitution(A)

    for element in soln
        println(element)
    end
end

main()
