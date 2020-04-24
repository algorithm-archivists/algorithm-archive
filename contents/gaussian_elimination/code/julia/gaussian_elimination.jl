function gaussian_elimination!(A::Array{Float64,2})

    rows = size(A,1)
    cols = size(A,2)

    # Row index
    row = 1

    # Main loop going through all columns
    for col = 1:(cols-1)

        # finding the maximum element for each column
        max_index = argmax(abs.(A[row:end,col])) + row-1

        # Check to make sure matrix is good!
        if (A[max_index, col] == 0)
            println("matrix is singular!")
            continue
        end

        # swap row with highest value for that column to the top
        temp_vector = A[max_index, :]
        A[max_index, :] = A[row, :]
        A[row, :] = temp_vector

        # Loop for all remaining rows
        for i = (row+1):rows

            # finding fraction
            fraction = A[i,col]/A[row,col]

            # loop through all columns for that row
            for j = (col+1):cols

                 # re-evaluate each element
                 A[i,j] -= A[row,j]*fraction

            end

            # Set lower elements to 0
            A[i,col] = 0
        end
        row += 1
    end
end

function back_substitution(A::Array{Float64,2})

    rows = size(A,1)
    cols = size(A,2)

    # Creating the solution Vector
    soln = zeros(rows)

    for i = rows:-1:1
        sum = 0.0
        for j = rows:-1:i
            sum += soln[j]*A[i,j]
        end
        soln[i] = (A[i, cols] - sum) / A[i, i]
    end

    return soln
end


function gauss_jordan_elimination!(A::Array{Float64,2})

    rows = size(A,1)
    cols = size(A,2)


    # After this, we know what row to start on (r-1)
    # to go back through the matrix
    row = 1
    for col = 1:cols-1
        if (A[row, col] != 0)

            # divide row by pivot and leaving pivot as 1
            for i = cols:-1:col
                A[row,i] /= A[row,col]
            end

            # subtract value from above row and set values above pivot to 0
            for i = 1:row-1
                for j = cols:-1:col
                    A[i,j] -= A[i,col]*A[row,j]
                end
            end
            row += 1
        end
    end
end

function main()
    A = [2. 3 4 6;
         1 2 3 4;
         3 -4 0 10]

    gaussian_elimination!(A)
    println(A)

    gauss_jordan_elimination!(A)
    println(A)

    soln = back_substitution(A)
    println(soln)

end

main()
