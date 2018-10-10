function sorted_array = bubblesort(array)
    for i = 1 : length(array)
        for j = 1 : length(array) - i
            if array(j) > array(j+1)
                % swap elements in the list
                temp       = array(j);
                array(j)   = array(j+1);
                array(j+1) = temp;
            end
        end
    end
    sorted_array = array;
end

function main()
    array = floor(rand(1, 7) * 100);
    disp('Before Sorting:')
    disp(array)

    array = bubble_sort(array);
    disp('After Sorting:')
    disp(array)
end

main()
