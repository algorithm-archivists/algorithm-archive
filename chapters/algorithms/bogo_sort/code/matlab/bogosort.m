function main()
    array = floor( rand(1,7)*100 );
    disp('Before Sorting:')
    disp(array)

    array = bogo_sort(array);
    disp('After Sorting')
    disp(array)
end

function retval = is_sorted(array)
    for i=1:length(array)-1
        if array(i) > array(i+1)
            retval = false;
            return
        end
    end
    retval = true;
end

function sorted_array = bogo_sort(array)
    while ~is_sorted(array)
        % create a list of random permutation indices
        i = randperm(length(array));
        array = array(i);
    end
    sorted_array = array;
end


