using DelimitedFiles

# n = number of events
# prob = probability of incrementing counter
function random_count(n, prob)
    output = zeros(n)

    if rand() < prob
        output[1] = 1/prob
    end

    for i = 2:n
        if rand() <= prob
            output[i] = output[i-1]+1/prob
        else
            output[i] = output[i-1]
        end
    end

    return output
end

# m = number of counting trials
# l = number of saved trials
function multi_count(n, m, l, prob;
                     stops = [100000, 500000, 1000000])
    out = zeros(n, l)

    chosen_set = zeros(Int, l)
    
    for i = 1:l
        chosen_number = rand(1:m)
        while chosen_number in chosen_set
            chosen_number = rand(1:m)
        end
        chosen_set[i] = chosen_number
    end

    histograms = zeros(Int, m, length(stops))

    out_count = 1
    for i = 1:m
        current_dist = random_count(n, prob)

        for j = 1:length(stops)
            histograms[i,j] = current_dist[stops[j]]
        end

        if i in chosen_set
            out[:,out_count] = current_dist
            out_count += 1
        end
    end

    #output_file = open("out.dat", "w")
    #writedlm(output_file, out)
    #close(output_file)

    formatted_histograms = [[],[],[]]
    # Going through the histogram data to put it into the right format
    for j = 1:length(stops)

        max = maximum(histograms[:,j])
        min = minimum(histograms[:,j])

        println(min, '\t', max, '\t', sum(histograms[:,j])/m)

        temp_array = zeros(max - min+1)

        for i = 1:m

            temp_array[histograms[i,j]-min+1] += 1

        end

        formatted_histograms[j] = temp_array
    end

    # output histograms into different files for each one

    for i = 1:length(stops)
        #histogram_file = open("histogram_" * string(i)* ".dat", "w")
        #writedlm(histogram_file, formatted_histograms[i])
        #close(histogram_file)
    end
    return histograms
end

