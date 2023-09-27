using DelimitedFiles

# n = number of events
# prob = probability of incrementing counter
function random_count(n, prob; a=30, prob_calc=false)
    v = 0
    base = 1+1/a
    output = zeros(n)

    if prob_calc
        prob = 1
        v = 1
    end

    if rand() < prob
        output[1] = 1/prob
    end

    for i = 2:n
        if prob_calc
            prob = 1/((a*(base^(v+1)-1))-output[i-1])

            if rand() <= prob
                v += 1
            end
            output[i] = a*(base^v-1)
        else
            if rand() <= prob
                output[i] = output[i-1]+1/prob
            else
                output[i] = output[i-1]
            end
        end
    end

    return output
end

# m = number of counting trials
# l = number of saved trials
function multi_count(n, m, l, prob; a=30, prob_calc=false, file_mod="",
                     stops = [100000, 500000, 1000000])
    out = zeros(n, l)
    extremes = zeros(n, 2)

    chosen_set = zeros(Int, l)
    
    for i = 1:l
        chosen_number = rand(1:m)
        while chosen_number in chosen_set
            chosen_number = rand(1:m)
        end
        chosen_set[i] = chosen_number
    end

    histograms = zeros(Float64, m, length(stops))

    out_count = 1
    for i = 1:m
        current_dist = random_count(n, prob; a, prob_calc)
        if i == 1
            extremes[:,1] .= current_dist
            extremes[:,2] .= current_dist
        else
            for j = 1:n
                if current_dist[j] < extremes[j,1]
                    extremes[j,1] = current_dist[j]
                end
                if current_dist[j] > extremes[j,2]
                    extremes[j,2] = current_dist[j]
                end
            
            end
        end

        for j = 1:length(stops)
            histograms[i,j] = current_dist[stops[j]]
        end

        if i in chosen_set
            out[:,out_count] = current_dist
            out_count += 1
        end
    end

    output_file = open("out"*file_mod*".dat", "w")
    writedlm(output_file, out)
    close(output_file)

    extreme_output_file = open("extremes"*file_mod*".dat", "w")
    writedlm(extreme_output_file, extremes)
    close(extreme_output_file)

    formatted_histograms = [[],[],[]]
    # Going through the histogram data to put it into the right format
    for j = 1:length(stops)

        max = floor(Int,maximum(histograms[:,j]))
        min = floor(Int,minimum(histograms[:,j]))

        println(min, '\t', max, '\t', sum(histograms[:,j])/m)

        temp_array = zeros(max - min+1)

        for i = 1:m

            temp_array[floor(Int,histograms[i,j])-min+1] += 1

        end

        formatted_histograms[j] = temp_array
    end

    # output histograms into different files for each one

    for i = 1:length(stops)
        histogram_file = open("histogram_" * string(i)*file_mod* ".dat", "w")
        writedlm(histogram_file, formatted_histograms[i])
        close(histogram_file)
    end
    return formatted_histograms
end

