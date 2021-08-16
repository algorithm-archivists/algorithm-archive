using Random

const mnames = ["A", "B", "C", "D"]
const wnames = ["E", "F", "G", "H"]

const Preferences = Dict{String,Vector{String}}
const Pairs = Dict{String,String}

# Returns a name => preference list dictionary, in decreasing order of preference
function genpreferences(mannames::Vector{String}, womannames::Vector{String})
    men   = Dict(map(m -> (m, shuffle(womannames)), mannames))
    women = Dict(map(w -> (w, shuffle(mannames)), womannames))
    return men, women
end

# Returns if `person` prefers the `first` candidate over the `second` one.
# This translates to `first` appearing *sooner* in the preference list
prefers(prefs, person, first, second) =
    findfirst(m -> m == first, prefs[person]) <
    findfirst(m -> m == second, prefs[person])

isfree(person, pairs) = !haskey(pairs, person)

function galeshapley(men::Preferences, women::Preferences)
    mentowomen = Dict{String,String}()
    womentomen = Dict{String,String}()
    while true
        bachelors = [m for m in keys(men) if isfree(m, mentowomen)]
        if length(bachelors) == 0
            return mentowomen, womentomen
        end

        for bachelor in bachelors
            for candidate in men[bachelor]
                if isfree(candidate, womentomen)
                    mentowomen[bachelor] = candidate
                    womentomen[candidate] = bachelor
                    break
                elseif prefers(women, candidate, bachelor, womentomen[candidate])
                    delete!(mentowomen, womentomen[candidate])
                    mentowomen[bachelor] = candidate
                    womentomen[candidate] = bachelor
                    break
                end
            end
        end
    end
end

function isstable(men::Preferences, women::Preferences, mentowomen::Pairs, womentoman::Pairs)
    for (husband, wife) in mentowomen
        for candidate in men[husband]
            if candidate != wife &&
               prefers(men, husband, candidate, wife) &&
               prefers(women, candidate, husband, womentoman[candidate])
                return false
            end
        end
    end
    return true
end

function main()
    men, women = genpreferences(mnames, wnames)
    mentowomen, womentomen = galeshapley(men, women)
    println(mentowomen)
    println(isstable(men, women, mentowomen, womentomen) ? "Stable" : "Unstable")
end

main()
