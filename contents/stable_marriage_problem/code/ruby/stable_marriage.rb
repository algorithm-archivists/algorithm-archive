class Generator
    def initialize(size)
        @size = size
    end

    def generate(prefix, r)
        Array.new(@size){|i|
            Person.new(
                i,
                "#{prefix} #{i}",
                (0 ... @size).to_a.shuffle!(random: r)
            )
        }
    end
end

class Person
    def initialize(id, name, prefs)
        @id      = id
        @name    = name
        @prefs   = prefs
        @partner = LONELY
        @choices = 0
    end

    def lonely
        @partner == LONELY
    end

    def propose(partners)
        if !self.lonely
            raise '%s is not lonely!' % self.name
        end
        choice = @prefs[@choices]
        partners[choice].onPropose(self)
        @choices += 1
    end

    # Represents this Person as an Array of strings
    def print
        name = if self.lonely
            then "Lonely"
            else @partner.name
        end
        [
            "##{@id} | #{@name}",
            "Partner: #{name}"
        ]
    end

    protected
    attr_reader :id, :name
    attr_writer :partner

    # Acts upon a given Proposal
    def onPropose(partner)
        if !self.lonely
            offer = score(partner)
            current = score(@partner)
            return unless offer > current 
            @partner.partner = LONELY
        end
        @partner = partner
        partner.partner = self
    end

    private
    LONELY = nil

    # Determines the preference of a given partner
    def score(partner)
        return 0 if partner == nil
        @prefs.size - @prefs.index(partner.id)
    end
end

# Deterministic Output, feel free to change seed
r = Random.new(42)

# Determines Output Columns
cols = 4
gen = Generator.new(4)
men = gen.generate("Man", r)
women = gen.generate("Woman", r)

# Assume no Name is longer than 20 characters
spacer = '-' * 20 * cols 

round = 0
# Solve the Problem
loop do
    round += 1
    singles = men.select(&:lonely)
    singles.each do |m|
        m.propose(women)
    end
    break if singles.empty?

    # Pretty Print
    puts "Round #{round}"
    puts spacer
    puts [men, women] # Array of Arrays of Persons
            .flatten # => Array of Persons
            .map(&:print) # => Print Person
            .each_slice(cols) # => split by columns
            .map{|o| # Turn 2D array in 1D array
                o.transpose.map{|a| 
                    a.map{|e|'%-20s' % e}.join
                }
            }
            .zip([spacer].cycle) # Add Spacer inbetween
    puts
end