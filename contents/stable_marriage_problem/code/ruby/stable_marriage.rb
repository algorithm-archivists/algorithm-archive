class Person
    def initialize(id, name, prefs)
        @id      = id
        @name    = name
        @prefs   = prefs
        @partner = nil
        @choices = 0
    end

    def lonely?
        @partner.nil?
    end

    def propose(partners)
        unless self.lonely?
            raise '%s is not lonely!' % self.name
        end
        choice = @prefs[@choices]
        partners[choice].onPropose(self)
        @choices += 1
    end

    def to_s
      "#{@name.rjust(20)}: #{self.lonely? && "Lonely" || @partner.name}"
    end

    def self.generate(size, prefix, r)
        Array.new(size){|i|
            Person.new(
                i,
                "#{prefix} #{i}",
                (0 ... size).to_a.shuffle(random: r)
            )
        }
    end

    protected
    attr_reader :id, :name
    attr_writer :partner

    # Acts upon a given Proposal
    def onPropose(partner)
        unless self.lonely?
            offer = score(partner)
            current = score(@partner)
            return unless offer > current 
            @partner.partner = nil
        end
        @partner = partner
        partner.partner = self
    end

    private
    # Determines the preference of a given partner
    def score(partner)
        return 0 if partner.nil?
        @prefs.size - @prefs.index(partner.id)
    end
end

# Deterministic Output, feel free to change seed
r = Random.new(42)

# Determines Output Columns
men = Person.generate(4, "Man", r)
women = Person.generate(4, "Woman", r)

# Assume no Name is longer than 20 characters
spacer = '-' * (20 * 2 + 2)

# Solve the Problem
1.step do |round|
    singles = men.select(&:lonely?)
    singles.each do |m|
        m.propose(women)
    end

    break if singles.empty?

    puts "Round #{round}"
    puts spacer
    puts men, women
    puts spacer
end
