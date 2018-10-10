import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

class StableMarriage {

    /*
     * Use the stable marriage algorithm to find stable pairs from the
     * lists of men and women.
     */
    public static void findStableMarriages(List<Woman> women, List<Man> men) {
        // We might have more men/women than women/men. In this case, not everybody can
        // get a mate. We should aim to give every member of the less numerous gender a mate,
        // as this is always possible.
        List<? extends Person> leastCommonGender = women.size() <= men.size() ? women : men;
        do {
            // Every single man proposes to a woman.
            for (Man man : men)
                if (man.isLonely())
                    man.propose();

            // The women pick their favorite suitor.
            for (Woman woman : women)
                woman.chooseMate();

            // End the process if everybody has a mate.
            if (!leastCommonGender.stream().anyMatch(Person::isLonely))
                break;

        } while (true);

        women.forEach(w -> System.out.println(w + " married to " + w.getMate()));
    }

    public static void main(String[] args) {
        int nPairs = 5;
        List<Woman> women = new ArrayList<>();
        List<Man> men = new ArrayList<>();
        for (char i = 'A'; i < 'A' + nPairs; ++i) {
            women.add(new Woman("" + i));
            men.add(new Man("" + i));
        }
        // Make the genders unbalanced:
        women.add(new Woman("X"));

        women.forEach(w -> {
            w.receiveOptions(men);
            System.out.println(w + " prefers " + w.getPreferredMates());
        });
        men.forEach(m -> {
            m.receiveOptions(women);
            System.out.println(m + " prefers " + m.getPreferredMates());
        });

        findStableMarriages(women, men);
    }

}

class Person {
    private final String name;
    protected Person mate;
    protected List<Person> preferredMates;

    public Person(String name) {
        this.name = name;
    }

    public boolean isLonely() {
        return mate == null;
    }

    public void setMate(Person mate) {
        // Only set mates if there is a change.
        if (this.mate != mate) {
            // Remove old mates mate.
            if (this.mate != null)
                this.mate.mate = null;

            // Set the new mate.
            this.mate = mate;

            // If new mate is someone, update their mate.
            if (mate != null)
                mate.mate = this;
        }
    }

    public Person getMate() {
        return mate;
    }

    public void receiveOptions(List<? extends Person> mates) {
        // Preferences are subjective.
        preferredMates = new ArrayList<>(mates);
        Collections.shuffle(preferredMates);
    }

    public List<Person> getPreferredMates() {
        return preferredMates;
    }

    public String toString() {
        return getClass().getName() + "(" + name + ")";
    }
}

class Woman extends Person {
    private List<Man> suitors = new ArrayList<>();

    public Woman(String name) {
        super(name);
    }

    public void recieveProposal(Man suitor) {
        suitors.add(suitor);
    }

    public void chooseMate() {
        for (Person mostDesired : preferredMates) {
            if (mostDesired == mate || suitors.contains(mostDesired)) {
                setMate(mostDesired);
                break;
            }
        }
    }
}

class Man extends Person {
    public Man(String name) {
        super(name);
    }

    public void propose() {
        if (!preferredMates.isEmpty()) {
            Woman fiance = (Woman) preferredMates.remove(0);
            fiance.recieveProposal(this);
        }
    }
}
