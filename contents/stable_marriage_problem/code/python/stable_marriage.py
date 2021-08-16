# Submitted by Marius Becker
# Updated by Amaras


from random import shuffle
from copy import copy
from string import ascii_uppercase, ascii_lowercase


def main():
    # Set this to however many men and women you want, up to 26
    num_pairs = 5

    # Create all Person objects
    men = [Person(name) for name in ascii_uppercase[:num_pairs]]
    women = [Person(name) for name in ascii_lowercase[:num_pairs]]

    # Set everyone's preferences
    for man in men:
        man.preference = copy(women)
        shuffle(man.preference)

    for woman in women:
        woman.preference = copy(men)
        shuffle(woman.preference)

    # Run the algorithm
    stable_marriage(men, women)

    # Print preferences and the result
    print('Preferences of the men:')
    for man in men:
        print(man)

    print()

    print('Preferences of the women:')
    for woman in women:
        print(woman)

    print('\n')

    print('The algorithm gave this solution:')
    for man in men:
        print(f'{man.name} + {man.partner.name}')


def stable_marriage(men, women):
    """Finds pairs with stable marriages"""

    while True:
        # Let every man without a partner propose to a woman
        for man in men:
            if not man.has_partner:
                man.propose_to_next()

        # Let the women pick their favorites
        for woman in women:
            woman.pick_preferred()

        # Continue only when someone is still left without a partner
        if all((man.has_partner for man in men)):
            return


class Person:

    def __init__(self, name):
        self.name = name
        self.preference = []
        self.candidates = []
        self.pref_index = 0
        self._partner = None

    @property
    def next_choice(self):
        """Return the next person in the own preference list"""
        try:
            return self.preference[self.pref_index]
        except IndexError:
            return None

    def propose_to_next(self):
        """Propose to the next person in the own preference list"""
        person = self.next_choice
        person.candidates.append(self)
        self.pref_index += 1

    def pick_preferred(self):
        """Pick a new partner or stay with the old one if they are preferred"""
        # Iterate own preferences in order
        for person in self.preference:
            # Pick the first person that's either a new candidate or the
            # current partner
            if person == self.partner:
                break
            elif person in self.candidates:
                self.partner = person
                break

        # Rejected candidates don't get a second chance
        self.candidates.clear()

    @property
    def partner(self):
        return self._partner

    # The call self.partner = person sets self._partner as person
    # However, since engagement is symmetrical, self._partner._partner
    # (which is then person._partner) also needs to be set to self
    @partner.setter
    def partner(self, person):
        """Set a person as the new partner and sets the partner of that
        person as well"""

        # Do nothing if nothing would change
        if person != self._partner:
            # Remove self from current partner
            if self._partner is not None:
                self._partner._partner = None

            # Set own and the other person's partner
            self._partner = person
            if self._partner is not None:
                self._partner._partner = self

    # This allows use of self.has_partner instead of self.has_partner()
    @property
    def has_partner(self):
        """Determine whether this person currently has a partner or not."""
        return self.partner is not None

    # This allows the preferences to be printed more elegantly
    def __str__(self):
        return f'{self.name}: {", ".join(p.name for p in self.preference)}'


if __name__ == '__main__':
    main()
