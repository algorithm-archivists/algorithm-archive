# Submitted by Marius Becker
# Updated by Amaras

import sys
from random import shuffle
from copy import copy
from string import ascii_uppercase, ascii_lowercase


def main():
    # Set this to however many men and women you want
    try:
        num_pairs = int(sys.argv[1])
    except (IndexError, ValueError):
        # If you either did not set how many pairs you wanted or you
        # did not set it as a number, use default value of 5
        num_pairs = 5


    # There are only 26 possible names for each sex
    if num_pairs > 26:
        print("You can't have more than 26 pairs.")
        return

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
    resolve(men, women)

    # Print preferences and the result
    for man in men:
        print(f"{man.name}: {', '.join((p.name for p in man.preference))}")

    print('')

    for woman in women:
        print(f"{woman.name}: {', '.join((p.name for p in woman.preference))}")

    print('\n')

    for man in men:
        print(f'{man.name} + {man.partner.name}')

def resolve(men, women):
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
        if self.pref_index >= len(self.preference):
            return None

        return self.preference[self.pref_index]

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

        # Rejected candidates don't get a second chance. :(
        self.candidates.clear()

    @property
    def partner(self):
        return self._partner

    # This allows one to change both self.partner and person.partner
    # with the simple call: "self.partner = person"
    @partner.setter
    def partner(self, person):
        """Set a person as the new partner and sets the partner of that
        person as well"""

        # Do nothing if nothing would change
        if person != self.partner:
            # Remove self from current partner
            if self.partner is not None:
                self._partner._partner = None

            # Set own and the other person's partner
            self._partner = person
            if self.partner is not None:
                self._partner._partner = self

    # This allows use of self.has_parnter instead of self.has_partner()
    @property
    def has_partner(self):
        """Determine whether this person currently has a partner or not."""
        return self.partner is not None

if __name__ == '__main__':
    main()
