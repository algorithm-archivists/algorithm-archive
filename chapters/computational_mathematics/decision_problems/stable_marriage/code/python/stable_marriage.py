# Submitted by Marius Becker

import sys
from random import shuffle
from copy import copy
from string import ascii_uppercase

def main():
    # Set this to however many men and women you want
    if len(sys.argv) > 1:
        num_pairs = int(sys.argv[1])
    else:
        num_pairs = 5

    # There are only 26 possible names
    if num_pairs > 13:
        print('You can\' have more than 13 pairs.')
        return

    # Create all Person objects
    men = [ Person(name) for name in ascii_uppercase[0:num_pairs] ]
    women = [ Person(name) for name in ascii_uppercase[num_pairs:num_pairs*2] ]

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
        print('{}: {}'.format(man.name, ', '.join([ p.name for p in man.preference ])))

    for woman in women:
        print('{}: {}'.format(woman.name, ', '.join([ p.name for p in woman.preference ])))

    print('')

    for man in men:
        print('{} + {}'.format(man.name, man.partner.name))

def resolve(men, women):
    """Finds pairs with stable marriages"""
    cont = True
    while cont:
        # Let every man without a partner propose to a woman
        for man in men:
            if not man.has_partner():
                man.propose_to_next()

        # Let the women pick their favorites
        for woman in women:
            woman.pick_preferred()

        # Continue only when someone is still left without a partner
        cont = False
        for man in men:
            if not man.has_partner():
                cont = True
                break

class Person:
    name = None
    preference = None
    pref_index = 0
    candidates = None
    partner = None

    def __init__(self, name):
        self.name = name
        self.preference = []
        self.candidates = []

    def get_next_choice(self):
        """Return the next person in the own preference list"""
        if self.pref_index >= len(self.preference):
            return None

        return self.preference[self.pref_index]

    def propose_to_next(self):
        """Propose to the next person in the own preference list"""
        person = self.get_next_choice()
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
                self.set_partner(person)
                break

        # Rejected candidates don't get a second chance. :(
        self.candidates.clear()

    def get_partner(self):
        """Return the current partner"""
        return self.partner

    def set_partner(self, person):
        """Set a person as the new partner and run set_partner() on that person
           as well"""
        # Do nothing if nothing would change
        if person != self.partner:
            # Remove self from current partner
            if self.partner is not None:
                self.partner.partner = None

            # Set own and the other person's partner
            self.partner = person
            if self.partner is not None:
                self.partner.partner = self

    def has_partner(self):
        """Determine whether this person currently has a partner or not"""
        return self.partner != None

if __name__ == '__main__':
    main()
