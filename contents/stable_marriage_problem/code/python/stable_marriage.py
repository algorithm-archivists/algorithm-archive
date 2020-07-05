# Submitted by Marius Becker
# Updated by Amaras
# Updated again by Jonathan Dönszelmann

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

    people = [*men, *women]

    for person in people:
        if person.partner is None:
            # find someone in this person's preferences who
            # doesn't already have a partner.
            for possible_partner in person.preference:
                if possible_partner.partner is None:
                    partner = possible_partner
                    break
            else:
                raise Exception(f"Couldn't find a partner for {person}")
        else:
            partner = person.partner

        person.partner = partner
        partner.partner = person


class Person:
    def __init__(self, name):
        self.name = name
        self.preference = []
        self.partner = None

    # This allows the preferences to be printed more elegantly
    def __str__(self):
        return f'{self.name}: {", ".join(p.name for p in self.preference)}'


if __name__ == '__main__':
    main()
