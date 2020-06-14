use std::vec::Vec;
use std::collections::HashMap;
use std::fmt;

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
struct PersonId(pub char);

impl fmt::Display for PersonId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone)]
struct Person {
    id: PersonId,
    partner: Option<PersonId>,
    preferences: Vec<PersonId>,
}

fn gale_shapley(people: Vec<Person>) -> HashMap<PersonId, PersonId> {
    let mut stable_matching: HashMap<PersonId, PersonId> = HashMap::new();

    for person in people {
        let partner = stable_matching
            .get(&person.id)
            .or_else(||     // execute this if person doesn't have partner
                person.preferences
                .iter()
                // skip all preferences which already have a pair
                .skip_while(|id| stable_matching.contains_key(id))
                .next()
            )
            .map(|&id| id);

        if let Some(partner) = partner {    // set partner
            stable_matching.insert(person.id, partner);
            stable_matching.insert(partner, person.id);
        }
    }
    stable_matching
}

fn main() {
    // create men vector
    let men: Vec<Person> = vec![
        ('A', vec!['E', 'G', 'F', 'H']),
        ('B', vec!['F', 'H', 'E', 'F']),
        ('C', vec!['F', 'E', 'H', 'G']),
        ('D', vec!['E', 'H', 'F', 'G'])
    ]
    .into_iter()
    .map(|(id, pref)|
        Person {
            id: PersonId(id),
            partner: None,
            preferences: pref.into_iter().map(PersonId).collect(),
        }
    ).collect();

    // create women vector
    let women: Vec<Person> = vec![
        ('E', vec!['A', 'D', 'C', 'B']),
        ('F', vec!['D', 'B', 'A', 'C']),
        ('G', vec!['D', 'A', 'C', 'B']),
        ('H', vec!['B', 'A', 'D', 'C'])
    ]
    .into_iter()
    .map(|(id, pref)|
        Person {
            id: PersonId(id),
            partner: None,
            preferences: pref.into_iter().map(PersonId).collect(),
        }
    ).collect();

    let people: Vec<Person> = men.iter().cloned().chain(
        women.iter().cloned()).collect();

    println!("Men: \n");
    for man in &men {
        println!("\t{}: {:?}", man.id, man.preferences.clone()
            // convert men.preference to vector of chars
            .into_iter()
            .map(|person| person.0)
            .collect::<Vec<char>>());
    }
    println!("Women: \n");
    for woman in &women {
        println!("\t{}: {:?}", woman.id, woman.preferences.clone()
            // convert women.preference to vector of chars
            .into_iter()
            .map(|person| person.0)
            .collect::<Vec<char>>());
    }

    let stable_matching: HashMap<PersonId, PersonId> = gale_shapley(people);

    // display stable matches
    for man in men {
        let partner_id = stable_matching.get(&man.id);
        match partner_id {
            Some(p) => println!("{} + {}", man.id, p),
            None => ()
        }
    }
}
