#[derive(Eq, PartialEq, PartialOrd)]
struct Person {
    name: char,     // a single character denoting the person e.g 'A', 'B', 'F'
    index: usize,   // index in men/women vecotr
    preference: Vec<usize>,
    pref_index: usize,
    candidates: Vec<usize>, // all those men/women who have proposed to self
    partner: Option<usize>
}

impl Person {
    fn propose_to_next(&mut self, women: &mut Vec<Person>) {
        /* propose to next most preferred person */

        if self.pref_index >= self.preference.len() {
            ()
        }

        let person = self.preference[self.pref_index];
        // add self to the preferred woman's candidates
        women[person].candidates.push(self.index);

        self.pref_index += 1;

    }

    fn pick_preferred(&mut self, men: &mut Vec<Person>) {
        /* pick the highest preferred man among candidates */

        for person in &self.preference {
            if Some(*person) == self.partner {
                break
            } else if self.candidates.contains(&person) {
                match self.partner {
                    Some(ind) => {
                        // If self currently has a partner, set self's partner's partner to None
                        men[ind].partner = None;
                    },
                    None => {
                        self.partner = Some(*person);
                        men[*person].partner = Some(self.index);
                    }
                }
                break
            }
        }
    }

}

fn init_person(name: char, index: usize, pref: Vec<usize>) -> Person {
    let person = Person {
        name: name,
        index: index,
        preference: pref,
        pref_index: 0,
        candidates: Vec::new(),
        partner: None
    };
    person
}

fn main() {
    let mut men = vec![
        init_person('A', 0, vec![1, 0, 3, 2]),
        init_person('B', 1, vec![3, 0, 2, 1]),
        init_person('C', 2, vec![2, 3, 1, 0]),
        init_person('D', 3, vec![1, 2, 0, 3])
    ];

    let mut women = vec![
        init_person('E', 0, vec![3, 2, 0, 1]),
        init_person('F', 1, vec![0, 2, 1, 3]),
        init_person('G', 2, vec![0, 1, 2, 3]),
        init_person('H', 3, vec![2, 0, 1, 3])
    ];

    println!("Men: ");
    for man in &men {
        println!("\t{}, preference = {:?}", man.name, {
            let mut vect: Vec<char> = Vec::new();
            for man in &man.preference {
                vect.push(women[*man].name);
            }
            vect
        });
    }

    println!("Women: ");
    for woman in &women {
        println!("\t{}, preference = {:?}", woman.name, {
            let mut vect: Vec<char> = Vec::new();
            for woman in &woman.preference {
                vect.push(men[*woman].name);
            }
            vect
        });
    }


    /* Resolve */
    let mut cont = true;
    while cont {
        // Each man proposes to his highest preference
        for man in &mut men {
            if man.partner.is_none() {
                man.propose_to_next(&mut women);
            }
        }

        // Each woman picks here highest preference
        for woman in &mut women {
            woman.pick_preferred(&mut men);
        }

        // If there is a single man who does not have a partner, continue resolving
        cont = false;
        for man in &men {
            if man.partner.is_none() {
                cont = true;
                break;
            }
        }
    }

    println!("\nPairs: ");
    for man in &men {
        match man.partner {
            Some(p) => {
                println!("\t{} + {}", man.name, women[p].name);
            },
            None => ()
        }
    }

}
