#include <algorithm>
#include <iostream>
#include <iterator>
#include <numeric>
#include <random>
#include <vector>

// this header is so that we can use `not` and `and` on MSVC
#include <ciso646>

#include <cstddef>

using std::size_t;

/*
  we use these to generate random numbers in this program.
  this makes the program simpler,
  by not having to pass around random number generators.
*/
static thread_local std::random_device global_random_device;
static thread_local std::mt19937 global_rng(global_random_device());

struct person {
  /*
    this is a poor person's std::optional,
    but since we're attempting to be compileable on C++14,
    we won't worry too much about it.
  */
  bool finished;
  size_t preference;

  std::vector<size_t> preference_list;
};

/*
  this function generates a list of people with size `number_of_partners`.

  each person's `preference_list` will be a randomly sorted list of
  the numbers in the range [0, number_of_partners),
  with no duplicates.
*/
std::vector<person> make_person_list(size_t number_of_partners) {
  auto random_pref_list = [&] {
    std::vector<size_t> ret(number_of_partners);
    std::iota(begin(ret), end(ret), size_t(0));
    std::shuffle(begin(ret), end(ret), global_rng);

    return ret;
  };

  std::vector<person> ret;
  std::generate_n(std::back_inserter(ret), number_of_partners, [&] {
    return person{false, 0, random_pref_list()};
  });

  return ret;
}

template <typename LeadIter, typename FollowIter>
void stable_match(LeadIter leads, LeadIter leads_end, FollowIter follows) {
  // for each index in the leads' preference list, we'll go through this
  size_t const number_of_partners = leads_end - leads;
  for (size_t proposal_index = 0; proposal_index < number_of_partners;
       ++proposal_index) {
    /*
      each follow will get their own vector of proposals to them
      for each entry in the leads' proposal list

      if this weren't example code, this would likely go outside the loop
      to cut down on allocations
    */
    std::vector<std::vector<size_t>> proposals(number_of_partners);

    // for each lead, we'll make a proposal to their favorite follow
    for (size_t i = 0; i < number_of_partners; ++i) {
      if (not leads[i].finished) {
        auto pref = leads[i].preference_list[proposal_index];
        proposals[pref].push_back(i);
      }
    }

    // for each follow, we'll look at their preference list
    for (size_t i = 0; i < number_of_partners; ++i) {
      for (size_t pref : follows[i].preference_list) {
        for (size_t proposal : proposals[i]) {
          // and, if they were given a proposal, then they'll choose their
          // favorite here
          if (pref == proposal and not follows[i].finished) {
            follows[i].preference = pref;
            follows[i].finished = true;

            leads[pref].preference = i;
            leads[pref].finished = true;
          }
        }
      }
    }
  }
}

int main() {
  // these are the number of partners in each group
  size_t const number_of_partners = 5;

  // in this case, the leads shall propose to the follows
  auto leads = make_person_list(number_of_partners);
  auto follows = make_person_list(number_of_partners);

  stable_match(begin(leads), end(leads), begin(follows));

  // the happy marriages are announced to the console here :)
  for (size_t i = 0; i < number_of_partners; ++i) {
    std::cout << "the partnership of lead " << i << " and follow "
              << leads[i].preference << " shall commence forthwith!\n";
  }
}
