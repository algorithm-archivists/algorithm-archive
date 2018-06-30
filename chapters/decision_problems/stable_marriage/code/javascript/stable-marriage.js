class Person {
  constructor(name) {
    this.name = name;
  }

  get hasFiance() {
    return !!this.fiance;
  }

  prefers(other) {
    return this.preferences.indexOf(other) < this.preferences.indexOf(this.fiance);
  }

  engageTo(other) {
    if (other.hasFiance) {
      other.fiance.fiance = undefined;
    }

    this.fiance = other;
    other.fiance = this;
  }
}

function stableMarriage(guys, girls) {
  const bachelors = [...guys];
  while (bachelors.length > 0) {
    const guy = bachelors.shift();
    for (const girl of guy.preferences) {
      if (!girl.hasFiance) {
        guy.engageTo(girl);
        break;
      } else if (girl.prefers(guy)) {
        bachelors.push(girl.fiance);
        guy.engageTo(girl);
        break;
      }
    }
  }
}

function shuffle(iterable) {
  const array = [...iterable];
  for (let i = array.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [array[i], array[j]] = [array[j], array[i]];
  }
  return array;
}

const guys = [..."ABCDE"].map(name => new Person(name));
const girls = [..."FGHIJ"].map(name => new Person(name));

console.log("Guys");
for (const guy of guys) {
  guy.preferences = shuffle(girls);
  console.log(`${guy.name}: ${guy.preferences.map(p => p.name).join()}`)
}

console.log("\nGirls");
for (const girl of girls) {
  girl.preferences = shuffle(guys);
  console.log(`${girl.name}: ${girl.preferences.map(p => p.name).join()}`)
}

stableMarriage(guys, girls);

console.log("\nPairings");
for (const guy of guys) {
  console.log(`${guy.name}: ${guy.fiance.name}`);
}

