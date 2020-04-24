<?php
declare(strict_types=1);

class Person
{
    private $name;
    private $suitors = [];
    private $preferences = [];
    private $match;

    public function __construct($name)
    {
        $this->name = $name;
    }

    public function getName(): string
    {
        return $this->name;
    }

    public function setPreferences(array $preferences): void
    {
        $this->preferences = $preferences;
    }

    public function getMatch(): ?Person
    {
        return $this->match;
    }

    public function getPreferences(): array
    {
        return $this->preferences;
    }

    public function isSingle(): bool
    {
        return $this->match === null;
    }

    public function unmatch(): void
    {
        $this->match = null;
    }

    public function setMatch(Person $match): void
    {
        if ($this->match !== $match) {
            if ($this->match !== null) {
                $this->match->unmatch();
            }
            $this->match = $match;
            $match->setMatch($this);
        }
    }

    public function propose(): void
    {
        if (!empty($this->preferences)) {
            $fiance = array_shift($this->preferences);
            $fiance->receiveProposal($this);
        }
    }

    public function receiveProposal(Person $man): void
    {
        $this->suitors[] = $man;
    }

    public function chooseMatch(): void
    {
        foreach ($this->preferences as $preference) {
            if ($preference === $this->match || in_array($preference, $this->suitors)) {
                $this->setMatch($preference);
                break;
            }
        }

        $this->suitors = [];
    }

    public function __toString(): string
    {
        return $this->name;
    }
}

function stable_marriage(array $men, array $women): void
{
    do {
        foreach ($men as $man) {
            if ($man->isSingle()) {
                $man->propose();
            }
        }

        foreach ($women as $woman) {
            $woman->chooseMatch();
        }

        $unmarried = false;
        foreach ($women as $woman) {
            if ($woman->isSingle()) {
                $unmarried = true;
                break;
            }
        }

    } while ($unmarried);
}

$groupSize = 10;
$men = [];
$women = [];

for ($i = 1; $i <= $groupSize; $i++) {
    $men[] = new Person("M${i}");
    $women[] = new Person("W${i}");
}

foreach ($men as $man) {
    $preferences = $women;
    shuffle($preferences);
    $man->setPreferences($preferences);
    printf('%s\'s choices: %s', $man->getName(), implode(',', $man->getPreferences()));
    echo PHP_EOL;
}
echo PHP_EOL;
foreach ($women as $woman) {
    $preferences = $men;
    shuffle($preferences);
    $woman->setPreferences($preferences);
    printf('%s\'s choices: %s', $woman->getName(), implode(',', $woman->getPreferences()));
    echo PHP_EOL;
}
echo PHP_EOL;

stable_marriage($men, $women);
foreach ($women as $woman) {
    printf('%s is married to %s', $woman, $woman->getMatch());
    echo PHP_EOL;
}
