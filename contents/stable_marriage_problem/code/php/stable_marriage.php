<?php
declare(strict_types=1);

abstract class Person
{
    private $name;
    protected $preferences = [];
    protected $match;

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
        shuffle($preferences);
        $this->preferences = $preferences;
    }

    public function getMatch(): Person
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
            if ($match !== null) $match->setMatch($this);
        }
    }

    public function __toString(): string
    {
        return $this->name;
    }
}

class Man extends Person
{
    public function propose(): void
    {
        if (!empty($this->preferences)) {
            $fiance = array_shift($this->preferences);
            $fiance->receiveProposal($this);
        }
    }
}

class Woman extends Person
{
    private $suitors = [];

    public function receiveProposal(Man $man): void
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
    }
}

function stable_marriage(array $men, array $women): void
{
    $smallerGroup = count($men) < count($women) ? $men : $women;
    do {
        foreach ($men as $man)
            if ($man->isSingle()) $man->propose();

        foreach ($women as $woman)
            $woman->chooseMatch();

        if (empty(array_filter($smallerGroup, function (Person $person) {
            return $person->isSingle();
        }))) break;

    } while (true);

    foreach ($women as $woman) printf('%s is married to %s%s', $woman, $woman->getMatch(), PHP_EOL);
}

$groupSize = 10;
$men = [];
$women = [];

for ($i = 1; $i <= $groupSize; $i++) {
    $men[] = new Man("M${i}");
    $women[] = new Woman("W${i}");
}

foreach ($men as $man) {
    $man->setPreferences($women);
    printf('%s\'s choices:%s', $man->getName(), PHP_EOL);
    printf('%s%s', implode(',', $man->getPreferences()), PHP_EOL);
}
echo PHP_EOL;
foreach ($women as $woman) {
    $woman->setPreferences($men);
    printf('%s\'s choices:%s', $woman->getName(), PHP_EOL);
    printf('%s%s', implode(',', $woman->getPreferences()), PHP_EOL);
}
echo PHP_EOL;
stable_marriage($men, $women);
