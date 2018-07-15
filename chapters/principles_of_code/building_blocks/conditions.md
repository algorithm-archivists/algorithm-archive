### Conditions

Of all the programming building blocks, conditions might be the most intuitive and they basically flow from natural speech.
Let's say you are up late studying for an exam schedule for later in the month, but you are finding yourself lacking in motivation and distracted by the internet.
You hate wasting time, so you try to figure out how tired you are and think to yourself, *if I am tired, I will go to sleep. Otherwise, I should get back to work!*
In code, this looks like:

```julia
if (me.tired() == true)
    me.sleep()
else
    me.work()
end
```

Here, we are assuming you are a type of `human` with the abilities to both `sleep()` and `work()`.
Note that the condition is checking to see if the statement provided is `true` or `false` before continuing.
In other words, it is condensing the statement `me.tired() == true` into a simple boolean value.
This is important to remember when dealing with loops later.
Conditions allow you to modify the flow of your program depending on other values produced and are a powerful and intuitive tool.

It's worth noting here that all the mathematical conditions work in these statements:

| Symbol                       | Meaning                                 |
| ---------------------------  | --------------------------------------- |
| a == b                       | a is equal to b                         |
| a > b                        | a is greater than b                     |
| a < b                        | a is less than b                        |
| a != b                       | a is not equal to b                     |
| a == b && b == c             | a is equal to b **and** b is equal to c |
| a == b &#124;&#124; b == c   | a is equal to b **or** b is equal to c  |

Here, the `&&` operator means both conditions must be `true` for the statement as a whole to be considered `true`, while the `||` operator means that only one of the two statements must be `true` for the statement to be considered `true`.
Also note that the `!` operator can be used in front of boolean values to signify the value should be to opposite.
For example, if you are trying to indicate that you are not tired (even though you clearly are), you might say `!me.tired()`.
`me.tired()` will return `true`, but the `!` operator will flip the `true` to `false`.

In addition to the `if` and `else` keywords, there is also the `else if`, which varies notationally depending on the language you are using.
Imagine that we wanted to add two additional functions to the `human` type: `care()`, which evaluates the amount you care about a particular topic and `procrastinate()`, which is the action of wasting time.
Imagine (again) that you are studying for an exam that is a month away, but this time, you are studying for a class you do not care about.
You might update your thought to be something like, *if I am tired, I will go to sleep. If I don't care about the class, I can continue procrastinating. Otherwise, I should get back to work!*
In code, this looks like:

```julia
if (me.tired())
    me.sleep()
else if(!me.care())
    me.procrastinate()
else
    me.work()
end
```

To be clear: if `me.care()` returns `true`, then the `!` will flip the statement in the `else if` to `false`, and thus you will not procrastinate.
If `me.care()` returns `false`, the `!` will flip the statement to `true` and you will begin procrastinating if you are not tired.
This might take a little thought, and I encourage you to meditate on it, if you so desire.

Now on to something slightly more complicated: *switches*.
Imagine you have a lot of conditions to keep in mind, all of which depend on the same variable.
You couldgo through these conditions one-by-one with a chain of `if` and `else if` conditions, like so:

```julia
if (val == 0)
    scenario_0()
else if (val == 1)
    scenario_1()
else if (val == 2)
    scenario_2()
else if (val == 3)
    scenario_3()
else
    scenario_4()
end
```
Here, we need to run different functions depending on the value of the variable `val`.
In these cases, it's worth bringing in a faster and sometimes more elegant solution: the `switch` statement.

```julia
switch(val)
    case 0:
        scenario_0()
        break
    case 1:
        scenario_1()
        break
    case 2:
        scenario_2()
        break
    case 3:
        scenario_3()
        break
    case 4:
        scenario_4()
        break
end
```


To clarify: not all languages have a `switch` statement, and as mentioned, it's not precisely necessary.
That said, in languages that do have a `switch`, it's often preferred to a chain of `else if` statements;

Though simple, conditions are essential to almost all modern code, so get used to seeing them everywhere!
