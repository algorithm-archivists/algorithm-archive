### Conditions

These might be the most intuitive building blocks and basically flow from natural speech.
If you are tired, you should go to sleep, or else you should go to work! 
In code, this looks like:

```julia
if (me.tired() == true)
    me.sleep()
else
    me.work()
end
```

Here, we are assuming you are a type of human with the abilities to both `sleep()` and `work()`.
Note that the condition is checking to see if the statement on the inside is `true` or `false` before continuing.
In other words, it is condensing the statement `me.tired() == true` into a simple boolean.
This is important to remember when dealing with loops later.

Though simple, conditions are essential to almost all modern code, so get used to seeing them everywhere!
