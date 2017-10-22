### Loops

Loops are weird. I'm not going to lie, it took me a super long time to figure out how and when to use them. Nowadays, I see them as essential elements to almost any program I write. 
There two essential loop types: `for` and `while`, which are syntactically simlar and reasonably intuitive.
Let's say you want to walk out of a room with a closed door. In code, this might look something like:

```julia
while(me.position < door.position)
    me.take_step()
end

me.open_door()
```

Here again, we assume that you are part of a type of `human`s with the functions `take_step()` and `open_door()`.
Here, the `while` loop simply keeps going until the condition is no longer met.

... More to come ...
