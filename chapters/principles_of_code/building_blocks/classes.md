### Classes and Structs

A while ago, one of my friends asked me the purpose of classes in programming.
No matter what I did or how I tried to reason with him, I could not convince him that classes were useful and necessary programming constructs.
To put it simply, classes are data types that allow programmers to store multiple data types within them.
That said, depending on the language, classes might look slightly different.
For example, Julia forgoes classes altogether and simply allows programmers to define a type of types.
I am personally a fan of this approach and will be using it the bulk psuedocode for this text:

```julia
type Human
    Height::Float64
    Weight::Float64
    Popularity::Float64
end
```

Now, here's where things get a little sticky.
The truth is that there is a philosophical difference between how languages implement classes (among other things), which basically boils down to whether languages allow functions to be held within data types or not.
*Functional* programming languages argue that functions should always act on data types.
*Object-Oriented* languages argue that certain data types (like Human, above), should be able to *do* things, so it makes sense to put functions within classes.
There is merit to both of these arguments, so it's best to go with whatever you feel comfortable with.

In the case of object-oriented languages, classes have an additional layer of complexity associated with them that should definitely be discussed.
That said, I would like to forgo that discussion at this time and come back to it in the near future.
Please bug me if you think I might have forgotten!
