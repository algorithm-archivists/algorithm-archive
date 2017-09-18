# The Principles of Code

At least to me, programming is a form of creative expression. Sure, programming is difficult. It's difficult in the same way that art, music, or writing is difficult: you know exactly what you want to create, but it takes time to learn how to create it. As I mentioned previously, for a significant portion of my life, I wanted to be an author, so I spent hours and hours every day writing fantasy novels. The more I wrote, the better I got at writing, but when I went back to read the initial chapters of my books, I was frankly revolted at how poor my writing was. By the time I finished revising those chapters and writing more on the side, I felt that the second half of the story was in need of revision too. In the end, I wrote and re-wrote and re-wrote again until I was sick of the story altogether, so I wrote something else. Same characters, different story. It was a vicious cycle that ultimately lead to failure on my part to publish anything. 

Here's that cycle again in code:

```julia
type Human
    ability::Int64
    standard::Int64
end

function create_something(me::Human)
    while (me.ability < me.standard)
        me.ability += 1
        me.standard += 1
        println("I am not good enough. Continuing...")
    end
end

create_something(Human(0,1))
```

I am sure this has a name, but I like to call it the *perfectionist's loop*. It's obviously endless and the only way out is by forcing the code to terminate early with `ctrl+c`. Any artist knows this problem. At first, the act of creation is fun, but after a few iterations, it becomes frustrating. Soon, you are curled into a ball in the corner of a room with a paintbrush in your hand and a myriad of colors splattered all over you and the immediate area. In front of you lay the remains of your art, toppled to the ground. It happens. It will probably happen when you learn programming. When it happens, there is only one thing to do: *keep iterating through the loop!* 

The moment you press `ctrl_c` is the moment you stop improving. Don't stop improving. Don't lower your standards. If you ever need motivation to continue, look at who you were a few months ago. You should be "better" than that person. If not, figure out what's holding you back and keep iterating through the loop!

The problem is that when it comes to programming, there are a bunch of technical problems that crop up and prevent us from improving. This chapter is specifically written to help you make decisions and improve your ability to program. We'll start with choosing a language -- a question that kept me from even starting programming to begin with. Then we'll move on to programming building blocks and important data structures to remember. The idea is that we'll link to the building block sections when necessary throughout the book. This section will probably be the section that changes the most frequently as the archive evolves. after all, the more algorithms we cover, the more building blocks will be necessary to write them.

As always, let me know if there's anything that is unclear or you think needs to be fixed! Thanks for reading and good luck!
