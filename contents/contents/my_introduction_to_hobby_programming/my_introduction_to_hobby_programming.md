# My Introduction to Hobby Programming

Here's the thing. I love programming. I know it's fun, interactive, and addictive, but it's hard to see that when you are looking through a pair of spectacles that sees the world ever-so-slightly differently than mine. For that reason, I feel I should start with my own introduction to the world of computer science, with the hope of inspiring you to share your own story. Full disclosure: this story is a decade old and involved Linux. In fact, the open source community was one of the main reasons I ever became interested in hobby programming to begin with. Regardless, let's start at the start.

I don't know if you guys had a similar childhood to mine or not, but I found that no matter who I spoke to, there was one central theme in all of our lives -- something so fundamental to the way we communicated with each other that it was nearly impossible to have a discussion without bringing it up: **Video Games**.

Like most of my friends, I spent my entire childhood in front of a television screen or monitor, trying desperately to compete with world-destroying supervillains or at the very least, anthropomorphic turtles with spikes on their backs. During school, we would come together and talk about the latest video game gossip. At the time, the internet was still young and I hardly used it for any real purpose. Sure, I would get online every now and again for a school project or two, but it was definitely not a central component to my life... Well, at least not until high school when I became addicted to one Massively Multiplayer Online game, in particular (which will remain nameless).

I bought my first laptop after working all summer as a lifeguard my freshman year in high school, but at the time, I was downright awful with computers. I couldn't even figure out antivirus software and my grandfather had to help me get everything set up. It was the era of Windows Vista and that seemed fine to me, but when I returned to school, my friends (who all seemed to know way more than I did on the subject), seemed to believe Vista to be a demon spawned from the pits of... Microsoft?

Being the curious kid I was, I asked what I should do to avoid Vista and they said I should install another version of the Windows operating system, like Windows XP. I then asked a question that will forever change my life, "What is an operating system?"

My friends could have just said, "It's the thing that operates your hardware. It brings up the UI elements and stuff you work with." Sure, it's not quite the whole story, but as a computer novice, it would have been just fine for me. That said, they took it a step further and said, "Nowadays, there are many operating systems to choose from, including Windows, Mac, and Linux." I may have been a complete novice, but I at least knew about Windows and Mac. Linux, though? What the heck was that?

Of course I asked about it, but this was early high school. My friends may have been smarter than I was, but there was a limit to their knowledge. Apparently, the only thing they knew about Linux was that it was supposedly faster and couldn't catch viruses. That caught me.

Remember how I said I couldn't get the virus software to work on my computer? Yeah. At this point, I was thinking, *What? A computer that couldn't catch viruses? No way! That's gotta be **leagues** easier to use than Windows!*

*Oh ho* I was... well... I was downright naive. I had no idea what I was doing. That night, I ran home, super excited to learn about Linux and operating systems, and spent the entire night browsing the internet, trying to find whatever information was out there. At the time, my google-fu was weak and my internet was slow. The first thing I learned was that there were apparently different types of linuxes called "distributions." At that point, I probably searched "What is the best Linux?" or something to that effect. Somehow, I managed to find [distrowatch](https://distrowatch.com/), which had a list of all the most popular distros on the side.

I don't remember the exact order, but I knew the key players were there: Ubuntu, Mint, Fedora, Debian, and Arch. Now, here's where my years of gaming experience came in. I personified each distribution as a class in a game world. Ubuntu was the easy to use axe-wielding warrior that would get the job done. Fedora was the Archer in the back with a feather in his cap and a quick quip for everything. Debian was the grandmotherly spellcaster just trying to keep everyone alive. Then there was Arch, the one who rushed into combat without any armor and uses only the environment as a weapon.

Which one did I choose? Arch. I've always been a fan of playing the underdog characters. In fact, when I looked up what made Arch Arch, I found the [Archwiki](https://wiki.Archlinux.org/), which was absolutely beautiful and filled with any information I could ever want to know about the distribution, including a page on ["The Arch Way"](https://wiki.Archlinux.org/index.php/Arch_Linux#Principles) -- Which now redirects to a page about Archlinux principles instead. After reading through it, there was not a single shadow of a doubt in my mind. Arch was the distribution for me.

In hindsight, this could have been the most disastrous decision of my life. I hope that at this point, you understand how bad I was with computers, and a slow internet connection didn't help. The next day, I went to the store and bought a 25 pack of writable DVD's. So now for the hard part: Installation.

#### Step 1: Burning to a disk

Now, I know many of you reading think this is an easy thing to do. In fact, I also currently think it's an easy thing to do. Back then, though, it was nearly impossible. I wasted 3-4 DVD's backing up my Windows set-up, just in case. Then I wasted another 15 trying to figure out what to do. At first, I just copied the Archiso onto the disk. That obviously didn't work. It took me ~5 hours to realize that *burning* to a disk was different from simply dragging and dropping files. I then needed to figure out how to do the burning with Windows Vista. By the time I figured this out, nearly my entire stack of DVD's was gone. Oh well, lessons learned, right?

#### Step 2: Just type "root"

After the disk burning fiasco, I thought to myself, *Alright. It'll only get easier from here, right?* Nope. I booted up the Archiso and up popped a prompt talking about how to install Arch. I don't remember what it said exactly, but I know it mentioned that if you wanted to install Arch on a "kerosene-powered cheese grater" that you should consult the wiki. Now, here's the thing: for anyone else, this would just be an idle quip -- something to chuckle at before moving on, but I was not able to move past this prompt. Right under it was a rather surprising adversary, the phrase `Archiso login:` with a blinking cursor.

In my head, I knew what this meant. I needed to create my username, right? So I tried putting in what I wanted my username to be. No luck. I tried any number of different combinations of letters and characters. I even tried our Wi-Fi password. Nothing. I tried re-burning the disk a few more times, but I was still completely lost. What did they want? What was I supposed to type to move forward with the installation? I couldn't figure it out, and after hours of sitting there, looking at a blinking prompt, I started wondering if my computer was actually a kerosene-powered cheese grater and consulted the wiki, but even that wasn't particularly clear. So I googled some more, and then some more and more. Somehow, I stumbled onto a page that gave me the answer -- the magical password that would allow me access to the Arch installer:

**root**

Now, at the time, I didn't realize what I was doing. I didn't know that root was the god of my computer -- the user who decides all other user disputes. At the time, I thought it was an odd, quirky word chosen by the Arch developers in the same way they were talking about cheese graters before. I suppose that knowledge would come in time. At that moment, I was celebrating what might have been the most difficult thing I had done to that point in my life: typing in a 4 letter word.

#### Step 3: The blue screen of death

If you install Arch nowadays, things are different. There is a terminal prompt and the entire install is done manually. I love the new installer because it forces new users to really dig down and understand what makes Arch Arch. At the time of my first installation, though, this was not how things went. Instead of a terminal prompt, there was a semi-graphical installer, where the entire background was blue.

Now, I have blue eyes. I am fairly certain they were blue before the installation, but I cannot be certain. I do know that this blue hue was burned eternally into my retinas. I was staring at this screen for so long that the moment I would look up from the screen, everything seemed red.

Truth be told, I learned a lot at this stage of the installation, including mounting, partitioning, and how to use vi (not vim). That said, the most important thing I learned was how to use the Archwiki's Beginner's Guide. Holy cow! After this part of the installation, I felt like a real *man*. That is to say, I learned about the man command. Seriously, though, I was bombarded with a flurry of new ideas and methods that I could have never dreamed about before, and yet I took it all in stride. Quite frankly, it felt amazing!

#### Step 4: Blow on the Hard Drive

In the middle of the installation, when all the packages were popping up, I remember getting a very particular error under the acronym DRDY. I didn't know what this meant. In fact, I don't think very many people did. The error said it was fatal, so I restarted the install from scratch. I got the same error, but this time in a different place, so I did the install again. And then again. And then again. I repeated the installation so many times that I had memorized every last part from start to finish. I had read all the guides online, and yet I was still getting that darned error! DRDY. What could it mean?

Well, I didn't know, but at that point, it was late at night and I was a bit delirious. I phonetically sounded out the error and realized it sounded like the word "dirty." I figured this meant that the hard drive was dirty, so I unplugged it, blew on the socket, and plugged it back in. No more error.

To this day, I don't know how or why this worked.

#### Step 5: The aftermath

I'm skipping the bootloader and everything because that was surprisingly straightforward for my first install. The next thing I remember is the post-install. I had finally finished the grueling installation process after almost 48 hours without sleep. I rebooted my system and was welcomed with the most beautiful sight I had ever seen: a completely black screen, save one blinking prompt:

```
Blitz login:
```

The same as I saw in the installation before. Except this time, I didn't sit there waiting for hours, trying to figure out some password to enter my system. I simply entered my moniker and got to setting up my system.

I'll be honest, even though this happened almost a decade ago, it is burned into my head as one of the best learning experiences I have ever had. I am sure many, many people would have found it incredibly frustrating. They would have thrown their computer at the wall, exclaiming, "What am I supposed to do?" For me, though, it was captivating. I would never see my computer in the same way again and to this day when I pop open a terminal, I cannot help but smile a little.

Basically, since the first time I was able to directly interact with my computer, I have been captivated with the idea, and though I was not able use the knowledge until much later in my life, it stuck with me and definitely shaped my perspective of the world.

## License

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/master/LICENSE.md)).

##### Text

The text of this chapter was written by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

##### Pull Requests

After initial licensing ([#560](https://github.com/algorithm-archivists/algorithm-archive/pull/560)), the following pull requests have modified the text or graphics of this chapter:
- none
