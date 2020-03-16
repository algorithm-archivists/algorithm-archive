# The Easter Algorithm (Computus)

Though the word *computus* can technically describes any sort of computation {{ "bede725" | cite }} or else a set of medieval tables for calculating various astrological events {{ "dictcomputus" | cite }}, it is also one of the most common historical names for the calculation of the Christian holiday of Easter every year.
Nominally, Easter happens the Sunday after the first full moon after the spring equinox (roughly March 21st).
This particular full moon is known by a number of names, such as the pink (strawberry) moon, hunter's moon, snow moon, along with several others.
The most common name for it is the paschal full moon, which translates to "passover" in Greek and signifies an importan Jewish festival.

For the first few centuries, the date of Easter each year was dictated by the Pope; however, after that point, it was not straightforward to communicate this date to all of Christendom.
As such, the church did what they could to algorithmically generate tables for clergy to determine the date of Easter each year.
To this day, the calculation of Easter still poses a problem, with western and eastern (orthodox) churches celebrating on different dates approximately 50% of the time.

I'll be honest, there is a lot of good, Christian drama surrounding the calculation of this event and it's remarkably interesting to read about it {{ "bien2004" | cite }}.
Suffice it to say that the date of Easter bamboozled many historical scholars, with at least one algorithm appearing in the early archives of the now famouse scientific journal of Nature {{ "computus1876" | cite }}.
The calculation was so complicated for the time that even Frederick Gauss had to try his hand at it (and failed before being corrected by oe of his students).
As strange as it might seem, the development of the Gregorian calendar, which is currently the most used calendar worldwide, was prompted specifically to make the calculation of Easter easier.

As an important note, because Easter depends on the lunar cycle, it is static in the lunar calendar.
In this way, computus is the act of mapping a lunar cycle onto annual calendars everyone knows and loves.
At the time, the question was, "Which annual calendar do we map it to? The Julian calendar or the Gregorian."
The western churches chose Gregorian and the eastern chose Julian.
The Gregorian calendar more accurately represents the true date of the paschal new moon; however, this is one reason why western and eastern churches sometimes celebrate on different dates.

Though there are many methods to calculate Easter, for now, we will focus only on Gauss's algorithm; however, we will certainly come back (in subsequent years) to incorporate other Easter algorithms.
These algorithms are some of my favorite gems in the history of algorithm design because of all the drama surrounding the calculation of something that should bbe trivial!
After all, how hard could it be to calculate Easter?

## Gauss's Easter algorithm history

Gauss is known for a lot of things: Gaussian elimination, the Cooley-Tukey method before Cooley or Tukey even existed, Gauss's Law for electromagnetism, etc.
One thing he is *not* particularly well known for is an algorithm he devised in 1800, which was later corrected by his student Peter Paul Tittle in 1816.
In fact, there were a series of publications from Gauss in this era all relating to the precise date of Easter.
The legend goes that Gauss actually did not know his actual birthday in the Gregorian calendar and used this same algorithm to determine it.
Apparently, His mother only told him that he was born on a Wednesday 8 days before Ascention Day in 1777, corresponding to April 30th {{ "bien2004" | cite }}.

Honestly, Gauss's Easter algorithm was the 19th century equivalent of undocumented code.
I could imagine Gauss grumpily "patching" his method when users complained that it did not work on dates past 4200 or certain dates within his own era.
Soem of his compatriots (such as Johan Lambert and Jean Joseph Delambre) expressed their concern over the method's performance, Gauss replied by saying,

> The investigation by which the formula [...] is found is based on higher arithmetic, for which I presumably cannot refer to any publication.

Which was the 19th century equivalent of saying, "you are too dumb to understand my genious."
I have definitely met a few fledgling programmers who feel the same, but none of them were anywhere near as prolific as Gauss.

One of the most important fans of Gauss's work was Servois, who created a calendar based on Gauss's 1800 publication, shown below:

ADD TABLE

The task for this chapter will be to explain (to the best of my abilities) how one would go about using Gauss's Easter algorithm to update the table with Gauss's patched algorithm.

## The actual algorithm

As alluded to in Gauss's quote above, the Easter algorithm is closer to a set of formula than an algorithm used to compute anything on a modern computer.
This is in-part because of bad software engineering by Gauss and in-part because computers did not really exist at that point.
Considering this method was literally called *Computus*, I guess there wasn't much to compute at the time at all.


### Bibliography

{% references %} {% endreferences %}

