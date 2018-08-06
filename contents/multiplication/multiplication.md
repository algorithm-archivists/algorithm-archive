# Multiplication
Multiplying two numbers is a relatively simple process.
For the most part, elementary school students learn how to do so quickly before moving on to more complicated mathematical operations.
Even so, there are plenty of incredibly complicated algorithms out there to perform the operation.

In some sense, this is what I love about computer science research!
It takes operations that everyone knows and loves, abstracts them, and transforms them into fast and efficient methods for the computer to implement.
Sometimes these algorithms are completely baffling.
I remember laughing for ten minutes after I heard that FFT convolutions could be used to calculate a simple integer multiplicationing the Schönhage–Strassen algorithm.
I thought, "Why would you ever want to use an FFT to do something to trivial? This has to be a joke!"

Oh boy was I wrong.
The Schönhage–Strassen algorithm was actually the most efficient method to multiply two numbers until around 2007 when it was dethroned by the Fürer's algorithm.
Even so, the Schönhage–Strassen is still used in practice by the math libraries of many languages due to its straightforward implementation and a few other factors.

Here's the point: through time, computer science researchers have managed to take some of the simplest operations imaginable, overcomplicate them, and make them incredibly impressive.
Sometimes it's even worth looking at trivial operations through a new lens.


<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>
