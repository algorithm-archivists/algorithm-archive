# Proportional-Integral-Derivative Controller

The Proportional-Integral-Derivative controller (PID controller) is a control loop feedback mechanism, used for continuously modulated control.
The PID controller has three components: proportional controller, integral controller, and derivative controller.

Before we get into how a PID controller works, we need a good example to explain things.
For the following sections, imagine you are designing a self driving RC car that tries to remain on a line as it is moving with a constant speed. How would you keep it on course?

### Proportional Controller

Imagine our RC car is too far to the right of the line, in this case it makes sense to turn left.
Since there are a range of angles you could turn the wheel by, it is unclear what strategy would work best to return the RC car to the line; however, it is clear that if the angle chosen is proportional to the distance from the line, the car will always be moving towards it.
This is what the proportional controller (P controller) does, which is described by

$$ P = K_{p} e(t), $$

Where $$K_{p}$$ is an arbitrary constant and $$e(t)$$ is the current distance from the line, which is called the error.
The performance of the controller improves with larger $$K_{p}$$;
however, if $$K_{p}$$ and $$e(t)$$ are too high then the system becomes unstable.
If the proportional controller is used alone, the car will not move appropriately.

For example, if the car is too far out, the angle will be above $$2\pi$$, which means that the car cannot quite return to the line and will instead move in circles, as shown below:

<div style="text-align:center">
<video width="500" height="500" autoplay controls loop>
  <source src="res/circling.mp4" type="video/mp4">
Your browser does not support the video tag.
</video>
</div>

On the other hand, if the car is sufficiently close to the line, it will oscillate back and forth, but never rest on the line, itself, as shown below:

<div style="text-align:center">
<video width="500" height="500" autoplay controls loop>
  <source src="res/oscillation.mp4" type="video/mp4">
Your browser does not support the video tag.
</video>
</div>

### Derivative Controller

The P controller works well but it has the added problem of overshooting, we need to dampen this motion.
One way to do this is to make the RC car resistant to sudden changes of error.
This is what the derivative controller (D controller) does, which is described by

$$ D = K_{d} \frac{de(t)}{dt}$$

Where $$K_{d}$$ is a constant and $$\frac{de(t)}{dt}$$ is the derivative of the error function $$e(t)$$.
If $$K_{d}$$ is too high then the system is overdamped, i.e. the car takes too long to get back on track.
If it's too low, the system is underdamped, i.e. the car oscillates around the line.
When the car returns to the track and there is little to no oscillations, the system is critically damped.

<div style="text-align:center">
<video width="500" height="500" autoplay controls loop>
  <source src="res/damping.mp4" type="video/mp4">
Your browser does not support the video tag.
</video>
</div>

### Integral Controller

The Proportional and Derivative controllers are robust enough to keep the RC car on course, but what if some wind or other external force starts pushing the car constantly off track?
Well, we would need to know if we are spending too long on one side to account for this, and we can figure that out by taking the sum of all the displacements, usually referred to as errors, and multiplying it by a constant.
This is what the integral controller (I controller) does, which is described by

$$ I = K_{i} \int_{0}^{t} e(\uptau) d\tau, $$

Where $$K_{i}$$ is a constant.
The peformance of the controller is better with higher $$K_{i}$$; but with higher $$K_{i}$$ it can introduce oscillations.

### Proportional-Integral-Derivative Controller

The PID controller is just a sum of all three controllers and is of the form

$$ U = K_{p} e(t) + K_{i} \int_{0}^{t} e(x) dx + K_{d} \frac{de(t)}{dt} $$

To use a PID controller, you need to tune it by setting the constants, $$K_{p}$$, $$K_{i}$$, and $$K_{d}$$.
If you choose the parameters for your PID controller incorrectly, the output will be unstable, i.e., the output diverges.
There are multiple methods of tuning like manual tuning, Ziegler–Nichols, Tyreus Luyben, Cohen–Coon, and Åström-Hägglund.{{ "wikipid" | cite }}

Theoretically, PID controllers can be used for any process with a measurable output and a known ideal output,
but controllers are used mainly for regulating temperature, pressure, force, flow rate, feed rate, speed and more.

## Putting it all together

Luckily the algorithm is very simple, you just need to make the PID equation discrete.
Thus, the equation looks like this:

$$ U = K_{p} e(t_{j}) + \sum_{l=0}^{j} K_{i} e(t_{l}) \Delta t + K_{d} \frac{e(t_{j-1}) - e(t_{j})}{\Delta t}. $$

In the end the code looks like this:

{% method %}
{% sample lang="c" %}
[import:26-34, lang:"c"](code/c/pid_controller.c)
{% endmethod %}

## Example Code

The example code is of a 1-dimensional RC car that is trying to change from the first lane to the second lane, where the numbers represent the center of the lane.
In this example, we can't calculate the time elapsed, so we are instead setting a value called dt for time elapsed.

{% method %}
{% sample lang="c" %}
[import, lang:"c"](code/c/pid_controller.c)
{% endmethod %}

### Bibliography

{% references %} {% endreferences %}

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>
## License

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/master/LICENSE.md)).

##### Text

The text of this chapter was written by Gathros and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

##### Images/Graphics
- The animation "[damping](res/damping.mp4)" was created by Gathros and is licenced under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
- The animation "[circling](res/circling.mp4)" was created by Gathros and is licenced under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)
