# Proportional-Integral-Derivative Controller
Written by Gathros

The Proportional-Integral-Derivative controller (PID controller) is a control loop feedback mechanism, used for continuously modulated control.
The PID controller is comprised of three parts: proportional controller, integral controller, and derivative controller.

Before we get into how a PID controller works, we need a good example to explain things.
Imagine you are making a self-driving RC car that drives on a line, how could we keep the car on track if it moves with a constant speed?

### Proportional Controller

Imagine our RC car is moving too far to the right, in this case it makes sense to turn left.
Since there are a range of angles you can turn the wheel, you should turn proportional to the distance from the line.
This is what the proportional controller (P controller) does, which is described by,

$$ P = K_{p} e(t), $$

Where $K_{p}$ is a constant and $e(t)$ is the current distance from the line, which is called the error.
The performance of the controller improves with larger $K_{p}$;
if $K_{p}$ is too high then when the error is too high, the system becomes unstable.
In this example, the car would turn in circles, since there is a maximum angle the wheel can turn, else it would zig zag around the line.

### Derivative Controller

The P controller works well but it has the added problem of overshooting a lot, we need to dampen these oscillations.
One way to solve this is to make the rc car resistant to sudden changes of error.
This is what the derivative controller (D controller) does, which is described by,

$$ D = K_{d} \frac{de(t)}{dt}$$

Where $K_{d}$ is a constant.
If $K_{d}$ is too high then the system is overdamped, i.e. the car takes too long to get back on track.
If it's too low the system is underdamped, i.e. the car oscillates around the line.
When the car returns to the track and there is little to no oscillations, the system is critically damped.

### Integral Controller

The Proportional and Derivative controllers are robust enough to keep on course, but what if some wind starts pushing the car and introducing a constant error?
Well, we would need to know if we are spending too long on one side to account for it, and we can figure it out by summing up all the errors and multiply it by a constant.
This is what the integral controller (I controller) does, which is described by,

$$ I = K_{i} \int_{0}^{t} e(\uptau) d\uptau, $$

Where $K_{i}$ is a constant.
The peformance of the controller is better with higher $K_{i}$; but with higher $K_{i}$ it can introduce oscillations.

### Proportional-Integral-Derivative Controller

The PID controller is just a sum of all three controllers and is of the form,

$$ U = K_{p} e(t) + K_{i} \int_{0}^{t} e(x) dx + K_{d} \frac{de(t)}{dt} $$

To use a PID controller, you need to tune it by setting the constants, $K_{p}$, $K_{i}$, and $K_{d}$.
If you choose the parameters for your PID controller incorrectly, the output will be unstable, i.e., the output diverges.
There are multiple methods of tuning like, manual tuning, Ziegler–Nichols, Tyreus Luyben, Cohen–Coon, and Åström-Hägglund.

Theoretically, PID controllers can be used for any process with a measurable output and a known ideal output,
but controllers are used mainly for regulating temperature, pressure, force, flow rate, feed rate, speed and more.

## The Algorithm

Luckily the algorithm is very simple, you just need to make the PID equation discrete.
Thus, the equation looks like this:

$$ U = K_{p} e(t_{j}) + \sum_{l=0}^{j} K_{i} e(t_{l}) \Delta t + K_{d} \frac{e(t_{j-1}) - e(t_{j})}{\Delta t}. $$

In the end the code looks like this:

{% method %}
{% sample lang="c" %}
[import:26-34, lang:"c_cpp"](code/c/pid_controller.c)
{% endmethod %}

## Example Code

The example code is of a 1-dimensional RC car that is trying to change from the first lane to the second lane, where the numbers represent the center of the lane.
In this example, we can't calculate the time elapsed, so we are instead setting a value called dt for time elapsed.

{% method %}
{% sample lang="c" %}
[import, lang:"c_cpp"](code/c/pid_controller.c)
{% endmethod %}

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>
