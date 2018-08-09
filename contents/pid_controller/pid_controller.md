#Proportional-Integral-Derivative Controller

The proportional-integral-derivative controller (PID controller) is a control loop feedback mechanism, used for continuously modulated control.
The PID controller is comprised of three parts: proportional controller, integral controller, and derivative controller.

Before we get into how a PID controller works, we need a good example to explain things.
Imagine you are making a self-driving RC car that drives on a line, how would make it work given that the car moves with a constant speed.

### Proportional Controller

If the car is too far to the right then you would turn left and vice versa.
But there are a range of angles you can turn the wheel, so you can turn proportional to how far you are from the line.
This is what the proportional controller (P controller) does, which is given by,

$$ P = K_{p} e(t), $$

Where $K_{p}$ is a constant and $e(t)$ is the current error.
The performance of the controller improves with larger $K_{p}$;
if $K_{p}$ is too high then when the error is too high, the system becomes unstable, i.e. the rc car drives in a circle.

### Derivative Controller

The P controller works well but it has the added problem of overshoting a lot.
we need to dampen the oscillation, on way to solve this is to make the rc car resistant to sudden changes of error.
This is what the derivative controller (D controller) does, which is given by,

$$ D = K_{d} \frac{de(t)}{dt}$$

Where $K_{d}$ is a constant.
If $K_{d}$ is too high then the system is overdamped, i.e. the car takes too long to get back on track.
If it's too low the system is underdamped, i.e. the car oscillates around the line.
When the car is getting back on track quickly with little to no oscillations then the system is called critically damped.

### Integral Controller

I looks like we are done, we start driving but if some wind starts pushing the car then we get a constant error.
We need to know if we are spending too long on one side and account for that.
The way to do that is to sum up all the errors and multiply it by a constant.
This is what the integral controller (I controller) does, which is given by,

$$ I = K_{i} \int_{0}^{t} e(x) dx, $$

Where $K_{i}$ is a constant.
The peformance of the controller is better with higher $K_{i}$; but with higher $K_{i}$ it can introduce oscillations.

### Proportional-Integral-Derivative Controller

The PID controller is just a sum of all there three constrollers, of the form,

$$ U = K_{p} e(t) + K_{i} \int_{0}^{t} e(x) dx + K_{d} \frac{de(t)}{dt} $$

To use a PID controller, you need to tune it by setting the constants, $K_{p}$, $K_{i}$, and $K_{d}$.
There are multiple methods of tuning like, manual tuning, Ziegler–Nichols, Tyreus Luyben, and more.

The uses of PID controllers are theoretically any process which has measurable output and a known ideal output,
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

This example is not calculating the time elapsed, instead it is setting a value called dt.

{% method %}
{% sample lang="c" %}
[import, lang:"c_cpp"](code/c/pid_controller.c)
{% endmethod %}

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>
