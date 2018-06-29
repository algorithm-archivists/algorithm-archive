///Returns true if a point (x, y) is in the circle with radius r
bool inCircle(double x, double y, double r = 1.0)
{
    return x ^^ 2 + y ^^ 2 < r ^^ 2;
}

///Calculate pi using monte carlo
real monteCarloPI(ulong n)
{
    import std.algorithm : count;
    import std.random : Random, uniform01, unpredictableSeed;
    import std.range : generate, take;
    import std.typecons : tuple;

    auto rnd = Random(unpredictableSeed);
    return generate(() => tuple!("x", "y")(rnd.uniform01, rnd.uniform01))
        .take(n)
        .count!(a => inCircle(a.x, a.y)) / cast(real) n * 4.0;
}

void main()
{
    import std.math : abs, PI;
    import std.stdio : writeln;

    auto p = monteCarloPI(100_000);
    writeln("Estimated pi: ", p);
    writeln("Percent error: ", abs(p - PI) * 100 / PI);
}
