///Returns true if a point (x, y) is in the circle with radius r
bool inCircle(real x, real y)
{
    return x ^^ 2 + y ^^ 2 < 1.0;
}

///Calculate pi using monte carlo
real monteCarloPI(ulong n)
{
    import std.algorithm : count;
    import std.random : uniform01;
    import std.range : generate, take;
    import std.typecons : tuple;

    auto piCount =  generate(() => tuple!("x", "y")(uniform01, uniform01))
        .take(n)
        .count!(a => inCircle(a.x, a.y));
    return piCount * 4.0 / n;
}

void main()
{
    import std.math : abs, PI;
    import std.stdio : writeln;

    auto p = monteCarloPI(100_000);
    writeln("Estimated pi: ", p);
    writeln("Percent error: ", abs(p - PI) * 100 / PI);
}
