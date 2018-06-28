import std.range : hasAssignableElements, isRandomAccessRange, hasLength;

void bubbleSort(R)(ref R range)
if (isRandomAccessRange!R && hasAssignableElements!R && hasLength!R)
{
    import std.algorithm : swap;

    foreach (i; 0 .. range.length) {
        bool isSorted = true;
        foreach (j; 0 .. range.length - 1)
            if (range[j + 1] < range[j]) {
                swap(range[j + 1], range[j]);
                isSorted = false;
            }
        if (isSorted)
            return;
    }
}

void main() @safe
{
    import std.stdio : writefln;
    import std.range : generate, take;
    import std.array : array;
    import std.random : uniform01;

    auto input = generate!(() => uniform01!float).take(10).array;
    writefln!"before sorting:\n%s"(input);
    bubbleSort(input);
    writefln!"after sorting:\n%s"(input);
}
