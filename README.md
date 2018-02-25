# advent-of-haskell

Advent of code in Haskell lts-10.4

Remember that benchmarks aren't perfect, and can vary between runs for a variety
of reasons. I use them to see which days I need to optimize,
not gather absolute data on.

Report generated by running
`stack build && stack exec advent-code --output report.html`

    Slowest (in seconds):

    - Day 15 5.4
    - Day 25 4.4
    - Day 22 3.5
    - Day 24 1.7
    - All others finish under a second

![Criterion Benchmark Report](./benchmarks/criterion-report.png)
