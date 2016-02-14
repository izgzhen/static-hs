Static Analysis Algorithms with Haskell
----

The code is written for practice purpose during the author's learning of *Principles of Program Analysis* [[1]](#ppa).

Honestly speaking, this book is not very good for learning something about program analysis, esp. for newbies. Actually, I went through the [CMU's course](https://www.cs.cmu.edu/~aldrich/courses/15-819O-13sp/) sometime earlier. But I also believe that, there is no easy way to learn, just easier a bit maybe if you got right.

Although it is just for practice, I will try to make the library as modular as possible and the code quality as high as possible.

Why Haskell? First, I have been programming with it for almost a year and will be less distracted when implementing something complex with it. Second, the polymorphism and type class can make things as abstract as possible, while reducing the pain of programming.

### Modules
* Data Flow Analaysis: `Language.DFA`
    + Monotone Framework: `Language.DFA.Core.Mono`
    + Intra-procedural Analysis:
        + Reaching Definition Analysis: `Language.DFA.Packages.RDA`
        + Available Expressions Analysis: `Language.DFA.Packages.AEA`
        + Very Busy Expressions Analysis: `Language.DFA.Packages.VBEA`
        + Live Variable Analysis: `Language.DFA.Packages.LVA`
    + Inter-procedural Analysis:
        + Detection of Sign Analysis: `Language.DFA.Packages.DSA`

## TODOs
1. Test inter-procedural analysis with the recursive function
2. Abstract 0-CFA Analysis

### References
1. <p id="ppa"></p>Flemming Nielson, Hanne R. Nielson, and Chris Hankin. 1999. Principles of Program Analysis. Springer-Verlag New York, Inc., Secaucus, NJ, USA.
