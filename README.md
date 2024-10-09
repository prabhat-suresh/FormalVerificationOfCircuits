# Formal Verification of Circuits
## Tools as options
- bluespec 
    - proper HDL - meant for circuit design - sync is implicitly done - doesn't have enough types to prove correctness
- hardcaml - DSL which produces circuits
- Kami - written in coq

- clash
- implement synchronous featues in case of clash
- asynchronous circuits
- for proving use coq then extract to haskell and finally run clash on it

## We settled on clash
- clash --> generate verilog also and check on EDAplayground if the actual synthesis works
- work out on clash with examples this semester
- next semester we'll look into the proof verification part with coq
#### Check out the following projects to get familiar with clash
- [check out different versions of verilator](https://verilator.org/guide/latest/changes.html)
- [clashilator](https://github.com/gergoerdi/clashilator)
- [Where Lions roam - RISC-V](https://github.com/standardsemiconductor/lion)
- Lions library - difficult
- RISC-V formal - 5 stage formally verified
- [contranomy - simpler](https://github.com/christiaanb/contranomy)
- [space invaders](https://github.com/gergoerdi/clash-spaceinvaders)
- [clash intel8080](https://github.com/gergoerdi/clash-intel8080)
- Regex matching circuits

> Can ask to purchase this [Clash Book](https://erdi.dev/retroclash/)

## Reference Book:
Certified programming with dependent types (CPDT) by Adam Chlipala
