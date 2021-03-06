# Debugging PIR

## Command Line

PIR comes with a variety of options to analyze the output of the compiler in different
stages. To manage the different options we use environment variables. For instance

    PIR_ENABLE=off bin/R -f yourScript.r

completely disables the PIR optimizer. As follows are the different Options available.

### Debug flags

#### Controlling compilation

    PIR_ENABLE=
        on                default, automatically optimize after a number of invocations
        off               disable pir
        force             optimize every function after compiling to rir
        force_dryrun      as above, but throw away the result

    PIR_WARMUP=
        number:            after how many invocations a function is (re-) optimized

#### Debug output options

    PIR_DEBUG=                     (only most important flags listed)
        help                       list all available flags
        PrintIntoStdout            print without buffering (useful for crashes during compilation)
        PrintEarlyRir              print after initial rir2pir translation
        PrintOptimizationPasses    print after every pass
        PrintOptimizationPhases    print before/after every phase of the compiler
        PrintPirAfterOpt           print the fully optimized pir
        PrintFinalPir              print pir after lowering and CSSA conversion
        PrintFinalRir              print rir produced by pir backend

    PIR_DEBUG_PASS_FILTER=
        regex      only show passes matching regex (might need .*)

    PIR_DEBUG_FUNCTION_FILTER=
        regex      only show functions matching regex
                   (might need .*, sometimes names are missing)

    PIR_DEBUG_STYLE=
        Standard   print pir in human-readable format, to view directly in console
        GraphViz   print pir in GraphViz, displaying all instructions within BBs
        GraphVizBB print pir in GraphViz, displaying only BB names and connections
    PIR_MEASURE_COMPILER=
        1          print overal time spend in different passes on shutdown
    RIR_CHECK_PIR_TYPES=
        0        Disable
        1        Assert that each PIR instruction conforms to its return type during runtime
        2        Also print out failing instructions (leaks memory and might cause slowdown)

#### Extended debug flags

    PIR_DEOPT_CHAOS=
        1          randomly trigger some percent of deopts

    PIR_DEBUG_DEOPTS=
        1          show failing assumption when a deopt happens

#### Optimization heuristics

    PIR_INLINER_INITIAL_FUEL=
        n          how many inlinings per inline pass

    PIR_INLINER_MAX_INLINEE_SIZE=
        n          max instruction count for inlinees

    PIR_INLINER_MAX_SIZE=
        n          max instruction count for callers

#### Serialize flgas

    RIR_PRESERVE=
        1          serialize RIR closures on exit. NOTE: will deserialize a
                   compiled closure from a prior session even if this is off

    RIR_SERIALIZE_CHAOS=
        n          serialize and deserialize the dispatch table on every `n`th
                   RIR call. WARNING: This sometimes prevents optimization

### Disassembly annotations

#### Assumptions

* `!ExpMi`: Called with no explicitly missing arguments
* `!TMany`: Called with at most the number of required args
* `!TFew` : Called with at least the number of required args
* `EagerN`: Argument `N` is already evaluated
* `!ObjN` : Argument `N` is not an object
* `CooOrd`: Arguments are passed in the correct order (ie. callee reorders)

#### Type Annotations (aka type flags)

* `$` : Is scalar
* `^` : May be lazy (and wrapped in a promise)
* `~` : May be wrapped in a promise (but evaluated)
* `?` : May be missing
* `'` : May not be an object

#### Effects

* `v` : Visibility
* `w` : Warn
* `e` : Error
* `f` : Force
* `r` : Reflection
* `l` : LeakArg
* `C` : ChangesContexts
* `R` : ReadsEnv
* `W` : WritesEnv
* `L` : LeaksEnv
* `D` : TriggerDeopt
* `X` : ExecuteCode
* `d` : DependsOnAssume

`!` means that an instruction has all effects *except* the following. e.g. `!r`
means an instruction has all effects but reflection, just `!` means it has all
effects.

## Within R

PIR also adds some functions to the global environment, for the sole purpose of
debugging:

* `rir.markOptimize`: Tells the compiler to optimize the function
* `rir.isValidFunction`: Returns TRUE if the argument is a rir-compiled closure
* `rir.disassemble`: prints the disassembled rir function
* `rir.printInvocation`: prints how many times the (optimized) rir function was
  called
* `rir.compile`: compiles the given closure or expression, returns the compiled
  version
* `pir.compile`: expects a rir-compiled closure, optimizes it
* `pir.tests`: runs some internal regression tests
* `pir.check`: returns TRUE if f, when PIR compiled, satisfies the given checks.
* `pir.debugFlags`: creates a bitset with pir debug options
* `pir.setDebugFlags`: sets the default debug options for pir compiler
* `rir.compile.program`: compiles code of the given file all in a function, and
  returns the functon
* `rir.eval`: evaluates the code in RIR
* `rir.body`: returns the body of rir-compiled function. The body is the vector
  containing its ast maps and code objects
* `.printInvocation`: prints invocation during evaluation
* `.int3`: breakpoint during evaluation
