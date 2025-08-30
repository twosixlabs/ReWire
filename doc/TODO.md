# Process for adding a new language feature (e.g., rwPrimUsingExtern)

1. Add the new primitive to `rewire-user/src/RWC/Primitives.hs`. Give it a type
   and a Haskell definition for use by GHC. Primitives should start with the
   prefix `rwPrim`.

2. Add a user-level interface to the new primitive in the apropriate place in
   `rewire-user/src/ReWire`.

3. Add some simple tests using the new user-level interface in
   `tests/regression`.

4. Add the new primitive as a constructor for the `Builtin` type around line
   191 in `rewire-core/src/ReWire/Crust/Syntax.hs`.

5. Add a translation for the new primitive from RWCrust to the RWCore IR (i.e.,
   define it in terms of RWCore). Extend the `transBuiltin` function in
   `rewire-core/src/ReWire/Crust/ToCore.hs` with these definitions (there are many
   examples already there). Some primitives, such as Verilog built-in operators,
   you'd instead want to also add new built-ins to the RWCore language and then
   define the RWCore built-ins in terms of Verilog.

6. For new built-in types (e.g., `Vec`, `ExtDev`), extend
   `rewire-core/src/ReWire/Crust/PrimBasis.hs` with their definition.

7. Run the simple tests with the `-v` RWC flag. Depending on how far RWC gets,
   output should look something like:
   ```
   [...]
   Debug: [2] (Haskell) Annotating.
   Debug: [3] (Haskell) Desugaring.
   Debug: [4] (Haskell) Translating to Crust IR.
   Debug: [5] Concatenating Crust IR for module: fibo2.hs
   Debug: [6] Adding primitives
   Debug: [7] Removing the Main.main definition (before attempting to typecheck it).
   Debug: [8] Inlining INLINE-annotated definitions.
   Debug: [9] Expanding type synonyms.
   Debug: [10] Typechecking, inference.
   Debug: [11] Removing Haskell definitions for externs.
   Debug: [12] Removing unused definitions.
   Debug: [13] Eliminating pattern bindings (case expressions).
   Debug: [14] Partial evaluation.
   Debug: > Specializing...
   Debug: > Purging...
   Debug: > Reducing...
   Debug: [15] Normalizing bind.
   Debug: [16] Lifting, shifting, eta-abstracting lambdas.
   Debug: [17] Purifying.
   Debug: [18] Final lifting, shifting, eta-abstracting lambdas.
   Debug: [19] Final purging of unused definitions.
   Debug: [20] Translating to core & HDL.
   Debug: [21] Core.
   Debug: Partially evaluating/reducing core IR. If this is taking too long, consider disabling with --rtl-opt=0.
   ```

8. Dump the IR for the passes leading up to the point of failure. The numbers
   in brackets above indicate the current "pass", which are points in the
   compilation process for which IR can be dumped. Passes 1-4 are Haskell (HSE),
   passes 5-20 are RWCrust, and pass 21 is RWCore. The corresponding IR for a
   certain pass (or several passes) can be dumped with `-d` flag, e.g., `rwc -d
   1,3,4 myfile.hs > output.hs`. Note that if you add `-v` you'll also get the
   `show` output for the IR in addition to the `prettyPrint` output.

9. Debug the issue. Most of the RWC pipeline can be found in
   `rewire-core/src/ReWire/ModCache.hs`:
   ```haskell
    -- [...]
    >=> pass 6 "Adding primitives"
    >=> pure . addPrims
    >=> pass 7 "Removing the Main.main definition (before attempting to typecheck it)."
    >=> pure . removeMain
    >=> pass 8 "Inlining INLINE-annotated definitions."
    >=> inlineAnnotated
    >=> pass 9 "Expanding type synonyms."
    >=> expandTypeSynonyms
    >=> pass 10 "Typechecking, inference."
    >=> kindCheck >=> typeCheck start
    >=> pass 11 "Removing Haskell definitions for externs."
    >=> pure . neuterExterns
    >=> pass 12 "Removing unused definitions."
    >=> purge start >=> extraTC
    >=> pass 13 "Eliminating pattern bindings (case expressions)."
    >=> elimCase >=> liftLambdas >=> extraTC
    >=> pass 14 "Partial evaluation."
    >=> simplify conf >=> extraTC
    >=> pass 15 "Normalizing bind."
    >=> normalizeBind >=> extraTC
    -- [...]
   ```
   If e.g. the IR from pass 13 does not exhibit the bug, but the IR from pass
   14 does, then the problem is likely caused by either the `elimCase` or
   `liftLambdas` transformations. Note that the RWC `--debug-typecheck` flag
   can also be used to enable extra typechecking of the Crust IR after most major
   transformations (at points indicated by `extraTC` in the pipeline excerpt
   above).

# TODOs

## General

- Replace HSE with ghc-lib-parser:
  https://hackage.haskell.org/package/ghc-lib-parser

- RWC should take a flag that causes it to automatically generate a testbench
  so the generated RTL can be immediately simulated in some way.

- Need to completely refactor error handling. Errors should be thrown as early
  as possible -- if an (non-debug-level) error needs to be thrown further down
  the chain, special concessions should be made to ensure it provides good
  feedback. Don't annotate the AST with the whole AST. Maybe use one of the
  hackage libraries for producing pretty errors.

- Have RWC automatically dump current IR on a crash. Especially annoying when a
  crash happens building a big program and you just got to immediately re-run
  rwc with the right -d flag (assuming you know the right one).

- Add a pragma (or similar) to explicitly tell RWC to ignore a definition meant
  for GHC. Make the implicit behavior of ignoring definitions in the Prims file
  explicit. Similarly, one should be able to write the type annotation for the
  GHC `main` function without RWC complaining (although for this case we could
  just explicitly have RWC ignore `main` in particular).

- Add a flag for the RWC interpreter (i.e., `--interp`) to have it print
  "state" values instead of just outputs at every clock tick. Currently it just
  prints outputs, which can make debugging more difficult in certain cases.

## For supporting something like the AI accelerators project

- Currently, the only way to interact with an "external" device defined only in
  RTL is through our `extern` built-in function, which can instantiate
  arbitrary Verilog modules. But it really only works for purely combinational
  modules because RWC treats it as a pure function (and therefore will
  duplicate calls and such). In some cases you can get away with using
  non-combinational modules with `extern` but it's very sketchy.

  To support composition of (non-combinational, i.e. clocked) devices, I
  suggest something like this:
  ```haskell
  data ExtDev i o = Tick o (i -> ExtDev i o)

  rwPrimUsingExtern :: GHC.Monad m => ExtDev i' o' -> ReacT (o', i) (i', o) m a -> ReacT i o m a
  rwPrimUsingExtern (Tick _ res') (GHC.ReacT m) = GHC.ReacT GHC.$ m GHC.>>= \ ma' -> case ma' of
        GHC.Left y               -> GHC.return (GHC.Left y)
        GHC.Right ((i', o), res) -> let t'@(Tick o' _) = res' i'
              in GHC.return (GHC.Right (o, \ i -> rwPrimUsingExtern t' (res (o', i))))
  ```
  These are Haskell definitions (and maybe not totally correct), but this would
  be a builtin and support interfacing a device in Haskell with a device in RTL
  (typed as `ExtDev i o`) in a reasonably type-safe way.

  NOTE: I started implementing this and some bits of that work made it into
  master -- there's a `UsingExtern` built-in (in `Crust/Syntax.hs`) and a
  `ExtDev` type in `Crust/PrimBasis.hs`. These are vestigial and should really
  be removed if the rest isn't implemented.

- AI devices will need good support for matrices and matrix operations. I'd
  suggest starting with a simple operation relevant to AI like 2D convolution
  -- try to implement a device performing the operation in RW and see how hard it
  is and start adding features or libraries to make it easier. Probably want to
  find a good library on Hackage for matrices and add "native" support for it to
  RWC kinda like we did with vectors. Or maybe alternatively, start with a
  Hackage library for neural networks (e.g., hasktorch?).
