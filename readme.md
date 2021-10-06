# AEC Readme
This file contains instructions for the POPL22 artifact evaluation process.

Although our paper referred to our language as Pipe for the double-blind
review, in reality Pipe is a more modern version of the Lucid language. Hence we will
refer to our artifact as Lucid from now on.

The source code for Lucid is available at https://github.com/PrincetonUniversity/lucid.
The contents of this artifact submission are a copy of the popl22_artifact branch, with the addition of
a vagrant box containing the necessary tools to build Lucid from scratch. This branch is a subset of the
main branch, with some minor tweaks and scripts to make evaluation easier. The download for the full
artifact is located at [TODO].

Within the repo, the source code for Lucid is located in the src/ folder. Modules related to the typechecker are located in
src/lib/frontend/typing, and the main file for the dpt executable is located at src/bin/Main.ml.

## Kicking the tires
* This artifact requires both virtualbox (https://www.virtualbox.org/wiki/Downloads) and vagrant (https://www.vagrantup.com/downloads). It has been confirmed to work with virtualbox 6.1.26 and vagrant 2.2.18, on both Windows 10 and Big Sur 11.5.2. the virtual machine uses an unmodified version of Ubuntu 18.04.4 LTS.
* In a terminal, navigate to the [ART]/vm/ directory, and run `setupvm.sh`
* If your computer does not support .sh files, you can instead run `vagrant box add lucid "lucid.box"`
* Run `vagrant up` to start up the VM (may take a few minutes) and `vagrant ssh` to enter it.
* Once in the VM, navigate to the /lucid directory with `cd /lucid`. This is a mirror of the [ART] directory on the host computer,
so changes to the files will be reflected in the VM, and vice-versa.
* To build Lucid, simply run `make` from within the /lucid directory. If the build is successful, you
will see some parse warnings about shift/reduce conflicts, and there should be a `dpt` binary in the /lucid directory.
This is the Lucid binary (the name dpt is from the project's original name, Data Plane Threads).
* As a first test, run `./dpt examples/interp_tests/basics.dpt`. This should print out a lot of text, ending with the lines
```
entry events handled: 1
total events handled: 13

}
dpt: Done
```
The test should complete near-instantaneously. If you do not see any obvious error messages, the test was successful.
* For a more comprehensive test, run `make test`. If you see a linker error (only observed when hosting the VM on a windows machine), delete the [ART]/_build directory on the host machine and try again; you will have to do this every time you run a `make ...` command. If the tests are successful, the last two lines of the output should be:
```
Errors: ['bad_ordering.dpt']
Diffs: []
```
The tests should complete quickly (well under a minute to finish all of them). If either the Errors or Diffs list doesn't match the above output, then something went wrong.

## Claims in the paper:
Our paper makes two types of claims: claims that we support certain language features (page 2), and claims about
the performance of our implementation (page 24). The bulk of the evaluation should consist of
1. Examining some or all of the files in the /lucid/examples/popl22 directory to verify that they make use of the language features, and
2. Running the `dpt` binary on these files to show that they pass the typechecker, and take the expected amount of time. This can be automated with the `make evaluate` command.

### Language feature claims
We now list the language features we claim to support, how to recognize them in a program, and an example file which uses each feature. Most features appear in multiple examples. All examples files are in the [ART]/examples/popl22 directory.
* Polymorphism: Polymorphism is denoted by type variables beginning with a tick (as in OCaml). Our artifact actually supports
multiple kinds of polymorphism:
  * _Size polymorphism_ is denoted by type variables appearing within double angle brackets, e.g. `int<<'a>>`. This kind of polymorphism is not relevant to the features described in the paper and can safely be ignored.
  * _Type polymorphism_ is denoted by type variables appearing where a type would otherwise appear, e.g. declaring a function parameter as `'a foo` instead of `int foo`. An example of this is in the Bloomfilter.dpt example, where the type variable 'any is used to ensure that the second argument of `add_to_filter` and `in_filter` can be any type.
  * _Location polymorphism_ is mostly implicit in Lucid's syntax, since locations are not exposed directly to the user. However, evidence of it can be seen in function constraints in module interfaces. Bloomfilter.dpt is another examples of this; the constraints `[start <= filter; end filter]` restrict the implicitly-polymorphic `filter` argument to be within a certain bound.
* Hierarchical locations: As argued in section 2.5, hierarchical locations are necessary to include existential types (i.e. modules). Again, Bloomfilter.dpt is an example of this -- the type `t<<'idx_sz, 'k>>` is declared in the module interface without a definition, and is therefore abstract. Note that it is defined within the module body as a record.
* Compile-time constructors: Constructors are used whenever we define abstract types, and use the `constr` keyword. Once again, Bloomfilter.dpt provides an example.
* Vectors and loops: In order to be fully general, most of our modules use these. Vector types have the form `<type>[<size>]`, e.g. `int[2]` is the type of integer lists of length 2. Loops have the form `for (i < <size>) { ... }`. We also support vector comprehensions, which have the form `[<expresson> for i < <size>]`. Again, see BloomFilter.dpt.
* Type inference. As with polymorphism, there are actually three kinds of inference: size inference, location inference, and type inference. Again, size inference is not relevant and may be ignored. Location inference is evident from the fact that we do not include explicit location annotations in the code -- indeed, the only place locations appear is in constraints on functions in module interfaces. For example, the type `Nat.t` (in NAT.dpt) is global, and therefore must be associated with a location. But we do not write the location when writing functions that take `Nat.t` as inputs. Type inference is rarely used in practice, but can be done by replacing the type of a local variable with the keyword `auto` during its declaration; the NAT.dpt file contains two examples of this.

Although the Lucid type system already had the ability to catch ordering errors, it is important to verify that our type system
catches them as well, and does not simply accept every program. Otherwise, our claims that we support the above features
would be trivial! To demonstrate this, we include a simple example `bad_ordering.dpt`. By running `./dpt examples/popl/bad_ordering.dpt`, you will see that the type system generates an ordering error, and correctly points to the `Array.set(arr1, 0, 1)` line as the first out-of-order access. Reviewers are encouraged to try tweaking other examples to verify that ordering errors are in fact caught (the BloomFilterTimeout example is a good one for this, as it contains 3 BloomFilter globals).

### Performance claims
Our performance claims appear in figures 14 and 15 of the paper, on page 24. Note that lines of code are not including comments; for easy evaluation, we have provided stripped-down versions of each example in the [ART]/examples/popl22_stripped directory that remove any comments and extraneous code. Note that we generally do not count `include` statements, since we are only evaluating the amount of _new_ code each written for each file. Note as well that the length of the Bidirectional Map file was reported incorrectly in the paper, and should be ~40 LoC instead of ~28.

The `make evaluate` command can be used to automatically run Lucid on each example file and report the time taken to typecheck it; remember that times prefixed with a + in the paper are cumulative with the above time. Due to running on a VM, times may be [TODO] slower than reported in the paper. The evaluation script is [ART]/test/aec_evaluate.py, and the code used to time the typechecking process appears on lines 13-15 of [ART]/src/lib/frontend/FrontendPipeline.ml. The evaluation script prints its results to the command line, but can be redirected to a file easily, e.g. `make evaluate > tmp.txt`.
* Our first set of claims is that we have implemented several general-purpose modules, as depicted in figure 14 of the paper. The table entries correspond to the files below:
  * Bloom Filter -> BloomFilter.dpt.
  * BloomFilter + Aging -> BloomFilterTimeout.dpt
  * Hash table -> Hashtable.dpt
  * Hash table + Cuckoo -> HashtableCuckoo.dpt
  * Hash table w/ timeout -> HashtableTimeout.dpt
  * Hash table w/timeout + Cuckoo -> HashtableCuckooTimeout.dpt
  * Bidirectional map -> Bimap.dpt
  * Count-min sketch -> CountMinSketch.dpt
  * CMS + Aging -> CountMinSketchTimeout.dpt
* Our second set of claims is that we have implemented several specific applications, listed in figure 15 of the paper. Modules used may be verified by sight. The table entries correspond to the files below:
  * Stateful Firewall -> stateful_fw.dpt
  * Closed-loop DNS defense -> dnsguard.dpt
  * *Flow -> starflow.dpt
  * Distributed Prob. Firewall -> chain_prob_stateful_firewall.dpt
  * Distributed Prob. Firewall + Aging -> chain_prob_stateful_firewall_timeout.dpt
  * Simple NAT -> NAT.dpt
  * Historical Prob. Queries -> countmin_historical.dpt

## Resuability
Reviewers are encouraged to try tweaking the given examples, or writing their own programs from scratch! Tutorials on the language are available in the [ART]/docs folder -- the most relevant one is tutorial_language, although it is also possible to run our interpreter in this artifact, if reviewers are curious (the interpreter is not related to any claims about the paper). Examples of programs that can be used with the interpreter appear in the [ART]/examples/interp_tests folder. Reviewers may further information about most Lucid language features can be found on the Lucid repo's wiki, at https://github.com/PrincetonUniversity/lucid/wiki.
