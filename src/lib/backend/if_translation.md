###Translating if/else expressions and merging match-action lists

An important part of compilation is converting an `if/else` statement into a list of match-action rules. A match-action rule `r_i` has two components: a match condition `c_i` and an instruction pointer `a_i`. When we evaluate a list of match-action rules, we test every rule in order and jump to the instruction of the first rule whose match condition evaluates to true. Because we test rules in order, the semantics of a rule's condition (i.e., the program states that cause it to execute) depends on which rules come before it in the list. Specifically, if we have a rule list `[r_1; r_2; ...; r_{n-1}; r_n]`, where rule `r_i` has condition `c_i`, then rule `r_n` actually tests `c_n` *and the negation of all prior rules*, i.e., `~(c_1 & c_2 & ... & c_{n-1}) & c_n`. This makes converting an if statement into a list of match action rules a bit tricky. 

###Disjunctive normal form
The first step in converting an if statement into a list of match-action rules is to put the if statement's expression into the following disjunctive normal form (DNF): 

``
  exp  := term [|| term]
  term := atom [&& atom]
  atom := var < == | != > constant
``

The expression is a disjunction of terms, each term is a conjunction of atoms, and an atom tests a variable against a constant. Each term has at most one atom per variable. We can convert an arbitrary expression into this form using known algorithms (e.g., already implemented in z3) and by precomputing subexpressions as needed to simplify the atoms themselves (for example, atoms that test two variables). 

###DNF to MA
Given an expression in the above DNF, how do we convert it to a list of match-action rules (MA)? 

**Atoms** First, we can translate every atom into two rules using the following macros: 

Equality atom case:
``
a == c --> 
(a : c) -> (true)
(a : _) -> (false)
``

Inequality atom case:
``
a != c --> 
(a : c) -> (false)
(a : _) -> (true)
``

Note how this takes advantage of MA rule ordering to efficiently implement negation. In the inequality atom case, the first rule is blocking out (or *shadowing*) part of the space that the second rule would match. 

**Terms** Translating a term (a conjunction of atoms) into a list of MA rules is also straightforward. 
We explain using the following term as an example: ``(a == 1) && (b != 2) && (c == 3) && (d != 4)``
The steps are: 

1. for each inequality atom, generate a rule that evaluates to false if the negation of the atom matches. For the example, we generate the rules: 
  ``
    (a:_; b:2; c: _; d:_;) -> false
    (a:_; b:_; c: _; d:4;) -> false
  ``
2. for all the equality atoms, generate a single rule that evaluates to true if all the atoms match. For the example this is: 
  ``
    (a:1; b:_; c: 3; d:_;) -> true
  ``
3. finally, add a default rule that evaluates to false if none of the previous rules match, this represents the scenario where _none_ of the equality or inequality tests match. 
  ``
    (a:_; b:_; c: _; d:_;) -> false
  ``

The entire rule set for the example is: 
  ``
    (a:_; b:2; c: _; d:_;) -> false
    (a:_; b:_; c: _; d:4;) -> false
    (a:1; b:_; c: 3; d:_;) -> true
    (a:_; b:_; c: _; d:_;) -> false
  ``

Again, this takes advantage of MA rule ordering to efficiently implement negations. 

**Full DNF expressions** Finally, to translate a DNF expression that is a disjunction of terms, we generate rule lists for each term and then combine the rule sets. Observe that we can't just concatenate two rule sets. For example, say we have the expression: ``((a == 1)&&(b != 2))||((a != 2)&&(b == 2))``. The first term translates into: 
  ``
  (a == 1)&&(b != 2) --> 
    1) a:1; b:2; -> false;
    2) a:1; b:_; -> true;
    3) a:_; b_:_; -> false;
  ``
the second term translates into: 
  ``
  (a != 2)&&(b == 2)  --> 
    4) a:2; b:2; -> false;
    5) a:_; b;2; -> true;
    6) a:_; b_:_; -> false;
  ``
Unfortunately, concatenating per-term rule lists does not result in a rule list that is semantically equivalent to the complete expression. The problem is shadowing. For example, `a = 1; b = 2` would evaluate to true in the second expression, due to the second term. However, in the concatenated match action rules list, the first or third rule would fire when `a = 1; b = 2`, thus evaluating to false.

Negative rules (rules that evaluate to false) from term 1 are shadowing the positive rules (rules that evaluate to true) from term 2. We can work around this by 'poking holes' in the rules to remove the shadows. Concretely, we perform the following two transformations to the concatention of per-term rule lists to make it equivalent to the complete expression: 

  1. remove the final negation rule from all but the last term's rule list (i.e., remove rule (3) in the above example). The final negation rule represents the case where no term matches, so there should only be one such rule, at the very end of the combined rule list. 
  2. For each term's rule list with positive rule R, update the rule lists from all preceeding terms so that none of the negative rules shadow R. For a negative rule N->false that comes before positive rule R->true, what we want to do is to make sure that the intersection of the two rules evaluates to true. So we generate the rule (N^R)->true and put it immediately before N->false. 

After the following two transformations, the concatenated rule list is equivalent to the original expression. The example's rule list becomes: 

``
    1) a:1; b;2; -> true; * added to prevent (2) from shadowing (5) *
    2) a:1; b:2; -> false; 
    3) a:1; b:_; -> true;
    4) a:2; b:2; -> false;
    5) a:_; b;2; -> true;
    6) a:_; b_:_; -> false;
``

As a final optimization, we remove rules that cannot be matched in the list, for example, rule (2) in the above list. 