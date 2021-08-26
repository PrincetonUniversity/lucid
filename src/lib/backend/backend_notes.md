## Conventions in the backend IR
This note describes assorted details about 
the backend and low level IR. 

### Intermediate representations

Here is the path and main transformations that a lucid program 
takes through the backend pipeline. 

1. Lucid source (Syntax.ml) -- The source representation. 

2. Op. statement IR (OGSyntax.ml) -- Represents just the handlers in a program, as control flow graphs rather than syntax trees of statements. Derived from Lucid source. 

3. Low level IR (LLSyntax.ml) -- Represents the entire Lucid source program in a lower-level language that is similar to a P4 program, but with more syntactic restrictions to reflect the instruction set of the target. Derived from Lucid source + Op statement IR. (LLSyntax.ml)

4. DataFlow IR (DFSyntax.ml) -- Similar to LIR, but with edges representing data flow rather than control. Derived from Lucid IR. 

5. Pipeline IR (PipeSyntax.ml) -- The DataFlow LIR, laid out and optimized into a pipeline of stages. Derived from DataFlow LIR. 

6. Straightline IR (SLSyntax.ml) -- The pipeline IR as a straight-line sequence of tables. Prints to P4 with simple macros. 


### Comparing variables

Relational operations in a branch are ultimately compiled into tests that compare local variables against constants. This requires breaking comparisons of local variables against other local variables into two steps. This section describes how different types of relational operators are implemented. (in normalizeBools.ml).

#### (in)equality tests 
``if (x == y) {}`` --> 
``int tmp = x - y; if(tmp == 0){}``

``if (x != y) {}`` --> 
``int tmp = x - y; if(tmp != 0){}``

#### range tests
*Note: |-| is saturating subtraction, i.e., ``x |-| y == max(x - y, 0)``*

``if (x > y) {}`` --> 
``int tmp = x |-| y; if (tmp != 0){}``

``if (x < y) {}`` --> 
``int tmp = y |-| x; if (tmp != 0){}``

``if(x >= y) {}`` --> 
``tmp1 = x - y; tmp2 = x |-| y; if (tmp1 == 0 || tmp2 != 0) {} ``

``if(x <= y) {}`` --> 
``tmp1 = y - x; tmp2 = y |-| x; if (tmp1 == 0 || tmp2 != 0) {} ``

**Sloppy proofs**

    x > y
    === x - y > y - y
    === x - y > 0
    === x |-| y != 0
    so, x > y === tmp = x |-| y; tmp != 0

    x < y
    === x - y < y - y
    === x - y < 0 
    === y - x > 0
    === y |-| x != 0
    so, x < y === tmp = y |-| x; tmp != 0;

    x >= y
    === x - y >= y - y
    === x - y >= 0
    === (x - y == 0) || (x |-| y != 0)
    so, x >= y === tmp1 = x - y; tmp2 = x |-| y; (tmp1 == 0 || tmp2 != 0) 

    x <= y
    === (x - y == 0) || (y |-| x != 0)
    so, x <= y === tmp1 = x - y; tmp2 = y |-| x; (tmp1 == 0 || tmp2 != 0) 


### Structs
A **struct** is a group of local variables that we call *fields*, like a metadata struct or header in P4. Structs are defined globally in an IR program, as a list of fields. 


``
struct_def dptMeta_t {
    timestamp : 32;
    eventType : 16;    
}
``

A **struct instance** is declared globally. The fields in a struct are then accessed with the ``.`` operator. 

``
struct dptMeta_t md.dptMeta; 
var<32> x;
var<16> y; 
...
md.dptMeta.timestamp = x; 
y = md.dptMeta.eventType;
... 
``
The struct instance fields are compound identifiers. 

#### Conventions

- A struct identifier can be a compound id, as seen in the above example. 

- Struct fields are all basic (non-compound) ids. 

### Finding identifier declarations

Sometimes we need to find the declaration of an identifier that may be a field in a struct instance, like ``md.dptMeta.eventType`` in the above example. 

The general algorithm to find the declaration of an identifier is: 

If the identifier is not compound, there should be a global declaration of the identifier. 

If the identifier references the field of a struct instance, it will be compound. Given the form ``x_1.x_2...x_(n-1).x_n``, then ``x_1.x_2...x_(n-1)`` is the identifier of the struct instance and ``x_n`` is the identifier of the field in the struct. 


### Events, handlers, and their parameters
Events translate to structs in the IR, while handlers translate to a directed acyclic graph of compute objects (tables, actions, alus, etc.).

A careful part of the translation from source to IR is synchronizing the parameters of each event and its corresponding handler. An event and handler always have the same id when translation to IR begins. However, the ids of their parameters may be different. 

Early in the translation to IR, information about the events is recorded in TofinoContex.ml as an event_rec. This includes the id and width of each event parameter and the ids of the corresponding handler parameters. 

When translating the body of a handler, this context is used to map each occurrence of a handler parameter id into a compound id that references the appropriate field of the event's struct. 

