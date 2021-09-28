# The P4-Lucid interface generator

This is about designing the configuration file that links headers from a P4 program to entry events in a Lucid program. 

**Design choice** The harness file describes the conditions for every event's execution. This compiles into a table at the start of the match-action pipeline. 

The params definition seems more important than the event definition.

I wonder if a simpler way might be: 

1. have a main event in the program. 
2. main event decides what gets called. ..
- nah. 