# He Who Dreams Of Electric Sheep

(Code name: dreamer)


## Building

Ensure you have sbt installed: www.scala-sbt.org

```bash
sbt one-jar
```


## Running

```bash
sbt run
```

## TODO

* Referent qualifiers ("the other", "that", "this", "the first", "your", "the \* from/in \*", etc.)
* More GameActions: reification, open/close, questions (where, what).
* IsA clarifications: "The dog is a dog, an animal."
* Factoring Tells, e.g. "In the house, there is a cat and a dog." Might require Collection concepts.
* NPCs
* Reality model
* Asking clarification: what, which
* Enhancement? Add a "weird" adjective to objects spawned via dreamWeirdDefaults

## Problems with ConceptNet

* It can be offensive. Unfiltered, it will put gay people in closets and prostitutes in bedrooms.
* Relations are sometimes reversed, e.g. restaurant AtLocation rice
* No quantifiers - forall, thereexists.
* Dual-meaning nodes: java IsA island, but java IsA program_language.
* Lack of easily queriable transitivity.
