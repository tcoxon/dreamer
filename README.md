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

* "go door" to go through the door, into another location the door occupies.
* Referent qualifiers ("the other", "that", "this", "the first", "your", "the \* from/in \*", etc.)
* Filter conceptnet better: recursive locations, banned concepts, qualified concepts (e.g. your grandmother).
* Spawn random things when nothing AtLocation here.
* More GameActions: reification, open/close, questions (where, what).
* IsA clarifications: "The dog is a dog, an animal."
* Doors/portals - objects at two locations? PartOf to add doors?
* Factoring Tells, e.g. "In the house, there is a cat and a dog." Might require Collection concepts.
* Refine 'it'. Need to avoid "I am in it." ??
* NPCs
* Reality model
* Asking clarification: what, which
* Bug: realized HasA's don't have an AtLocation!

## Problems with ConceptNet

* It can be offensive. Unfiltered, it will put gay people in closets and prostitutes in bedrooms.
* Relations are sometimes reversed, e.g. restaurant AtLocation rice
* No quantifiers - forall, thereexists.
* Dual-meaning nodes: java IsA island, but java IsA program_language.
* Lack of easily queriable transitivity.
