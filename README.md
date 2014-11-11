# Dreamer of Electric Sheep

TODO brief description of the game

## Running (download)

If you have the JAR download, ensure you have Java 1.5 installed and then simply double-click the JAR file.

You can also run it without the UI:

```bash
java -cp dreamer.jar CLI
```

## Building (source)

Ensure you have sbt installed: www.scala-sbt.org

```bash
sbt one-jar
```

You can also run it from source:

```bash
sbt run
```

## License

Dreamer of Electric Sheep is copyright 2014 Tom Coxon and licensed to you under
the terms of the MIT license.
http://opensource.org/licenses/MIT

ConceptNet5 data is under the CC-BY-SA license.
https://github.com/commonsense/conceptnet5/wiki/Copying-and-sharing-ConceptNet

The font in the applet game is "Beeb" by Andy Armstrong, under CC-BY-SA.
http://fontstruct.com/fontstructions/show/beeb

Spritely, which generates the sprites used in the game is public domain.
https://github.com/gamesbyangelina/spritely

The sprites automatically searched for and displayed in the game are from
openclipart.org, which are all released into the public domain:
https://openclipart.org/share

The blue-face protagonist sprites are also copyright Tom Coxon, but you can use
them under the CC-BY-SA license if you like.
https://creativecommons.org/licenses/by-sa/3.0/

## TODO

* Info front page.
* Speed up spritely as much as possible: reduce search, reduce processing
* Spritely loading/failed indication
* Erase/delete/kill commands
* Hint at actions the dreamer can perform while playing.
* Referent qualifiers ("the other", "that", "this", "the first", "your", "the \* from/in \*", etc.)
* More GameActions: open/close, questions (where, what).
* Factoring Tells, e.g. "In the house, there is a cat and a dog." Might require Collection concepts.
* NPCs
* Asking clarification: what, which
* Enhancement? Add a "weird" adjective to objects spawned via dreamWeirdDefaults

## Problems with ConceptNet

* It can be offensive. Unfiltered, it will put gay people in closets and prostitutes in bedrooms.
* Relations are sometimes reversed, e.g. restaurant AtLocation rice
* No quantifiers - forall, thereexists.
* Dual-meaning nodes: java IsA island, but java IsA program_language.
* Lack of easily queriable transitivity.
* HasA relationship isn't always between nouns, e.g. "The old person has a so much to teach us."
* Related parts of the graph seem barely connected?

## Good things about ConceptNet

* The license! Hard to imagine CC-BY-SA being unacceptable to anyone reasonable.

