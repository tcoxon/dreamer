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

### TODO

1. Add PastAction(val verb: String) relation so that 'leave house' can give
"I left the house," before describing the street.
2. More GameActions.
3. Factoring Tells, e.g. "A cat and a dog are the house." Might require
Collection concepts.
4. Refine 'it'. Need to avoid "I am in it."
