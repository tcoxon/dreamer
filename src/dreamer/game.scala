package dreamer.game
import scalaz._, Scalaz._
import util.Util._
import util.forked._, ForkedState._
import dreamer.context._, Context._
import dreamer.concept._, Concept._, Relation._
import dreamer.lang._, Language._


object Game {
  type GameAction = State[Context,Response]


  def createPlace: State[Context,Concept] =
    for {
      options <- searchWhat(Question(What,IsA,Place))
      val _ = assert(options.size > 0)
      ctx <- get
      real <- reify(options(ctx.r.nextInt(options.size)))
    } yield real

  def locationOf(x: Concept): State[Context,Concept] = {
    for {
      heres <- reifyingSearch(Question(x,AtLocation,What))
      ctx <- get
      here <- (heres match {
          case Nil =>
            if (isAwake(ctx)) state(Unknown)
            else for {
              place <- createPlace
              val _ = debug("Forcing a location for locationOf: "+place)
              _ <- tell(Edge(x,AtLocation,place))
            } yield place
          case h :: Nil => state(h)
          case h1 :: hs =>
            assert(false, x.toString+" at multiple locations: "+heres.toString)
            state(h1)
        }): State[Context,Concept]
    } yield here
  }

  def ownerOf(x: Concept): State[Context,Option[Concept]] = for {
    loc <- locationOf(x)
    isOwner <- ask(Question(loc,HasA,x)).map(_.size != 0)
  } yield if (isOwner) Some(loc) else None

  def getLocation: State[Context,Concept] =
    locationOf(Self)

  def setLocation(place: Concept): State[Context,Unit] = for {
    here <- getLocation
    _ <- forget(Edge(Self,AtLocation,here))
    _ <- tell(Edge(Self,AtLocation,place))
  } yield ()

  def getIsA(c: Concept): State[Context,Option[Concept]] = for {
    arches <- searchWhat(Question(c,IsA,What))
  } yield arches match {
    case x :: xs => Some(x)
    case Nil => None
  }

  def setIsA(c: Concept, archetype: Concept): State[Context,Unit] = for {
    arches <- searchWhat(Question(c,IsA,What))
    _ <- modify { ctx:Context =>
          ctx.copy(mind=arches.foldLeft(ctx.mind){ (mind,arche) =>
            mind - Edge(c,IsA,arche)
          })
        }
    _ <- tell(Edge(c,IsA,archetype))
  } yield ()

  def dropEverything: State[Context,Unit] = for {
    items <- searchWhat(Question(What,AtLocation,Self))
    _ <- modify { ctx: Context =>
          ctx.copy(mind=items.foldLeft(ctx.mind){ (mind,item) =>
            mind - Edge(Self,HasA,item) - Edge(item,AtLocation,Self)
          })
        }
  } yield ()

  def getStates(c: Concept): State[Context,List[Concept]] = for {
    ans <- searchWhat(Question(c,HasState,What))
  } yield ans
  
  def setState(c: Concept, st: Concept): State[Context,Unit] = for {
    states <- getStates(c)
    _ <- modify { ctx: Context =>
        states.foldLeft(ctx){ (ctx, sta) =>
          ctx.copy(mind= ctx.mind - Edge(c, HasState, sta))
        }
      }
    _ <- tell(Edge(c, HasState, st))
  } yield ()

  implicit def edgeToResponse(edge: Edge): Response =
    Tell(edge :: Nil)

  implicit def edgesToResponse(edges: List[Edge]): Response =
    Tell(edges)

  implicit def actionToAmbiguousMeaning(action: GameAction): AmbiguousMeaning =
    fork(action)

  // Here we normalize out things like HasA and AtLocation being closely
  // related. If you have a cat, the cat's location is you. So when describing
  // the cat, we say "You have the cat." rather than "The cat is in you."
  def normalizeTell(ctx: Context, edges: List[Edge]): List[Edge] = {
    def preferHas(edge: Edge) = edge match {
      case Edge(x,AtLocation,y) =>
        if (ctx.mind.ask(Question(y,HasA,x)).size != 0) Edge(y,HasA,x)
        else edge
      case _ => edge
    }
    edges.map(preferHas).distinct
  }

  def dreamOnly(action: GameAction): GameAction = for {
    ctx: Context <- get
    val awake = isAwake(ctx)
    owner <- ownerOf(Self)
    ownerArche <- (if (owner.isEmpty) state(None)
                    else getIsA(owner.get)): State[Context,Option[Concept]]
    r <- (if (awake) state(CantDoThat)
          else if (ownerArche == Some(Human) || ownerArche == Some(Person))
            state(CantDoThat + Edge(owner.get,HasA,Self))
          else action): GameAction
  } yield r

  def registerLook(x: Concept): State[Context,Unit] = for {
    isa <- getIsA(x)
    val arche = isa match {
        case Some(a) => a
        case None => Thing
      }
    _ <- modify{ctx:Context =>
        ctx.copy(justLookedAt=Some(Context.Ref(x, arche)))
      }
  } yield ()


  def lookAround: GameAction =
    for {
      here <- getLocation
      things <- lookAroundNoSelf
    } yield Edge(Self,AtLocation,here) + things

  def lookAroundNoSelf: GameAction =
    // do the same as lookAround, but without referencing Self.
    for {
      here <- getLocation
      things0 <- reifyingSearch(Question(What, AtLocation, here))
      val things1 = things0.filter(_!=Self)
      val things2 =
          if (things1.size == 0) List(Nothingness)
          else things1
      states <- getStates(here)
      _ <- registerLook(here)
    } yield states.map(Edge(here,HasState,_)) +
        things2.map(Edge(_,AtLocation,here))

  def lookIn(place: Concept): GameAction = for {
    things <- reifyingSearch(Question(What, AtLocation, place))
    _ <- registerLook(place)
  } yield (if (things.size == 0) List(Nothingness) else things).map(
      Edge(_, AtLocation, place))


  def lookAt(x: Concept): GameAction =
    for {
      whats <- reifyingSearch(Question(x,IsA,What))
      wheres <- reifyingSearch(Question(x,AtLocation,What))
      hases <- reifyingSearch(Question(x,HasA,What))
      _ <- registerLook(x)
    } yield 
      whats.map(w => Edge(x,IsA,w)) ++
      wheres.map(w => Edge(x,AtLocation,w)) ++
      hases.map(w => Edge(x,HasA,w))


  def leave: GameAction = dreamOnly {
    for {
      here <- getLocation
      up <- locationOf(here)
      _ <- setLocation(up)
      r <- lookAround
    } yield Edge(Self,Verb("left"),here) + r
  }

  def leave(location: Concept): GameAction = dreamOnly {
    for {
      here <- getLocation
      r <- (if (here == location) {
          leave
        } else {
          state(Edge(Self,AtLocation,here)::Nil)
        }): GameAction
    } yield r
  }

  def enter(target: Concept): GameAction = dreamOnly {
    for {
      _ <- setLocation(target)
      r <- lookAroundNoSelf
    } yield Edge(Self,Verb("went into"),target) + r
  }

  def take(item: Concept): GameAction = dreamOnly {
    for {
      _ <- registerLook(item)
      loc <- locationOf(item)
      owner <- ownerOf(item)
      r <- (owner match {
        case Some(Self) =>
          state(Edge(Self,Verb("already have"),item) :: Nil)
        case None => for {
            _ <- forget(Edge(item,AtLocation,loc))
            _ <- tell(Edge(item,AtLocation,Self))
            _ <- tell(Edge(Self,HasA,item))
          } yield Edge(Self,Verb("took"),item) :: Nil
        case Some(owner) => for {
            _ <- forget(Edge(item,AtLocation,loc))
            _ <- forget(Edge(owner,HasA,item))
            _ <- tell(Edge(item,AtLocation,Self))
            _ <- tell(Edge(Self,HasA,item))
          } yield Edge(Self,Verb("took"),item) :: Nil
      }): GameAction
    } yield r
  }

  def drop(item: Concept, location: Concept): GameAction = dreamOnly { for {
    _ <- registerLook(item)
    owner <- ownerOf(item)
    r <- (owner match {
        case Some(Self) => for {
            _ <- forget(Edge(item,AtLocation,Self))
            _ <- forget(Edge(Self,HasA,item))
            _ <- tell(Edge(item,AtLocation,location))
          } yield Edge(Self,Verb("dropped"),item) ::
            Edge(item,AtLocation,location) :: Nil
        case _ => state(Edge(Self,Verb("don't have"),item) :: Nil)
      }): GameAction
  } yield r }

  def give(item: Concept, to: Concept): GameAction = dreamOnly { for {
    _ <- registerLook(to)
    owner <- ownerOf(item)
    r <- (owner match {
        case Some(Self) => for {
            _ <- forget(Edge(item,AtLocation,Self))
            _ <- forget(Edge(Self,HasA,item))
            _ <- tell(Edge(item,AtLocation,to))
            _ <- tell(Edge(to,HasA,item))
          } yield Edge(Self,Verb("gave"),item) ::
            Edge(to,HasA,item) :: Nil
        case _ => state(Edge(Self,Verb("don't have"),item) :: Nil)
      }): GameAction
  } yield r }

  def invent: GameAction = for {
    items <- searchWhat(Question(Self,HasA,What))
  } yield
    if (items.size == 0) {
      Edge(Self,HasA,Nothingness) :: Nil
    } else {
      items.map(x => Edge(Self,HasA,x))
    }

  def goDirection(dir: String, opposite: String): GameAction = dreamOnly { for {
    here <- getLocation
    inDirs <- reifyingSearch(Question(What,NextTo(dir),here))
    val _ = debug("inDirs = "+inDirs.toString)
    val _ = assert(inDirs.size == 1)
    val inDir = inDirs.head
    _ <- tell(Edge(here,NextTo(opposite),inDir))
    _ <- setLocation(inDirs.head)
    r <- lookAroundNoSelf
  } yield Edge(Self,Verb("went "+dir+" in"),inDir) + r }

  def goThrough(portal: Concept, verb: Verb): GameAction = dreamOnly {
    portal match {
      case Self =>
        for {
          here <- getLocation
          _ <- forget(Edge(Self,AtLocation,here))
          r <- lookAround
        } yield Edge(Self,verb, portal) + r
      case _ =>
        for {
          otherSides <- reifyingSearch(Question(What,NextTo("through"),portal))
          val _ = debug("other side: "+otherSides.toString)
          val _ = assert(otherSides.size == 1)
          val otherSide = otherSides.head
          _ <- tell(Edge(portal,NextTo("through"),otherSide))
          target <- locationOf(otherSide)
          _ <- setLocation(target)
          r <- lookAround
        } yield Edge(Self, verb, otherSide) + r
    }
  }

  def reifyThereIs(archetype: Concept, location: Concept): GameAction =
    dreamOnly { for {
      real <- reify(archetype)
      val _ = debug("reifyThereIs("+archetype.toString+", "+location.toString)
      _ <- tell(Edge(real,AtLocation,location))
      _ <- registerLook(real)
    } yield Edge(real,AtLocation,location) :: Nil }

  def build(archetype: Concept, location: Concept, verb: String): GameAction =
    dreamOnly { for {
      real <- reify(archetype)
      val _ = debug("build("+archetype.toString+", "+location.toString)
      _ <- tell(Edge(real,AtLocation,location))
      _ <- registerLook(real)
    } yield
      Edge(Self, Verb(verb), real) :: Edge(real,AtLocation,location) :: Nil }

  def move(real: Concept, toLocation: Concept, tellPrefix: List[Edge]=Nil)
      : GameAction = dreamOnly { for {
    currentLoc <- locationOf(real)
    val _ = debug("move("+real.toString+", "+tellPrefix.toString)
    _ <- forget(Edge(real,AtLocation,currentLoc))
    _ <- forget(Edge(currentLoc,HasA,real))
    _ <- tell(Edge(real,AtLocation,toLocation))
  } yield tellPrefix ++ List(Edge(real, AtLocation, toLocation)) }

  def reifyHasA(owner: Concept, archetype: Concept): GameAction = dreamOnly {
    for {
      real <- reify(archetype)
      _ <- tell(Edge(real,AtLocation,owner))
      _ <- tell(Edge(owner,HasA,real))
      _ <- registerLook(real)
    } yield Edge(owner,HasA,real) :: Nil
  }

  def moveOwnership(owner: Concept, item: Concept): GameAction = dreamOnly {
    for {
      currentOwner <- locationOf(item)
      _ <- forget(Edge(item,AtLocation,currentOwner))
      _ <- forget(Edge(currentOwner,HasA,item))
      _ <- tell(Edge(item,AtLocation,owner))
      _ <- tell(Edge(owner,HasA,item))
      _ <- registerLook(item)
    } yield Edge(owner,HasA,item) :: Nil
  }

  def wakeUp: GameAction = for {
    ctx: Context <- get
    r <- (if (!isAwake(ctx)) for {
        _ <- forget(Edge(Self,HasState,Sleeping))
        _ <- tell(Edge(Self,HasState,Awake))

        _ <- modify{ctx:Context => ctx.copy(refList=Nil, it=None)}

        here <- getLocation
        _ <- forget(Edge(Self,AtLocation,here))
        _ <- forget(Edge(here,HasA,Self))
        computer <- reify(Abstract("/c/en/computer"))
        java <- reify(Abstract("/c/en/java"))
        _ <- tell(Edge(java,AtLocation,computer))
        _ <- tell(Edge(computer,AtLocation,Unknown))
        _ <- setLocation(java)

        _ <- setIsA(Self,DreamerGame)

        _ <- dropEverything
        conceptnet <- reify(Abstract("/c/en/conceptnet"))
        _ <- tell(Edge(Self,HasA,conceptnet))
        _ <- tell(Edge(conceptnet,AtLocation,Self))

        r <- lookAround
      } yield Edge(Self,HasState,Awake) + Edge(Self,IsA,DreamerGame) + r
    else
      state(ParseFailure())): GameAction
  } yield r

  def whatState(x: Concept): GameAction = for {
    states0 <- reifyingSearch(Question(x,HasState,What))
  } yield {
    val states =
      if (states0.size == 0) List(Nothingness)
      else states0
    states.map(Edge(x,HasState,_))
  }

  def goToSleep: GameAction = for {
    _ <- forget(Edge(Self,HasState,Awake))
    _ <- tell(Edge(Self,HasState,Sleeping))

    here <- getLocation
    _ <- forget(Edge(Self,AtLocation,here))

    selfIsA <- getIsA(Self)
    _ <- (selfIsA match {
        case Some(kind) => forget(Edge(Self,IsA,kind))
        case None => state(())
      }): State[Context,Unit]
    _ <- tell(Edge(Self,IsA,Unknown))
    
    _ <- dropEverything
    r <- lookAround
  } yield Edge(Self,HasState,Sleeping) + Edge(Self,IsA,Unknown) + r

  def simulateWorld: GameAction = {

    def nextState(states: List[Concept]): (Concept,Option[(Relation,Concept)]) = {
      if (states contains Abstract("/c/en/scary")) {
        (Abstract("/c/en/looming"), Some((Verb("whispered"),Speech("wake up"))))
      } else if (states contains Abstract("/c/en/looming")) {
        (Abstract("/c/en/grabbing"), Some((Verb("shouted"),Speech("WAKE UP"))))
      } else if (states contains Abstract("/c/en/grabbing")) {
        (Abstract("/c/en/laughing_maniacally"), None)
      } else {
        (Abstract("/c/en/scary"), None)
      }
    }
    def updateHumans(humans: List[Concept]): GameAction = humans match {
      case human::tail =>
        for {
          states <- getStates(human)
          val (st,sp) = nextState(states)
          _ <- setState(human, st)
          r1 <- (if (st == Abstract("/c/en/laughing_maniacally"))
                  for {
                    r <- moveOwnership(human, Self)
                    _ <- tell(
                      Edge(human,HasState,Abstract("/c/en/yelling_wake_up")))
                  } yield r
                else state(MultiResponse(Nil))):GameAction
          recur <- updateHumans(tail)
          val result0 = r1 + Edge(human,HasState,st)
          val result = if (sp.isEmpty) result0
                        else result0 + Edge(human, sp.get._1, sp.get._2)
        } yield result + recur
      case _ => state(MultiResponse(Nil))
    }

    for {
      here <- locationOf(Self)
      owner <- ownerOf(Self)
      humans0 <- search(
        Question(What,AtLocation,here),
        Question(What,IsA,Human))
      humans1 <- search(
        Question(What,AtLocation,here),
        Question(What,IsA,Person))
      val humans = (humans0++humans1).map(_.get(()).get)
      r <- updateHumans(humans)
    } yield r
  }
}
