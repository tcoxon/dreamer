package dreamer.game
import scalaz._, Scalaz._
import util.Util._
import util.forked._, ForkedState._
import dreamer.context._, Context._
import dreamer.concept._, Concept._, Relation._
import dreamer.lang._, Language._


object Game {
  type ActionResult = List[Edge]
  type GameAction = State[Context,ActionResult]


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
      here <- (heres match {
          case Nil => for {
              place <- createPlace
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

  def getUp(place: Concept): State[Context,Concept] =
    locationOf(place)

  def setLocation(place: Concept): State[Context,Unit] = for {
    here <- getLocation
    _ <- forget(Edge(Self,AtLocation,here))
    _ <- tell(Edge(Self,AtLocation,place))
  } yield ()

  implicit def actionToAmbiguousMeaning(action: GameAction): AmbiguousMeaning =
    for {
      edges <- fork(action)
    } yield Tell(edges)

  implicit def actionResultToResponse(result: ActionResult): Response =
    Tell(result)

  // Here we normalize out things like HasA and AtLocation being closely
  // related. If you have a cat, the cat's location is you. So when describing
  // the cat, we say "You have the cat." rather than "The cat is in you."
  def normalizeTell(ctx: Context, edges: ActionResult): ActionResult = {
    def preferHas(edge: Edge) = edge match {
      case Edge(x,AtLocation,y) =>
        if (ctx.mind.ask(Question(y,HasA,x)).size != 0) Edge(y,HasA,x)
        else edge
      case _ => edge
    }
    edges.map(preferHas).distinct
  }


  def lookAround: GameAction =
    for {
      here <- getLocation
      things0 <- reifyingSearch(Question(What, AtLocation, here))
      val things1 = things0.filter(_!=Self)
      val things2 =
          if (things1.size == 0) List(Nothingness)
          else things1
    } yield Edge(Self,AtLocation,here) ::
        things2.map(Edge(_,AtLocation,here))

  def lookAroundNoSelf: GameAction =
    // do the same as lookAround, but without referencing Self.
    for {
      r <- lookAround
    } yield {
      r.filter(e => e match {
        case Edge(Self,AtLocation,_) => false
        case _ => true
      })
    }


  def lookAt(x: Concept): GameAction =
    for {
      whats <- reifyingSearch(Question(x,IsA,What))
      wheres <- reifyingSearch(Question(x,AtLocation,What))
      hases <- reifyingSearch(Question(x,HasA,What))
    } yield 
      whats.map(w => Edge(x,IsA,w)) ++
      wheres.map(w => Edge(x,AtLocation,w)) ++
      hases.map(w => Edge(x,HasA,w))


  def leave: GameAction =
    for {
      here <- getLocation
      up <- getUp(here)
      _ <- setLocation(up)
      r <- lookAround
    } yield Edge(Self,Verb("left"),here) :: r

  def leave(location: Concept): GameAction =
    for {
      here <- getLocation
      r <- (if (here == location) {
          leave
        } else {
          state(Edge(Self,AtLocation,here)::Nil)
        }): GameAction
    } yield r

  def enter(target: Concept): GameAction =
    for {
      _ <- setLocation(target)
      r <- lookAroundNoSelf
    } yield Edge(Self,Verb("went into"),target) :: r

  def take(item: Concept): GameAction =
    for {
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

  def drop(item: Concept, location: Concept): GameAction = for {
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
  } yield r

  def give(item: Concept, to: Concept): GameAction = for {
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
  } yield r

  def invent: GameAction = for {
    items <- searchWhat(Question(Self,HasA,What))
  } yield
    if (items.size == 0) {
      Edge(Self,HasA,Nothingness) :: Nil
    } else {
      items.map(x => Edge(Self,HasA,x))
    }

  def goDirection(dir: String, opposite: String): GameAction = for {
    here <- getLocation
    inDirs <- reifyingSearch(Question(What,NextTo(dir),here))
    val _ = debug("inDirs = "+inDirs.toString)
    val _ = assert(inDirs.size == 1)
    val inDir = inDirs.head
    _ <- tell(Edge(here,NextTo(opposite),inDir))
    _ <- setLocation(inDirs.head)
    r <- lookAroundNoSelf
  } yield Edge(Self,Verb("went "+dir+" in"),inDir) :: r

  def goThrough(portal: Concept, verb: Verb): GameAction = portal match {
    case Self =>
      for {
        here <- getLocation
        _ <- forget(Edge(Self,AtLocation,here))
        r <- lookAround
      } yield Edge(Self,verb, portal) :: r
    case _ =>
      for {
        otherSides <- reifyingSearch(Question(What,NextTo("through"),portal))
        val _ = debug("other side: "+otherSides.toString)
        val _ = assert(otherSides.size == 1)
        val otherSide = otherSides.head
        _ <- tell(Edge(portal,NextTo("through"),otherSide))
        target <- getUp(otherSide)
        _ <- setLocation(target)
        r <- lookAround
      } yield Edge(Self, verb, otherSide) :: r
  }

}
