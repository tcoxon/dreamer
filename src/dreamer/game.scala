package dreamer.game
import scalaz._, Scalaz._
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
      heres <- searchWhat(Question(x,AtLocation,What))
      here <- (heres match {
          case Nil => createPlace
          case h :: Nil => state(h)
          case h1 :: h2 :: Nil =>
            assert(false, x.toString+" at multiple locations")
            state(h1)
        }): State[Context,Concept]
    } yield here
  }

  def getLocation: State[Context,Concept] =
    locationOf(Self)

  def getUp(place: Concept): State[Context,Concept] =
    locationOf(place)

  def lookAround: GameAction =
    for {
      here <- getLocation
      things <- reifyingSearch(Question(What, AtLocation, here))
    } yield Edge(Self,AtLocation,here) ::
        things.filter(_ != Self).map(Edge(_,AtLocation,here))


  def lookAt(x: Concept): GameAction =
    for {
      whats <- reifyingSearch(Question(x,IsA,What))
      wheres <- reifyingSearch(Question(x,AtLocation,What))
    } yield 
      whats.map(w => Edge(x,IsA,w)) ++
      wheres.map(w => Edge(x,AtLocation,w))


  def leave: GameAction =
    for {
      here <- getLocation
      up <- getUp(here)
      _ <- forget(Edge(Self,AtLocation,here))
      _ <- tell(Edge(Self,AtLocation,up))
      r <- lookAround
    } yield Edge(Self,PastAction("left"),here) :: r

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
      here <- getLocation
      _ <- forget(Edge(Self,AtLocation,here))
      _ <- tell(Edge(Self,AtLocation,target))
      r <- lookAround
    } yield Edge(Self,PastAction("went into"),target) :: r

  implicit def actionToAmbiguousMeaning(action: GameAction): AmbiguousMeaning =
    for {
      edges <- fork(action)
    } yield Tell(edges)

  implicit def actionResultToResponse(result: ActionResult): Response =
    Tell(result)
}
