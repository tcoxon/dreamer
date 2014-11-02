package dreamer.lang.en
import scalaz._, Scalaz._
import util.Util._
import util.forked._, ForkedState._
import dreamer.context._, Context._
import dreamer.concept._, Concept._, Relation._
import dreamer.lang._, Language._
import dreamer.game._, Game._


class English extends Language {

  override def dictionary: State[Context,Dictionary] = state(Map(

    "(take a )?l(ook( around.*)?)?" -> {case _ => lookAround},

    "(take a )?l(ook( at)?)? (.+)" -> {case List(_, _, _, x) =>
      for {
        xref <- referent(x)
        r <- lookAt(xref)
      } yield r
    },
    
    "(exit|leave)( here| this place)?" -> {case _ => leave},

    "(exit|leave) (.+)" -> {case List(_, x) =>
      for {
        xref <- referent(x)
        r <- fork(leave(xref))
      } yield r
    },

    "(enter|go( into)?) (.+)" -> {case List(_, _, x) =>
      for {
        xref <- referent(x)
        r <- fork(enter(xref))
      } yield r
    },

    "(take|grab) (.+)" -> {case List(_, x) =>
      for {
        xref <- referent(x)
        r <- fork(take(xref))
      } yield r
    },

    "(drop|put) (.+)" -> {case List(_, x) =>
      for {
        xref <- referent(x)
        here <- fork(getLocation)
        r <- fork(drop(xref, here))
      } yield r
    },

    "(drop|put|leave) (.+) in(side)? (.+)" -> {case List(_,x,_,y) =>
      for {
        xref <- referent(x)
        loc <- (y match {
            case "here" => fork(getLocation)
            case "there" => getIt
            case _ => referent(y)
          }): ForkedState[Context,Concept]
        r <- fork(drop(xref, loc))
      } yield r
    },

    "give (.+) to (.+)" -> {case List(x,y) =>
      for {
        xref <- referent(x)
        yref <- referent(y)
        r <- fork(give(xref,yref))
      } yield r
    },

    "i(nvent(ory)?)?" -> {case _ =>
      for { 
        r <- invent
      } yield r
    }

    //// Sequencing:
    //"(.+) (and|then|and then) (.+)" -> {},

    //// Reification:
    //"there (is|are) (.+)( here)?" -> {},
    //"there (is|are) (.+) in(side)? (.+)" -> {},
    //"(.+) (is|are) (.+)" -> {},
    //"(.+) (is|are) in(side)? (.+)" -> {},
    //"(.+) (is|are) (here|there)" -> {},
    //"(.+) (has|have) (.+)" -> {},

    //// Open/close:
    //"open (.+)" -> {},
    //"close (.+)" -> {},
    //
    //// Questions:
    //"what (is|are) (.+)" -> {}, // referent could be abstract or realized...
    //"where (is|are) (.+)" -> {},
    //"what do(es)? (.+) have" -> {},

  ))


  def referent(text: String): ForkedState[Context,Concept] = text match {
    case "you" => forked(Self)
    case "yourself" => forked(Self)
    case "it" => for (x <- getIt) yield x
    case _ =>
      val qualStripped = "(a|an|the) ".r.replaceAllIn(text, "")
      for {
        ctx:Context <- fget
        val namedCOpt = ctx.mind.named(qualStripped)
        val namedC = namedCOpt match {
          case Some(x) => x
          case _ => Abstract(qualStripped)
        }
        val _ = debug("namedC: "+namedC.toString)
        val ref = ctx.refList.find(r => r.real == namedC || r.arche == namedC)
        _ <- continueIf(!ref.isEmpty)
      } yield ref.get.real
  }

  def getArchetypeName(concept: Concept): State[Context,String] =
    concept match {
      case Nothingness => state("nothing")
      case _ =>
        for (ctx <- get)
          yield {
            ctx.mind.nameOf(concept) match {
              case Some(name) => name
              case None =>
                concept match {
                  case Abstract(uri) => uri
                  case _ =>
                    assert(false)
                    concept.toString
                  }
              }
          }
    }

  def describeArchetype(concept: Concept): State[Context,String] =
    concept match {
      case Nothingness => getArchetypeName(concept)
      case _ => getArchetypeName(concept).map(singular)
    }

  def describeUnqual(concept: Concept, pos: NounPos): State[Context,String] =
    concept match {
      case Self => state(pos match {
        case SubjectPos => "I"
        case ObjectPos => "me"
      })
      case Abstract(uri) =>
        assert(false)
        state(uri)
      case r@Realized(_) =>
        for {
          arche <- archetype(r)
          desc <- getArchetypeName(arche)
        } yield desc
    }

  def singular(noun: String): String =
    if (noun.length > 0 && "aeiou".contains(noun.charAt(0))) "an "+noun
    else "a "+noun

  def describe(concept: Concept, pos: NounPos): State[Context,String] =
    concept match {
      case Self => describeUnqual(concept, pos)
      case Abstract(_) => describeArchetype(concept)
      case Realized(_) =>
        for {
          desc <- describeUnqual(concept, pos)
          ctx <- get
        } yield
          if (isIt(ctx, concept)) "it" else
            (if (isReffed(ctx, concept)) "the "+desc else singular(desc))
    }

  private def describeV(subj: Concept, rel: Relation): String =
    " " + (rel match {
      case IsA => subj match {
        case Self => "am"
        case _ => "is"
      }
      case AtLocation => subj match {
        case Self => "am in"
        case _ => "is in"
      }
      case HasA => subj match {
        case Self => "have"
        case _ => "has"
      }
      case Verb(verb) => verb
    }) + " "

  private def describeSVO(edge: Edge): State[Context,String] =
    for {
      sdesc <- describe(edge.start, SubjectPos)
      odesc <- describe(edge.end, ObjectPos)
      _ <- ref(edge.start)
      _ <- ref(edge.end)
      _ <- setIt(if (edge.start == Self) edge.end else edge.start)
    } yield sdesc + describeV(edge.start, edge.rel) + odesc

  def describe(edge: Edge): State[Context,String] = edge match {
    case Edge(_, rel, _) =>
      rel match {
        case _ => describeSVO(edge)
      }
  }

  private def sentence(text: String): String =
    text.capitalize + "."

  def describe(edges: List[Edge]): State[Context,List[String]] = for {
    ctx <- get
    r <- (normalizeTell(ctx,edges) match {
        case Nil => state(Nil)
        case e::edges => 
          for (edesc <- describe(e); esdesc <- describe(edges))
            yield sentence(edesc) :: esdesc
      }): State[Context,List[String]]
  } yield r

  def describe(response: Response): State[Context,String] = response match {
    case Ack => state("OK.")
    case Tell(es) => for (descs <- describe(es)) yield descs.mkString(" ")
    case Clarify() => state("Can you clarify that?")
    case ParseFailure() => state("I don't understand.")
  }

}

