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

    "(enter|go into|go inside) (.+)" -> {case List(_, x) =>
      for {
        xref <- referent(x)
        r <- fork(enter(xref))
      } yield r
    },

    "go through (.+)" -> {case List(x) =>
      for {
        xref <- referent(x)
        r <- fork(goThrough(xref, Verb("went through")))
      } yield r
    },

    "ride (.+)" -> {case List(x) =>
      for {
        xref <- referent(x)
        r <- fork(goThrough(xref, Verb("rode")))
      } yield r
    },

    "(go )?([nesw]|north|east|south|west)" -> {case List(_, dir0) =>
      val dir = dir0 match {
        case "n" => "north"; case "e" => "east"; case "s" => "south";
        case "w" => "west"; case _ => dir0 }
      val opposite = dir match {
        case "north" => "south"
        case "east" => "west"
        case "south" => "north"
        case "west" => "east"
      }
      for (r <- fork(goDirection(dir, opposite))) yield r
    },
    
    "go" -> {case _ => forked(Clarify(
          "go into (enter) <thing>" ::
          "go through (ride) <thing>" ::
          "go n|s|e|w" :: Nil))},

    "go (.+)" -> {case List(x) =>
      // Ambiguous use of go - go nesw / go through / go into?
      for {
        xref <- referent(x)
        desc <- fork(describe(xref, ObjectPos))
      } yield Clarify(
          "go into (enter) "+desc ::
          "go through (ride) "+desc ::
          "go n|s|e|w" :: Nil)
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
        _ <- fork(setIt(xref))
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
        _ <- fork(setIt(xref))
        yref <- referent(y)
        r <- fork(give(xref,yref))
      } yield r
    },

    "i(nvent(ory)?)?" -> {case _ =>
      for { 
        r <- invent
      } yield r
    },

    // Sequencing:
    "(.+?)(\\.|,| and| then) (.+)" -> {case List(x,_,y) =>
      for {
        xr <- parse(x)

        // get the 'it' that would be referred to by describing xr
        ctx:Context <- fget
        val it = describe(xr)(ctx)._1.it
        _ <- if (it.isEmpty) forked[Context,Unit](())
              else fork(setIt(it.get.real))

        yr <- parse(y)
      } yield MultiResponse(List(xr,yr))
    },
    "(then|and) (.+)" -> {case List(_,x) => parse(x)},

    // Reification:
    "there (is|are) (.+?)( here)?" -> {case List(_,x,_) =>
      for {
        xr <- abstractReferent(x)
        here <- fork(getLocation)
        r <- fork(reifyThereIs(xr, here))
      } yield r
    },
    "there (is|are) (.+) in(side)? (.+)" -> {case List(_,x,_,y) =>
      for {
        xr <- abstractReferent(x)
        loc <- referent(y)
        r <- fork(reifyThereIs(xr, loc))
      } yield r
    },
    "in(side)? (.+) (is|are) (.+)" -> {case List(_,locStr,_,concept) =>
      for {
        x <- abstractReferent(concept)
        loc <- referent(locStr)
        r <- fork(reifyThereIs(x, loc))
      } yield r
    }
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
    //
    //// Miscellaneous
    //"h(elp)?( (.*))?" -> {}

  ))


  def abstractReferent(text: String): ForkedState[Context,Concept] = for {
    stripped <-
        (if (text.startsWith("a ")) forked(text.substring(2))
        else if (text.startsWith("an ")) forked(text.substring(3))
        else if (text.endsWith("s"))
           fork(List(text.substring(0,text.length-1), text))
        else forked(text)): ForkedState[Context,String]
    ctx:Context <- fget
    val maybeArche = ctx.mind.named(stripped)
    _ <- continueIf(!maybeArche.isEmpty)
  } yield maybeArche.get

  def referent(text: String): ForkedState[Context,Concept] = text match {
    case "you" => forked(Self)
    case "yourself" => forked(Self)
    case "it" => for (x <- getIt) yield x
    case "itself" => for (x <- getIt) yield x
    case _ =>
      val qualStripped = "^(a|an|the) ".r.replaceAllIn(text, "")
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
      case NextTo(dir) => subj match {
        case Self => "am in the "+dir+" of"
        case _ => "is in the "+dir+" of"
      }
    }) + " "

  private def describeSVO(edge: Edge): State[Context,String] = edge match {
    case Edge(Self,rel,Self) => state("I"+describeV(Self,rel)+"myself")
    case _ =>
      for {
        sdesc <- describe(edge.start, SubjectPos)
        _ <- ref(edge.start)
        _ <- setIt(edge.start)
        odesc <- if (edge.start == edge.end) state[Context,String]("itself")
                  else describe(edge.end, ObjectPos)
        _ <- ref(edge.end)
        _ <- setIt(if (edge.start == Self) edge.end else edge.start)
      } yield sdesc + describeV(edge.start, edge.rel) + odesc
  }

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

  def describeList(conj: String, elems: List[String]): String = elems match {
    case x1 :: x2 :: Nil => x1 + " or " + x2
    case x1 :: x2 :: xs => x1 + ", " + describeList(conj, x2 :: xs)
    case x :: Nil => x
    case Nil => ""
  }

  def describe(response: Response): State[Context,String] = response match {
    case Ack => state("OK.")
    case Tell(es) => for (descs <- describe(es)) yield descs.mkString(" ")
    case Clarify(options) => state("Can you clarify that?" + (options match {
      case Nil => ""
      case _ => " Did you mean "+describeList("or", options.map("\""+_+"\""))+"?"
    }))
    case ParseFailure() => state("I don't understand. Try rephrasing.")
    case MultiResponse(resps) =>
      val describers = resps.map(describe)
      def folder(ctxacc: (Context,String), descr: State[Context,String]) = {
        val (ctx,acc) = ctxacc
        val (ctx1, desc) = descr(ctx)
        (ctx1, acc+" "+desc)
      }
      for {
        ctx:Context <- get
        val (ctx1, desc) = describers.foldLeft((ctx,""))(folder)
        _ <- put(ctx1)
      } yield desc.trim()
  }

}

