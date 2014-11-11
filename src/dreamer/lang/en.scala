package dreamer.lang.en
import scalaz._, Scalaz._
import util.Util._
import util.forked._, ForkedState._
import dreamer.context._, Context._
import dreamer.concept._, Concept._, Relation._
import dreamer.lang._, Language._
import dreamer.game._, Game._


class English extends Language {

  override def dictionary: State[Context,Dictionary] = state(List(

    "(take a )?l(ook( around.*)?)?" -> {case _ => lookAround},

    "(take a )?l(ook( at)?)? (.+)" -> {case List(_, _, _, x) =>
      for {
        xref <- referent(x)
        r <- lookAt(xref)
      } yield r
    },

    "(take a)?look in(side|to)? (.+)" -> {case List(_,_,x) =>
      for {
        xref <- referent(x)
        r <- lookIn(xref)
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
    "(.+?)(\\.|,|!|\\?| and| then) (.+)" -> {case List(x,_,y) =>
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
    },
    "(.+) (is|are) in(side)? (.+)" -> {case List(concept,_,_,locStr) =>
      for {
        x <- abstractReferent(concept)
        loc <- referent(locStr)
        r <- fork(reifyThereIs(x, loc))
      } yield r
    },
    "(.+) (is|are) (here|there)" -> {case List(concept,_,locStr) =>
      for {
        x <- abstractReferent(concept)
        loc <- locationReferent(locStr)
        r <- fork(reifyThereIs(x, loc))
      } yield r
    },
    "(.+) (has|have) (.+)" -> {case List(x, _, a) =>
      for {
        xref <- referent(x)
        arche <- abstractReferent(a)
        r <- fork(reifyHasA(xref, arche))
      } yield r
    },
    "(create|build|make|reify|realize|rez) (.+)" -> {case List(verb,archeStr) =>
      val pastTense = verb match {
        case "create" => "created"
        case "build" => "built"
        case "make" => "made"
        case "reify" => "reified"
        case "realize" => "realized"
        case "rez" => "rezzed"
        case _ => verb
      }
      for {
        arche <- abstractReferent(archeStr)
        loc <- fork(getLocation)
        r <- fork(build(arche, loc, pastTense))
      } yield r
    },

    // Movement:
    "in(side)? (.+) (is|are) (.+)" -> {case List(_,locStr,_,concept) =>
      for {
        x <- referent(concept)
        loc <- referent(locStr)
        r <- fork(move(x, loc))
      } yield r
    },
    "(.+) (is|are) in(side)? (.+)" -> {case List(concept,_,_,locStr) =>
      for {
        x <- referent(concept)
        loc <- referent(locStr)
        r <- fork(move(x, loc))
      } yield r
    },
    "move (.+) (in|to|into) (.+)" -> {case List(x, _, y) =>
      for {
        xref <- referent(x)
        loc <- referent(y)
        r <- fork(move(xref, loc, List(Edge(Self,Verb("moved"),xref))))
      } yield r
    },
    "(.+) (is|are) (here|there)" -> {case List(x,_,l) =>
      for {
        xref <- referent(x)
        loc <- locationReferent(l)
        r <- fork(move(xref, loc))
      } yield r
    },
    "(.+) (has|have) (.+)" -> {case List(x,_,y) =>
      for {
        xref <- referent(x)
        yref <- referent(y)
        r <- fork(moveOwnership(xref, yref))
      } yield r
    },

    //// Open/close:
    //"open (.+)" -> {},
    //"close (.+)" -> {},
    //
    //// Questions:
    //"what (is|are) (.+)" -> {}, // referent could be abstract or realized...
    //"where (is|are) (.+)" -> {},
    //"what do(es)? (.+) have" -> {},
    
    // Miscellaneous
    "h(elp)?" -> {case _ =>
      forked(UsageInfo(
          "I can look around. I can look at things. "+
          "I can look inside things.") +
        UsageInfo("I can enter places. I can leave places.")+
        UsageInfo("I can go through doors. I can ride vehicles.")+
        UsageInfo("I can go N/S/E/W. I can go into places.")+
        UsageInfo("I can take things. I can drop things."+
          "I can give things. I have an inventory.")+
        UsageInfo("I can create things.")+
        UsageInfo("I can move things into places."))
    },


    // Dreamer game:
    "stop (sleeping|dreaming)|wake up" -> {case _ => wakeUp },
    "go (back )?to sleep|sleep|dream" -> {case _ => goToSleep },
    "what (is|are) (.+) doing" -> {case List(_,x) =>
      for {
        xref <- referent(x)
        r <- whatState(xref)
      } yield r
    }

  ))


  override def normalizeInput(text: String) = {
    val t1 = super.normalizeInput(text)
    if (t1.length == 0) t1
    else {
      val lastCh = t1.charAt(t1.length-1)
      if (".!?" contains lastCh)
        t1.substring(0,t1.length-1).trim()
      else t1
    }
  }

  def locationReferent(text: String): ForkedState[Context,Concept] =
    text match {
      case "here" => fork(getLocation)
      case "there" => getIt
      case _ => cancelFork
    }

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
        for {
          ctx <- get
        } yield ctx.mind.nameOf(concept) match {
          case Some(x) => x
          case None => uri
        }
      case r@Realized(_) =>
        for {
          arche <- archetype(r)
          desc <- getArchetypeName(arche)
        } yield desc
      case Speech(text) => state("\""+text+"\"")
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
      case Speech(text) => describeUnqual(concept, pos)
    }

  private def describeV(subj: Concept, rel: Relation): String =
    " " + (rel match {
      case IsA|HasState => subj match {
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

  private def describeSVO(edge: Edge, clarify: Boolean=false)
      : State[Context,String] = edge match {
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
        ctx:Context <- get
        supk <- archetype(edge.end)
        superDesc <- if (!clarify || supk == edge.end) state[Context,String]("")
                      else describe(supk, ObjectPos).map(", "+_)
      } yield sdesc + describeV(edge.start, edge.rel) + odesc + superDesc
  }

  def describeState(c: Concept): State[Context,String] = c match {
    case Sleeping => state("dreaming")
    case Awake => state("awake")
    case Open => state("open")
    case Closed => state("closed")
    case Nothingness => state("doing nothing")
    case _ => describeUnqual(c, ObjectPos)
  }

  def describeHasState(edge: Edge): State[Context,String] = for {
    sdesc <- describe(edge.start, SubjectPos)
    _ <- ref(edge.start)
    _ <- setIt(edge.start)
    odesc <- describeState(edge.end)
    _ <- ref(edge.end)
    _ <- setIt(if (edge.start == Self) edge.end else edge.start)
  } yield sdesc + describeV(edge.start, edge.rel) + odesc

  def describe(edge: Edge): State[Context,String] = edge match {
    case Edge(c, IsA, Unknown) if c != Nothingness =>
      for (desc <- describe(c, SubjectPos))
        yield "I don't know what "+desc+" "+describeV(c,IsA).trim()
    case Edge(c, AtLocation, Unknown) if c != Nothingness =>
      for (desc <- describe(c, SubjectPos))
        yield "I don't know where "+desc+" "+describeV(c,IsA).trim()

    case Edge(_, HasState, _) => describeHasState(edge)

    case Edge(real, IsA, arche) => describeSVO(edge, true)

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
    case CantDoThat => state("I can't do that.")
    case UsageInfo(x) => state(x)
  }

}

