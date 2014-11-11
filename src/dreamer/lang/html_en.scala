package dreamer.lang.html_en
import scalaz._, Scalaz._
import util.Util._
import util.forked._, ForkedState._
import dreamer.context._, Context._
import dreamer.concept._, Concept._, Relation._
import dreamer.lang._, dreamer.lang.en._, Language._


abstract sealed class Sentence(val color: String) {
  def text: String
  override def toString: String = "<font color='"+color+"'>"+text+"</font>"
  def isUsage: Boolean = this match {
    case UsageSentence(_) => true
    case _ => false
  }
}
case class NormalSentence(val text: String) extends Sentence("#FFFFFF")
case class ErrorSentence(val text: String) extends Sentence("#FFFF00")
case class SelfStateSentence(val text: String) extends Sentence("#00FFFF")
case class ScarySentence(val text: String) extends Sentence("#FF0000")
case class UsageSentence(val text: String) extends Sentence("#FFFF00")


class HTMLEnglish extends English {

  def edgeToSentence(edge: Edge): State[Context,Sentence] = for {
    desc <- super.describe(edge)
    ctx <- get
  } yield edge match {
    case Edge(Self,IsA,_) | Edge(Self,HasState,_) => SelfStateSentence(desc)
    case Edge(subj@Realized(_),_,_) =>
      val arche = archetype(ctx, subj)
      if (arche == Human || arche == Person) ScarySentence(desc)
      else NormalSentence(desc)
    case _ => NormalSentence(desc)
  }

  def toSentences(resps: List[Response]): State[Context,List[Sentence]] = for {
    ctx:Context <- get
    val (ctx1,r) = resps.foldLeft((ctx,List[Sentence]())){ (ctx_acc, resp) =>
        val (ctx0,acc) = ctx_acc
        val (ctx1,sentences) = toSentences(resp)(ctx0)
        (ctx1, acc ++ sentences)
      }
    _ <- put(ctx1)
  } yield r

  def edgesToSentences(edges: List[Edge]): State[Context,List[Sentence]] =
    State{ctx =>
      edges.foldLeft((ctx, List[Sentence]())){ (ctx_acc, edge) =>
        val (ctx0,acc) = ctx_acc
        val (ctx1, sentence) = edgeToSentence(edge)(ctx0)
        (ctx1, acc :+ sentence)
      }
    }

  def toSentences(response: Response): State[Context,List[Sentence]] =
    response match {
      case Tell(edges) => edgesToSentences(edges)
      case Clarify(_) | ParseFailure() | CantDoThat =>
        super.describe(response).map(desc => ErrorSentence(desc) :: Nil)
      case MultiResponse(resps) => toSentences(resps)
      case UsageInfo(text) => state(List(UsageSentence(text)))
    }

  override def describe(response: Response): State[Context,String] = for {
    sentences <- toSentences(response)
  } yield flattenToHTML(sentences)

  def flattenToHTML(sentences: List[Sentence]): String = {
    var color: String = null
    var curr = ""
    for (sentence <- sentences) {
      if (sentence.color != color || sentence.isUsage) {
        if (color != null) curr += "</p>"
        color = sentence.color
        curr += "<p style='color: "+color+"'>"
      }

      var capitalized = sentence.text.capitalize
      if (capitalized.length > 0 &&
          !(".!?".contains(capitalized.charAt(capitalized.length-1))))
        capitalized += "."
      curr += capitalized + "&nbsp;&nbsp;"
    }
    if (color != null) curr += "</p>"
    curr
  }
  
}
