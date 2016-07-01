import leon.collection._
import leon.webDSL.webDescription._
import leon.webDSL.webBuilding._
import implicits._

object Main {


val css = Style(
"table tr td:first-child":=(
  ^.css("width") := "100px",
  ^.verticalAlign := "top"
),
"":=(
  ^.background := "#AAA",
  ^.padding := "1em",
  ^.css("font-family") := "Arial Narrow"
),
"h2":=(
 ^.css("text-transform") := "uppercase",
 ^.css("font-size") := "1.5em"
),
"span.headprogram":=(
  ^.css("font-weight") := "bold"
)
)
  
    val tcheck = ^.tpe := "checkbox"
    def check(s: (String, String)): WebTree = <.tr(<.td(s._1), <.td(<.span(<.label(<.input(tcheck, ^.value := s._2), s._2), <.br())))
  
    val presentationChoices = List(
      ("9:15", "Fiber-optic Communication via the non-linear Fourier transform"),
      ("10:45", "Deciphering the Good-Turing Enigma"),
      ("11:45", "Information Storage in DNA"),
      ("12:45", "Poster session and lunch")
    )
    
    val title = "IC Research Day"
    
    def makeList(l: List[(String, String)]): String = l match {
      case Cons(a, b: Cons[(String, String)]) => a._2 + ", " + makeList(b)
      case Nil() => ""
      case Cons(a, Nil()) => a._2
    }

// The main webpage
def main() = {
  val welcome = "Welcome to " + title + "!"
  val introduction = <.p(
    <.span(
      "Program of " + title,
      ^.classes:="headprogram"),
    <.br(),
    "What will you attend? Please make your availability known for ",
    <.i(makeList(presentationChoices)))
  val welcomeTitle = <.h2(
    welcome, ^.color := "white")
  val finalButton = <.input(
    ^.tpe := "button",
    ^.value := "Vote for "+title+"!")
  val c = presentationChoices.map(presentation => check(presentation))

  val choices = <.table(c)
  
  WebPage(<.div(
    <.img(^.src := "http://ic.epfl.ch/files/content/sites/ic/files/Inka/EPFL_HOMEPAGE_652x372_V8.jpg", ^.width := "100%"),
    welcomeTitle,
    introduction,
    choices,
    finalButton), css)
  }
}
