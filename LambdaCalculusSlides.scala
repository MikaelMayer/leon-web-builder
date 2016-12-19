import leon.lang._
import leon.annotation._
import leon.collection._
import leon.collection.ListOps._
import leon.lang.synthesis._
import leon.math._
import leon.webDSL.webBuilding._
import leon.webDSL.webDescription._
import implicits._

object Main {
  // Display parameters
  val notationLambda = ("\\lambda ","", ".\\;","")
  val notationArrow = ("", ",", "\\rightarrow ", "")
  val useLambdaNotation = true
  val notation = if(useLambdaNotation)
    notationLambda else notationArrow

  val height = 600
  val language = "en"
  val translations = Map(
    "en" -> Map(
      "speaker" ->  "US English Male",
      "cando" -> " can do: ",
      "slide0title" -> "Lambda Calculus and LISP",
      "slide0subt0" -> "Welcome to this introductory course to Lambda Calculus",
      "slide0subt1" -> "Knowing Lambda Calculus clarifies the essence of the notion of a functional language.",
      "slide0subt2" -> "We will explain LISP in a second part.",
      "slide0subt3" -> "Let's start with Lambda Calculus",
      "slide1title" -> "Lambda Calculus: First Functional Language",
      "slide1subt0" -> "Lambda calculus is the first functional language.",
      "slide1subt1" -> "It was introduced by Alonzo Church in 1932.",
      "slide1example" -> "Example",
      "slide1subt2" -> "If you already know scala, you can consider the following equivalent:",
      
    "scalaequivalentHeader" -> "Scala equivalent",
    "lambdacalculusHeader" -> "Lambda calculus",
    "slide1subt3" -> "This is a curried Scala function taking two arguments, applied to a and b.",
    "slide1subt4" -> "This is the equivalent in Lambda calculus.",
    "slide1introlambda" -> "Lambda calculus has only variables (x,y,a,b,…) and these two constructs:",
    "slide1subt5" -> "Lambda calculus is a very simple programming language with variables and these two constructs:",
    "application" -> "Application",
    "abstraction" -> "Abstraction",
    "slide1subt6" -> "The application, where you can think of f typically as a machine or a function",
    "slide1subt7" -> "and x as an input for this function",
    "slide1subt8" -> "so that f x is the result of giving x to f.",
    "slide1subt9" -> "The abstraction allows you to create your own machines or functions.",
    "slide1subt10" -> (if(useLambdaNotation) "Take a variable x, wrap it with a lambda on the left and a period on the right," else "Take a variable x, write an arrow to its right"),
    "slide1subt11" -> "and whatever you would like to do with x, insert it in the M expression."
    ),
    "fr" -> Map(
      "speaker" -> "French Female",
      "cando" -> " sait faire: ",
      "slide0title" -> "Lambda Calcul et LISP",
      "slide0subt0" -> "Bienvenue à ce cours d'introduction au Lambda Calcul.",
      "slide0subt1" -> "Le Lambda Calcul rend la notion de langage fonctionnel beaucoup plus clair.",
      "slide0subt2" -> "Nous verrons LISP dans une deuxième partie.",
      "slide0subt3" -> "Commençons par le Lambda Calcul.",
      "slide1title" -> "Lambda Calcul: Premier langage fonctionnel",
      "slide1subt0" -> "Le Lambda calcul est le premier langage dit fonctionnel.",
      "slide1subt1" -> "C'est Alonzo Church qui l'a introduit en 1932.",
      "slide1example" -> "Exemple",
      "slide1subt2" -> "Si vous connaissez déjà Scala, considérez l'équivalence suivante:",
      
      "scalaequivalentHeader" -> "Equivalent Scala",
    "lambdacalculusHeader" -> "Lambda calcul",
    "slide1subt3" -> "ça c'est une fonction scala currifiée à deux arguments et appliquée à a et b.",
    "slide1subt4" -> "ça c'est l'équivalent en Lambda Calcul.",
    "slide1introlambda" -> "Le Lambda Calcul possède uniquement des variables (x,y,a,b,…) et ces deux constructions:",
    "slide1subt5" -> "Le Lambda Calcul est un langage de programmation très simple comprenant des variables et ces deux constructions:",
    "application" -> "Application",
    "abstraction" -> "Abstraction",
    "slide1subt6" -> "L'application, où vous pouvez penser à f comme une machine ou une fonction",
    "slide1subt7" -> "et x comme ce que vous fournissez à la machine",
    "slide1subt8" -> "de telle sorte que f x est le résultat que fournit f quand on lui passe x.",
    "slide1subt9" -> "L'abstraction vous permet de créer vos propres machines ou fonctions.",
    "slide1subt10" -> (if(useLambdaNotation) "Prenez une variable x, écrivez un lambda sur sa gauche et un point à droite," else "Prenez une variable x, ajoutez-lui une flèche à droite,"),
    "slide1subt11" -> "et utilisez x dans l'expression M pour faire quelque chose avec."
    )
  )
  
  case class Translator(language: String) {
    def apply(key: String) = {
      translations.getOrElse(language, translations("en")).getOrElse(key, key)
    }
  }
  val t = Translator(language)
  
  val defaultVoice = t("speaker")
  
  
  // Applies p once on the list, where it is defined.
  def oneStepConversion[T](l: List[T], p: T => Option[T]): Option[List[T]] = l match {
    case Cons(h,t) =>
      p(h) match {
        case None() =>
          oneStepConversion(t, p) match {
            case None() => None()
            case Some(res) => Some(Cons(h, res))
          }
        case Some(r) => Some(Cons(r, t))
      }
    case Nil() => None()
  }
  
  val highlightclass = "highlightedElement"

  // The css of the webpage.
  val css = Style(
    ".centeredtitle" := (
      ^.position := "relative",
      ^.top := (height/2).toString+"px",
      ^.css("transform") := "translateY(-50%)"
    ),
    ".scalalambda tr:first-child" := (
      ^.fontWeight := "bold"
    ),
    ".scalalambda td,.scalalambda th":= (
      ^.textAlign := "center"
    ),
    "pre" := (
      ^.fontSize := "1em"
    ),
    "section:not(.verticalsplit)" := (
      ^.css("font-size") := "0.6em !important",
      ^.padding := "10px",
      ^.minHeight:= height + "px",
      ^.border := "1px solid black"
    ),
    "> div" := (
      ^.position := "absolute",
      ^.width := "100%",
      ^.top := "300px"
    ),
    "h2" := (
      ^.margin := "10px"
    ),
    ".flaglang" := (
      ^.width:= "50px",
      ^.css("float"):= "right",
      ^.border := "none"
    ),
    "." + highlightclass := (
      //^.outline := "1px solid red",
      ^.fontWeight := "bold",
      ^.color := "darkred",
      ^.background := "#EEE"
    ),
    ".playpausemenu" := (
      ^.position := "absolute",
      ^.bottom:= "0px",
      ^.right:="0px",
      ^.css("opacity"):="0.2",
      ^.css("z-index"):="100"
    ),
    ".playpausemenu i" := (
      ^.cursor := "pointer",
      ^.font := "normal normal normal 14px/1 FontAwesome !important",
      ^.fontSize:= "1em !important",
      ^.padding:="2px !important"
    ),
    ".playpausemenu.playing i.play" := (
      ^.display := "none"
    ),
    ".playpausemenu:not(.playing) i.pause" := (
      ^.display := "none"
    ),
    ".playpausemenu i:hover" := (
      ^.color := "blue",
      ^.css("opacity"):="1"
    ),
    ".subtitlewrapper" := (
      ^.position := "absolute",
      ^.display := "none",
      ^.bottom := "0px",
      ^.width := "100%"
    ),
    ".subtitlewrapper.top" := (
      ^.position := "absolute",
      ^.bottom := "default",
      ^.top := "5px",
      ^.width := "100%"
    ),
    ".subtitle" := (
      ^.position := "relative",
      ^.left := "-50%",
      ^.backgroundColor := "rgba(128, 128, 128, 0.2)",
      ^.color := "black",
      ^.padding := "5px"
    ),
    ".highlighton, .highlightoff" := (
      ^.display := "none"
    ),
    ".hiddenvoice" := (
      ^.display := "none"
    )
  )
  
  val lambdaPrefix = notation._1
  val lambdaVarInfix = notation._2
  val lambdaInfix = notation._3
  val lambdaSuffix= notation._4
  
  // Abstract class from which we derive L (abstraction), A (application), V (variable), N (notation)
  abstract class LambdaTerm {
    // Wraps the expression with parentheses if not a variable or a notation.
    def wrapParenthesesIfNeeded(p: LambdaTerm => String) = (l: LambdaTerm) => l match {
      case v: V => p(v)
      case n: N => p(n)
      case b => "(" + p(b) + ")"
    }
    
    // Renders long variable names as text.
    def longNamesAreText(n: String): String = if(n.length > 1) "\\text{" + n + "}" else n
    
    // Default conversion to string
    def mkString: String = this match {
      case l@L(p, b) => 
        l.toCustomString(lambdaPrefix, lambdaVarInfix, lambdaInfix, lambdaSuffix, _.mkString)
      case A(a: L, b) => "(" + a.mkString + ")" + "\\;" + List.mkString(b, "\\;", wrapParenthesesIfNeeded(_.mkString))
      case A(a, b) => a.mkString + "\\;" + List.mkString(b, "\\;", wrapParenthesesIfNeeded(_.mkString))
      case V(name) => longNamesAreText(name)
      case N(subexprs, interstrings, _) =>
        subexprs.zip(interstrings.tail).foldLeft(interstrings.head)((prevs, news) => prevs + news._1.mkString + news._2)
    }
    
    // Conversion to string with more parentheses.
    def mkStringUnambiguous: String = this match {
      case l@L(p, b: V) =>
        l.toCustomString(lambdaPrefix, lambdaVarInfix, lambdaInfix, lambdaSuffix, _.mkStringUnambiguous)
      case l@L(p, b) =>
        l.toCustomString(lambdaPrefix, lambdaVarInfix, lambdaInfix + "(", ")" + lambdaSuffix, _.mkStringUnambiguous)
      case A(a: V, b) => a.mkStringUnambiguous + "\\;" + List.mkString(b, "\\;", wrapParenthesesIfNeeded(_.mkStringUnambiguous))
      case A(a, b) => "(" + a.mkStringUnambiguous + ")" + "\\;" + List.mkString(b, "\\;", wrapParenthesesIfNeeded(_.mkStringUnambiguous))
      case V(name) => longNamesAreText(name)
      case N(subexprs, interstrings, _) =>
      subexprs.zip(interstrings.tail).foldLeft(interstrings.head)((prevs, news) => prevs + news._1.mkStringUnambiguous + news._2)
    }

    // Expands all notations and transform multi-variables lambdas into multiple lambdas.
    def expand: LambdaTerm = this match {
      case L(Nil(), b) => b.expand
      case L(Cons(x, xs), b) => L(List(x), L(xs, b).expand)
      case A(a, Nil()) => a.expand
      case A(a, Cons(b, bs)) => 
        bs.foldLeft(A(a.expand, List(b)))((ex, arg) =>
          A(ex, List(arg))
        )
      case V(name) => this
      case N(l, _, builder) => builder(l.map(_.expand))
    }
    
    // Expands only the notations.
    def expandNames: LambdaTerm = this match {
      case L(vars, b) => L(vars, b.expandNames)
      case A(a, b) => A(a.expandNames, b.map((el: LambdaTerm) => el.expandNames))
      case V(name) => this
      case N(l, _, builder) => builder(l.map(_.expandNames))
    }

    // Applies a beta-reduction to the lambda term.
    def beta: LambdaTerm = this match {
      case A(L(Nil(), body), Nil()) => body
      case A(L(Cons(x, xs), body), Cons(arg, args)) =>
        A(L(xs, body.substitute(x, arg)), args).beta
      case A(a: A, args) => 
        A(a.beta, args)
      case A(v: V, args) => 
      oneStepConversion[LambdaTerm](args, (l: LambdaTerm) => {
        val r = l.beta
        if(isError(r)) None()
        else Some(r)
      }) match {
        case None() => error(this)
        case Some(l) => A(v, l)
      }
      
      case A(N(l, _, builder), args) => A(builder(l.map(_.expandNames)), args)
      case N(l, _, builder) => this.expandNames
      case _ => error(this)
    }
    
    // Renders the lambda term into a scala-like program.
    def mkStringScala: String = this match {
      case L(Nil(), b) => b.mkStringScala
      case L(Cons(x, l), b) => "(" + x.mkStringScala + ": \\text{Any}) \\Rightarrow " + L(l, b).mkStringScala
      case A(a: L, b) => "(" + a.mkStringScala + ")" + "(" + List.mkString(b, ")(", (l: LambdaTerm) => l.mkStringScala) + ")"
      case A(a, b) => a.mkStringScala + "(" + List.mkString(b, ")(", (l: LambdaTerm) => l.mkStringScala) + ")"
      case V(name) => name
    }
    
    // Html wrapping routines for the above rendering functions
    def html: String = "\\(" + this.mkString + "\\)"
    def htmlUnambiguous: String = "\\(" + this.mkStringUnambiguous + "\\)"
    def htmlScala: String = "\\(" + this.mkStringScala + "\\)"
    
    // Substitute the variable x by t in the lambda term.
    def substitute(x: V, t: LambdaTerm): LambdaTerm =
      this match {
      case V(y) if y == x.name => t
      case V(y) => this
      case L(xs, body) => if(xs.exists(n => n == x)) this else L(xs, body.substitute(x, t))
      case A(l, args) => A(l.substitute(x, t), args.map((e: LambdaTerm) => e.substitute(x, t)))
      case N(l, interstrings, builder) =>
        N(l.map(_.substitute(x, t)), interstrings, builder)
    }
    
    // We can do something like a(b) where a and b are two lambda terms
    // to build an application.
    def apply(l: LambdaTerm) = A(this, List(l))
    def apply(l: List[LambdaTerm]) = A(this, l)
    // Special notation naming a lambda term without argument.
    def named(name: String) = N(Nil(), List("\\text{"+name+"}"), _ => this)
  }
  // Abstraction / Function
  case class L(params: List[V], body: LambdaTerm) extends LambdaTerm {
    def toCustomString(prefix: String,
                       variableInfix: String,
                       beforeBody: String,
                       afterBody: String,
                       recurse: LambdaTerm => String) = {
      prefix + List.mkString(params, variableInfix, (s: V) => recurse(s)) +
      beforeBody + recurse(body) + afterBody
    }
  }
  
  // Application
  case class A(lambda: LambdaTerm, arguments: List[LambdaTerm]) extends LambdaTerm

  // Variable
  case class V(name: String) extends LambdaTerm
  
  // Notation
  // Displays a list of subsymbols interleaved with strings.
  case class N(subsymbols: List[LambdaTerm], interstrings: List[String], builder: List[LambdaTerm] => LambdaTerm) extends LambdaTerm {
    require(interstrings.length == subsymbols.length + 1)
    def build(): LambdaTerm = builder(subsymbols)
  }

  // Very useful implicit conversion from a String to a variable name.
  implicit def toV(name: String): V = V(name)

  // Function to align webtrees using a table.
  def align(l: List[List[WebTree]]): Element =
    <.table(
      <.tbody(
        l.map(ll =>
          <.tr(ll.map(e => <.td(e)))
        )
      )
    )
  
  // The beta arrow
  val b_=> = " \\(\\Rightarrow_\\beta\\) "
  
  // Church booleans
  val ltrue = L(List("x", "y"), "x") named "true"
  val lfalse = L(List("x", "y"), "y") named "false"
    
  // Church pair
  def pair(a: LambdaTerm, b: LambdaTerm) =
    N(List(a, b), List("(", ",", ")"), l => L(List("f"), A("f", l)))
  
  // Church pair extraction 1
  def _1(p: LambdaTerm) =
    N(List(p), List("", "_1"), l => A(l.head, List(ltrue.expandNames)))

  // Church pair extraction 2
  def _2(p: LambdaTerm) =
    N(List(p), List("", "_2"), l => A(l.head, List(lfalse.expandNames)))
  
  // Church empty list
  def lNil =
    N(List(), List("\\text{Nil}"), l => L(List("m", "n"), "m"))
  
  // Church cons
  def lCons(p: LambdaTerm, q: LambdaTerm) =
    N(List(p, q), List("\\text{Cons}(", ",",")"), l => L(List("m", "n"), A("n", List(pair(l(0), l(1))))))
    
  // Church numbers
  def lNum(i: Int): LambdaTerm = {
    require(i >= 0)
    if(i == 0) {
      N(Nil(), List(i.toString), l => L(List("f", "x"), "x"))
    } else {
      N(Nil(), List(i.toString), l => L(List("f", "x"), A("f", List(A(lNum(i-1).expandNames, List("f", "x")).beta))))
    }
  }
  
  // Church addition
  def lPlus(p: LambdaTerm, q: LambdaTerm) = {
    N(List(p, q), List("", "+", ""), l => L(List("f", "x"), A(l(0), List("f", A(l(1), List("f", "x"))))))
  }
  
  // Church-like error (used in beta reduction when no reduction can be made)
  def error(l: LambdaTerm): LambdaTerm = N(List(l), List("Cannot convert (", ")"), p => p.head)

  // Checking if a church term is an error
  def isError(l: LambdaTerm) = l match {
    case N(_, Cons("Cannot convert (", _), _) => true
    case _ => false
  }

  // Displays in one row the beta expansion of the lambda term.
  def betacompute(l: LambdaTerm, max: Int = 10): String = {
    val l2 = l.beta
    if(max <= 0 || isError(l2)) {
      l.html
    } else {
      l.html + b_=> + betacompute(l2, max-1)
    }
  }
  
  // A term used in the slides.
  def mkZ: LambdaTerm =
    N(Nil(), List("mkZ"), l => 
      L(List("list"), A("list", List(lNil, L(List("p"), lCons("Z", A(mkZ, List(_2("p")))))))))

  // The infinite combinator
  val w = A(L(List("x"), A("x", List("x"))), List(L(List("x"), A("x", List("x")))))
  
  // The Y combinator using F
  val ycombinatorTotal = L(List("F"), A(L(List("x"), A("F", List(A("x", List("x"))))), List(L(List("x"), A("F", List(A("x", List("x"))))))))
  
  // The Y combinator on any function
  def ycombinator(F: LambdaTerm): N = 
    N(List(F), List("Y_{", "}"), l => A(ycombinatorTotal, List(l.head)).beta)
  
  // A lambda term used in the slide again.
  def mkZF: LambdaTerm = 
    L(List("self"),
      L(List("list"), A("list", List(lNil, L(List("p"), lCons("Z", A("self", List(_2("p")))))))))
      
  def subtitle(s: String) = 
    <.span(^.classes := "action hiddenvoice", s)
  
  def subtitle(s: List[String]): WebTree =
    s.map(subtitle)
  
  @ignore
  def subtitle(s: String*): WebTree =
    subtitle(varargToList(s))
    
  def say(s: String): WebTree = {
    subtitle(s)
  }
  
  def say(s: List[String]): WebTree = {
    s.map(say)
  }
  
  @ignore
  def say(s: String*): WebTree = {
    say(varargToList(s))
  }
    
  def sayOn(s: List[String], selector: String) = {
    List(highlighton(selector)) ++
    List(say(s)) ++
    List(highlightoff(selector))
  }

  def highlighton(selector: String) = {
    <.span(^.display := "none", ^.classes := "action highlighton", selector)
  }

  def highlightoff(selector: String) = {
    <.span(^.display := "none", ^.classes := "action highlightoff", selector)
  }
  
  def nextSlide() = {
    <.span(^.display := "none", ^.classes := "action nextslide")
  }
  
  def highlightonafter(selector: String, delayms: Int) = {
    <.span(^.display := "none", ^.classes := "action highlighton delay", selector, ^("delay") := delayms.toString)
  }
  
  def proxySay(s: List[String]) = say(s)
  
  case class Sayable(t: WebTree) {
    def say(s: List[String])(implicit state: IntGenerator): Element = {
      val kt = t.asInstanceOf[Element]
      val (newt, newId): (Element, String) = kt.properties.find((property: WebAttribute) => property.attributeName == "id") match {
        case None() =>
          val id = nextId
          (kt(^.id := id), id)
        case Some(attr) => (kt, attr.attributeName)
      }
      newt(
      highlighton("#" + newId),
      proxySay(s),
      highlightoff("#" + newId)
      )
    }
    
    def highlightOn(selector: String): WebTree = {
      t match {
        case e: Element =>
          e(highlighton(selector))
      }
    }
    
    def highlightOff(selector: String): WebTree = {
      t match {
        case e: Element =>
          e(highlightoff(selector))
      }
    }

    def say(s: String)(implicit state: IntGenerator): Element = {
      say(List(s))
    }
  }
  implicit def toSayable(t: WebTree) = Sayable(t)
  
  case class IntGenerator(var id: Int)
  
  /*case class ActionGenerator(var actions: List[WebTree]) {
    def pop(): List[WebTree] = {
      val res = actions.reverse
      actions = Nil()
      res
    }
  }*/
  /*
  def actionPop()(implicit state: ActionGenerator): WebTree = {
    val res = state.actions.reverse
    state.actions = Nil()
    res
  }*/
  
  def nextId(implicit state: IntGenerator) = {
    state.id += 1
    "ref" + state.id
  }
  
  def slide(l: List[WebTree]): Element = {
    <.section(
      <.div(^.classes := "playpausemenu", 
        <.i(^.classes:="stepbackward fa fa-step-backward"),
        <.i(^.classes:="play fa fa-play-circle"),
        <.i(^.classes:="pause fa fa-pause-circle"),
        <.i(^.classes:="stepforward fa fa-step-forward")),
      <.span(^.classes := "subtitlewrapper",
        <.div(^.classes := "subtitle"
        )
        ), // Will be populated later.
      l: WebTree
    )(nextSlide())
  }
  
  @ignore
  def slide(elems: WebTree*): Element = {
    slide(varargToList(elems))
  }
  
  def collectActions(input: WebElement): List[WebElement] = {
    input match {
      case Element("span", _, 
      Cons(WebAttribute("class" , "action highlighton"), _), _) =>
      List(input)
      case Element("span", _, 
      Cons(WebAttribute("class" , "action highlightoff"), _), _) =>
      List(input)
      case Element("span", _, 
      Cons(WebAttribute("class" , "action hiddenvoice"), _), _) =>
      List(input)
      case Element("span", _, 
      Cons(WebAttribute("class" , "action nextslide"), _), _) =>
      List(input)
      case Element(_, sons, _, _) => collectActions(sons)
      case  _ => Nil()
    }
  }
  
  def collectActions(input: List[WebElement]): List[WebElement] = {
    input.flatMap(collectActions)
  }
  
  def regroupSubtitles(s: WebElement): WebElement = {
    s match {
      case Element("section",
        Cons(playmenu, Cons(Element("span",
          Cons(Element("div", sons3, attrs3, styles3), n),
          attrs2, styles2),
        remaining)), attrs, styles) =>
        Element("section",
        Cons(playmenu, Cons(Element("span",
          Cons(Element("div", sons3 ++ collectActions(remaining), attrs3, styles3), n),
          attrs2, styles2),
        remaining)), attrs, styles)
      case Element(tag, sons, attrs, styles) =>
        Element(tag, sons.map(regroupSubtitles), attrs, styles)
      case _ => s
    }
  }

  def slides = {
    implicit val state = IntGenerator(0)
  
    val example1 = A(L(List("x","y"),"x"),List(V("a"),V("b")))
    val app = A("f", List("x"))
    val lam = L(List("x"), "M")
    
    val ex2 = L(List("x", "y"),A("M", List("N")))
    val ex3 = A("f", List("M", "N"))
    val betareduction = "\\(\\beta\\)-reduction"
    val lambdacalculus = "\\(\\lambda\\)-calculus"
    val can_do = t("cando")
    
    val pfirst = _1(pair("M", "N"))
    val psecond = _2(pair("M", "N"))
    <.div(^.classes := "slides",
slide(List(<.h2(t("slide0title"), ^.classes := "centeredtitle")), say(t("slide0subt0"), t("slide0subt1"), t("slide0subt2"), t("slide0subt3"))),
slide(<.h2(t("slide1title")) say t("slide1subt0"),
  <.ul(
    <.li(<.span("Church, A., 1932") say t("slide1subt1"), """, “A set of postulates for the foundation of logic”, """, <.i("Annals of Mathematics"), """(2nd Series), 33(2): 346–366.""")
  ),
  <.p(t("slide1example")) say t("slide1subt2"),
  <.table(^.classes := "scalalambda",
    <.tbody(
      <.tr(<.th(t("scalaequivalentHeader"), ^.id := "hdScala1"), <.th(t("lambdacalculusHeader"))),
      <.tr(<.td(example1.htmlScala) highlightOn "#hdScala1"
        say t("slide1subt3") highlightOff "#hdScala1"
      ,<.td(example1.html)
        say t("slide1subt4"))
    )
  ),
  <.br(),
  <.p(t("slide1introlambda"))
  say t("slide1subt5"),
  <.table(^.classes := "scalalambda",
    <.tbody(
      <.tr(<.th(""), <.th(t("scalaequivalentHeader")), <.th(t("lambdacalculusHeader"))),
      <.tr(<.td(t("application")), <.td(app.htmlScala),<.td(app.html) say List(t("slide1subt6"),t("slide1subt7"), t("slide1subt8"))),
      <.tr(<.td(t("abstraction")), <.td(lam.htmlScala),<.td(lam.html) say List(t("slide1subt9"), t("slide1subt10"), t("slide1subt11")) )
    )
  )
),
slide(
  <.h2("The main rule: argument substitution ("+betareduction+")"),
  <.p("Functions have one argument. We use abbreviations such as these:"),
  align(List(
    List(ex2.html, "=", ex2.expand.htmlUnambiguous, "similar to (x,y) => M(N)"),
    List(ex3.html, "=", ex3.expand.htmlUnambiguous, "similar to f(M, N)")
  )),
  <.p("We do not worry about types in the (untyped) " + lambdacalculus),
  <.p("Examples of applying " + betareduction + " (special case of Lecture 1 substitution model):"),
  <.p(A(L(List("x"), "M"), List("N")).html + b_=> + " “term obtained from M by replacing all x occurrences with N”"),
  <.p({
    val e = A(L(List("x"), "x"), List(A("a", List("b"))))
    e.html + b_=> + e.beta.expand.html
  }),
  <.p({
    val e = A(L(List("x", "y"), A("c", List("x"))), List("a", "b"))
    val e1 = e.expand
    val e2 = e1.beta
    val e3 = e2.beta
    e.html + " = " + e1.htmlUnambiguous + b_=> + e2.htmlUnambiguous + b_=> + e3.htmlUnambiguous
  }),
  <.p({
    val e = A(L(List("f", "x"), A("f", List(A("f", List("x"))))), List(L(List("y"), "a"), "b"))
    val e1 = e
    val e2 = e1.beta
    val e3 = e2.beta
    e.html + b_=> + e2.htmlUnambiguous + b_=> + e3.htmlUnambiguous})
),
<.section(^.classes := "verticalsplit",
slide(
  <.h2(lambdacalculus + can_do + "Booleans"),
  <.p("Given hypothetical if statement 'if (b) M N' represent Boolean values as the functions corresponding to “if (b)” code fragment"),
  align(List(
    List("if(true) M N", "should be", "M"),
    List("if(false) M N", "should be", "N")
  ))(^.css("border-spacing") := "20px 0", ^.css("border-collapse") := "separate"),
  <.p("Define"),
  {
    val t1 = A(ltrue, List("M", "N"))
    val f1 = A(lfalse, List("M", "N"))
    align(List(
     List(ltrue.html + " = " + ltrue.expandNames.html, betacompute(t1)),
     List(lfalse.html + " = " + lfalse.expandNames.html, betacompute(f1)))
    )(^.css("border-spacing") := "10px 0", ^.css("border-collapse") := "separate")
  },
  <.p("So instead of 'if (b) M N' we just write (b M N)")
),
slide(
  <.h2(lambdacalculus + can_do + "Integers, addition"),
  <.p("A number defines how many times to compose a function with itself."),
  <.p("Define"), {
    val zero = lNum(0)
    val one = lNum(1)
    val two = lNum(2)
    
    val n = N(Nil(), List("n"), l => L(List("f", "x"), A("f", List(A(V("(n-1)"), List("f", "x"))))))

    align(List(
      List(zero.html, "=", zero.expandNames.html),
      List(one.html, "=", one.expandNames.html),
      List(two.html, "=", two.expandNames.html),
      List(n.html, "=", n.expandNames.html)
    ))
  },
  <.p("We can define addition now using function composition:"),
  lPlus("P", "Q").html + "="+ lPlus("P", "Q").expandNames.html,
  <.p("It works. For example:"),
  {
    betacompute(A(lPlus(lNum(1), lNum(2)), List("F", "X"))) + " = " + A(lNum(3), List("F", "X")).html
  }
)),
slide(
  <.h2(lambdacalculus + can_do + "Pairs"),
  <.p("Pair is something from which we can get the first and the second element"),
  <.p("Define"), {
    val p = pair("M", "N")
    align(List(
      List(p.html, "=", p.expandNames.html),
      List(_1("p").html, "=",_1("p").expandNames.html),
      List(_2("p").html, "=",_2("p").expandNames.html)
    ))
  },
  <.p("Why does this work?"),
  betacompute(pfirst),
  <.br(),
  betacompute(psecond)
),
slide(
  <.h2(lambdacalculus + can_do + "Lists"),
  <.p("A list is something we can match on and deconstruct if it is not empty:"),
  <.pre("""list match {
  case Nil => M
  case Cons(x, y) => N(x, y)
}
"""),
  <.p("Define"), {
    val c = lCons("P", "Q")
    align(List(
      List(lNil.html, "=", lNil.expandNames.html),
      List(c.html, "=",c.expandNames.html)
    ))
  },
  <.p("Why does this work?"),
  betacompute(A(lNil, List("M", L(List("p"), _1("p"))))),
  <.br(),
  betacompute(A(lCons("P", "Q"), List("M", L(List("p"), _1("p")))), 3),
  <.p("Cons is like a pair, but takes m as argument, too, to fit along with Nil.")
),
slide(
  <.h2("Returning pair (tail,tail) if list non-empty, else Z"),
  <.pre(<.code(^.classes := "Scala", """list match {
  case Nil => Z
  case Cons(x,y) => (y,y)
}""")),{
  val l = A("list", List("Z", L(List("p"), A(L(List("y"), pair("y", "y")), List(_2("p"))))))
  List[WebTree](
    <.p("Becomes nothing else but"),
    l.html,
    <.p("i.e."),
    l.expandNames.html
  )
}),
slide(
  <.h2("Computation that takes any number of steps"),
  <.p(w.html + b_=> + w.beta.html + b_=> + "... loops"),
  <.p("More usefully :"),
  betacompute(ycombinator("F").expandNames, 1),
  <.p("If we denote " + ycombinator("F").html + " = " + ycombinator("F").expandNames.html, <.b(" for each term F")," then " + ycombinator("F").html + b_=> + A("F", List(ycombinator("F"))).html),
  <.p("A recursion that uses itself in its body (typically applies it):"),
  <.pre("""def h(x: Any) = P(h(Q(x)),x)
def h = (x: Any) => P(h(Q(x)),x)"""
    ),
  <.p("Denote right-hand side of the last ", <.b("def")," by F(h), since x is a bound variable"), {
    val yF = ycombinator("F")
    val yFF = A("F", List(yF))
    val yFFF = A("F", List(yFF))
    val hFFF = A("F", List(A("F", List("h"))))
  align(List(List(<.pre("def h = F(h)"), "to unfold recursion, replace h by F(h) in body"),
  List("We define h = "+ yF.html + " so ",
        "h = " + yF.html + b_=> +
                 yFF.html + b_=> +
                 yFFF.html + b_=> +
                 hFFF.html + b_=> + "...")))
  }
),
slide(
  <.h2("Replace all list elements by Z: List(1,2,3) \\(\\to\\) List(Z,Z,Z)"),
  <.pre("""def mkZ(list) = list match {
  case Nil => Nil
  case Cons(x, y) => Cons(Z, mkZ(y))
}
"""),
  <.p("After encoding match, still using recursion"),
  mkZ.html + " = " + mkZ.expandNames.html,
  <.p("After encoding recursion, it becomes mkZ = " + ycombinator("F").html + " for:"),
  "F = " + mkZF.html,<.br(),
  <.p("So " + mkZ.html + " can be defined as " + ycombinator("F").html + "which in this case is:"),
  <.p(
  L(List("x"), A(mkZF, List(A("x", List("x"))))).html,<.br(),
  "\\(\\;\\)" + L(List("x"), A(mkZF, List(A("x", List("x"))))).html)
)
          )
}

  def main = {
    WebPage(
      <.div(
        <.div(^.classes := "reveal",
          regroupSubtitles(slides)
        )), css)
  }
  
  def jsReveal = """$.getScript("/assets/js/reveal.js", function() {
    var i = -1;
    $("section").each(function(index, e) { if($(e).hasClass("present")) {i = index; console.log("i = " + index); } });
    var options = {
        width: "100%",
        height: "100%",
        margin: 0,
        minScale: 1,
        maxScale: 1,
        slideNumber: false,
        mouseWheel: true,
        transition: 'none' // default/none/fade/slide/convex/concave/zoom
      }
    if(i == -1) {
      Reveal.initialize(options)
    } else {
    
      Reveal.configure(options);
    }
    Reveal.slide( i, 0, 0 );
  });
  if($("#revealcsslink").length == 0) {
    $("head").append($('<link id="revealcsslink" rel="stylesheet" media="screen" href="/assets/css/reveal.css">'))
  }
  
  if($("#themewebbuilder").length == 0) {
    $("head").append($('<link id="themewebbuilder" rel="stylesheet" media="screen"/>'))
  }
  
  $("#themewebbuilder").attr("href", "/assets/css/theme/simple.css");
  """
  
  def jsKaTeX = """if($("#katexcsslink").length == 0) {
    $("head").append($('<link id="katexcsslink" rel="stylesheet" media="screen" href="https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.6.0/katex.min.css">'))
  }
  $.getScript("https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.6.0/katex.min.js", function() {
    $.getScript("https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.6.0/contrib/auto-render.min.js", function() { 
      renderMathInElement(document.getElementById("htmlDisplayerDiv"));
    })
  });"""
  
  def jsReponsiveVoice = """
  var defaultVoice = """" + defaultVoice + """"
  var highlightclass = """"+highlightclass+""""
  var paragraphs = function(elements) {
    return elements.find(".subtitlewrapper .subtitle .action").toArray()
  }
  
  var overlaps = (function () {
    function getPositions( elem ) {
        var pos, width, height;
        pos = $( elem ).position();
        width = $( elem ).width();
        height = $( elem ).height();
        return [ [ pos.left, pos.left + width ], [ pos.top, pos.top + height ] ];
    }
    function comparePositions( p1, p2 ) {
        var r1, r2;
        r1 = p1[0] < p2[0] ? p1 : p2;
        r2 = p1[0] < p2[0] ? p2 : p1;
        return r1[1] > r2[0] || r1[0] === r2[0];
    }
    return function ( a, b ) {
        var pos1 = getPositions( a ),
            pos2 = getPositions( b );
        return comparePositions( pos1[0], pos2[0] ) && comparePositions( pos1[1], pos2[1] );
    };
})();
  var menuSetPlaying = function() {
    $(".playpausemenu").addClass("playing")
  }
  var menuStopPlaying = function() {
    $(".playpausemenu").removeClass("playing")
  }
  var playAction = function() {
    if(responsiveVoice.isPlaying()) {
      responsiveVoice.resume()
    } else {
      read($("div.reveal > div.slides section.present"))
    }
    menuSetPlaying()
  }
  $("i.play").off("click.play").on("click.play", function() {
    playAction()
  })
  var pauseAction = function() {
    responsiveVoice.pause()
    menuStopPlaying()
  }
  $("i.pause").off("click.pause").on("click.pause", function() {
    pauseAction()
  })
  
  $("i.stepforward").off("click.stepforward").on("click.stepforward", function() {
    responsiveVoice.cancel()
    process($("section.present .subtitlewrapper .subtitle .hiddenvoice:visible").nextAll(".action").toArray(), $("section.present").get(0))
  })
  $("i.stepbackward").off("click.stepbackward").on("click.stepbackward", function() {
    responsiveVoice.cancel()
    var news = $("section.present .subtitlewrapper .subtitle .hiddenvoice:visible").prevAll(".hiddenvoice").first()
    if(news.prev(".highlighton").length > 0) {
      news = news.prev(".highlighton")
    }
    process(news.add(news.nextAll(".action")).toArray(), $("section.present").get(0))
  })
  
  var process = function(remainingActions, elem) {
    if(remainingActions.length != 0) {
      menuSetPlaying()
      var action = $(remainingActions.shift());
      var text = action.text()
      if(action.hasClass("hiddenvoice")) {
        action.siblings().hide()
        action.show()
        $(elem).find(".subtitlewrapper").css('display', 'inline-block')
        if($("." + highlightclass).length > 0 && overlaps($("." + highlightclass).get(0), $(elem).find(".subtitlewrapper").get(0))) {
          $(elem).find(".subtitlewrapper").addClass("top")
        }
        var todoatend = function() {
          action.hide()
          $(elem).find(".subtitlewrapper").css('display', 'none');
          $(elem).find(".subtitlewrapper").removeClass("top")
        }
        responsiveVoice.speak(text, defaultVoice, {
          onerror: function() {
            todoatend()
          },
          onend: function() {
          todoatend()
          process(remainingActions, elem)
        }})
      } else if(action.hasClass("highlighton")) {
        $("."+highlightclass).removeClass(highlightclass)
        $(text).addClass(highlightclass)
        process(remainingActions, elem)
      } else if(action.hasClass("highlightoff")) {
        $(text).removeClass(highlightclass)
        process(remainingActions, elem)
      }/* else if(action.hasClass("nextslide")) {
        setTimeout(function() { Reveal.next() }, 1000)
      }*/
    } else {
      menuStopPlaying()
    }
  }
  
  var read = function(elem) {
    var actions = paragraphs($(elem))
    console.log("reading aloud ", elem)
    process(actions, elem)
  }
  
  var $target = $("div.reveal > div.slides section");
  var current = null
  $target.each(function(index, elem) {
    var observer = new MutationObserver(function(mutations) {
      mutations.forEach(function(mutation) {
          if (mutation.attributeName === "class") {
              var attributeValue = $(mutation.target).prop(mutation.attributeName);
              if (attributeValue == ("present") && mutation.target != current){
                  responsiveVoice.cancel();
                  read(mutation.target);
                  current = mutation.target;
              }
          }
      });
    });
    observer.observe(elem,  { attributes: true });
  })
  
  setTimeout(function() {
  if($("#responsivevoice").length == 0) {
    $("head").append($('<script id="responsivevoice" src="https://code.responsivevoice.org/responsivevoice.js"/>'))
  }
  setTimeout( function() { read("section.present") }, 2500 )
  }, 500)"""
  
  def javascript = jsReveal + jsKaTeX + jsReponsiveVoice 
}
