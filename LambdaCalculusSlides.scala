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
  val notation = notationLambda // notationLambda, notationArrow
  val height = 600
  
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

  // The css of the webpage.
  val css = Style(
    ".centeredtitle" := (
      ^.marginTop := "50%",
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
      
  def slides = {
    val example1 = A(L(List("x","y"),"x"),List(V("a"),V("b")))
    val app = A("f", List("x"))
    val lam = L(List("x"), "M")
    
    val ex2 = L(List("x", "y"),A("M", List("N")))
    val ex3 = A("f", List("M", "N"))
    val betareduction = "\\(\\beta\\)-reduction"
    val lambdacalculus = "\\(\\lambda\\)-calculus"
    val can_do = " can do: "
    
    val pfirst = _1(pair("M", "N"))
    val psecond = _2(pair("M", "N"))
    <.div(^.classes := "slides",
<.section(<.h2("Lambda Calculus and LISP", ^.classes := "centeredtitle")),
<.section(<.h2("Lambda Calculus: First Functional Language"),
  //^.background := slideColor,
  <.ul(
    <.li("""Church, A., 1932, “A set of postulates for the foundation of logic”, """, <.i("Annals of Mathematics"), """(2nd Series), 33(2): 346–366.""")),
  <.p("Example"),
  <.table(^.classes := "scalalambda",
    <.tbody(
      <.tr(<.th("Scala Equivalent"), <.th("Lambda calculus")),
      <.tr(<.td(example1.htmlScala),<.td(example1.html))
    )
  ),
  <.br(),
  <.p("Lambda calculus has only variables (x,y,a,b,…) and these two constructs:"),
  <.table(^.classes := "scalalambda",
    <.tbody(
      <.tr(<.th(""), <.th("Scala Equivalent"), <.th("Lambda calculus")),
      <.tr(<.td("Application"), <.td(app.htmlScala),<.td(app.html)),
      <.tr(<.td("Abstraction"), <.td(lam.htmlScala),<.td(lam.html))
    )
  )
),
<.section(
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
<.section(
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
<.section(
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
<.section(
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
<.section(
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
<.section(
  <.h2("Returning pair (tail,tail) if list non-empty, else Z"),
  <.pre(<.code(^.classes := "Scala", """list match {
  case Nil => Z
  case Cons(x,y) => (y,y)
}"""))){
  val l = A("list", List("Z", L(List("p"), A(L(List("y"), pair("y", "y")), List(_2("p"))))))
  List[WebTree](
    <.p("Becomes nothing else but"),
    l.html,
    <.p("i.e."),
    l.expandNames.html
  )
},
<.section(
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
<.section(
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
          slides
        )), css)
  }
  
  def jsReveal = """$.getScript("/assets/js/reveal.js", function() {
    var i = -1;
    $("section").each(function(index, e) { if($(e).hasClass("present")) {i = index; console.log("i = " + index); } });
    var options = {
        slideNumber: true,
        transition: 'convex' // default/none/fade/slide/convex/concave/zoom
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
  if($("#responsivevoice").length == 0) {
    $("head").append($('<script id="responsivevoice" src="https://code.responsivevoice.org/responsivevoice.js"/>'))
  }
  var paragraphs = function(elements) {
    return elements.find("*").contents().filter(function() { return this.nodeType === 3; }).map(function(index, elem) { return elem.textContent; })
  }
  
  var read = function(elem) {
    paragraphs($(elem)).each(function(index, text) { responsiveVoice.speak(text) })
  }
  
  var $target = $("div.reveal > div.slides section");
  var current = null
  
  /*$target.each(function(index, elem) {
    var observer = new MutationObserver(function(mutations) {
      mutations.forEach(function(mutation) {
          if (mutation.attributeName === "class") {
              var attributeValue = $(mutation.target).prop(mutation.attributeName);
              if (attributeValue == ("present") && mutation.target != current){
                  read(mutation.target);
                  current = mutation.target;
              }
          }
      });
    });
    observer.observe(elem,  { attributes: true });
  })*/
  
  /*setTimeout( function() { read("section.present") }, 3000 )*/"""
  
  def javascript = jsReveal + jsKaTeX /* + jsResponsiveVoice */
}