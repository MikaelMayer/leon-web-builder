/** 
  * Name:     RevealPresentation.scala
  * Creation: 24.08.2016
  * Author:   Mikael Mayer (mikael.mayer@epfl.ch)
  * Comments: A presentation using Reveal.js and ResponsiveVoice
  */

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
  val slideColor = "cyan"
  val name = "Marion"
  val language = "fr"
  val height = "500"
  
  /*
  val slideColor = "cyan"
  val name = "Marion"
  val language = "fr"
  val height = "600"
*/  
  
  val css = Style(
    "section" := (
      ^.css("font-size") := "0.6em !important",
      ^.padding := "10px",
      ^.minHeight:= height + "px !important",
      ^.border := "1px solid black",
      ^.top := "0px !important"
    ),
    ".slides" := (
      ^.height := "100% !important",
      ^.left := "0px !important"/*,
      ^.top := "0px !important"*/
    ),
    ".reveal" := (
      ^.overflow:= "visible"  
    ),
    "" := (
      ^.height := "100%"
    ),
    "> div, > div > div" := (
      ^.position := "absolute",
      ^.width := "100%",
      ^.height := "100%"
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
  
  val translations = Map(
  "fr" -> Map(
    "slideTitle" -> "Website builder",
    "hello" -> "Salut",
    "canwehaveboth" -> "peux-tu avoir les deux?",
    "edit" -> "Pour éditer un site web, d'habitude soit",
    "useframework" -> " utilise un logiciel adapté et édite dedans.",
    "usecoding" -> " utilise un langage de programmation et modifie le code.",
    "continue" -> " Tourne la molete pour continuer.",
    "langchange" -> "Langue : ",
    "colorofthisslide" -> "La couleur de ce slide est : ",
    "putinpink" -> "Change la couleur de cette diapo en pink (rose)",
    "congratulations" -> "Félicitations ",
    "heightis" -> "La hauteur des diapo est de ",
    "heightisend" -> " pixels. Peux-tu la mettre à 400?"
  ),
  "es" -> Map(
    "slideTitle" -> "Site Web maker",
    "hello" -> "Salut",
    "canwehaveboth" -> "peux-tu avoir les deux?",
    "edit" -> "Pour éditer un site web, d'habitude soit",
    "useframework" -> " utilise un logiciel adapté et édite dedans.",
    "usecoding" -> " utilise un langage de programmation et modifie le code.",
    "continue" -> " Tourne la molete pour continuer.",
    "langchange" -> "Langue : ",
    "colorofthisslide" -> "La couleur de cette diapo est : ",
    "putinpink" -> "Change la couleur de cette diapo en pink (rose)",
    "congratulations" -> "Félicitations ",
    "heightis" -> "La hauteur des diapo est de ",
    "heightisend" -> " pixels. Peux-tu la mettre à 400?"
  ),
  "pt" -> Map(
    "slideTitle" -> "Site web maker",
    "hello" -> "Salut",
    "canwehaveboth" -> "peux-tu avoir les deux?",
    "edit" -> "Pour éditer un site web, d'habitude soit",
    "useframework" -> " utilise un logiciel adapté et édite dedans.",
    "usecoding" -> " utilise un langage de programmation et modifie le code.",
    "continue" -> " Tourne la molete pour continuer.",
    "langchange" -> "Idioma: ",
    "colorofthisslide" -> "La couleur de cette diapo est : ",
    "putinpink" -> "Change la couleur de cette diapo en pink (rose)",
    "congratulations" -> "Félicitations ",
    "heightis" -> "La hauteur des diapo est de ",
    "heightisend" -> " pixels. Peux-tu la mettre à 400?"
  ),
  "en" -> Map(
    "slideTitle" -> "Powerpoint Creator",
    "hello" -> "Hello",
    "canwehaveboth" -> "can you have both?",
    "edit" -> "To edit a website, usually either",
    "useframework" -> " uses a framework and edit inline.",
    "usecoding" -> " uses general-purpose coding and modify the code.",
    "continue" -> " Turn mousewhel down to continue.",
    "langchange" -> "Language : ",
    "colorofthisslide" -> "The color of this slide is : ",
    "putinpink" -> "Change the color of this slide to pink",
    "congratulations" -> "Congratulations ",
    "heightis" -> "The height of this diapo is ",
    "heightisend" -> " pixels. Can you set it to 400?"
  ),
  "de" -> Map(
    "slideTitle" -> "Website Builder",
    "hello" -> "Guten Morgen",
    "canwehaveboth" -> "kannst du beides haben?",
    "edit" -> "To edit a website, usually either",
    "useframework" -> " uses a framework and edit inline.",
    "usecoding" -> " uses general-purpose coding and modify the code.",
    "continue" -> " Turn mousewhel down to continue.",
    "langchange" -> "Language : ",
    "colorofthisslide" -> "The color of this slide is : ",
    "putinpink" -> "Change the color of this slide to pink",
    "congratulations" -> "Congratulations ",
    "heightis" -> "The height of this diapo is ",
    "heightisend" -> " pixels. Can you set it to 400?"
  ))
  
  case class Translator(lang: String) {
    def apply(key: String): String = 
      translations.getOrElse(lang, translations("fr")).getOrElse(key, key)
  }
  
  case object Flags {
    def apply(key: String): String = key match {
      case "en" => "uk"
      case "fr" => "ch"
      case _ => key
    }
  }

  def main = {
    val t = Translator(language)
    WebPage(
      <.div(
        <.div(^.classes := "reveal",
          <.div(^.classes := "slides",
            <.section(<.h2(t("slideTitle")),
              ^.background := slideColor,
              <.ul(
                <.li(t("langchange") + language, <.img(^.src := "http://www.kidlink.org/icons/f0-"+Flags(language)+".gif", ^.alt:= language, ^.classes := "flaglang")),
                <.li(t("hello") + " " + name + "!"),
                <.li(t("edit")),
                <.ol(
                  <.li(name + t("useframework")),
                  <.li(name + t("usecoding"))
                ),
                <.li(<.i(name+", " + t("canwehaveboth")))
              ), <.br(), t("continue")),
            <.section(
              <.h2(t("slideTitle")),
              ^.background := slideColor,
              t("colorofthisslide") + slideColor,<.br(),
              <.ul(
                <.li(if(slideColor != "pink") t("putinpink") else (t("congratulations") + name + "!")),
                (slideColor == "pink") ?= <.li(if(height != "400") t("heightis") + height + t("heightisend") else t("congratulations") + name + "!")
              )
              )
          )
        )/*,
        script(^.src :="/assets/js/reveal.js")*/
    ), css)
  }
  
  def expected(title: String): Boolean = {
    title == "Introduction"
  }
  
  def javascript = """
  $.getScript("/assets/reveal.js/js/reveal.js", function() {
    Reveal.initialize(
      {width: "100%",
        height: "100%",
        margin: 0,
        minScale: 1,
        maxScale: 1,
        slideNumber: false,
        mouseWheel: true,
        transition: 'none' // default/none/fade/slide/convex/concave/zoom
    });
  });
  
  if($("#revealcsslink").length == 0) {
    $("head").append($('<link id="revealcsslink" rel="stylesheet" media="screen" href="/assets/reveal.js/css/reveal.css">'))
  }
  
  if($("#themewebbuilder").length == 0) {
    $("head").append($('<link id="themewebbuilder" rel="stylesheet" media="screen"/>'))
  }
  
  $("#themewebbuilder").attr("href", "/assets/reveal.js/css/theme/simple.css");
    /*
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
  
  $target.each(function(index, elem) {
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
  })
  
  setTimeout( function() { read("section.present") }, 3000 ) */
  """  
}
