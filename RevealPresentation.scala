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
  val css = Style(
    "> div" := (
      ^.position := "absolute",
      ^.width := "100%",
      ^.top := "300px"
    )
  )

  val slideTitle = "Unnamed"
  
  val slideColor = "cyan"
  
  def main = {
    WebPage(
      <.div(
        <.div(^.classes := "reveal",
          <.div(^.classes := "slides",
            <.section(<.h2(slideTitle),
              ^.background := slideColor,
              <.ul(
                (!expected(slideTitle) ?= <.li("These slides are interactive! Turn the mousewheel down to go to the second slide.")),
                <.li(<.i("The title of this slide is \"" + slideTitle + "\" because we think it's cool."))
              ),
              <.p(if(expected(slideTitle) && slideColor != "pink") "Congratulations! you edited your first presentation." else ""),
              <.p(if(expected(slideTitle)) (if(slideColor != "pink") "Now the background of these slides is "+slideColor+". Can you change it to pink by just modifying the word after \"these slides is\" in this paragraph?" else "Well done! That's the end of the demo.") else "")),
            <.section(
              <.h2(slideTitle),
              ^.background := slideColor,
              <.ul(
                <.li(if(!expected(slideTitle)) "Please replace the title (currently \"" + slideTitle + "\") by \"Introduction\"" else "Well done."),
                <.li("Mousewheel up to go back to the previous slide.")
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
  $.getScript("/assets/js/reveal.js", function() {
    Reveal.initialize();
  });
  if($("#themewebbuilder").length == 0) {
    $("head").append($('<link id="themewebbuilder" rel="stylesheet" media="screen"/>'))
  }
  
  $("#themewebbuilder").attr("href", "/assets/css/theme/simple.css");
  
  if($("#responsivevoice").length == 0) {
    $("head").append($('<script id="responsivevoice" src="https://code.responsivevoice.org/responsivevoice.js"/>'))
  }
  
  if($("#revealcsslink").length == 0) {
    $("head").append($('<link id="revealcsslink" rel="stylesheet" media="screen" href="/assets/css/reveal.css">'))
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
  
  setTimeout( function() { read("section.present") }, 3000 )

  """
}
