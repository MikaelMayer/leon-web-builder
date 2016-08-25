/** 
  * Name:     JsonRender.scala
  * Creation: 25.11.2015
  * Author:   Mikael Mayer (mikael.mayer@epfl.ch)
  * Comments: Json specifications
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

  val slideTitle = "Introduction"
  
  val b = false
  val slideColor = if(b) "red" else "yellow"
  
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
              <.p(if(expected(slideTitle)) (if(slideColor != "pink") "Now the background of these slides is "+slideColor+". Can you change it to pink by just modifying the word in this paragraph?" else "Well done! That's the end of the demo.") else "")),
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

  """
}
