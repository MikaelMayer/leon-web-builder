import leon.collection._
import leon.webDSL.webDescription._
import leon.webDSL.webBuilding._
import implicits._
import scala.language.dynamics
import leon.lang.Map

object Main {
  val css = Style(
    "h2 div" := (
      ^.css("display") := "inline",
      ^.css("font-size") := "0.9em",
      ^.css("font-weight") := "bold"
    ),
    "h2" := (
      ^.css("margin-top") := "0.1em"
    ),
    ".date" := (
      ^.css("display") := "block",
      ^.css("margin-left") := "auto",
      ^.css("margin-right") := "auto",
      ^.css("width") := "13em",
      ^.css("font-weight") := "bold",
      ^.css("font-size") := "1.5em"
    )
  )
  
  val data = List(
      (
        "LMS: Generative Programming in Scala",
        "Nada Amine",
        "Lightweight Modular Staging (LMS) is a Scala library for generative programming: the key idea is to write high-level generic programs that generate low-level specialized programs at runtime."
      ),
      (
        "SCOPES project Research and Education Goals and Results",
        "Srdjan Skrbic",
        "In this talk I will give details about research activities foreseen within the SCOPES IZ74Z0_160453 project"
      ),
      (
        "Domain Specific Languages on HPC systems",
        "Sergi Mateo Bellido",
        "The talk will present our experiences developing a framework to build Domain Specific Languages for HPC platforms."
      )
    )
  
  val translationMaps = Map(
      "eng" -> Map(
        "Scope" -> "SCOPES ",
        "Date" -> "29 June 2016",
        "Speaker" -> "Speaker",
        "Title" -> "Title",
        "Abstract" -> "Abstract"
      ),
      "esp" -> Map(
        "Scope" -> "SCOPES ",
        "Date" -> "29 junio 2016",
        "Speaker" -> "Orador",
        "Title" -> "Título",
        "Abstract" -> "Resumen"            
      ),
      "srb" -> Map(
        "Scope" -> "SCOPES ",
        "Date" -> "29. јун 2016",
        "Speaker" -> "звучник",
        "Title" -> "наслов",
        "Abstract" -> "Апстрактан"
      )
  )
  
  def render(language: String) = {
    val transl = translationMaps(language)
    def talk(talkData: (String, String, String)): WebTree = {
      <.div(
        <.h2(transl("Title")+": ",
          <.div(talkData._1)),
        <.h2(transl("Speaker")+": ",
          <.div(talkData._2)),
        <.h2(transl("Abstract")+": "),
        <.h4(talkData._3),
        <.p("\n")
      )
    }
    WebPage(
      <.div(
        <.h1(transl("Scope")+transl("Date"),^.color := "blue"),
        <.div(data.map(talk)),
        <.span(^.classes := "date", transl("Date"))
      ), css  
    )
  }
  
  def main() = {
    render("eng")
  }
}

