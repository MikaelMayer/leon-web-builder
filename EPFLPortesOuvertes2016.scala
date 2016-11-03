import leon.collection._
import leon.webDSL.webDescription._
import leon.webDSL.webBuilding._
import leon.lang.Map
import implicits._

object Main {
  
val css = Style(
"#body" := (
  ^.fontSize:= "15px"
),
".media-list .media:first-child":=(
  ^.borderTop := "1px solid black;",
  ^.verticalAlign := "top"
),
".media-list .media" := (
  ^.padding := ".3075em 0 0;"
),
".media" := (
  ^.position:= "relative",
  ^.padding:= ".3075em",
  ^.overflow:= "hidden",
  ^.display := "block",
  ^.fontSize:= ".8125em",
  ^.lineHeight:= "1.4em"
),
".g-row:after, .g-row:before" := (
  ^.css("content"):= "",
  ^.display:= "table",
  ^.css("clear") := "both"
),
".g-span-od-1_6" := (
  ^.display := "block",
  ^.css("float") := "left"
),
".media-list .media-visual" := (
  ^.marginBottom := "0",
  ^.maxHeight := "7.4em",
  ^.overflow := "hidden"
),
".media-visual" := (
  ^.overflow := "hidden",
  ^.position := "relative",
  ^.marginBottom := ".615em"
),
".media, .media-info, .media-visual, .panel-button .panel-content a, .panel-button .panel-header a" := (
  ^.display:= "block"
),
".media-visual, .media-visual img, .nav-block .nav-link+[aria-controls][aria-expanded], .nav-block .nav-link:link+[aria-controls][aria-expanded], .nav-block .nav-link:visited+[aria-controls][aria-expanded]" := (
  ^.verticalAlign:= "bottom"
),
"a" := (
  ^.css("textDecoration"):= "none",
  ^.backgroundColor:= "transparent"
),
"a, b, dfn, hr, strong" := (
  ^.color := "#000"
),
".media-header a:focus, .media-header a:hover" := (
  ^.color:= "#ae0010"
),
".media-header a" := (
  ^.border:= "none",
  ^.color:= "inherit"
),
".btn-inline.themed:hover .icon, .btn-inline:hover .icon, .media-header a, .themed .btn-inline:hover .icon" := (
  ^.css("text-decoration") := "none"
),
"[tabindex=\"0\"]:focus, a:active, a:focus, a:hover" := (
  ^.css("outline"):= "0"
),
"a:-webkit-any-link" := (
  ^.css("cursor"):= "auto"
),
".media-list .media-header" := (
  ^.css("overflow"):= "hidden",
  ^.css("text-overflow") := "ellipsis",
  ^.css("text-transform") := "none",
  ^.css("white-space") := "nowrap"
),
".media-header" := (
  ^.marginTop := "0.4em",
  ^.marginBottom:= "0.3em",
  ^.padding:= "0",
  ^.margin:= "0 0 .3075em",
  ^.border:= "none",
  ^.css("font"):= "700 1em/1.077em Arial,Helvetica,Verdana,sans-serif",
  ^.color:= "#000",
  ^.css("letter-spacing"):= "0",
  ^.css("text-transform"):= "none"
),
".icon" := (
  ^.padding:= "0",
  ^.margin:= "0",
  ^.background:= "0 0",
  ^.fontSize:= "inherit"
),
".sprite-calendar" := (
  ^.backgroundImage:= "url(http://static.epfl.ch/latest/images/sprites.png)",
  ^.width:= "13px",
  ^.height:= "12px",
  ^.css("background-position"):= "0 0"
),
".media-content" := (
  ^.fontSize:= ".92308em",
  ^.lineHeight:= "1.25em"
),
".g-span-row1" := (
  ^.height:= "7.231em",
  ^.width:= "74.99997%",
  ^.padding:= "0 0 .3075em .3075em",
  ^.margin:= "0",
  ^.paddingLeft :="116px"
),
".autocomplete-more, .media-info, .menu-item-back, .nav-pagination .nav-item, blockquote, cite, figcaption, q" :=(
  ^.fontFamily:= """Georgia,"Times New Roman",Times,serif""",
  ^.css("font-style"):= "italic",
  ^.fontWeight:= "100",
  ^.css("letter-spacing"):= ".07em"
),
".opendays-event-size" := (
    ^.fontSize:= "12px"
),
".media-list .media:last-child" := (
    ^.borderBottom:= "1px dotted #000"
),
".media-list .media" := (
    ^.padding:= ".3075em 0 0",
    ^.borderTop:= "1px dotted #000"
)
)

  val translations = Map(
    "fr" -> Map(
      "moreabout" -> "En apprendre plus sur",
      "addevent" -> "Ajouter l'événement à mon calendrier",
      "samedi" -> "Samedi 5 novembre 2016  09:00 - 18:00",
      "dimanche" -> "Dimanche 6 novembre 2016  10:00 - 17:00",
      "titredesir" -> "Un ordinateur qui comprend (vraiment) vos désirs",
      "resumedesir" -> "L'ordinateur fait tout ce qu'on lui demande. Pour autant qu'on sache programmer! Des chercheurs montrent comment mettre la programmation à la portée de tous avec un générateur de pages web qui tolère les corrections sur le résultat final.",
      "urldesir" -> "http://memento.epfl.ch/event/un-ordinateur-qui-comprend-vraiment-vos-desirs/",
      "titredrone" -> "Capturer des débris spatiaux à l’aide d’un drone",
      "resumedrone" -> "L’intense activité spatiale de ces 60 dernières années a laissé de nombreux débris dans le proche espace de la Terre. Pour prendre la mesure du phénomène et de ses dangers, essayez de capturer des objets à l’aide de drones ! Activité conseillée dès l’âge de 10 ans.",
      "urldrone" ->  "http://memento.epfl.ch/event/capturer-des-debris-spatiaux-a-laide-dun-drone/",
      "titredronesolution" -> "Une solution innovante pour piloter votre drone",
      "resumedronesolution" -> "Le marché des drones est en pleine expansion mais la manière de les contrôler n'a pas évolué depuis 50 ans. Les co-fondateurs de MotionPilot vous présenteront leur solution à travers une démonstration de leur prototype",
      "urldronesolution" -> "http://memento.epfl.ch/event/une-solution-innovante-pour-piloter-votre-drone/"
    )
  )

  def eventList(t: Translator) = List[Event](
    Event(Sat, BC, MODemo, t("titredesir"), t("resumedesir"), t("urldesir")),
    Event(Sat, RLC, ESActivite, t("titredrone"), t("resumedrone"), t("urldrone")),
    Event(Sat, RLC, QUDemo, t("titredronesolution"), t("resumedronesolution"), t("urldronesolution")),
    Event(Sun, BC, MODemo, t("titredesir"), t("resumedesir"), t("urldesir"))
  )
  
  case class Translator(lang: String) {
    def apply(t: String): String = tr(lang, t)
  }
  
  def tr(lang: String, key: String) = translations.getOrElse(lang, translations("tr")).getOrElse(key, key)
  
  abstract class EventType {
    def imageURL: String
    def fullImageURL: String = "http://memento.epfl.ch"+imageURL
  }
  
  case object MODemo extends EventType {
    def imageURL = "/public/img/portes-ouvertes/MO-Demo.png"
  }
  
  case object ESActivite extends EventType {
    def imageURL = "/public/img/portes-ouvertes/ES-Activite.png"
  }
  
  case object QUDemo extends EventType {
    def imageURL = "/public/img/portes-ouvertes/QU-Demo.png"
  }
  
  abstract class Place {
    def name: String
    def url: String
  }
  case object BC extends Place {
    def name = "BC Atrium"
    def url = "https://plan.epfl.ch/theme/portes_ouvertes_thm?wfs_layer=mondesv_demo&wfs_id=38"
  }
  case object RLC extends Place {
    def name = "RLC E1 240"
    def url = "https://plan.epfl.ch/theme/portes_ouvertes_thm?wfs_layer=espacet_activite&wfs_id=31"
  }
  
  abstract class Day {
    def name(t: Translator): String
  }
  case object Sun extends Day {
    def name(t: Translator) = t("dimanche")
  }
  case object Sat extends Day {
    def name(t: Translator) = t("samedi")
  }
  
  case class Event(day: Day, place: Place, tpe: EventType,
    title: String,
    summary: String,
    url: String)

  
    val tcheck = ^.tpe := "checkbox"
    def check(s: (String, String)): WebTree = <.tr(<.td(s._1), <.td(<.span(<.label(<.input(tcheck, ^.value := s._2), s._2), <.br())))
  
    val presentationChoices = List(
      ("9:15", "Fiber-optic Communication via the non-linear Fourier transform"),
      ("10:45", "Deciphering the Good-Turing Enigma"),
      ("11:45", "Information Storage in DNA"),
      ("12:45", "Poster session and lunch")
    )
    
    val pagetitle = "IC Research Day"
    
    def makeList(l: List[(String, String)]): String = l match {
      case Cons(a, b: Cons[(String, String)]) => a._2 + ", " + makeList(b)
      case Nil() => ""
      case Cons(a, Nil()) => a._2
    }
    
  def renderEvent(event: Event, t: Translator): WebTree = {
    import ^._
    <.div(classes:="media event",
   	    <.div(classes:="g-row",
         <.div(classes:="g-span-od-1_6",
	         <.div(classes:="media-visual",
	           id:="media-visual-opendays",
	           maxHeight:="none",
		<.a(href:=event.url, <.img(src:=event.tpe.fullImageURL, alt:="Thumbnail"))
	)),
		<.div(classes:="g-span-3_4 g-span-row1",
			<("small")(classes:="media-info",
			 <.a(href:="#",title:=t("addevent"), <.span(classes:="icon sprite-calendar",
			 ^("role"):="presentation"),"\u00A0",
					<.span(classes:="hour full-hour opendays-event-size",event.day.name(t))),
				<.span(classes:="opendays-event-size",
	        <.a(href:=event.place.url, <.span(" " +event.place.name)))),
			<.h2(classes:="media-header",
				<.a(href:=event.url, title:=event.title, event.title)),
			<.div(classes:="media-content",
			  <.span(event.summary),
			  <.a(classes:="more",href:=event.url,<.span(classes:="visuallyhidden", t("moreabout") + " \"" + event.title + "\""))))))
  }
    
  def render(lang: String) = {
    val translator = Translator(lang)
    import ^._
    <.div(classes:="media-list event-list")(
      eventList(translator).map((event: Event) => renderEvent(event, translator))
    )
  }

// The main webpage
def main() = {
  WebPage(<.div(^.id:="body",
    <.h1("Portes ouvertes 2016"),
    render("fr")), css)
  }
}
