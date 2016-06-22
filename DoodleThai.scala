import leon.collection._
import leon.webDSL.webDescription._
import leon.webDSL.webBuilding._
import implicits._
import scala.language.dynamics
import leon.lang._

object DoodleThai {
  val css = Style(
    ".hiddenAcc" := (
        ^.display:= "block",
        ^.position:= "absolute",
        ^.top:= "-999em"
    ),
    """.form-control, textarea, select, input[type="text"], input[type="password"], input[type="datetime"], input[type="datetime-local"], input[type="date"], input[type="month"], input[type="time"], input[type="week"], input[type="number"], input[type="email"], input[type="url"], input[type="search"], input[type="tel"], input[type="color"], .uneditable-input""" := (
    ^.height := "30px",
    ^.paddingTop := "4px",
    ^.paddingBottom := "4px",
    ^.borderRadius := "0",
    ^.borderColor := "#b7b7b7",
    ^.fontSize := "13px"
    ),
    ".textPoll th, .textPoll .foot td" := (
        ^.maxWidth:= "94px"
    ),
    "label" := (
        ^.fontWeight:= "normal"
    ),
    "label" := (
        ^.display:= "inline-block",
        ^.marginBottom:= "5px",
        ^.fontWeight:= "bold"
    ),
    "table.poll tr.participation td" := (
      ^.textAlign:= "center",
      ^.backgroundColor:= "#eaeaea"
    ),
    "table.poll tr.participant td.n" := (
      ^.backgroundColor := "#ffccca"
    ),
    "table.poll tr.participant td.y" := (
      ^.backgroundColor := "#d1f3d1"
    ),
    "table.poll tr.participant td" := (
      ^.textAlign:= "center",
      ^.verticalAlign:= "middle",
      ^.height:= "33px"
    ),
    "table.poll tr.participation td:hover, table.poll tr.participation.inEdit td:hover" := (
      ^.backgroundColor := "#d6d6d6"),
    "table.poll tr.participation td.pname,table.poll tr.participant td.pname" := (
        ^.position := "relative",
        ^.minWidth := "182px",
        ^.width := "182px",
        ^.borderTop := "1px solid #fff",
        ^.borderBottom := "2px solid #fff",
        ^.background := "url('http://doodle.com/builtstatic/1465286543/doodle/graphics/sprites/common/normal-s92f91c2182.png') -15px -972px no-repeat #fff",
        ^.css("imageRendering") := "-moz-crisp-edges",
        ^.css("imageRendering") := "-o-crisp-edges",
        ^.css("imageRendering") := "-webkit-optimize-contrast",
        ^.css("imageRendering") := "crisp-edges",
        ^.css("-ms-interpolation-mode") := "nearest-neighbor",
        ^.padding := "0 2px 0 0"
    ),
    "table.poll tr.header.month th.xsep" := (
        ^.background := """url("http://doodle.com/graphics/polls/tick31r.png") right 0 repeat-y #3385e4""",
        ^.paddingRight := "13px"
    ),
    "table.poll td.pname div.pname" := (
        ^.textAlign := "left",
        ^.fontSize := "15px",
        ^.lineHeight := "15px",
        ^.padding := "8px 0 5px 0",
        ^.marginLeft := "3px",
        ^.whiteSpace := "nowrap",
        ^.overflow := "hidden",
        ^.maxWidth := "135px",
        ^.width := "135px",
        ^.position := "relative"
    ),
    "table.poll tr.date th, table.poll tr.date td" := (
    ^.background:= "#3385e4",
    ^.color:= "#fff"
    ),
    "table.poll" := (
      ^.borderCollapse:= "separate"
    ),
    "table.poll tr.date.month th" := (
      ^.paddingTop:= "7px",
      ^.paddingBottom:= "3px"
    ),
    "table.poll tr.header th, table.poll tr.header td" := (
      ^.padding:= "0 10px 0"
    ),
    "table.poll th, table.poll td" := (
      ^.fontSize:= "13px",
      ^.lineHeight:= "17px",
      ^.fontWeight:= "normal"
    ),
    "table.poll tr.participation td.pname input" := (
    ^.fontSize:= "12px",
    ^.height:= "24px",
    ^.lineHeight:= "20px",
    ^.margin:= "3px 0 3px 3px",
    ^.width:= "131px",
    ^.padding:= "2px 6px 2px 9px"
    ),
    
    "table.poll tr th.nonHeader.partCount, table.poll tr td.partCount" := (
    ^.background:= "#fff",
    ^.padding:= "0 0 9px 0",
    ^.verticalAlign:= "bottom",
    ^.fontSize:= "13px",
    ^.color:= "#323232",
    ^.minWidth:= "184px",
    ^.width:= "184px",
    ^.fontWeight:= "bold",
    ^.maxWidth:= "none"
    ),
    "table.poll tr.sums td" := (
    ^.textAlign := "center",
    ^.lineHeight := "16px",
    ^.paddingLeft := "5px",
    ^.paddingRight := "8px",
    ^.paddingTop := "5px",
    ^.color := "#6f6f6f"
    ),
    "table.poll tr.participant td.q" := (
    ^.backgroundColor:= "#eaeaea"
      )
  )
  
  
  val participants = List(
    "Leon Weber",
    "Keanu",
    "MysteriousMan"
    )
    
  val choix: List[(BigInt, BigInt, Boolean)] = List(
    (1, 1, true ),
    (1, 2, false),
    (1, 3, false),
    (1, 4, false),
    (1, 5, true ),
    (2, 1, false),
    (2, 2, true ),
    (2, 3, false),
    (2, 4, false),
    (3, 1, true ),
    (3, 2, true ),
    (3, 3, true ),
    (3, 4, false)
  )
  
  def total(menuId: BigInt): WebTree = {
      val t = choix.count(entry => entry._2 ==         menuId && entry._3 == true)
      <.td(t.toString)
      //if choix._3 = true
  }
  
  def checkBox(colonne: BigInt): WebTree = 
    <.td(
      ^.classes := "xsep", ^("data") := colonne.toString,
      ^.id := "box" + colonne, ^.title := "....",
        <.label( 
          ^.classes := "hiddenAcc", 
          ^.forid :="option" + colonne,"Boeuf (CH) au curry jaune, lait de coco, pommes de terre et cacahuètes"),
            <.input( 
              ^.id := "option" + colonne, 
              ^.name :="p", 
              ^.tpe :="checkbox"))
  
  def WantsToEat (personId: BigInt, menuId: BigInt): WebTree = choix.find(entry => entry._1 == personId && entry._2 == menuId) match{
    case Some(choix) => if (choix._3) 
    <. td(
      ^.classes := "partTableCell y xsep pok",
      ^.title := (participants(personId-1)) + ", " + (menus(menuId-1)) + ": Oui",
        <.img(
          ^.alt := (participants(personId-1)) + ", " + (menus(menuId-1)) + ": Oui",
          ^.classes := "preferenceTick",
          ^.src := "http://doodle.com/builtstatic/1464857356/doodle/graphics/polls/tick_pok.png"))
    else
      <.td (
        ^.classes := "partTableCell n xsep pn", ^.title := (participants(personId-1) + ", " + (menus(menuId-1))+ ": Non"))
    case None() => 
    <.td(
      ^.classes := "partTableCell q sep pog",
      ^.title := (participants(personId-1)) + ", " + (menus(menuId-1)),
      <.img(
        ^.alt := "Mikael Mayer, Toboggan de sauce tomate: ?",
        ^.classes := "preferenceTick",
        ^.src := "http://doodle.com/builtstatic/1466498440/doodle/graphics/polls/tick_pog.png",
        ^("data-tip") := "false"
      ))
  }

  val menus = List(
    "Boeuf (CH) au curry jaune, lait de coco, pommes de terre et cacahuètes",
    "Poulet (CH) au curry rouge, lait de coco et courge thaï",
    "Phat sen ma ma (nouilles jaunes) sautées aux crevettes", 
    "Veg - Tofu sauté aux légumes et graines de soja",
    "Soupe de nouilles de riz au porc (CH)"
    )
    
  def ligne(menu: String): WebTree = 
      <.td(menu)
      
  def nom(personId: BigInt): WebTree = 
    <.tr( ^.classes := "participant partMyself",
      <.td( ^.classes := "pname", ^.id := "part1367130755",
        <.div( ^.classes := "pname ",
        participants(personId-1))))(
        List.range(1, menus.length+1).map(menusId => WantsToEat(personId, menusId))
      
         
        )
  
  val page = 
<.table(
  ^.cellpadding := "0",
  ^.cellspacing := "0",
  ^.classes := "poll textPoll ",
  ^("summary") :="LARA Thai",
  <.tbody(
    <.tr(^.classes := "header date month",
      <.th(^.classes := "nonHeader partCount boldText",
      <.div(participants.length.toString + " participants")))
      (
        menus.map(menu => ligne(menu))
        ),
    <.tr())(
      List.range(1, participants.length+1).map(personId => nom(personId)))(

    <.tr( ^.classes := "participation yesNo",
      <.td( ^.classes := "pname",
          <.label( ^.classes := "hiddenAcc", ^.forid :="pname", ^.title := "l10n_yourName"),
          <.input( ^.classes := "form-control", ^.id := "pname", ^.maxlength :="64", ^.name :="name", ^.placeholder:="Votre nom", ^.tpe :="text", ^.value :=""))
  )(List.range(1, menus.length).map(menusId => checkBox(menusId))),
    <.tr( ^.classes := "sums",
      <.td( ^.classes := "nonHeader"))(
     List.range(1, menus.length+1).map(menusId => total(menusId))
    )
  )
)

  def render = {
    WebPage(
      <.div(
        <.h1("LARA Thai meeting"),
        <.p("Choose your menu. See you {enter date here} in {enter meeting place here} at {enter hour here}!"),
        page), css)
  }
}
