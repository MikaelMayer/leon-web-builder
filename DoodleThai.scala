import leon.collection._
import leon.webDSL.webDescription._
import leon.webDSL.webBuilding._
import implicits._
import scala.language.dynamics
import leon.lang.Map

object DoodleThai {
  val css = Style(
    ".hiddenAcc" := (
        ^.display:= "block",
        ^.position:= "absolute",
        ^.top:= "-999em"
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
    )
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
        <.div("2 participants")),
      <.th(^.classes := "xsep", ^.colspan := "1",
        <.div("Boeuf (CH) au curry jaune, lait de coco, pommes de terre et cacahuètes")),
      <.th( ^.classes := "xsep", ^.colspan := "1",
        <.div("Poulet (CH) au curry rouge, lait de coco et courge thaÃ¯")),
      <.th( ^.classes := "xsep", ^.colspan := "1",
        <.div("Phat sen ma ma (nouilles jaunes) sautées aux crevettes")),
      <.th( ^.classes := "xsep", ^.colspan := "1",
        <.div("Veg - Tofu sauté aux légumes et graines de soja")),
      <.th( ^.classes := "rsep", ^.colspan := "1",
        <.div("Soupe de nouilles de riz au porc (CH)"))),
    <.tr(),
    <.tr( ^.classes := "participant  partMyself",
      <.td( ^.classes := "pname", ^.id := "part1367130755",
        <.div( ^.classes := "inlineEdit  active",
          <.a( ^.classes := "inlineDeleteIcon", ^("dataid") := "1367130755", ^("dataname") := "Leon Weber", ^.title:="Effacer la saisie", ^.tabindex := "0"),
          <.a( ^.classes := "inlineEditIcon", ^("dataid") := "1367130755", ^.title := "Modifier la saisie", ^.tabindex := "0")),
        <.div( ^.classes := "avatarSmall hasMeetMe", ^("data") := "leonweber", ^.backgroundImage := "url('https://6a5edc300520d4037dd6-0732807511066685711db213ddc1d2df.ssl.cf2.rackcdn.com/ziiews13v6v78g0ovi81wz4libj4vgn8')"),
        <.div( ^.classes := "pname ", ^.title := "Leon Weber",
          <.a( ^.href := "/leonweber", ^.target := "_blank","Leon Weber"))),
      <.td( ^.classes := "partTableCell n xsep pn", ^.title := "Leon Weber, Boeuf (CH) au curry jaune, lait de coco, pommes de terre et cacahuÃ¨tes: Non"),
      <.td( ^.classes := "partTableCell y xsep pok", ^.title := "Leon Weber, Poulet (CH) au curry rouge, lait de coco et courge thaÃ¯: Oui",
        <.img( ^.alt := "Leon Weber, Poulet (CH) au curry rouge, lait de coco et courge thaÃ¯: Oui", ^.classes := "preferenceTick", ^.src := "http://doodle.com/builtstatic/1464857356/doodle/graphics/polls/tick_pok.png")),
      <.td( ^.classes := "partTableCell n xsep pn", ^.title := "Leon Weber, Phat sen ma ma (nouilles jaunes) sautÃ©es aux crevettes: Non"),
      <.td( ^.classes := "partTableCell n xsep pn", ^.title := "Leon Weber, Veg - Tofu sautÃ© aux légumes et graines de soja: Non"),
      <.td( ^.classes := "partTableCell n sep pn", ^.title := "Leon Weber, Soupe de nouilles de riz au porc (CH): Non")),
    <.tr( ^.classes := "participant ",
      <.td( ^.classes := "pname", ^.id := "part2124972605",
        <.div( ^.classes := "inlineEdit sr-only",
          <.a( ^.classes := "inlineDeleteIcon", ^("dataid") := "2124972605", ^("dataname") := "Keanu", ^.title := "Effacer la saisie", ^.tabindex := "0"),
          <.a( ^.classes := "inlineEditIcon", ^("dataid") := "2124972605", ^.title := "Modifier la saisie", ^.tabindex := "0")),
        <.div( ^.classes := "avatarSmall ", ^("data") := "", ^.backgroundImage := "url('http://doodle.com/builtstatic/1464857356/doodle/graphics/avatar.png')",
        <.div( ^.classes := "gravatar", ^.backgroundImage := "url('https://secure.gravatar.com/avatar/?s=30&amp;r=x&amp;d=blank')")),
        <.div( ^.classes := "pname ", ^.title := "Keanu", "Keanu")),
      <.td( ^.classes := "partTableCell n xsep pn", ^.title := "Keanu, Boeuf (CH) au curry jaune, lait de coco, pommes de terre et cacahuÃ¨tes: Non"),
      <.td( ^.classes := "partTableCell y xsep pok", ^.title := "Keanu, Poulet (CH) au curry rouge, lait de coco et courge thaÃ¯: Oui",
        <.img( ^.alt := "Keanu, Poulet (CH) au curry rouge, lait de coco et courge thaÃ¯: Oui", ^.classes := "preferenceTick", ^.src := "http://doodle.com/builtstatic/1464857356/doodle/graphics/polls/tick_pok.png")),
      <.td( ^.classes := "partTableCell n xsep pn", ^.title := "Keanu, Phat sen ma ma (nouilles jaunes) sautÃ©es aux crevettes: Non"),
      <.td( ^.classes := "partTableCell n xsep pn", ^.title := "Keanu, Veg - Tofu sautÃ© aux lÃ©gumes et graines de soja: Non"),
      <.td( ^.classes := "partTableCell n sep pn", ^.title := "Keanu, Soupe de nouilles de riz au porc (CH): Non")),
    <.tr( ^.classes := "participation yesNo",
      <.td( ^.classes := "pname",
        <.div( ^.width := "182px",
          <.div( ^.classes := "avatarSmall", ^.backgroundImage := "url('https://6a5edc300520d4037dd6-0732807511066685711db213ddc1d2df.ssl.cf2.rackcdn.com/ziiews13v6v78g0ovi81wz4libj4vgn8')"),
          <.label( ^.classes := "hiddenAcc", ^.forid :="pname", ^.title := "l10n_yourName"),
          <.input( ^.classes := "form-control", ^.id := "pname", ^.maxlength :="64", ^.name :="name", ^.placeholder:="Votre nom", ^.tpe :="text", ^.value :=""))),
      <.td( ^.classes := "xsep", ^("data") := "0", ^.id := "box0", ^.title := "Boeuf (CH) au curry jaune, lait de coco, pommes de terre et cacahuètes",
        <.label( ^.classes := "hiddenAcc", ^.forid :="option0","Boeuf (CH) au curry jaune, lait de coco, pommes de terre et cacahuÃ¨tes"),
        <.input( ^.id := "option0", ^.name :="p", ^.tpe :="checkbox")),
      <.td( ^.classes := "xsep", ^("data") := "1", ^.id := "box1", ^.title := "Poulet (CH) au curry rouge, lait de coco et courge thaÃ¯",
        <.label( ^.classes := "hiddenAcc", ^.forid :="option1","Poulet (CH) au curry rouge, lait de coco et courge thaÃ¯"),
        <.input( ^.id := "option1", ^.name :="p", ^.tpe :="checkbox")),
      <.td( ^.classes := "xsep", ^("data") := "2", ^.id := "box2", ^.title := "Phat sen ma ma (nouilles jaunes) sautÃ©es aux crevettes",
        <.label( ^.classes := "hiddenAcc", ^.forid :="option2","Phat sen ma ma (nouilles jaunes) sautÃ©es aux crevettes"),
        <.input( ^.id := "option2", ^.name :="p", ^.tpe :="checkbox")),
      <.td( ^.classes := "xsep", ^("data") := "3", ^.id := "box3", ^.title := "Veg - Tofu sautÃ© aux lÃ©gumes et graines de soja",
        <.label( ^.classes := "hiddenAcc", ^.forid :="option3","Veg - Tofu sauté aux légumes et graines de soja"),
        <.input( ^.id := "option3", ^.name :="p", ^.tpe :="checkbox")),
      <.td( ^.classes := "", ^("data") := "4", ^.id := "box4", ^.title := "Soupe de nouilles de riz au porc (CH)",
        <.label( ^.classes := "hiddenAcc", ^.forid :="option4","Soupe de nouilles de riz au porc (CH)"),
        <.input( ^.id := "option4", ^.name :="p", ^.tpe :="checkbox"))),
    <.tr( ^.classes := "sums",
      <.td( ^.classes := "nonHeader"),
      <.td( ^.classes := " ",  "0" ),
      <.td( ^.classes := " ",
        <.b( "2" )),
      <.td( ^.classes := " ", "0" ),
      <.td( ^.classes := " ", "0" ),
      <.td( ^.classes := " ", "0" )
    )
  )
)

  def render = {
    WebPage(page, css)
  }
}


