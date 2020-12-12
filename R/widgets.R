colored.html = function(txt, color="#cc0000") {
  if (is.null(color)) return(txt)
  paste0("<font color='",color,"'>",txt,"</font>")
}

simpleTimeInput = function(id,label=NULL, value="", width=NULL) {
  input = tags$input(type = "time", class = "form-control",name=id,id=id, value=value)
  inputLabel = if (!is.null(label)) tags$label(label, class = "control-label", class = if (is.null(label)) "shiny-label-null", `for` = id)

  tags$div(id = paste0("div-id"), class = "shiny-time-input form-group shiny-input-container", style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"), inputLabel, input)


}

simpleTable = function(id=random.string(1), df, col.names=colnames(df), col.tooltips=FALSE, class="simple-table", format.values = TRUE, wrap=TRUE, signif.digits=8, round.digits=8, row.data = list(rowid = seq_len(NROW(df))), sel.row=NULL, row.class=rep("", NROW(df))) {
  restore.point("simpeleTable")
  n = NROW(df)

  if (isTRUE(col.names)) {
    col.names = col.names(df)
  }
  has.colnames = is.character(col.names)

  if (has.colnames) {
    if (is.null(col.tooltips)) {
      inner = col.names
    } else {
      inner = paste0('<span title="', col.tooltips,'">', col.names, '<span>')
    }
    head = paste0('<th class=simple-th>',inner,'</th>', collapse="")
    head = paste0('<tr>', head, '</tr>')
  } else {
    head = ""
  }

  my.format.vals = function(vals) {
    format.vals(vals, signif.digits=signif.digits, round.digits=round.digits)
  }

  td.class = rep("simple-td", NROW(df))
  if (length(td.class)>0) {
    td.class[length(td.class)]="simple-td-bottom"
  }

  nowrap = if (wrap) "''" else "'nowrap'"
  cols = 1:NCOL(df)
  rows = seq_len(NROW(df))

  rdata = if (!is.null(row.data)) {
    str = ""
    for (i in seq_along(row.data)) {
      str = paste0(str, " data-",names(row.data)[i],"=\",",row.data[i],",\"")
    }
    str
  }

  sel.row.class = NULL
  if (!is.null(sel.row)) {
    sel.row.class = rep("",NROW(df))
    sel.row.class[sel.row] = " sel-row"
  }


  code = paste0('"<td class=\\"",td.class," row-",rows," col-',cols,'\\" ", ', nowrap,', ">", my.format.vals(df[[',cols,']]),"</td>"', collapse=",")
  code = paste0('paste0("<tr ',rdata,' class=\\"row-", rows, sel.row.class, " ", row.class, "\\">",',code,',"</tr>", collapse="\\n")')
  call = parse(text=code)
  main = eval(parse(text=code))

  #cols =  paste0(rep("<col>",NCOL(df)), collapse="")
  tab = paste0('<table id="',id,'" class="', class,'">\n <thead>\n',head,'\n</thead>\n<tbody>', main, "\n</tbody>\n</table>")
  tab

}

fontAwesomeHeader = function() {
  htmlDependency("font-awesome", "4.7.0", c(href = "shared/font-awesome"), stylesheet = "css/font-awesome.min.css")
}

tableSelectInputVector = function (inputId, choices, selected=1, width=NULL, value=NULL, extra.class = "", ...)  {
  restore.point("selectizeInputVector")

  code = rep("", length(inputId))

  choices.lab = names(choices)
  if (is.null(choices.lab)) choices.lab = choices


  if (is.null(width)) {
    chars = max(nchar(choices.lab))
    chars = min(20, chars)
    chars = max(4, round(0.8)*chars)
    width=paste0(chars,"em")
  }

  style = paste0("width: ", width,";")


  if (length(value)<=1 & !is.list(selected)) {
    if (!is.null(value)) {
      selected = match(value[[1]], choices)
    }
    selected.str = rep("", length(choices))
    selected.str[selected] = "selected"

    options.str = paste0(collapse="\n",
      '<option value="',choices,'" ',  selected.str,'>',
      choices.lab,'</option>'
    )
  } else if (!is.null(value)) {
    options.str = unlist(lapply(value, function(val) {
      selected.str = rep("", length(choices))
      selected = match(val, choices)
      selected.str[selected] = "selected"

      paste0(collapse="\n",
        '<option value="',choices,'" ',  selected.str,'>',
        choices.lab,'</option>'
      )
    }))

  } else {
    stop("selected as list not yet implemented")
  }

  code = paste0('
    <select id="',inputId,'" class="', extra.class,'">
    ',options.str,'
  </div>
  ')
  code
}

# from webforms
simpleButtonVector = function(id, label="",icon=NULL, size=c("default","sm","xs")[1], class=paste0("btn btn-default action-button", if (size != "default") paste0(" btn-",size)), extra.class = "", extra.head="") {
  if (is.null(icon)) {
    icon=""
  }
  paste0('<button id="',id,'" type="button" class="',class, ' ',extra.class,'" ',extra.head,'>',icon,label,'</button>')
}

# from webforms
checkBoxInputVector = function (inputId, label=NULL, value = FALSE, extra.class="",extra.head = "", wrap.shiny=FALSE,...)  {
  restore.point("checkBoxInputVector")
  code = rep("", length(inputId))
  checked.str = ifelse(value,' checked="checked"',"")
  value = rep(value, length.out=length(code))
  input.code = paste0('<input id="',inputId,'" type="checkbox" ', checked.str,' class="', extra.class,'" ', extra.head,'/>')

  if (!wrap.shiny) {
    return(input.code)
  }

  code = paste0('
<div class="form-group shiny-input-container">
  <div class="checkbox">
    ',
    #if(is.null(label)) {
    #  input.code
    #} else {
      paste0('<label>', input.code,'
      <span>',label,'</span>
      </label>')
    #},'
    ,'
  </div>
</div>
  ')
  code
}

# from webforms
simpleSelect = function(inputId, label, choices, selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL, class = if (!selectize) "form-control", extra.class="", style=if (!is.null(width)) paste0("width: ", width)) {
  class = paste0(class, " ", extra.class)

  selectTag <- tags$select(id = inputId, size = size, shiny:::selectOptions(choices, selected), class=class, style=style)
  if (multiple)
    selectTag$attribs$multiple <- "multiple"
  selectTag
}

# from shinyEventsUI

timedMessage = function(id,msg="",html=msg,ui=HTML(html), millis=3000, empty.msg = "", empty.ui=HTML(empty.msg), app=getApp()) {
  restore.point("timedMessage")
  try({
    setUI(id, ui)
    dsetUI(id, ui)
  })

  obs.id = paste0("..timedMessage..",id)
  flag.id = paste0("flag", obs.id)
  app[[flag.id]] = FALSE

  # destroy old observer
  if (!is.null(app[[obs.id]])) try(app[[obs.id]]$destroy())

  if (!is.finite(millis)) return()

  app[[obs.id]] = observe({
    if (!isTRUE(app[[flag.id]])) {
      app[[flag.id]] = TRUE
      invalidateLater(millis)
      return()
    }
    try(app[[obs.id]]$destroy())
    try({
      setUI(id, empty.ui)
      dsetUI(id, empty.ui)
    })
  })
}
