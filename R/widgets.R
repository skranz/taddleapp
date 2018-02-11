colored.html = function(txt, color="#cc0000") {
  if (is.null(color)) return(txt)
  paste0("<font color='",color,"'>",txt,"</font>")
}

simpleTimeInput = function(id,label=NULL, value="", width=NULL) {
  input = tags$input(type = "time", class = "form-control",name=id, value=value)
  tags$div(id = id, class = "shiny-time-input form-group shiny-input-container", style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"), if (!is.null(label)) shiny:::controlLabel(id, label), input)


}

simpleTable = function(id=random.string(1), df, col.names=colnames(df), col.tooltips=FALSE, class="simple-table", format.values = TRUE, wrap=FALSE, signif.digits=8, round.digits=8, row.data = list(rowid = seq_len(NROW(df)))) {
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

  nowrap = if (wrap) "" else "nowrap"
  cols = 1:NCOL(df)
  rows = seq_len(NROW(df))

  rdata = if (!is.null(row.data)) {
    str = ""
    for (i in seq_along(row.data)) {
      str = paste0(str, " data-",names(row.data)[i],"=\",",row.data[i],",\"")
    }
    str
  }

  code = paste0('"<td class=\\"",td.class," row-",rows," col-',cols,'\\" ", ', nowrap,', ">", my.format.vals(df[[',cols,']]),"</td>"', collapse=",")
  code = paste0('paste0("<tr ',rdata,' class=\\"row-", rows,"\\">",',code,',"</tr>", collapse="\\n")')
  call = parse(text=code)
  main = eval(parse(text=code))

  #cols =  paste0(rep("<col>",NCOL(df)), collapse="")
  tab = paste0('<table id="',id,'" class="', class,'">\n <thead>\n',head,'\n</thead>\n<tbody>', main, "\n</tbody>\n</table>")
  tab

}

fontAwesomeHeader = function() {
  htmlDependency("font-awesome", "4.7.0", c(href = "shared/font-awesome"), stylesheet = "css/font-awesome.min.css")
}
