examples.taddleApp = function() {
  restore.point.options(display.restore.point=TRUE)
  setwd("D:/libraries/taddle/")
  app = taddleApp("D:/libraries/taddle/shared")
  viewApp(app, url.args = list(rank="owcmqh"))
  viewApp(app)
}


taddleApp = function(taddle.dir, db.dir = file.path(taddle.dir, "db"), email.sender = "taddle@uni-ulm.de", ignore.methods =c("costmin_cubic"), ...) {
  restore.point("clickerClientApp")
  app = eventsApp()

  glob = app$glob

  glob$email.sender = email.sender
  glob$taddle.dir = taddle.dir
  glob$db.dir = db.dir

  glob$db = dbConnect(RSQLite::SQLite(), file.path(glob$db.dir, "taddledb.sqlite"))
  glob$db = set.db.schemas(glob$db, schema.file = system.file("schema/taddledb.yaml", package="taddleapp"))

  methods.file = system.file("methods/methods.rmd", package="taddleapp")
  methods = rmdtools::read.yaml(methods.file)
  glob$methods = lapply(methods[setdiff(names(methods),ignore.methods)], function(m) {
    m$descr = md2html(m$descr)
    m
  })

  sets = read.yaml(system.file("yaml/sets.yaml", package="taddleapp"))
  glob$sets = lapply(sets,unlist)
  glob$sets$method = glob$sets$method[!glob$sets$method %in% ignore.methods]

  shiny::addResourcePath("taddle",system.file("www", package="taddleapp"))

  txt = read.as.utf8(methods.file)
  yaml  =  parse.hashdot.yaml(txt)

  css.file = system.file("www/taddle.css", package="taddleapp")
  app$ui = fluidPage(theme=shinytheme("cerulean"),
    sparkline:::spk_dependencies(),
    fontAwesomeHeader(),
    mathjaxHeader(),
    includeCSS(css.file),
    uiOutput("mainUI"),
    tagList(tags$script(src="taddle/taddle.js"))
  )

  appInitHandler(function(...,session=app$session,app=getApp()) {
    observe(priority = -100,x = {
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query$rank)) {
        app$tat = get.rank.tat(rankkey=query$rank)
        show.rank.ui()
      } else if (!is.null(query$key)) {
        app$tat = get.res.tat(tatid=query$key)
        show.res.ui()
      } else {
        app$tat = empty.tat()
        show.new.ui()
      }
    })
  })
  app
}


show.step.ui = function(step = 1, app=getApp()) {
  panel = paste0("step", step)
  updateNavlistPanel(app$session, "mainPanel", panel)
}

parse.topic.text = function(topic.text, multi.line = FALSE) {
  restore.point("parse.topic.text")
  txt = str_trim(sep.lines(topic.text))
  txt = txt[nchar(txt)>0]
  txt
}

about.ui = function() {
  ui = tagList(
    h3("Taddle: Easily Allocate Seminar Topics"),
    p("Taddle has been created by Sebastian Kranz, Ulm University as part of a research project to study assignment methods.")
  )

}
