examples.taddleApp = function() {
  restore.point.options(display.restore.point=TRUE)
  setwd("D:/libraries/taddle/")
  app = taddleApp("D:/libraries/taddle/shared")
  viewApp(app, url.args = list(rank="owcmqh"))
  viewApp(app)
}


taddleApp = function(taddle.dir, db.dir = file.path(taddle.dir, "db"), email.sender = "taddle@uni-ulm.de", ignore.methods =c("costmin_cubic"), base.url = "taddle.mathematik.uni-ulm.de", ...) {
  restore.point("clickerClientApp")
  app = eventsApp()

  glob = app$glob

  glob$base.url = base.url
  glob$email.sender = email.sender
  glob$taddle.dir = taddle.dir
  glob$db.dir = db.dir

  glob$log.file = file.path(taddle.dir,"logs/taddle.log")

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
    h4("Who has created this website?"),
    HTML("Taddle has been created and is maintained by <a href='https://www.uni-ulm.de/mawi/mawi-wiwi/institut/mitarbeiter/skranz/' target = '_blank'>Sebastian Kranz, Ulm University</a>. It is part of a research project to study allocation methods."),
    h4("What does 'Taddle' mean?"),
    p("The 'Ta' in Taddle stands for 'Topic Assignment'. The remaining letters have no meaning.")
  )

}
