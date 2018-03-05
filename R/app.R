examples.taddleApp = function() {
  restore.point.options(display.restore.point=TRUE)
  setwd("D:/libraries/taddle/")
  app = taddleApp("D:/libraries/taddle/shared")
  viewApp(app, url.args = list(rank="owcmqh"))
  viewApp(app)
}


taddleApp = function(taddle.dir, db.dir = file.path(taddle.dir, "db"), email.sender = "taddle@uni-ulm.de", smtp.server = "", ignore.methods =c("costmin_cubic"), base.url = "http://taddle.mathematik.uni-ulm.de", app.title = "Taddle: Easily Allocate Seminar Topics",  ...) {
  restore.point("clickerClientApp")
  app = eventsApp()

  glob = app$glob

  glob$base.url = base.url
  glob$email.sender = email.sender
  glob$smtp.server = smtp.server
  glob$taddle.dir = taddle.dir
  glob$db.dir = db.dir

  glob$log.dir = file.path(taddle.dir,"logs")

  glob$db = dbConnect(RSQLite::SQLite(), file.path(glob$db.dir, "taddledb.sqlite"))
  glob$db = set.db.schemas(glob$db, schema.file = system.file("schema/taddledb.yaml", package="taddleapp"))

  #sets = read.yaml(system.file("yaml/sets.yaml", package="taddleapp"))
  #glob$sets = lapply(sets,unlist)
  glob$sets = readRDS(system.file("yaml/sets.Rds", package="taddleapp"))
  glob$sets$method = glob$sets$method[!glob$sets$method %in% ignore.methods]

  shiny::addResourcePath("taddle",system.file("www", package="taddleapp"))

  #css.file = system.file("www/taddle.css", package="taddleapp")
  app$ui = fluidPage(theme=shinytheme("cerulean"),
    tags$head(
      tags$title(app.title),
      tags$link(rel="stylesheet", type="text/css", href="taddle/taddle.css")
    ),
    sparkline:::spk_dependencies(),
    fontAwesomeHeader(),
    #mathjaxHeader(),
#   includeCSS(css.file),
    uiOutput("mainUI"),
    tagList(tags$script(src="taddle/topn.js")),
    tagList(tags$script(src="taddle/taddle.js"))
  )

  appInitHandler(function(...,session=app$session,app=getApp()) {
    app$session_code = random.string(1)
    observe(priority = -100,x = {
      cat("Parse query string...")
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query$rank)) {
        app$tat = get.rank.tat(rankkey=query$rank)
        show.rank.ui()
      } else if (!is.null(query$crank)) {
        app$tat = get.stud.tat(studkey=query$crank)
        show.rank.ui()
      } else if (!is.null(query$key)) {
        app$tat = get.res.tat(tatid=query$key)
        show.res.ui()
      } else {
        app$tat = empty.tat()
        log.action("create_session")
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


about.ui = function() {
  ui = tagList(
    h3("Taddle: Easily Allocate Seminar Topics"),
    h4("Who has created this website?"),
    HTML("Taddle has been created and is maintained by <a href='https://www.uni-ulm.de/mawi/mawi-wiwi/institut/mitarbeiter/skranz/' target = '_blank'>Sebastian Kranz, Ulm University</a>. It is part of a research project to study allocation methods."),
    h4("What does 'Taddle' mean?"),
    p("The 'Ta' in Taddle stands for 'Topic Assignment'. The remaining letters have no meaning."),
    h4("Impressum:"),
    p("Sebastian Kranz"),
    p("Universitaet Ulm, 89069 Ulm"),
    p("Email: sebastian.kranz(at)uni-ulm.de"),
    h4("Disclaimer:"),
    p("THE WEBSITE IS CREATED IN THE HOPE THAT IT WILL BE USEFUL, BUT WITHOUT ANY WARRANTY. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW THE AUTHOR WILL BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE WEBSITE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES), EVEN IF THE AUTHOR HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.")


  )

}
taddle.send.email = function(to, subject, body,from=app$glob$email.sender, smtp.server = app$glob$smtp.server, app=getApp()) {
  restore.point("taddle.send.email")
  if (is.empty.val(smtp.server)) {
    cat("\nNo smtp server specified.")
    return()
  }
  try(sendmailR::sendmail(from=from, to=to, subject=subject, msg=sep.lines(body), control=list(smtpServer=smtp.server)))
}
