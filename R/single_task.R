examples.single.task = function() {
  library(taddleapp)
  restore.point.options(display.restore.point=TRUE)
  setwd("C:/libraries/taddle/")

  stud.login = make.login.args(fixed.password="stud", , help.text="Please enter the password you received in the email.")
  admin.login = make.login.args(fixed.password="test", login.title="Please enter the administration password.")

  app = taddleApp("C:/libraries/taddle/shared", custom.ui = custom.ui, app.title = "ESP Course Selection", single.task=TRUE, just.methods="serialdict", strings=taddle.strings(Topic="Course"), stud.login=stud.login, admin.login = admin.login)
  viewApp(app, url.args = list(role="admin"))
  viewApp(app)

}


show.single.task.ui = function(..., app=getApp(), session=app$session) {
  restore.point("show.single.task.ui")
  glob = app$glob

  app$task = glob$task
  if (file.exists(glob$task.file)) {
    task = readRDS(glob$task.file)
    app$task = glob$task = task
  } else {
    app$task = glob$task = NULL
  }

  if (app$is.admin) {
    show.single.task.admin.ui()
  } else {
    show.single.task.stud.ui()
  }
}

show.single.task.admin.ui = function(app=getApp()) {
  restore.point("show.single.task.admin.ui")
  if (is.null(app$task)) {
    app$tat = empty.tat()
    log.action("create_session", email=NULL)
    show.new.ui()
    return()
  }
  app$tat = get.res.tat(app$task$tatid)
  show.res.ui(show.new = TRUE)
}

reset.deadline.click = function(...) {
  tags$table(
      tags$td(shiny::dateInput("reset_deadline_date","Deadline Date", value=tat$deadline_date)),
      tags$td(style="padding-left: 2em;", simpleTimeInput("reset_deadline_time", "Deadline Time", width="12em", value=tat$deadline_time))
  )

}

# need tp update for deadline
is.task.running = function(app=getApp()) {
  isTRUE(app$task$is.running)
}

no.task.ui = function(app=getApp()) {
  tagList(
    h3("Currently no choice can be made.")
  )
}

show.single.task.stud.ui = function(app=getApp()) {
  restore.point("show.single.task.stud.ui")
  if (!is.task.running()) {
    ui = no.task.ui()
    setUI("mainUI",ui)
    return()
  }
  query=app$query
  if (!is.null(app$query$crank)) {
    app$tat = get.stud.tat(studkey=query$crank)
    show.rank.ui()
  } else {
    app$tat = get.rank.tat(rankkey=app$task$rankkey)
    show.rank.ui()
  }
}


update.task.file = function(tat=getApp()$tat, is.running=TRUE, app=getApp()) {
  restore.point("update.task.file")
  task = list(tatid=tat$tatid, rankkey=tat$rankkey, deadline=tat$deadline, is.running=is.running)
  saveRDS(task, app$glob$task.file)
}
