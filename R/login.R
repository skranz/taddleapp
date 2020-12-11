taddle.show.login = function(app = getApp()) {
  restore.point("taddle.show.login")
  if (app$is.admin) {
    lop = app$glob$admin.lop
  } else {
    lop = app$glob$stud.lop
  }
  lop.login.handlers(lop)
  show.login.ui(lop)

}

taddle.login.fun = function(..., app=getApp()) {
  show.taddle.ui()
}

admin.login.fun = function(..., app=getApp()) {
  restore.point("admin.login.fun")
  if (app$glob$single.task) {
    show.single.task.admin.ui()
  } else {
    show.taddle.ui()
  }
}

stud.login.fun = function(..., app=getApp()) {
  if (app$glob$single.task) {
    show.single.task.stud.ui()
  } else {
    show.taddle.ui()
  }
}

make.login.args = function(fixed.password=NULL,db.name=NULL, just.pw=!is.null(fixed.password), need.userid = !just.pw, use.signup = !just.pw, ... , app=getApp()) {
  nlist(fixed.password, db.name, need.userid, use.signup, ...)
}

make.taddle.lop = function(login.args=NULL, app=getApp()) {
  library(shinyEventsLogin)
  if (is.null(login.args)) return(NULL)
  glob = app$glob

  restore.point("make.taddle.lop")
  login.args = replace.empty.elements(login.args,
    list(app.title = glob$app.title)
  )
  login.args$container.id = "mainUI"
  login.args$login.fun = taddle.login.fun

  login.args$smtp = list(from=glob$email.sender, smtp=list(host=glob$smtp.server))

  lop  = do.call(loginModule, login.args)
  lop
}
