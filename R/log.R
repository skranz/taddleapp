log.action = function(action,  ...,  tatid=app$tat$tatid, email=app$tat$email,app=getApp()) {
  restore.point("action.log")
  log_time = format(Sys.time())
  log.file = file.path(app$glob$log.dir, paste0("taddle-",format(Sys.Date(),"%y-%m"),".log"))

  if (is.null(tatid) & is.null(email)) {
    args = list(action=action,log_time=log_time, sid=app$session_code, ...)
  } else if (is.null(email)) {
    args = list(action=action,log_time=log_time, tatid=tatid, sid=app$session_code, ...)
  } else {
    args = list(action=action,log_time=log_time,tatid=tatid, email=email, sid=app$session_code, ...)
  }

  txt = toJSON(args)
  try(write(txt,log.file,append=TRUE))
}
