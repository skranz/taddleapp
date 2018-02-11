log.action = function(action,  ...,  tatid=app$tat$tatid, email=app$tat$email, log.file = app$glob$log.file,app=getApp()) {
  restore.point("action.log")
  log_time = format(Sys.time())

  args = list(action=action,log_time=log_time,tatid=tatid, email=email, ...)
  txt = toJSON(args)
  try(write(txt,log.file,append=TRUE))
}
