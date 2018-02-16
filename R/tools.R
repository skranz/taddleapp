tabulate.to = function(x, to=max(x)) {
  y = 1:to
  tabulate(c(x,y))-1
}

duration.string = function(t1,t2, pos.prefix="still ", neg.prefix="overdue ", names = c("years","months","days","hours","minutes")) {
  restore.point("duration.str")
  x = as.period(t1 %--% t2)
  fields = c(
    years = abs(x$year),
    months = abs(x$month),
    days = abs(x$day),
    hours = abs(x$hour),
    minutes = abs(x$minute)
  )
  use = which(fields != 0)
  if (length(use)==0) use = 5
  txt = paste0(fields[use]," ",names[use], collapse=", ")
  if (as.numeric(x) >=0) {
    txt = paste0(pos.prefix,txt)
  } else {
    txt = paste0(neg.prefix, txt)
  }
  txt
}


is.empty.val = function(val) {
  if (length(val)==0) return(TRUE)
  if (is.na(val)) return(TRUE)
  if (isTRUE(nchar(val)==0)) return(TRUE)
  return(FALSE)

}


to.label = function(val, keys, labels = names(keys)) {
  restore.point("to.label")
  if (is.null(keys)) return(val)
  ind = match(val, unlist(keys))
  labels[ind]
}

