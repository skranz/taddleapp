is.true = function(val) {
  if (length(val)==0)
    return(FALSE)
  val[is.na(val)] = FALSE
  return(val)
}

is.false = function(x) {
  if (!is.logical(x)) return(!isTRUE(x))
  if (is.null(x)) return(FALSE)
   x[is.na(x)] = TRUE
  !x
}

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

with.random.seed = function (expr, seed = 1234567890)
{
    #restore.point("with.random.seed")
    old.seed = get(".Random.seed", .GlobalEnv)
    set.seed(as.integer(seed))
    ret = eval(expr)
    assign(".Random.seed", old.seed, .GlobalEnv)
    runif(1)
    return(ret)
}

# from rmdtools
format.vals = function(vals, signif.digits=NULL, round.digits=NULL) {
  if (is.numeric(vals)) {
    if (is.null(signif.digits) & is.null(round.digits)) {
      return(vals)
    } else if (!is.null(signif.digits) & is.null(round.digits)) {
      return(signif(vals, signif.digits))
    } else if (is.null(signif.digits) & !is.null(round.digits)) {
      return(round(vals, signif.digits))
    } else {
      return(signif(round(vals, round.digits), signif.digits))
    }
  }
  vals
}

replace.empty.elements = function(x, repl) {
  rep.names = setdiff(names(repl),names(x))
  x[rep.names] = repl[rep.names]
  x
}

nlist =function (...) {
    li = list(...)
    li.names = names(li)
    names = unlist(as.list(match.call())[-1])
    if (!is.null(li.names)) {
        no.names = li.names == ""
        names(li)[no.names] = names[no.names]
    }
    else {
        names(li) = names
    }
    li
}
