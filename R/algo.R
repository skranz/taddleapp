example.algos = function() {
  n = 20
  T = 10
  topics = 1:8
  slots = rep(2,T)
  slots[1] = 1
  slots[2] = 0
  slots[7] = 1

  # Common utility
  u.com = rnorm(T,0,1)
  u.ind = matrix(rnorm(T*n,0,1), n, T)
  w = 0.8
  u = t(w * u.com + (1-w) * t(u.ind))

  prefs = t(apply(u,1,rank ))


  aspr = assignment.problem.alloc(prefs, slots=slots)
  aspr

  #prefs = t(replicate(n,sample(topics,replace = FALSE)))
  prios = runif(NROW(prefs))
  sedi = serial.dictator.alloc(prefs, prios, slots=slots)
  sedi
}

#' A simple serial dictator assignment
#' @param prefs A matrix row=students cols=topics
#' @param prios A list of priorities for each student
#' @return A vector with the assigned object for each student. NA means the student did not get an object
serial.dictator.alloc = function(prefs, prios = runif(NROW(prefs)), slots=rep(1,NCOL(prefs)), return.pref.ind = TRUE) {
  restore.point("serial.dictator.alloc")
  n = length(prios)
  T = NCOL(prefs)
  topics = (1:NCOL(prefs))[slots>0]


  ord = order(-prios)
  # get the person orders
  pers = 1:n
  pers[ord] = 1:n

  res = rep(NA_integer_,n)

  i = 1
  for (i in pers) {
    found = topics[which.min(prefs[i, topics])]
    if (length(found)==1) {
      res[i] = found
      slots[found] = slots[found]-1
      if (slots[found]<=0)
        topics = setdiff(topics, found)
    }
  }
  res
}


# First step: Move stars
# Second step: Assign single stars
# Third step: Assign remaining slots via serial.dictator.alloc
star.range.alloc = function(prefs, stars.max, stars.start=prefs[,1], prios = runif(NROW(prefs)), details=FALSE, verbose=TRUE) {
  restore.point("save.star.alloc")
  n = length(prios)
  topics = 1:max(prefs)


  # 1. Move stars if a better position is free

  # Set stars to their starting position
  stars = stars.start

  # Count of stars for each topic
  tosta = tabulate(c(stars, topics))-1

  ord = sample.int(n)
  has.moved = TRUE
  while(has.moved) {
    has.moved = FALSE

    for (i in ord) {
      pref = prefs[i,]
      # Has currently single star on topic
      if (tosta[stars[i]]==1) {
        star.ind = which(pref == stars[i])
        # Get already best topic for sure
        if (star.ind == 1) next
        # Higher preference as current star
        hpref = pref[seq_len(star.ind-1)]
        free = which(tosta[hpref]==0)
      # No single star on current position
      } else {
        hpref = pref[seq_len(stars.max[i])]
        free = which(tosta[hpref]==0)
      }
      if (length(free)>0) {
        if (verbose)
          cat("\nStudent", i ,"moves star from topic" , stars[i], "(num. ", tosta[stars[[i]]], ") ")
        tosta[stars[i]] = tosta[stars[i]]-1
        stars[i] = hpref[free[1]]
        if (verbose)
          cat("to topic", stars[i])
        tosta[stars[i]] = tosta[stars[i]]+1
        has.moved = TRUE
      }
    }
  }

  # Assign single star topics
  res = integer(n)
  stopics = which(tosta==1)
  sstud = match(stopics, stars)
  res[sstud] = stopics

  if (verbose) {
    cat(paste0("\nStudent ",sstud, " got topic ", stopics, " via single star.", collapse=""))
  }

  # Assign remaining topics
  # using serial random dictatorship
  rem.topics = setdiff(topics, stopics)
  rem.stud = which(res == 0)

  ord = order(-prios)
  ord = ord[ord %in% rem.stud]
  i = 1
  for (i in ord) {
    found = intersect(prefs[i,],rem.topics)[1]
    res[i] = found
    topics = setdiff(topics, found)
  }

  if (details) {
    return(list(alloc=res, star.topics = setdiff(topics, rem.topics)))
  }
  # Return res
  res
}


assignment.problem.alloc = function(prefs, rank.costs=seq_len(max(prefs))^2, slots=rep(1,NCOL(prefs)), no.match.cost = max(rank.costs)*1000) {
  restore.point("assignment.problem.algo")

  has.na = any(is.na(prefs))
  n = NROW(prefs)
  S = sum(slots)
  T = NCOL(prefs)

  multi.slot = any(slots != 1)

  costs = matrix(rank.costs[prefs],n,T)
  if (multi.slot) {

    slot.cols = unlist(lapply(seq_along(slots), function(t) rep(t, slots[t])))
    costs = costs[,slot.cols, drop=FALSE]
  }

  if (n>S) {
    costs = cbind(costs, matrix(no.match.cost,n,n-S))
  }

  # Not all topics are neccessarily ranked
  if (has.na) {
    not.cost = ceiling(max(costs, na.rm=TRUE)*(n+1))
    costs[is.na(costs)] = not.cost
  }

  res = solve_LSAP(costs)
  res[res>S] = NA
  res = as.integer(res)

  # Map back to topics
  if (multi.slot) {
    res = slot.cols[res]
  }

  # Set students without matched topics to NA
  if (has.na) {
    na.res = is.na(prefs[cbind(1:n,res)])
    res[na.res] = NA_integer_
  }

  res
}
