example.algos = function() {
  n = 10
  T = 10
  topics = 1:8

  # Common utility
  u.com = rnorm(T,0,1)
  u.ind = matrix(rnorm(T*n,0,1), n, T)
  w = 0.8
  u = t(w * u.com + (1-w) * t(u.ind))

  prefs = t(apply(u,1,rank ))


  aspr = assignment.problem.alloc(prefs)

  #prefs = t(replicate(n,sample(topics,replace = FALSE)))
  prios = runif(NROW(prefs))
  sedi = serial.dictator.alloc(prefs, prios)
  sedi

  stars.max = rep(3,n)
  stara = star.range.alloc(prefs, stars.max, prios=prios)
  stara
  prefs
}

#' A simple serial dictator assignment
#' @param prefs A matrix row=students cols=topics
#' @param prios A list of priorities for each student
#' @return A vector with the assigned object for each student. NA means the student did not get an object
serial.dictator.alloc = function(prefs, prios = runif(NROW(prefs))) {
  restore.point("serial.dictator.alloc")
  n = length(prios)
  topics = sort(unique(as.vector(na.omit(prefs))))
  ord = order(-prios)
  res = integer(n)
  i = 1
  for (i in ord) {
    found = intersect(prefs[i,],topics)[1]
    res[i] = found
    topics = setdiff(topics, found)
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


assignment.problem.alloc = function(prefs, rank.costs=seq_len(max(prefs))^2, no.match.cost = max(rank.costs)*1000) {
  restore.point("assignment.problem.algo")

  n = NROW(prefs)
  T = NCOL(prefs)


  costs = matrix(rank.costs[prefs],n,T)
  if (n>T) {
    costs = cbind(costs, matrix(no.match.cost,n,n-T))
  }
  res = solve_LSAP(costs)
  res[res>T] = NA
  as.integer(res)
}
