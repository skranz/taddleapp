example.algos = function() {
  n = 10
  T = 10
  topics = 1:8

  # Common utility
  u.com = rnorm(T,0,1)
  u.ind = matrix(rnorm(T*n,0,1), n, T)
  w = 0.5
  u = t(w * u.com + (1-w) * t(u.ind))

  prefs = t(apply(u,1,rank ))


  aspr = assignment.problem.alloc(prefs)

  #prefs = t(replicate(n,sample(topics,replace = FALSE)))
  prios = runif(NROW(prefs))
  sedi = serial.dictator.alloc(prefs, prios)

  jokers.max = rep(5,n)

  joal = save.joker.alloc(prefs, jokers.max, prios=prios)
  joal
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


# First step: Move jokers
# Second step: Assign based on jokers
# Third step: Assign remaining slots via serial.dictator.alloc

save.joker.alloc = function(prefs, jokers.max, jokers.start=prefs[,1], prios = runif(NROW(prefs)), details=FALSE, verbose=TRUE) {
  restore.point("save.joker.alloc")
  n = length(prios)
  topics = 1:max(prefs)


  # 1. Move jokers if a better position is free

  # Set jokers to their starting position
  jokers = jokers.start

  # Count of jokers for each topic
  top.jok = tabulate(c(jokers, topics))-1

  ord = sample.int(n)
  has.moved = TRUE
  while(has.moved) {
    has.moved = FALSE

    for (i in ord) {
      pref = prefs[i,]
      # Has currently single joker on topic
      if (top.jok[jokers[i]]==1) {
        joker.ind = which(pref == jokers[i])
        # Get already best topic for sure
        if (joker.ind == 1) next
        # Higher preference as current joker
        hpref = pref[seq_len(joker.ind-1)]
        free = which(top.jok[hpref]==0)
      # No single joker on current position
      } else {
        hpref = pref[seq_len(jokers.max[i])]
        free = which(top.jok[hpref]==0)
      }
      if (length(free)>0) {
        if (verbose)
          cat("\nStudent", i ,"moves joker from", jokers[i], "(num. ", top.jok[jokers[[i]]], ") ")
        top.jok[jokers[i]] = top.jok[jokers[i]]-1
        jokers[i] = hpref[free[1]]
        if (verbose)
          cat("to", jokers[i], "(num. ", top.jok[jokers[[i]]], ") ")
        top.jok[jokers[i]] = top.jok[jokers[i]]+1
        has.moved = TRUE
      }
    }
  }

  # Assign based on jokers
  res = integer(n)
  jtopics = which(top.jok>0)

  for (t in jtopics) {
    if (top.jok[t]==1) {
      i = which(jokers == t)
      res[i] = t
    } else if (top.jok[t]>1) {
      i = sample(which(jokers==t),1)
      res[i] = t
    }
  }

  # Assign remaining topics
  # using serial random dictatorship
  rem.topics = setdiff(topics, jtopics)
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
    return(list(alloc=res, joker.topics = setdiff(topics, rem.topics)))
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
    costs = cbind(costs, matrix(no.match.costs,n,n-T))
  }
  res = solve_LSAP(costs)
  as.integer(res)
}
