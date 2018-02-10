examples.taddleApp = function() {
  restore.point.options(display.restore.point=TRUE)

  setwd("D:/libraries/taddle/")
  app = taddleApp("D:/libraries/taddle/shared")
  viewApp(app, url.args = list(key="wBHxoAvHhvqVTYeHNtwa"))
  viewApp(app)

  create.random.ranks("zdaqjj", common.weight = 0.5)
}

show.res.ui = function(tat = app$tat, app=getApp(),...) {
  ui = navlistPanel(id="mainPanel",
    tabPanel("Status",value="home", res.home.ui()),
    tabPanel("About", about.ui()),
    widths = c(2,10)
  )
  setUI("mainUI", ui)
}


res.home.ui = function(...,tat=app$tat, app=getApp(), glob=app$glob) {
  restore.point("res.home.ui")
  diff.str = duration.string(Sys.time(),tat$deadline)

  ct.ui = allocs.count.table.ui(tat)

  ui = tagList(
    h4(paste0(tat$title)),
    p(HTML(paste0("Deadline: ", format(tat$deadline,"%A, %B %d at %H:%M"), " (",diff.str,")"))),
    p(HTML("So far ", tat$num.sub, " submissions for ", tat$num.topics, " topics.")),
    h4("Overview of results depending on allocation mechanism:"),
    HTML(ct.ui),
    uiOutput("resUI")
  )

  res.ui = allocation.info.ui("costmin_lin", tat)
  setUI("resUI", res.ui)

  #setUI("mainUI", ui)
  ui
}

get.res.tat = function(tatid, db=getApp()$glob$db) {
  restore.point("get.res.tat")
  tat = dbGet(db,"tat", list(tatid=tatid))
  tat = as.list(tat[1,])

  tops = dbGet(db, "topic",list(tatid=tatid))
  stu = dbGet(db, "student", list(tatid=tatid))
  ras = dbGet(db, "ranking", list(tatid=tatid))
  ras = left_join(ras, select(stu, studemail, studname), by="studemail")

  tat$stu = stu
  tat$ras = ras
  tat$tops = tops

  tat$num.sub = NROW(tat$stu)
  tat$num.topics = NROW(tops)

  tat$allocs =compute.tat.allocations(tat)
  tat = as.environment(tat)
  tat
}

allocs.count.table.ui = function(tat=app$tat, app=getApp()) {
  restore.point("allocs.count.table.ui")
  df = allocs.count.table(tat)
  mat = as.matrix(df)
  mat[mat=="0"] = ""

  mat[,1] = to.label(df$method, app$glob$sets$method)

  df = as.data.frame(mat)
  html = rmdtools::html.table(df,col.names = c("", paste0("Rank ", colnames(mat[,-1]))) )
  HTML(html)

}

allocs.count.table = function(tat=app$tat, app=getApp()) {
  restore.point("allocs.count.table")
  allocs = tat$allocs
  methods = intersect(app$glob$sets$method, unique(allocs$method))

  all = expand.grid(method=methods, rank=1:max(allocs$rank, na.rm=TRUE))

  sum = allocs %>% group_by(method, rank) %>%
    summarize(count = n()) %>%
    right_join(all, by=c("method","rank")) %>%
    spread(rank, count)
  sum[is.na(sum)] = 0L
  sum = sum[match(sum$method, methods),]
  sum
}


compute.tat.allocations = function(tat=app$tat, app=getApp()) {
  restore.point("compute.tat.allocations")
  methods = unlist(app$glob$sets$method)
  allocs = bind_rows(lapply(methods, compute.tat.allocation, tat=tat))
  return(allocs)
}

compute.tat.allocation = function(method = "costmin_lin", tat) {
  restore.point("compute.tat.allocation")
  ras = tat$ras

  ras = arrange(ras, studemail, pos)
  studs = unique(ras$studemail)
  n = length(studs)
  T = max(ras$pos)



  prefs = matrix(ras$rank, nrow=n, ncol=T, byrow = TRUE)

  if (method == "serialdict") {
    alloc = serial.dictator.alloc(prefs)
  } else if (method == "costmin_lin") {
    alloc = assignment.problem.alloc(prefs,rank.costs = 1:T)
  } else if (method == "costmin_quad") {
    alloc = assignment.problem.alloc(prefs,rank.costs = (1:T)^2)
  } else if (method == "costmin_cubic") {
    alloc = assignment.problem.alloc(prefs,rank.costs = (1:T)^3)
  }

  rank = prefs[cbind(1:n,alloc)]
  topics = arrange(tat$tops, pos)$topic

  res = data_frame(method = method, studemail=studs,rank=rank, pos=alloc, topic=topics[pos])

  res
}

allocation.info.ui = function(method = tat$method, tat=app$tat, app=getApp(), use.sparklines=TRUE) {
  restore.point("allocation.info.ui")

  .method = method
  alloc = filter(tat$allocs, method==.method) %>%
    arrange(pos) %>% left_join(tat$stu, by="studemail")

  todf = select(alloc, pos, topic, studname, rank)

  # Create sparklines
  ras = tat$ras
  max.rank = max(ras$rank)
  cc = rep("blue", max.rank)

  todf$sl = lapply(unique(todf$pos), function(.pos) {
    restore.point("jhkjahdkjshkfh")
    ranks = filter(ras, pos==.pos)$rank
    ccn = cc
    ccn[todf$rank[.pos]] = "red"
    spk_chr(tabulate.to(ranks,max.rank), type="bar", colorMap = ccn, tooltipFormat = '{{value}} ranked as 1+{{offset}}')
  })

  tohtml = rmdtools::html.table(todf,col.names = c("","Topic","Student","Ranked as","Topic Ranks"))

  mlab = to.label(method, app$glob$sets$method)
  ui = tagList(
    h4(paste0("Topic allocation via ", mlab)),
    HTML(tohtml)
  )


}
