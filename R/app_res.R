examples.taddleApp = function() {
  restore.point.options(display.restore.point=TRUE)

  setwd("D:/libraries/taddle/")

  app = taddleApp("D:/libraries/taddle/shared")
  #viewApp(app, url.args = list(key="wBHxoAvHhvqVTYeHNtwa"))
  viewApp(app, url.args = list(key="FqAbbMrWWuQzgmYYccVS"))

  viewApp(app)

  create.random.ranks("zdaqjj", common.weight = 0.25,n = 10)
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

  has.deadline = !is.empty.val(tat$deadline)
  diff.str = if (has.deadline) duration.string(Sys.time(),tat$deadline)

  if (is.null(tat$allocs)) {
    ui = tagList(
      h4(paste0(tat$title)),
      if (has.deadline) p(HTML(paste0("Deadline: ", format(tat$deadline,"%A, %B %d at %H:%M"), " (",diff.str,")"))),
      p(HTML("So far ", tat$num.sub, " submissions for ", tat$num.topics, " topics."))
    )
    return(ui)

  }

  ct.ui = allocs.count.table.ui(tat)

  ui = tagList(
    h4(paste0(tat$title)),
    if (has.deadline) p(HTML(paste0("Deadline: ", format(tat$deadline,"%A, %B %d at %H:%M"), " (",diff.str,")"))),
    p(HTML("So far ", tat$num.sub, " submissions for ", tat$num.topics, " topics.")),
    h4("Overview of allocation mechanisms: Number of students who got their n'th ranked topic"),
    HTML(ct.ui),
    helpText("Click on a row to see the details of the allocation."),
    uiOutput("resUI")
  )

  eventHandler(eventId="countsTableRowClick", id=NULL,fun= function(value,data,...) {
    restore.point("countsTableRowClick")
    method = tat$methods[[data$rowid]]
    res.ui = allocation.info.ui(method, tat)
    tat$method = method

    # Update selected in database
    dbUpdate(glob$db,"tat",vals = list(method=tat$method), where = list(tatid=tat$tatid))

    log.action("res_method",method=tat$method)

    setUI("resUI", res.ui)

  })

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

  tat$methods = intersect(app$glob$sets$method, unique(tat$allocs$method))


  tat = as.environment(tat)
  tat
}

allocs.count.table.ui = function(tat=app$tat, app=getApp()) {
  restore.point("allocs.count.table.ui")
  df = allocs.count.table(tat)

  count.mat = as.matrix(df[,-1])
  mat = as.matrix(df)
  mat[mat=="0"] = ""

  mat[,1] = to.label(df$method, app$glob$sets$method)


  df = as.data.frame(mat)

  # Create sparklines
  df$sl = unlist(lapply(seq_len(NROW(df)), function(row) {
    restore.point("count.sparkline")
    vals = count.mat[row,]
    names(vals) = NULL
    spk_chr(vals, type="bar")
  }))

  df = select(df, method, sl, everything())

  html = simpleTable(id="counts-table",class="simple-table count-table", df=df,col.names = c("","", paste0("Rank ", colnames(mat[,-(1)]))) )
  HTML(html)

}

allocs.count.table = function(tat=app$tat, app=getApp()) {
  restore.point("allocs.count.table")
  allocs = tat$allocs
  tat$methods = methods = intersect(app$glob$sets$method, unique(allocs$method))

  all = expand.grid(method=methods, rank=1:max(allocs$rank, na.rm=TRUE))

  sum = allocs %>% group_by(method, rank) %>%
    summarize(count = n()) %>%
    right_join(all, by=c("method","rank")) %>%
    spread(rank, count)
  sum[is.na(sum)] = 0L
  sum = sum[match(methods,sum$method),]
  sum
}


compute.tat.allocations = function(tat=app$tat, app=getApp()) {
  restore.point("compute.tat.allocations")
  if (NROW(tat$ras)==0) return(NULL)
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
    restore.point("serialdict.alloc")
  } else if (method == "costmin_lin") {
    alloc = assignment.problem.alloc(prefs,rank.costs = (1:T)^(1.01))
  } else if (method == "costmin_quad") {
    alloc = assignment.problem.alloc(prefs,rank.costs = (1:T)^2)
  } else if (method == "costmin_cubic") {
    alloc = assignment.problem.alloc(prefs,rank.costs = (1:T)^3)
  } else if (method == "costmin_3_5") {
    costs = (1:T)^1.01
    if (T>3) costs[4] = 1000
    if (T>4) costs[5] = 1050
    if (T>5) costs[6:T] = costs[6:T]*10000

    alloc = assignment.problem.alloc(prefs,rank.costs = costs)
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

  todf$studname = htmlEscape(todf$studname)

  # Create sparklines
  ras = tat$ras
  max.rank = max(ras$rank)
  cc = rep("blue", max.rank)
  todf$sl = ""

  todf$sl[!is.na(todf$pos)] = unlist(lapply(setdiff(unique(todf$pos),NA), function(.pos) {
    #restore.point("jhkjahdkjshkfh")
    ranks = filter(ras, pos==.pos)$rank
    ccn = cc
    ccn[todf$rank[.pos]] = "red"
    spk_chr(tabulate.to(ranks,max.rank), type="bar", colorMap = ccn, tooltipFormat = '{{value}} ranked as 1+{{offset}}')
  }))

  na.rows = which(is.na(todf$pos))
  todf$topic[na.rows] = "-No Topic-"
  todf$pos[na.rows] = ""
  todf$rank[na.rows] = ""


  tohtml = rmdtools::html.table(todf,col.names = c("","Topic","Student","Ranked as","Topic Ranks"))

  mlab = to.label(method, app$glob$sets$method)
  ui = tagList(
    h4(paste0("Topic allocation via ", mlab)),
    downloadButton("excelDownloadBtn","Download as Excel file"),
    #simpleButton("sendAllocEmailBtn", icon = icon("envelope"), "Results email for students..."),
    HTML(tohtml)
  )

  setDownloadHandler("excelDownloadBtn",
    filename=function(app = getApp())
      paste0("topic_allocation.xlsx"),
    content = function(file, ...) {
      restore.point("downloadTopics")
      app=getApp()
      withProgress(message="Excel file is generated, please wait a moment...", {
        alloc.df = select(alloc, Pos=pos, Topic=topic, Student=studname, Email=studemail, Rank=rank)
        write_xlsx(list(allocation=alloc.df), file)
      })
      log.action("res_excel",method=tat$method)
    }
  )

  ui
}
