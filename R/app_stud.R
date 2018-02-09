get.rank.tat = function(rankkey, db=getApp()$glob$db) {
  restore.point("get.rank.tat")
  ta = dbGet(db,"tat", list(rankkey = rankkey))
  if (NROW(ta)==0) return(NULL)
  ta = ta[NROW(ta),]
  ta = as.list(ta)
  tops = dbGet(db,"topic", list(tatid=ta$tatid))

  ta$tops = tops

  ta$stu = empty.stu(ta)

  ta

}

empty.stu = function(tat) {
  restore.point("empty.stu")
  ra = tat$tops

  if (isTRUE(tat$random_order)) {
    ra$shownpos = sample(ra$pos, replace = TRUE)
  } else {
    ra$shownpos = ra$pos
  }
  ra$rank = ra$shownpos
  ra = arrange(ra, rank)


  stu = list(ra=ra, studemail="", studname="", studkey=random.string(20))

  stu
}

show.rank.ui = function(tat = app$tat, app=getApp()) {
  restore.point("show.rank.ui")
  if (is.null(tat)) {
    ui = h3("Sorry, but the specified allocation task is not available.")
    setUI("mainUI",ui)
    return()
  }

  if (is.null(tat$descr)) {
    tat$descr = paste0("Please rank the topics for ", tat$title, " until <b>", format(tat$deadline,"%A, %B %d at %H:%M"),"</b>. Put your most preferred topic on top and your worst preferred topic on the bottom.")
  }

  ui = tagList(
    h4(tat$title),
    HTML(tat$descr),
    HTML(topic.rank.table(tat)),
    textInput("studname", "Your name:",value=stu$studname),
    textInput("studemail", "Your email:",value=stu$studname),
    helpText("To submit your ranking press the button below. You will be send an email with a link that allows you to change your ranking until the deadline."),
    simpleButton("submitRankingBtn","Submit Ranking")
  )
  setUI("mainUI", ui)
}

topic.rank.table = function(tat, app=getApp()) {
  restore.point("topic.rank.table")
  ra = tat$stu$ra
  n = NROW(ra)


  upBtn = simpleButtonVector(id=paste0("upBtn-", 1:n),icon = icon("arrow-up"),extra.class = "up-btn")
  downBtn = simpleButtonVector(id=paste0("downBtn-", 1:n),icon = icon("arrow-down"),extra.class = "down-btn")
  upBtn[1] = ""
  downBtn[n] = ""
  #btns = paste0(upBtn, downBtn)

  df = data_frame(rank = ra$rank,upBtn, downBtn, ra$topic)
  HTML(simpleTable(id="rank-table", df, class="rank-table", col.names = c("Rank","","", "Topic"), row.data=list(rowid = 1:n, pos=ra$pos)))


}

