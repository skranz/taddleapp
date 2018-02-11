examples.taddleApp = function() {
  restore.point.options(display.restore.point=TRUE)
  setwd("D:/libraries/taddle/")
  app = taddleApp("D:/libraries/taddle/shared")
  viewApp(app, url.args = list(rank="pdsywc"))

  create.random.ranks("pdsywc")

}

create.random.ranks = function(rankkey, n=NULL, common.weight=0.5, db=getApp()$glob$db) {
  restore.point("create.random.ranks")
  tat = get.rank.tat(rankkey)



  T = NROW(tat$tops)
  if (is.null(n)) n = T

  # Common utility
  u.com = rnorm(T,0,1)
  u.ind = matrix(rnorm(T*n,0,1), n, T)
  w = common.weight
  u = t(w * u.com + (1-w) * t(u.ind))


  studemail = paste0("random",1:n)
  studs = data_frame(tatid = tat$tatid, studkey=studemail, studemail=studemail, studname=studemail, first_ranking=Sys.time(), last_ranking=Sys.time(), num_ranking=0)

  rdf = data_frame(u=as.vector(u),tatid = tat$tatid, studemail=rep(studemail, times=T),pos=rep(1:T, each=n), shownpos=pos )
  rdf = rdf %>% group_by(studemail) %>% mutate(rank=rank(-u)) %>% select(-u) %>% ungroup()

  tatid = tat$tatid
  dbWithTransaction(db, {
    dbDelete(db,"student", nlist(tatid))
    dbDelete(db,"ranking", nlist(tatid))

    dbInsert(db, "student", studs)
    dbInsert(db, "ranking", rdf)
  })

}

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


get.stud.tat = function(studkey, db=getApp()$glob$db) {
  restore.point("get.stud.tat")
  stu = dbGet(db,"student", list(studkey=studkey))
  stu = as.list(stu)
  tatid = stu$tatid
  ta = dbGet(db,"tat", list(tatid = tatid))
  if (NROW(ta)==0) return(NULL)
  ta = ta[NROW(ta),]
  ta = as.list(ta)
  tops = dbGet(db,"topic", list(tatid=ta$tatid))


  ra = dbGet(db,"ranking", list(tatid=tatid, studemail=stu$studemail))
  ra = left_join(ra, select(tops,pos,topic), by="pos") %>% arrange(rank)
  stu$ra = ra

  ta$tops = tops
  ta$stu = stu
  ta
}


empty.stu = function(tat) {
  restore.point("empty.stu")
  ra = tat$tops

  if (isTRUE(tat$random_order)) {
    ra$shownpos = sample(ra$pos, replace = FALSE)
  } else {
    ra$shownpos = ra$pos
  }
  ra$rank = ra$shownpos
  ra = arrange(ra, rank)


  stu = list(ra=ra,tatid=tat$tatid, studemail="", studname="", studkey=random.string(1,20), num_ranking=0, last_ranking=NULL, first_ranking=NULL)

  stu
}

show.rank.ui = function(tat = app$tat, app=getApp()) {
  restore.point("show.rank.ui")
  stu = tat$stu
  if (is.null(tat)) {
    ui = h4("Sorry, but the specified allocation task is not available.")
    setUI("mainUI",ui)
    return()
  }
  if (!is.empty.val(tat$deadline)) {
    if (tat$deadline < Sys.time()) {
      ui = tagList(
        h4(tat$title),
        p(paste0("Sorry, but the deadline ", format(tat$deadline)," for your ranking has already passed."))
      )
      setUI("mainUI",ui)
      return()

    }
  }


  if (is.empty.val(tat$descr)) {
    tat$descr = paste0("Please rank the topics for ", tat$title, if(!is.empty.val(tat$deadline)) paste0(" until <b>", format(tat$deadline,"%A, %B %d at %H:%M"),"</b>"),". Put your most preferred topic on top and your worst preferred topic on the bottom.")
  }

  ui = tagList(
    h4(tat$title),
    HTML(tat$descr),
    HTML(topic.rank.table(tat)),
    textInput("studname", "Your name:",value=stu$studname),
    textInput("studemail", "Your email:",value=stu$studemail),
    helpText("To submit your ranking, press the button below. You will still be able to change it afterwards."),
    uiOutput("rankAlert"),
    simpleButton("submitRankingBtn","Submit Ranking", form.ids = c("studname","studemail"))
  )

  ranking.submit.event(function(ranks, formValues,tat=getApp()$tat,...) {
    restore.point("ranking.submit.handler")

    tat$stu$studemail = formValues$studemail
    tat$stu$studname = formValues$studname

    pos = unlist(ranks)
    ra = tat$stu$ra
    rows = match(pos, ra$pos)
    ra$rank[rows] = seq_along(pos)

    tat$stu$ra = ra

    submit.ranking(tat)

  })
  setUI("rankAlert","")
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


submit.ranking = function(tat=app$tat, app=getApp(), glob=app$glob,...) {
  restore.point("submit.ranking")
  stu = tat$stu

  res = verify.stu(tat$stu)
  if (!res$ok) {
    timedMessage("rankAlert",colored.html(res$msg), millis=20000)
    return()
  } else {
    shinyEventsUI::errorMessage("rankAlert","")
  }

  time = Sys.time()
  if (is.null(stu$first_ranking))
    stu$first_ranking = time
  stu$last_ranking = time
  stu$num_ranking = stu$num_ranking+1

  ras = transmute(stu$ra, tatid=tat$tatid, studemail = stu$studemail, pos=pos, shownpos=shownpos, rank=rank)

  #student = c(list(tat$tatid, tat$stu[c("studkey","studemail","studname","first_rank","last_rank")]
  # Insert dataset
  dbWithTransaction(glob$db,{
    # Overwrite previous rankings by the student
    dbDelete(glob$db, "student", list(tatid=tat$tatid, studemail=stu$studemail))
    dbDelete(glob$db, "ranking", list(tatid=tat$tatid, studemail=stu$studemail))


    dbInsert(glob$db, "student", as.list(tat$stu))
    dbInsert(glob$db, "ranking", ras)
  })

  log.action("rank",email=stu$studemail, studname=stu$studname)

  timedMessage("rankAlert", html=paste0("Thanks a lot, your ranking has been submitted. You also will receive an email from ", glob$email.sender, " with a link that allows you to modify your ranking until the deadline."), millis = 60000)

  rato = left_join(ras, select(tat$tops,pos,topic), by="pos") %>%
    arrange(rank) %>%
    select(rank, topic)

  url = paste0(app$glob$base.url,"?crank=",stu$studkey)
  body = paste0("Hello ", stu$studname,",\n\nyou just submitted your ranking of topics for '", tat$title,"'.\nIf you want to change your ranking ", if(!is.empty.val(tat$deadline)) paste0(" until the deadline ", format(tat$deadline,"%y-%m-%d %H:%M")), " use the following link:\n\n",url,"\n\nYour submitted ranking is as follows:\n\n", paste0(rato$rank,". ", rato$topic, collapse="\n"),
    "\n\n---\nThis is an automatically generated email. Please don't reply.")

  taddle.send.email(to=stu$studemail, subject = paste0(tat$title, ": Your Ranking of Topics"), body=body)

}


ranking.submit.event = function(fun) {
  restore.point("ranking.submit.event")
  customEventHandler(fun=fun,eventId = "submitRankingClick",id=NULL, event="click", css.locator = "#submitRankingBtn",extra.shiny.value.code = "formValues: shinyEventsExtractFormValues(e.target.id), ranks: get_rank_table_ranks()")
}


verify.stu = function(stu) {
  restore.point("verify.stu")
  if (is.empty.val(stu$studname)) {
    return(list(ok=FALSE, msg="Please enter your name."))
  }

  if (is.empty.val(stu$studemail)) {
    return(list(ok=FALSE, msg="Please enter your email address."))
  }

  return(list(ok=TRUE))
}

