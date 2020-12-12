examples.taddleApp = function() {
  restore.point.options(display.restore.point=TRUE)
  setwd("C:/libraries/taddle/")

  app = taddleApp("C:/libraries/taddle/shared")
  app = taddleApp("C:/libraries/taddle/shared", custom.ui = custom.ui)

  viewApp(app, url.args = list(rank="cwobzs"))

  #create.random.ranks("pdsywc")

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
  if (!is.null(app[["studemail"]]))
    tat$stu$studemail = app$studemail
  ta$ra = empty.ra(ta)
  as.environment(ta)
}


get.stud.tat = function(studkey, db=getApp()$glob$db, app=getApp()) {
  restore.point("get.stud.tat")
  stu = dbGet(db,"student", list(studkey=studkey))
  if (NROW(stu)==0) return(NULL)
  stu = as.list(stu)
  tatid = stu$tatid
  ta = dbGet(db,"tat", list(tatid = tatid))
  if (NROW(ta)==0) return(NULL)
  ta = ta[NROW(ta),]
  ta = as.list(ta)
  tops = dbGet(db,"topic", list(tatid=ta$tatid))


  ra = dbGet(db,"ranking", list(tatid=tatid, studemail=stu$studemail))
  ra = left_join(ra, select(tops,pos,topic), by="pos") %>% arrange(rank)
  ta$ra = ra

  ta$tops = tops
  ta$stu = stu
  if (is.null(app["studemail"]))
    app$studemail = ta$stu$studemail
  as.environment(ta)
}

empty.ra = function(tat) {
  restore.point("empty.ra")

  ra = tat$tops
  ro = runif(1,0,100) < tat$random_order
  cat("\nro = ", ro)
  if (ro) {
    ra$shownpos = sample(ra$pos, replace = FALSE)
  } else {
    ra$shownpos = ra$pos
  }
  if (is.empty.val(tat$topn)) {
    ra$rank = ra$shownpos
  } else {
    ra$rank = NA
  }
  ra = arrange(ra, shownpos)
  ra
}

empty.stu = function(tat) {
  list(tatid=tat$tatid, studemail="", studname="", studkey="", num_ranking=0, last_ranking=NULL, first_ranking=NULL, active=TRUE)
}

create.rank.handlers = function(tat=app$tat, app=getApp()) {
  restore.point("create.rank.handlers")

  customEventHandler(eventId="infoPanelClick",id=NULL, css.locator = "#rankInfoDiv a", event="click", fun= function(...) {
    restore.point("infoPanelClick")
    log.action("rank_info_click", email="")
  })

  buttonHandler("delRankingBtn",function(...) {
    restore.point("delRankingBtn")
    stu = tat$stu
    if (is.empty.val(stu$studkey)) {
      timedMessage("rankAlert",msg="You have not yet saved a ranking that can be deleted.", millis=5000)
      return()
    }

    db = app$glob$db
    dbWithTransaction(db,{
      dbDelete(db,"student", list(tatid=tat$tatid, studemail=tat$stu$studemail))
      dbDelete(db,"ranking", list(tatid=tat$tatid, studemail=tat$stu$studemail))
    })
    log.action("del_ranking", email=tat$stu$studemail)
    timedMessage("rankAlert",msg=paste0("Ranking for ", tat$stu$studemail, " has been deleted."), millis=5000)

    rank.url = paste0(app$glob$base.url,"?rank=", tat$rankkey)
    body = paste0("Hello ", tat$stu$studname,",\n\nyou just have deleted your topic ranking for ", tat$title,".\n\nYou can submit a new ranking under the following link:\n\n",rank.url, "\n\n---\nThis is an automatically generated email. Please don't reply.")

    taddle.send.email(to=tat$stu$studemail,subject = paste0(tat$title,": Your ranking has been deleted"), body = body)

    tat$stu$studemail = ""
    tat$stu$studkey = ""


  })


  # Submit Button Click
  inner.js.code = '$("#rankAlert").html("<p>Try to connect to server to submit your ranking...</p>")'

  if (is.empty.val(tat$topn)) {
    customEventHandler(fun=ranking.submit.click,eventId = "submitRankingClick",id=NULL, event="click", css.locator = "#submitRankingBtn",inner.js.code=inner.js.code, stop.propagation = TRUE, extra.shiny.value.code = "formValues: shinyEventsExtractFormValues(e.target.id), pos: get_rank_table_pos()")
  } else {
    customEventHandler(fun=ranking.submit.click,eventId = "submitRankingClick",id=NULL, event="click", css.locator = "#submitRankingBtn",inner.js.code=inner.js.code, stop.propagation = TRUE, extra.shiny.value.code = "formValues: shinyEventsExtractFormValues(e.target.id), shownpos: get_top_table_shownpos()")
  }
}

ranking.submit.click = function(pos=NULL, shownpos=NULL, formValues=NULL, tat=app$tat, app=getApp(),...) {
  restore.point("ranking.submit.click")

  if (is.empty.val(formValues$studname)) {
    msg=paste0("You have not entered a name.")
    timedMessage("rankAlert", html=colored.html(msg), millis=60000)
    return()
  }

  if (!isTRUE(app$glob$studemail.from.login)) {
    if (is.empty.val(formValues$studemail)) {
      msg=paste0("Your email adress is still empty.")
      timedMessage("rankAlert", html=colored.html(msg), millis=60000)
      return()
    }
  }

  ra = tat$ra
  if (is.empty.val(tat$topn)) {
    pos = unlist(pos)
    rows = match(pos, ra$pos)
    ra$rank[rows] = seq_along(pos)
  } else {
    shownpos = unlist(shownpos)
    # Check if topn topics are chosen
    count.shown = sum(shownpos!=0)

    if (tat$topn > 0 & tat$topn != count.shown) {
      msg=paste0("You have ranked ", count.shown, " topics, but you need to choose ", tat$topn, " topics.")
      timedMessage("rankAlert", html=colored.html(msg), millis=60000)
      return()
    }

    ra$rank = NA_integer_
    rows = match(shownpos, ra$shownpos)
    rows = rows[!is.na(rows)]

    ra$rank[rows] = seq_along(rows)
  }

  if (!isTRUE(app$glob$studemail.from.login)) {
    if (is.empty.val(tat$stu$studemail)) {
      tat$stu$studemail = tolower(formValues$studemail)
    } else if (tolower(formValues$studemail) != tolower(tat$stu$studemail)) {
      msg=paste0("Sorry, but you already specified the email address ", tat$stu$studemail,". To change your email address, you first must delete your ranking by pressing the button below.")
      timedMessage("rankAlert", html=colored.html(msg), millis=60000)
      return()
    }
  } else {
    tat$stu$studemail = app[["studemail"]]
  }
  tat$stu$studname = formValues$studname
  tat$ra = ra
  submit.ranking(tat)
}

show.rank.ui = function(tat = app$tat, app=getApp()) {
  restore.point("show.rank.ui")
  stu = tat$stu
  glob = app$glob

  if (is.null(glob$rank_info)) {
    dir = system.file("methods", package="taddleapp")
    glob$rank_info =list(
      serialdict=paste0(readLines(file.path(dir,"methods_rank_serialdict.html"),warn = FALSE), collapse="\n"),
      no= paste0(readLines(file.path(dir,"methods_rank_no.html"),warn = FALSE), collapse="\n")
    )
  }



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

  use.topn = !is.empty.val(tat$topn)

  ui.fun = app$glob$custom.ui$rank.ui.fun
  if (is.null(ui.fun)) {
    ui = tagList(
      h4(tat$title),
      if (use.topn) {
        if (tat$topn==-1) {
          rank.some.table.ui()
        } else {
          rank.topn.table.ui()
        }
      } else {
        rank.all.table.ui()
      },
      br(),
      div(id="rankInfoDiv",
          slimCollapsePanel("Info: How are the topics allocated?", value="alloc_info",HTML(glob$rank_info[[tat$org_method]]))
      ),
      textInput("studname", "Your name:",value=stu$studname),
      if (!isTRUE(app$glob$studemail.from.login)){
        textInput("studemail", "Email:",value=stu$studemail)
      } else {
        tags$p("Email: ", app[["studemail"]])
      },
      helpText("To submit your ranking, press the button below. You will still be able to change it afterwards."),
      uiOutput("rankAlert"),
      simpleButton("submitRankingBtn","Submit Ranking", form.ids = c("studname","studemail")),
      simpleButton("delRankingBtn","Delete your Ranking")
    )
  } else {
    ui = ui.fun(stu=stu)
  }


  create.rank.handlers()
  log.action("rank_session",email=stu$studemail, shownpos=tat$ra$shownpos, pos=tat$ra$pos, topn=tat[["topn"]])

  setUI("rankAlert","")
  setUI("mainUI", ui)
}

rank.all.table.ui = function(tat = app$tat, app=getApp()) {
  restore.point("rank.all.table.ui")
  ra = tat$ra
  n = NROW(ra)

  upBtn = simpleButtonVector(id=paste0("upBtn-", 1:n),icon = icon("arrow-up"),extra.class = "up-btn")
  downBtn = simpleButtonVector(id=paste0("downBtn-", 1:n),icon = icon("arrow-down"),extra.class = "down-btn")
  upBtn[1] = ""
  downBtn[n] = ""
  #btns = paste0(upBtn, downBtn)

  df = data_frame(rank = ra$rank,upBtn, downBtn, htmlEscape(ra$topic))

  tab = HTML(simpleTable(id="rank-table", df, class="rank-table striped-table", col.names = c("Rank","","", "Topic"), row.data=list(rowid = 1:n, pos=ra$pos)))


  descr = paste0("Please rank the topics for ", tat$title,
    if(!is.empty.val(tat$deadline)) paste0(" until <b>", format(tat$deadline,"%A, %B %d at %H:%M"),"</b> (Central European Time Zone)"),
    ". Put your more liked topics first.",
    "<br>(Hint: You can click on a topic name and then quickly move it with the up and down keys of your keyboard.)"
  )
  tagList(
    HTML(descr),
    tab,
    tags$script(src="taddle/all_rank.js")
  )
}


rank.topn.table.ui = function(tat = app$tat, app=getApp()) {
  restore.point("rank.topn.table.ui")
  ra = tat$ra %>% arrange(shownpos)

  rat = filter(ra, !is.na(rank)) %>%
    arrange(rank)

  n = tat$topn

  upBtn = simpleButtonVector(id=paste0("upBtn-", 1:n),icon = icon("arrow-up"),extra.class = "up-btn btn-sm")
  downBtn = simpleButtonVector(id=paste0("downBtn-", 1:n),icon = icon("arrow-down"),extra.class = "down-btn btn-sm")
  delBtn = simpleButtonVector(id=paste0("delBtn-", 1:n),icon = icon("remove"),extra.class = "del-btn btn-sm")

  upBtn[1] = ""
  downBtn[n] = ""
  #btns = paste0(upBtn, downBtn)

  df = data_frame(rank = 1:n, topic=htmlEscape(fill.vector(rat$topic,n,"")),delBtn, upBtn, downBtn)
  row.class = fill.vector(rep("top-filled",NROW(rat)),n,"top-empty")
  shownpos = fill.vector(rat$shownpos,n,0)

  top.tab = HTML(simpleTable(id="top-table", df, class="ra-table top-table striped-table", col.names = c("Rank","Topic","", "",""), row.class=row.class, row.data=list(rank = 1:n, shownpos=shownpos)))


  n = NROW(ra)
  row.class = ifelse(is.na(ra$rank),"unranked","ranked")

  addBtn = simpleButtonVector(id=paste0("addBtn-", 1:n),icon = icon("plus","fa-sm"),extra.class = "add-btn btn-sm")

  all.df = data_frame(addBtn, htmlEscape(ra$topic))
  all.tab =  HTML(simpleTable(id="all-table", all.df, class="ra-table all-table striped-table", col.names = c("Add", "Topic"), row.class=row.class, row.data=list(pos = ra$pos, shownpos=ra$shownpos)))


  descr = paste0("Please choose your Top ",  tat$topn, " topics from the list of not chosen topics", if(!is.empty.val(tat$deadline)) paste0("  until <b>", format(tat$deadline,"%A, %B %d at %H:%M"),"</b> (Central European Time Zone)"),
      ". Put your more liked topics first. You can remove a Top ", tat$topn," topic or change its rank with buttons on the right of the table. <b>Press the submit button at the bottom of the page to submit your ranking.</b>"
  )

  ui = tagList(
    includeCSS(system.file("www/topn.css", package="taddleapp")),
    HTML(descr),
    fluidRow(
      column(width = 4,
        h4("Not Chosen Topics:"),
        all.tab
      ),
      column(width=7,
        h4(paste0("Your Top ", tat$topn," Topics:")),
        div(id="top-tab-div", style=if(NROW(rat)==0) "display: none;",
          top.tab
        ),
        div(id="top-tab-empty-div", style=if(NROW(rat)>0) "display: none;",
          p("--- No topics selected yet ---")
        )
      )
    ),
    tags$script(src="taddle/topn_rank.js")
  )
}

# Student only ranks some topics
# Newly introduced
rank.some.table.ui = function(tat = app$tat, app=getApp(), table.ui.function = NULL, topic.header = "Topic") {
  restore.point("rank.some.table.ui")
  ra = tat$ra %>% arrange(shownpos)

  n = NROW(ra)


  rat = filter(ra, !is.na(rank)) %>%
    arrange(rank)

  upBtn = simpleButtonVector(id=paste0("upBtn-", 1:n),icon = icon("arrow-up"),extra.class = "up-btn btn-sm")
  downBtn = simpleButtonVector(id=paste0("downBtn-", 1:n),icon = icon("arrow-down"),extra.class = "down-btn btn-sm")
  delBtn = simpleButtonVector(id=paste0("delBtn-", 1:n),icon = icon("remove"),extra.class = "del-btn btn-sm")

  upBtn[1] = ""
  downBtn[n] = ""
  #btns = paste0(upBtn, downBtn)

  df = data_frame(rank = 1:n, topic=htmlEscape(fill.vector(rat$topic,n,"")),delBtn, upBtn, downBtn)
  row.class = fill.vector(rep("top-filled",NROW(rat)),n,"top-empty")
  shownpos = fill.vector(rat$shownpos,n,0)

  top.tab = HTML(simpleTable(id="top-table", df, class="ra-table top-table striped-table", col.names = c("Rank",topic.header,"", "",""), row.class=row.class, row.data=list(rank = 1:n, shownpos=shownpos)))


  n = NROW(ra)
  row.class = ifelse(is.na(ra$rank),"unranked","ranked")

  addBtn = simpleButtonVector(id=paste0("addBtn-", 1:n),icon = icon("plus","fa-sm"),extra.class = "add-btn btn-sm")

  all.df = data_frame(addBtn, htmlEscape(ra$topic))
  all.tab =  HTML(simpleTable(id="all-table", all.df, class="ra-table all-table striped-table", col.names = c("Add", topic.header), row.class=row.class, row.data=list(pos = ra$pos, shownpos=ra$shownpos)))


  descr = paste0("Please choose your topics from the list of not chosen topics", if(!is.empty.val(tat$deadline)) paste0("  until <b>", format(tat$deadline,"%A, %B %d at %H:%M"),"</b> (Central European Time Zone)"),
                 ". Put your more prefered topics first. You can remove a selected topic or change its rank with buttons on the right of the table. <b>Press the submit button at the bottom of the page to submit your ranking.</b>"
  )

  if (is.null(table.ui.function)) {
    ui = tagList(
      includeCSS(system.file("www/topn.css", package="taddleapp")),
      HTML(descr),
      fluidRow(
        column(width = 4,
               h4("Not Chosen Topics:"),
               all.tab
        ),
        column(width=7,
               h4(paste0("Chosen ", tat$topn," Topics:")),
               div(id="top-tab-div", style=if(NROW(rat)==0) "display: none;",
                   top.tab
               ),
               div(id="top-tab-empty-div", style=if(NROW(rat)>0) "display: none;",
                   p("--- No topics selected yet ---")
               )
        )
      ),
      tags$script(src="taddle/topn_rank.js")
    )
  } else {
    ui = table.ui.function(all.tab=all.tab, top.tab=top.tab, rat=rat, tat=tat)
  }
  ui
}


html.escape = function(text,attribute=FALSE) {
  if (length(text)==0) return(text)
  htmlEscape(text,attribute)
}

fill.vector = function(x, n, empty="") {
  if (length(x)>n) return(x[seq_len(n)])
  if (length(x)==n) return(x)
  c(x, rep(empty, n-length(x)))
}

rank.topn.table.ui.old = function(tat = app$tat, app=getApp()) {
  restore.point("rank.topn.table.ui")
  ra = tat$ra
  n = NROW(ra)

  upBtn = simpleButtonVector(id=paste0("upBtn-", 1:n),icon = icon("arrow-up"),extra.class = "up-btn")
  downBtn = simpleButtonVector(id=paste0("downBtn-", 1:n),icon = icon("arrow-down"),extra.class = "down-btn")
  upBtn[1] = ""
  downBtn[n] = ""
  #btns = paste0(upBtn, downBtn)

  df = data_frame(rank = ra$rank,upBtn, downBtn, htmlEscape(ra$topic))

  ui = HTML(simpleTable(id="rank-table", df, class="rank-table striped-table", col.names = c("Rank","","", "Topic"), row.data=list(rowid = 1:n, pos=ra$pos)))

  ui
}




submit.ranking = function(tat=app$tat, app=getApp(), glob=app$glob,...) {
  restore.point("submit.ranking")
  stu = as.list(tat$stu)

  res = verify.stu(stu)
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
  if (is.empty.val(stu$studkey))
    stu$studkey = random.string(1,20)
  tat$stu = stu

  ras = transmute(tat$ra, tatid=tat$tatid, studemail = stu$studemail, pos=pos, shownpos=shownpos, rank=rank)

  #student = c(list(tat$tatid, tat$stu[c("studkey","studemail","studname","first_rank","last_rank")]
  # Insert dataset
  dbWithTransaction(glob$db,{
    # Overwrite previous rankings by the student
    dbDelete(glob$db, "student", list(tatid=tat$tatid, studemail=stu$studemail))
    dbDelete(glob$db, "ranking", list(tatid=tat$tatid, studemail=stu$studemail))


    dbInsert(glob$db, "student", stu)
    dbInsert(glob$db, "ranking", ras)
  })

  log.action("sub_rank",email=stu$studemail, studname=stu$studname, pos=tat$ra$pos, rank=tat$ra$rank, shownpos=tat$ra$shownpos, topn=tat[["topn"]])

  timedMessage("rankAlert", html=paste0("Thanks a lot, your ranking has been successfully submitted. You also will receive an email from ", glob$email.sender, " with a link that allows you to modify your ranking until the deadline."), millis = 60000)

  rato = filter(ras, !is.na(rank)) %>%
    left_join(select(tat$tops,pos,topic), by="pos") %>%
    arrange(rank) %>%
    select(rank, topic)

  url = paste0(app$glob$base.url,"?crank=",stu$studkey)
  body = paste0("Hello ", stu$studname,",\n\nyou just submitted your ranking for '", tat$title,"'.\nIf you want to change your ranking", if(!is.empty.val(tat$deadline)) paste0(" until the deadline ", format(tat$deadline,"%y-%m-%d %H:%M")), ", use the following link:\n\n",url,"\n\nYour submitted ranking is as follows:\n\n", paste0(rato$rank,". ", rato$topic, collapse="\n"),
    "\n\n---\nThis is an automatically generated email. Please don't reply.")

  taddle.send.email(to=stu$studemail, subject = paste0(tat$title, ": Your Ranking"), body=body)

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

