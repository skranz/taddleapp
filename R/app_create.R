examples.taddleApp = function() {
  restore.point.options(display.restore.point=TRUE)
  setwd("D:/libraries/taddle/")
  app = taddleApp("D:/libraries/taddle/shared")
  viewApp(app)
}


show.new.ui = function(...) {
  ui = navlistPanel(id="mainPanel",
    tabPanel("Taddle",value="home", new.home.ui()),
    tabPanel("Step 1 (Topics)",value="step1", new.step1.ui()),
    tabPanel("Step 2 (Customize)",value="step2", new.step2.ui()),
    tabPanel("Step 3 (Submit)",value="step3", new.step3.ui()),
    tabPanel("About", about.ui()),
    widths = c(2,10)
  )
  setUI("mainUI", ui)
}


new.home.ui = function(..., app=getApp(), glob=app$glob) {
  ui = tagList(
    div(class="home-outer",
      div(class="home-inner",
        h3("Taddle: Easily Allocate Seminar Topics"),
        simpleButton("newAllocBtn", "New Allocation Task")
      )
    )
  )
  buttonHandler("newAllocBtn", function(...) {
    show.step.ui(1)
  })
  setUI("mainUI", ui)
}

empty.tat = function(...) {
  as.environment(list(new=TRUE, key = random.string(), topic.text="Example Topic 1\nExample Topic 2\n#2 Example Topic 3 (Two Slots)", topics=c("Example Topic 1", "Example Topic 2", "Example Topic 3 (Two Slots)"), slots=c(1,1,2), num.topics=0, def_slots=1, method="no", multiline=FALSE, deadline_date = NA, deadline_time="23:59", deadline_type="", email=NULL, random_order=TRUE, status="", descr="" ))
}

new.step1.ui = function(...,tat=app$tat, app=getApp(), glob=app$glob) {
  restore.point("show.new.alloc1")

  topics.ui = tagList(
    tags$b(length(tat$topics), " topics parsed"),
    parsed.topic.table(tat)
  )
  def_slots = 1:100
  names(def_slots) = 1:100

  ui = tagList(div(id="new-alloc-1",
    h3("Step 1: Enter Seminar Title and Topics"),
    textInput("titleInput","Seminar Title",value = tat$title, width="50em"),
    helpText("Simply enter or copy your topics in your text field below. Each topic should be on a new line. You can leave empty lines between topics."),
    tags$table(style="width: 100%",
      tags$tr(
        tags$td(style="width: 50%", valign="top",
          textAreaInput("topicsInput","Topics",value = tat$topic.text, cols=60, rows=10, resize="both")
        ),
        tags$td(style="width: 50%; padding-left: 1em",valign="top", div(id="parsedTopics", style="max-height: 20em; overflow-y: auto;",
            topics.ui
          )
        )
      ),
      tags$tr(class="create-options",
        tags$td(valign="top", checkboxInput("multilineTopics","Multiline topics separated by empty line.",value = tat$multiline)),
        tags$td(valign="top", tagList(HTML("Default no. of slots per topic:"), simpleSelect("def_slots","",choices=def_slots,selected = tat$def_slots,width = "4em")))
      )
    ),
    simpleButton("cont1Btn","Continue", form.ids = c("titleInput","topicsInput"))
  ))

  buttonHandler("cont1Btn", function(formValues, ...,tat=app$tat, app=getApp()) {
    restore.point("cont1Btn")
    show.step.ui(2)
  })
  ui
}


new.step2.ui = function(...,tat=app$tat, app=getApp(), glob=app$glob) {
  restore.point("new.step2.ui")
  methods = names(glob$methods)
  names(methods) = sapply(glob$methods, function(m) m$title)
  #methods = c(list("Choose matching method later"="no"), methods)

  ui = tagList(
    h3("Step 2: Customize"),
    p("You can enter a deadline until students should enter their preferences:"),
    tags$table(
      tags$td(shiny::dateInput("deadline_date","Deadline Date", value=tat$deadline_date)),
      tags$td(style="padding-left: 2em;", simpleTimeInput("deadline_time", "Deadline Time", width="12em", value=tat$deadline_time))
    ),
    selectInput("method","Allocation Method", methods),
    uiOutput("methodDescr"),
    selectInput("random_order", "How are topics shown?",list("Show topics in random order to students. (May yield more diversified rankings.)"=TRUE, "Show topics in the original order to students."=FALSE)),
    simpleButton("back2Btn","Back", form.ids = c("deadline_date","deadline_time","method","email", "agree")),
    simpleButton("cont2Btn","Continue", form.ids = c("titleInput","topicsInput"))
  )

  buttonHandler("back2Btn", function(formValues, ...,tat=app$tat, app=getApp()) {
    restore.point("back2Btn")
    show.step.ui(1)
  })
  buttonHandler("cont2Btn", function(formValues, ...,tat=app$tat, app=getApp()) {
    restore.point("cont2Btn")
    show.step.ui(3)
  })

  selectChangeHandler("method", function(value,...){
    restore.point("allocMethodChange")
    tat$method = value
    m = glob$methods[[tat$method]]
    setUI("methodDescr",withMathJaxNoHeader(HTML(m$descr)))
  })

  m = glob$methods[[tat$method]]
  setUI("methodDescr",withMathJaxNoHeader(HTML(m$descr)))
  ui
}

new.step3.ui = function(...,tat=app$tat, app=getApp(), glob=app$glob) {
  restore.point("new.step3.ui")
  methods = names(glob$methods)
  names(methods) = sapply(glob$methods, function(m) m$title)
  #methods = c(list("Choose matching method later"="no"), methods)

  ui = tagList(
    h3("Step 3: Submit"),
    textInput("email","Your Email"),
    checkboxInput("agree","I agree that anonymized data of the allocation task can be used and shared for research purposes.",value = FALSE),
    uiOutput("newSubmitAlert"),

    simpleButton("back3Btn","Back", form.ids = c("deadline_date","deadline_time","method","email", "agree")),
    simpleButton("createTatBtn","Create the Allocation Task", form.ids = c("deadline_date","deadline_time","method","email","agree", "random_order", "topicsInput", "titleInput","multilineTopics","def_slots"))
  )

  buttonHandler("back3Btn", function(formValues, ...,tat=app$tat, app=getApp()) {
    restore.point("back3Btn")
    show.step.ui(2)
  })

  buttonHandler("createTatBtn", function(formValues, ...,tat=app$tat, app=getApp()) {
    restore.point("createTatBtn")

    multi.line = formValues$multilineTopics
    def_slots = as.integer(formValues$def_slots)

    tat$email = formValues$email
    tat$deadline_date = formValues$deadline_date
    tat$deadline_time = formValues$deadline_time
    tat$method = formValues$method
    tat$agree = formValues$agree
    tat$random_order = formValues$random_order

    tat$title = formValues$titleInput
    tat$topic.text = formValues$topicsInput
    res = parse.topic.text(tat$topic.text,multi.line = multi.line, def_slots=def_slots)
    tat$topics = res$topics
    tat$slots = res$slots
    tat$num.slots = sum(tat$slots)
    tat$num.topics = length(tat$topics)

    submit.new.tat(tat)

  })
  ui
}


submit.new.tat = function(..., tat=app$tat, app=getApp(), glob=app$glob) {
  restore.point("submit.new.tat")
  res = verify.tat(tat)
  if (!res$ok) {
    timedMessage("newSubmitAlert",colored.html(res$msg), millis=20000)
    return()
  } else {
    shinyEventsUI::errorMessage("newSubmitAlert","")
  }

  if (!is.null(tat$tatid)) {
    # Do something here
    return()
  }

  tat$tatid = random.string(1,20)
  tat$rankkey = paste0(sample(letters,6, replace=TRUE), collapse="")
  tat$org_method = tat$method
  if (is.empty.val(tat$deadline_date)) {
    tat$deadline = NA
  } else {
    tat$deadline = as.POSIXct(paste0(tat$deadline_date," ", tat$deadline_time))
  }
  tat$create_time = Sys.time()

  tops = data_frame(tatid = tat$tatid, pos=seq_along(tat$topics), topic=tat$topics, slots=tat$slots)

  # Insert dataset
  dbWithTransaction(glob$db,{
    dbInsert(glob$db, "tat", as.list(tat))
    dbInsert(glob$db, "topic", tops)
  })

  log.action("submit_tat", method=tat$method)

  rank.url = paste0(app$glob$base.url,"?rank=", tat$rankkey)
  res.url = paste0(app$glob$base.url,"?key=", tat$tatid)
  ui = tagList(
    h4("The allocation task has been generated"),
    p("Your students that can now enter their ranking of topics under the following link. (You can inform them via email)."),
    tags$a(href=rank.url, target="_blank", rank.url),
    br(),p("You can see the results under the following link:"),
    tags$a(href=res.url, target="_blank", res.url),
    br(),p("We have also send you an email with this information.")
  )
  setUI("newSubmitAlert", ui)


  body = paste0("Hello,\n\nyou just generated a new topic allocation task with Taddle.\nYour students can enter their ranking of topics under the following link (you can send the link to your students):\n\n",rank.url,"\n\nYou can see the results under the following link (keep that link private):\n\n",res.url,"\n\n", if(!is.empty.val(tat$deadline)) paste0(" You have set the deadline ", format(tat$deadline,"%y-%m-%d %H:%M"),".\n\n"),
    "---\nThis is an automatically generated email. Please don't reply.")

  taddle.send.email(to=tat$email, subject = paste0("New Allocation Task: ", tat$title), body=body)
}

parsed.topic.table = function(tat) {
  txt = paste0("<tr>",
    "<td>",seq_along(tat$topics),"</td>",
    "<td>",tat$slots,"</td>",
    "<td>",tat$topics,"</td>",
    "</tr>", collapse="\n"
  )
  txt = paste0("<table class='simple-table'><thead><th>Pos</th><th>Slots</th><th>Topic</th><tbody>",txt,"</tbody></table>")
  HTML(txt)
}

parse.topic.text = function(topic.text, multi.line = FALSE, def_slots=1) {
  restore.point("parse.topic.text")
  txt = str_trim(sep.lines(topic.text))
  if (!multi.line) {
    txt = txt[nchar(txt)>0]

  # Multiline topics
  } else {
    txt[txt==""] = "\n"
    txt = paste0(txt, collapse=" ")
    txt = str_trim(sep.lines(txt))
    txt = txt[nchar(txt)>0]
  }
  # Parse no of slots
  slots = rep(def_slots, length(txt))
  rows = which(str.starts.with(txt,"#"))
  if (length(rows)>0) {
    rslots = as.integer(str.between(txt[rows],"#"," "))
    rows = rows[!is.na(rslots)]
    rslots = rslots[!is.na(rslots)]
    txt[rows] = str.trim(str.right.of(txt[rows]," "))
    slots[rows] = rslots
  }

  list(topics=txt, slots=slots)
}

verify.tat = function(tat) {
  if (length(tat$topics)==0) {
    return(list(ok=FALSE, msg="You have not parsed any topics. Please enter topics in step 1."))
  }

  if (is.empty.val(tat$email)) {
    return(list(ok=FALSE, msg="Please enter your email address, so that we can send you the link for monitoring the allocation task."))
  }

  if (is.empty.val(tat$title)) {
    return(list(ok=FALSE, msg="Please enter a seminar title in step 1."))
  }

  if (!isTRUE(tat$agree)) {
    return(list(ok=FALSE, msg="To use Taddle, please check the checkbox above, to allow us to use and share anonymized data for research purposes.

<br>Background: Taddle was generated at the Department of Mathematics and Economics at Ulm University. On the one hand, we just wanted to provide a simple tool to solve the common problem of assigning seminar topics. On the other hand, we also want to use anonymized data to further our understanding, how allocation mechanisms are used in practice. For the sake of open and reproducible science, we would also have the permission to share aanonymized data. There is only academic interest in the data."))
  }

  return(list(ok=TRUE))

}


