examples.taddleApp = function() {
  restore.point.options(display.restore.point=TRUE)
  setwd("D:/libraries/taddle/")
  app = taddleApp("D:/libraries/taddle/shared")
  viewApp(app)
}


show.new.ui = function(...) {
  ui = navlistPanel(id="mainPanel",
    tabPanel("Taddle",value="home", new.home.ui()),
    tabPanel("Step 1",value="step1", new.step1.ui()),
    tabPanel("Step 2",value="step2", new.step2.ui()),
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
  as.environment(list(new=TRUE, key = random.string(), topic.text="Example Topic 1\nExample Topic 2\nExample Topic 3", topics=c("Example Topic 1", "Example Topic 2", "Example Topic 3"), num.topics=0, method="no", multiline=FALSE, deadline.date = Sys.Date(), deadline.time="23:59", deadline_type="", email=NULL, random_order=TRUE, status="", descr="" ))
}

new.step1.ui = function(...,tat=app$tat, app=getApp(), glob=app$glob) {
  restore.point("show.new.alloc1")

  topics.ui = tagList(
    h4(length(tat$topics), " topics parsed"),
    tags$ol(HTML(paste0("<li>", tat$topics,"</li>")))
  )
  ui = tagList(
    fluidRow(column(10, offset=0,
    div(
      div(id="new-alloc-1",
        h3("Step 1: Enter Seminar Title and Topics"),
        textInput("titleInput","Seminar Title",value = tat$title, width="50em"),
        helpText("Simply enter or copy your topics in your text field below. Each topic should be on a new line. You can leave empty lines between topics."),
        tags$table(style="width: 100%", tags$tr(
          tags$td(style="width: 60%", valign="top",
            tagList(
            textAreaInput("topicsInput","Topics",value = tat$topic.text, cols=60, rows=10, resize="both"),
            checkboxInput("multilineTopics","Multiline topics separated by empty line.",value = tat$multiline)
            )
          ),
          tags$td(style="width: 40%; padding-left: 3em",valign="top",
            div(id="parsedTopics", style="max-height: 20em; overflow-y: auto;",
              topics.ui
            )
          )

        )),
        simpleButton("cont1Btn","Continue", form.ids = c("titleInput","topicsInput"))
      )
    )))
  )

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
    h3("Step 2"),
    p("Deadline until students can enter their preferences:"),
    tags$table(
      tags$td(shiny::dateInput("deadlineDate","Deadline Date", value=tat$deadline.date)),
      tags$td(style="padding-left: 2em;", simpleTimeInput("deadlineTime", "Deadline Time", width="12em", value=tat$deadline.time))
    ),
    selectInput("method","Allocation Method", methods),
    uiOutput("methodDescr"),

    textInput("email","Your Email"),
    checkboxInput("agree","I agree that anonymized data of the allocation task can be used and shared for research purposes.",value = FALSE),
    uiOutput("newSubmitAlert"),

    simpleButton("back2Btn","Back", form.ids = c("deadlineDate","deadlineTime","method","email", "agree")),
    simpleButton("createTatBtn","Create the Allocation Task", form.ids = c("deadlineDate","deadlineTime","method","email","agree", "topicsInput", "titleInput"))
  )

  buttonHandler("back2Btn", function(formValues, ...,tat=app$tat, app=getApp()) {
    restore.point("back2Btn")
    show.step.ui(1)
  })

  buttonHandler("createTatBtn", function(formValues, ...,tat=app$tat, app=getApp()) {
    restore.point("createTatBtn")
    tat$email = formValues$email
    tat$deadlineDate = formValues$deadlineDate
    tat$deadlineTime = formValues$deadlineTime
    tat$method = formValues$method
    tat$agree = formValues$agree

    tat$title = formValues$titleInput
    tat$topic.text = formValues$topicsInput
    tat$topics = parse.topic.text(tat$topic.text)
    tat$num.topics = length(tat$topics)

    submit.new.tat(tat)

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
  tat$deadline = as.POSIXct(paste0(tat$deadline.date," ", tat$deadline.time))
  tat$create_time = Sys.time()

  tops = data_frame(tatid = tat$tatid, pos=seq_along(tat$topics), topic=tat$topics)

  # Insert dataset
  dbWithTransaction(glob$db,{
    dbInsert(glob$db, "tat", as.list(tat))
    dbInsert(glob$db, "topic", tops)
  })


  cat("\nNew tat was inserted")


}


parse.topic.text = function(topic.text, multi.line = FALSE) {
  restore.point("parse.topic.text")
  txt = str_trim(sep.lines(topic.text))
  txt = txt[nchar(txt)>0]
  txt
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
