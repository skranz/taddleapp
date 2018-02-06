examples.taddleApp = function() {
  setwd("D:/libraries/taddle/")
  app = taddleApp("D:/libraries/taddle/shared")
  viewApp(app)
}


taddleApp = function(taddle.dir, db.dir = file.path(taddle.dir, "db"), ...) {
  restore.point("clickerClientApp")
  app = eventsApp()
  glob = app$glob

  glob$taddle.dir = taddle.dir
  glob$db.dir = db.dir

  css.file = system.file("www/taddle.css", package="taddleapp")
  app$ui = fluidPage(theme=shinytheme("cerulean"),
    includeCSS(css.file),
    fluidRow(column(offset=1, width=10,
      h3("Taddle: Easily Allocate Seminar Topics"),
      tabsetPanel(type="pills",
        tabPanel("Allocation Task",uiOutput("mainUI")),
        tabPanel("Help"),
        tabPanel("About")
      )
    ))
  )
  appInitHandler(function(...,session=app$session,app=getApp()) {
    observe(priority = -100,x = {
      query <- parseQueryString(session$clientData$url_search)
      if (is.null(query$key)) {
        app$tat = empty.tat()
        show.new.alloc1()
      } else {
        app$key = key
        show.taddle.ui()
      }
    })
  })
  app
}

show.home.ui = function(..., app=getApp(), glob=app$glob) {
  ui = tagList(
    div(class="home-outer",
      div(class="home-inner",
        h3("Taddle: Easily Allocate Seminar Topics"),
        simpleButton("newAllocBtn", "Create an Allocation Task")
      )
    )
  )
  buttonHandler("newAllocBtn", function(...) {
    app$tat = empty.tat()
    show.new.alloc()
  })
  setUI("mainUI", ui)
}

empty.tat = function(...) {
  as.environment(list(new=TRUE, key = random.string(), topic.text="Example Topic 1\nExample Topic 2\nExample Topic 3", topics=character(0), num.topics=0))
}

show.new.alloc = function(...) {
  show.new.alloc1(...)
}

show.new.alloc1 = function(...,tat=app$tat, app=getApp(), glob=app$glob) {
  restore.point("show.new.alloc1")
  ui = tagList(
    fluidRow(column(10, offset=0,
    div(
      div(id="new-alloc-1",
        h3("Step 1: Enter Seminar Title and Topics"),
        textInput("titleInput","Seminar Title",value = tat$title, width="50em"),
        helpText("Simply enter or copy your topics in your text field below. Each topic should be on a new line. You can leave empty lines between topics."),
        textAreaInput("topicsInput","Topics",value = tat$topic.text, cols=60, rows=10, resize="both"),
        simpleButton("cont1Btn","Continue", form.ids = c("titleInput","topicsInput"))
      )
    )))
  )

  buttonHandler("cont1Btn", function(formValues, ...,tat=app$tat, app=getApp()) {
    restore.point("cont1Btn")
    tat$title = formValues$titleInput
    tat$topic.text = formValues$topicsInput
    tat$topics = parse.topic.text(tat$topic.text)
    tat$num.topics = length(topics)
    show.new.alloc2()
  })

  setUI("mainUI", ui)
}


show.new.alloc2 = function(...,tat=app$tat, app=getApp(), glob=app$glob) {
  restore.point("show.new.alloc2")
  ui = tagList(
    h3("Step 2: Allocation Method"),
    selectInput("allocMethod", "Allocation Method"),
    simpleButton("back2Btn","Back"),
    simpleButton("cont2Btn","Continue", form.ids = c("titleInput","topicsInput"))
  )

  buttonHandler("back2Btn", function(formValues, ...,tat=app$tat, app=getApp()) {
    restore.point("back2Btn")
    show.new.alloc1()
  })

  buttonHandler("cont2Btn", function(formValues, ...,tat=app$tat, app=getApp()) {
    restore.point("cont1Btn")
    show.new.alloc3()
  })

  setUI("mainUI", ui)
}


show.new.alloc3 = function(...,tat=app$tat, app=getApp(), glob=app$glob) {
  restore.point("show.new.alloc3")
  ui = tagList(
    fluidRow(column(offset=0, width=10,
      h3("Step 2: Matching Date")
    )),
    fluidRow(
      column(offset=0, width = 4,
        shiny::dateInput("allocDate", label="Date of Allocation")
      ),
      column(width = 6,
        div(style="max-height: 20em",
          wellPanel(
          h4(paste0(tat$num.topics, " Topics: ",tat$title)),
          HTML(paste0("<ol>",paste0("<li>", htmlEscape(tat$topics),"</li>", collapse="\n"),"</ol>"))
          )
        )
      )
    ),
    fluidRow(column(offset=1, width=10,
      simpleButton("back2Btn","Back", form.ids = c("titleInput","topicsInput")),
      simpleButton("cont2Btn","Continue", form.ids = c("titleInput","topicsInput"))
    ))
  )

  buttonHandler("back2Btn", function(formValues, ...,tat=app$tat, app=getApp()) {
    restore.point("back2Btn")
    show.new.alloc1()
  })

  buttonHandler("cont2Btn", function(formValues, ...,tat=app$tat, app=getApp()) {
    restore.point("cont1Btn")
    tat$title = formValues$title
    tat$topic.text = formValues$topic.text
    show.new.alloc3()
  })

  setUI("mainUI", ui)
}

parse.topic.text = function(topic.text, multi.line = FALSE) {
  restore.point("parse.topic.text")
  txt = str_trim(sep.lines(topic.text))
  txt = txt[nchar(txt)>0]
  txt
}
