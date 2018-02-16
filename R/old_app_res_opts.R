
res.options.ui = function(tat = app$tat, app=getApp()) {
  restore.point("res.options.ui")
  ui = tagList(
    h4("Customization of allocation"),
    p("You can further customize your allocation by deactivating some topics or students, fix some student-topic match, or adapt the number of slots of some topics."),
    HTML("Add a customization:"),
    simpleSelect("opttype","",choices=app$glob$sets$options,width = "12em"),
    #smallButton("addoptBtn","Go..."),
    uiOutput("optCreateUI")
  )

  selectChangeHandler("opttype", function(value, ...) {
    restore.point("opttype_change")
    show.opt.create.ui(value)
  })

  ui
}

show.opt.create.ui = function(type, tat=app$tat, app=getApp()) {
  restore.point("show.opt.create.ui")

  if (type=="da_top") {
    tops = tat$tops$pos
    names(tops) = tat$tops$topic
    ui = tagList(
      selectInput("da_top", "Topics that shall be deactivated", tops,multiple = TRUE),
      simpleButton("da_topBtn","Ok",form.ids = "da_top")
    )
    buttonHandler("da_topBtn", function(formValues,...) {
      restore.point("da_topBtn")
      setUI("optCreateUI",NULL)
      opt = data_frame(tatid=tat$tatid, optid = random.string(1,20), active=TRUE, type=type, studemail=list(NULL), pos = formValues$da_top, slots=NULL)
      tat$opts = rbind(tat$opts, opt)
      refresh.alloc.and.ui()
    })
  } else {
    ui = p("Not yet implemented")
  }


  setUI("optCreateUI",wellPanel(ui))

}
