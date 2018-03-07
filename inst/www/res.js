$('body').on('click', '#counts-table tbody tr',function(e){
  var id = "counts-table";
  var tr = $($(e.target).closest("tr"));
  $("#"+id).find("tr").removeClass("sel-row");
  tr.addClass("sel-row");

  Shiny.onInputChange("countsTableRowClick", {eventId: "countsTableRowClick", id: e.target.id, value: tr.data("rowid"), data: tr.data(),nonce: Math.random()});

});


$('body').on('click', '#resMainPanel li',function(e){
  var obj = $(e.target);
  Shiny.onInputChange("resMainPanelClick", {eventId: "resMainPanelClick", id: "resMainPanelClick", value: obj.data("value"), nonce: Math.random()});
});

$('body').on('click','.stud-active',function(e){
  var checked = e.target.checked;
  var tr = $($(e.target).closest("tr"));

  if (checked) {
    tr.removeClass("inactive");
  } else {
    tr.addClass("inactive");
  }
});
