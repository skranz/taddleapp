// Add button
$('body').on('click', '.all-table .add-btn',function(e){
  var tr = $($(e.target).closest("tr"));
  toptableAddTopic(tr);
  e.stopPropagation();
});

// Remove button
$('body').on('click', '.top-table .del-btn',function(e){
  var tr = $($(e.target).closest("tr"));
  toptableRemoveTopic(tr);
  e.stopPropagation();
});



// Rank buttons
$('body').on('click', '.top-table .up-btn',function(e){
  var tr = $($(e.target).closest("tr"));
  toptableMoveRow(tr, -1);
  e.stopPropagation();
});

$('body').on('click', '.top-table .down-btn',function(e){
  var tr = $($(e.target).closest("tr"));
  toptableMoveRow(tr, 1);
  e.stopPropagation();
});

// Select or unselect row when clicked on topic
$('body').on('click', '.top-table tr', function(e) {
  var id = "top-table";
  var tr = $($(e.target).closest("tr"));

  if (tr.hasClass("sel-row")) {
    tr.removeClass("sel-row");
    return;
  }
  $("#"+id).find("tr").removeClass("sel-row");
  tr.addClass("sel-row");
  tr.find(".col-5 button").focus();
  tr.find(".col-4 button").focus();

});


toptableAddTopic = function(str) {

  dtr = $("#top-table .top-empty:first");

  if (dtr.length === 0) {
    alert("You have already ranked enough topics. You have to remove a ranked topic (x button) before you can add a new one.");
    return;
  }

  var rank = dtr.data("rank");
  var shownpos = str.data("shownpos");

  dtr.data("shownpos", shownpos);

  var topic = str.find(".col-2").html();

  dtr.find(".col-2").html(topic);

  str.removeClass("unranked").addClass("ranked");
  dtr.removeClass("top-empty").addClass("top-filled");
  $("#top-tab-div").css( "display", "block" );
  $("#top-tab-empty-div").css( "display", "none");

};

toptableRemoveTopic = function(str) {
  var rank = str.data("rank");
  var shownpos = str.data("shownpos");

  dtr = $("#all-table .row-"+shownpos);
  dtr.data("shownpos", shownpos);


  // Loop through all ranked topics and move forward
  var rtr = $("#top-table .top-filled");
  var last = rtr.length;
  var td;
  if (last > rank) {
    tr = $(rtr[rank-1]);
    td = tr.find(".col-2");
    for (var i=rank; i<last; i++) {
      var ntr = $(rtr[i]);
      var ntd = ntr.find(".col-2");
      td.html(ntd.html());
      tr.data("shownpos", ntr.data("shownpos"));
      td = ntd;
      tr = ntr;
    }
  } else {
    tr = str;
    td = str.find(".col-2");
  }


  if (last == 1) {
    $("#top-tab-div").css( "display", "none" );
    $("#top-tab-empty-div").css( "display", "block");
  }


  dtr.removeClass("ranked").addClass("unranked");
  td.html("");
  tr.removeClass("top-filled").addClass("top-empty");
  tr.data("shownpos",0);
  rtr.removeClass("sel-row");


};


toptableMoveRow = function(tr, by) {
  var id = "top-table";
  var row = tr.data("rank");
  var drow = row+by;
  var dtr = $("#"+id+" tr.row-"+drow);

  // No destination row
  if (dtr.hasClass("top-empty")) {
    return;
  }


  if (by==-1) {
    if (drow>1) {
      dtr.find(".col-4 button").focus();
    } else {
      dtr.find(".col-5 button").focus();
    }
  } else if (by==1) {
    dtr.find(".col-4 button").focus();
    dtr.find(".col-5 button").focus();
  }

  var stopic = tr.find(".col-2").html();
  var dtopic = dtr.find(".col-2").html();

  var spos = tr.data("shownpos");
  var dpos = dtr.data("shownpos");

  tr.data("shownpos", dpos);
  dtr.data("shownpos", spos);

  dtr.find(".col-2").html(stopic);
  tr.find(".col-2").html(dtopic);

  $("#"+id).find("tr").removeClass("sel-row");
  dtr.addClass("sel-row");
};


get_top_table_shownpos = function() {
  var shownpos = [];
  $("#top-table tr" ).each(function( index ) {
    if (index>0)
      shownpos.push($( this ).data("shownpos"));
  });
  return shownpos;
};
