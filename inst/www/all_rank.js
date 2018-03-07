// Rank buttons
$('body').on('click', '.rank-table .up-btn',function(e){
  var tr = $($(e.target).closest("tr"));
  ranktableMoveRow(tr, -1)
});

$('body').on('click', '.rank-table .down-btn',function(e){
  var tr = $($(e.target).closest("tr"));
  ranktableMoveRow(tr, 1)
});

// Select or unselect row when clicked on topic
$('body').on('click', '.rank-table td.col-4, .rank-table td.col-1', function(e) {
  var id = "rank-table"
  var tr = $($(e.target).closest("tr"));

  if (tr.hasClass("sel-row")) {
    tr.removeClass("sel-row");
    return;
  }
  $("#"+id).find("tr").removeClass("sel-row");
  tr.addClass("sel-row");
  tr.find(".col-3 button").focus();
  tr.find(".col-2 button").focus();

});


// Key up on down on selected row
$('body').on('keydown', '.rank-table .sel-row', function(e) {
  var tr = $($(e.target).closest("tr"));
  if (e.which == 38) {   // up
    var row = tr.data("rowid");
    if (row>1) {
      ranktableMoveRow(tr, -1)
    }
  }

  if (e.which == 40) {   // down
    if (!tr.is(':last-child')) {
      ranktableMoveRow(tr, 1)
    }
  }
  e.preventDefault(); // prevent the default action (scroll / move caret)
});


ranktableMoveRow = function(tr, by) {
  var id = "rank-table"
  var row = tr.data("rowid");
  var drow = row+by;
  var dtr = $("#"+id+" tr.row-"+drow);

  if (by==-1) {
    if (drow>1) {
      dtr.find(".col-2 button").focus();
    } else {
      dtr.find(".col-3 button").focus();
    }
  } else if (by==1) {
    dtr.find(".col-2 button").focus();
    dtr.find(".col-3 button").focus();
  }

  var stopic = tr.find(".col-4").html();
  var dtopic = dtr.find(".col-4").html();

  var spos = tr.data("pos");
  var dpos = dtr.data("pos")

  tr.data("pos", dpos);
  dtr.data("pos", spos);

  dtr.find(".col-4").html(stopic);
  tr.find(".col-4").html(dtopic);

  $("#"+id).find("tr").removeClass("sel-row");
  dtr.addClass("sel-row");
};

get_rank_table_pos = function() {
  var pos = [];
  $("#rank-table tr" ).each(function( index ) {
    if (index>0)
      pos.push($( this ).data("pos"));
  });
  return pos;
}
