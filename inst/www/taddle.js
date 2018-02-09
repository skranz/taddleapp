$('body').on('input', '#topicsInput',function(e){
  parse_and_show_topics();
});

$('body').on('click', '#multilineTopics',function(e){
  parse_and_show_topics();
});

parse_and_show_topics = function() {
    var multiline = ($('#multilineTopics').is(":checked"))
    var topics = parse_topics($("#topicsInput").val(), multiline);
    show_topics(topics, "#parsedTopics");

}

show_topics = function(topics, sel) {
  var li = "";

  for (var i = 0; i < topics.length; i++) {
    li = li + "<li>" + topics[i] + "</li>";
  }
  var head = "<h4>"+topics.length + " topics parsed" + "</h4>";

  var html = head + "<ol>" + li + "</ol>";

  $(sel).html(html);
}


parse_topics = function(txt, multiline) {
  var str = txt.split(/\r?\n/);

  var topics = [];
  if (!multiline) {
    for (var i = 0; i < str.length; i++) {
      var s = (str[i].trim());
      if (s !== "") {
        topics.push(s);
      }
    }
  } else {
    var cur_topic = "";
    for (var i = 0; i < str.length; i++) {
      var s = (str[i].trim());
      if (s === "") {
        if (cur_topic !== "") {
          topics.push(cur_topic);
        }
        cur_topic = "";
      } else {
        if (cur_topic == "") {
          cur_topic = s;
        } else {
          cur_topic = cur_topic + "<br>" + s;
        }
      }
    }
    if (cur_topic !== "") {
      topics.push(cur_topic);
    }
  }
  return topics;
}

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

}
