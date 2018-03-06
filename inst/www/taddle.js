$('body').on('input', '#topicsInput',function(e){
  parse_and_show_topics();
});

$('body').on('click', '#multilineTopics',function(e){
  parse_and_show_topics();
});

$('body').on('change', '#def_slots',function(e){
  parse_and_show_topics();
});

parse_and_show_topics = function() {
    var multiline = ($('#multilineTopics').is(":checked"));
    var def_slots = parseInt($('#def_slots').val());
    var res = parse_topics($("#topicsInput").val(), multiline, def_slots);
    show_topics(res.topics, res.slots, "#parsedTopics");

};

show_topics = function(topics, slots, sel) {
  var body = "";

  for (var i = 0; i < topics.length; i++) {
    body = body + "<tr>"+
      "<td>" + (i+1) + "</td>" +
      "<td>" + slots[i] + "</td>" +
      "<td>" + topics[i] + "</td>" +
      "</tr>";
  }
  var head = "<b>"+topics.length + " topics parsed" + "</b>";

  var html = head + "<table class='simple-table'><thead><th>Pos</th><th>Slots</th><th>Topic</th><tbody>" + body + "</tbody></table>";

  $(sel).html(html);
};


parse_topics = function(txt, multiline, def_slots) {
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

  // compute slots
  var slots=[];
  for (var i = 0; i < topics.length; i++) {
    if (topics[i].startsWith("#")) {
      var num = topics[i].substring(1,topics[i].indexOf(" "));
      num = parseInt(num);
      if (isNaN(num)) {
        slots.push(def_slots);
      } else {
        slots.push(parseInt(num));
        topics[i] = topics[i].substring(topics[i].indexOf(" ")+1);
      }
    } else {
      slots.push(def_slots);
    }

  }

  return {topics: topics, slots: slots};
};

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

get_rank_table_pos = function() {
  var pos = [];
  $("#rank-table tr" ).each(function( index ) {
    if (index>0)
      pos.push($( this ).data("pos"));
  });
  return pos;
}

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
