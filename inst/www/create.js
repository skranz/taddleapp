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
