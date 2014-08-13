
var whiteboard = {};

var ws_initialize = function(WebSocketURL) {

    whiteboard = {
	pengine: undefined,

	currentTool: "oval",

	connection: undefined,

	writeln: function(string) {
		$('#output').append(string + "<br />")
	},

	unchoose_tools: function() {
		$("#whiteboard .componentbar IMG").removeClass("selected");
	},

	newElement: function(e) {
			$("#msg").text("down " + e.clientX + " " + e.clientY);
	},

	newElementMoveOrDrag: function(e) {
		if (mouseDownCount === 0)
			return;

		var x = e.offsetX;
		var y = e.offsetY;

		$("#msg").text("drag " + x + " " + y + " " + e.button);
	},

	newElementCommit: function(e) {
		$("#msg").text("commit " + e.offsetX + " " + e.offsetY);
		whiteboard.sendChat("commit(" + whiteboard.currentTool + ", " + e.offsetX +
		                ", " + e.offsetY + ")");
	},

	openWebSocket: function() {
	      connection = new WebSocket("ws://"+
				window.location.host+WebSocketURL,
			     ['echo']);

	      connection.onerror = function (error) {
                  console.log('WebSocket Error ' + error);
              };

              connection.onmessage = function (e) {
		console.log(e.data);
		var data = eval('(' + e.data + ')');
		var cmd = data.args[0];
		var x = data.args[1];
		var y = data.args[2];
		var canvas = $("#whiteboard .drawarea").get(0);
                var context = canvas.getContext('2d');

                context.setTransform(1, 0, 0, 1, 0, 0);
                context.beginPath();
		switch (cmd) {
	        case  "rect":
			   context.rect(x-50, y-37.5, 100, 75);
			   break;
	        case  "oval":
			   context.arc(x, y, 37.5, 0, Math.PI * 2);
			   break;
		case  "diamond":
			   context.moveTo(x-50, y);
			   context.lineTo(x, y-37.5);
			   context.lineTo(x+50,y);
			   context.lineTo(x, y+37.5);
			   context.closePath();
			   break;
		default:
			   break;
		}

                context.fillStyle = 'yellow';
                context.fill();
                context.lineWidth = 3;
                context.strokeStyle = 'black';
                context.stroke();
	      };
	},

        sendChat: function(msg) {
              connection.send(msg);
        }
    };

    $("#rect_tool").on("mouseup", function() {
	   whiteboard.unchoose_tools();
	   whiteboard.currentTool = "rect";
	   $("#rect_tool").addClass("selected");
    });
    $("#oval_tool").on("mouseup", function() {
	   whiteboard.unchoose_tools();
	   whiteboard.currentTool = "oval";
	   $("#oval_tool").addClass("selected");
    });
    $("#diamond_tool").on("mouseup", function() {
	   whiteboard.unchoose_tools();
	   whiteboard.currentTool = "diamond";
	   $("#diamond_tool").addClass("selected");
    });

    $("#whiteboard .drawarea").on(
	      {	"mousedown": whiteboard.newElement,
		"mousemove": whiteboard.newElementMoveOrDrag,
		"mouseup": whiteboard.newElementCommit});

    whiteboard.openWebSocket();
};

var mouseDown = [0, 0, 0, 0, 0, 0, 0, 0, 0],
    mouseDownCount = 0;
	
$(document).ready( function() {
	document.body.onmousedown = function(evt) {
	  ++mouseDown[evt.button];
	  ++mouseDownCount;
	};
	document.body.onmouseup = function(evt) {
	  --mouseDown[evt.button];
	  --mouseDownCount;
	};
});
