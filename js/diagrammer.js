
var diagrammer = {};

function stopEvent(event){
	if(event.preventDefault != undefined)
		event.preventDefault();
	if(event.stopPropagation != undefined)
		event.stopPropagation();
}
	
var ws_initialize = function(WebSocketURL) {

    diagrammer = {
	pengine: undefined,

	currentTool: "oval",

	connection: undefined,

	writeln: function(string) {
		$('#output').append(string + "<br />")
	},

	unchoose_tools: function() {
		$("#diagrammer .componentbar IMG").removeClass("selected");
	},
	
	mouseDownAtX: 0,
	mouseDownAtY: 0,
	
	newElement: function(e) {
			$("#msg").text("down " + e.offsetX + " " + e.offsetY);
		//	diagrammer.sendChat("down(" + e.offsetX + "," + e.offsetY + ")");
			diagrammer.mouseDownAtX = e.offsetX;
			diagrammer.mouseDownAtY = e.offsetY;
			stopEvent(e);
			return false;
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
		diagrammer.sendChat("commit(" + diagrammer.currentTool + ", " + 
					diagrammer.mouseDownAtX + ", " + diagrammer.mouseDownAtY + ", " +
					e.offsetX + ", " + e.offsetY + ", " + e.button + ")");
	},
	
	ctx: function() {
		return $("#diagrammer .drawarea").get(0).getContext('2d');
	},
	
	strokeOnly: function() {
        var context = diagrammer.ctx();
		context.lineWidth = 3;
		context.strokeStyle = 'black';
		context.stroke();	
	},
	
	strokeFill: function() {
        var context = diagrammer.ctx();
		context.fillStyle = 'yellow';
		context.fill();
		context.lineWidth = 3;
		context.strokeStyle = 'black';
		context.stroke();	
	},

	addRect: function(x, y) {
		console.log('rect' + x + ', ' + y);
        var context = diagrammer.ctx();
        context.setTransform(1, 0, 0, 1, 0, 0);
        context.beginPath();
		context.rect(x-50, y-37.5, 100, 75);
		diagrammer.strokeFill();
	},
	
	addOval: function(x, y) {
		console.log('rect' + x + ', ' + y);
        var context = diagrammer.ctx();
        context.setTransform(1, 0, 0, 1, 0, 0);
        context.beginPath();
		context.arc(x, y, 37.5, 0, Math.PI * 2);
		diagrammer.strokeFill();
	},
	
	addDiamond: function(x, y) {
		console.log('rect' + x + ', ' + y);
		var context = diagrammer.ctx();
        context.setTransform(1, 0, 0, 1, 0, 0);
        context.beginPath();
		context.moveTo(x-50, y);
		context.lineTo(x, y-37.5);
		context.lineTo(x+50,y);
		context.lineTo(x, y+37.5);
		context.closePath();
		diagrammer.strokeFill();
	},
	
	clear: function() {
		var canvas = $("#diagrammer .drawarea").get(0);
		diagrammer.ctx().clearRect(0, 0, canvas.width, canvas.height);
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
			var data = eval(e.data);
	      };
	},

        sendChat: function(msg) {
              connection.send(msg);
        }
    };

    $("#rect_tool").on("mouseup", function() {
	   diagrammer.unchoose_tools();
	   diagrammer.currentTool = "rect";
	   $("#rect_tool").addClass("selected");
    });
    $("#oval_tool").on("mouseup", function() {
	   diagrammer.unchoose_tools();
	   diagrammer.currentTool = "oval";
	   $("#oval_tool").addClass("selected");
    });
    $("#diamond_tool").on("mouseup", function() {
	   diagrammer.unchoose_tools();
	   diagrammer.currentTool = "diamond";
	   $("#diamond_tool").addClass("selected");
    });
	$("#text_tool").on("mouseup", function() {
	   diagrammer.unchoose_tools();
	   diagrammer.currentTool = "text";
	   $("#text_tool").addClass("selected");
    });

    $("#diagrammer .drawarea").on(
	      {	"mousedown": diagrammer.newElement,
		"mousemove": diagrammer.newElementMoveOrDrag,
		"mouseup": diagrammer.newElementCommit,
		"contextmenu": function(){ return false; }});

    diagrammer.openWebSocket();
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
