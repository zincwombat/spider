<script src="/jsolait/jsolait.js"></script>
<script src="/js/jquery-1.2.5.js"></script>
<script src="/js/jquery.tablesorter.js"></script>
<script type="text/javascript">

var serviceURL = "/engine/json";
var methods = [ "systatus", "getmenu", "syslog" ];

var jsonrpc = imprt("jsonrpc");
var service = new jsonrpc.ServiceProxy(serviceURL, methods);
var timerId;
var json_response;
var current_state;

var TimerCount=0;


$(document).ready(function(){
	StartTimer();
	$("#syslogtable").tablesorter(); 
	$("div#main").click(function(event) {
		var $target=$(event.target);
		if ($target.is('input') && $target.hasClass('SYSLOG')){
			syslog($target.attr("name"),$target.attr("value"));
		}
	})
});


function syslog(Module,Level){
	var html_response;
	try {
		html_response=service.syslog(Module,Level);
		$("#main").load("syslogtable",function(){
			$("#syslogtable").tablesorter();
		});
	}
	catch(e) {
		alert(e);
	}
	return html_response;
}

function systatus() {
	var json_response;
    	try {
     		json_response=service.systatus();
    	} 
	catch(e) {
        	alert(e);
     	}	 
     	return json_response;
}

function Menu() {
	$("#navblock").load("menu");
}

function WStats() {
	$("#writer").load("writestats");
}


function StartTimer()
{
	timerId=setInterval('TimerHandler()',5000);
}

function StopTimer()
{
	clearTimeout(timerId);
}

function TimerHandler()
{
	var status;
	status=systatus();

	if (status.cstate == 'RUNNING') {
		WStats();
	}

	if (status.cstate == 'IDLE') {
		StopTimer();
	}

	if (status.cstate != current_state) {
		$("#runstate > p").text(status.cstate);
		if (status.cstate=="RUNNING") {
			$("#runstate").addClass("spin");
		}
		else {
			$("#runstate").removeClass("spin");
		}
		Menu();
		current_state=status.cstate;
	};


	TimerCount=TimerCount+1;
}

</script>
