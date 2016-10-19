var rootId = 0;
var apiPath = "http://api.brain-map.org/api/v2/";
var structureGraphId = 1;



$(document).ready(function(){

function importData(id, long,short, callback) {
	var structures = null;
	var expression = null;
	var intensity = null;

	processStructures(long)


	apiQuery(short,
			 processExpression);

function apiQuery(short, onsuccess) {
		var rows = [];
		var num_rows = 2000;
		var total_rows = -1;

    short = JSON.parse(short[0]);
    		rows.push.apply(rows, short.msg);
    		total_rows = parseInt(short.total_rows);
    		if (total_rows < 0 || isNaN(total_rows))
    			apiError("total_rows incorrect", url);
    		else if (rows.length >= total_rows)
    			onsuccess(rows);
    		else
    			apiPageQuery();
	}

	function processStructures(data) {
		d = JSON.parse(data[0])
		structures = findChild(d.msg[0], rootId);
		processData();
	}


	function processExpression(data) {
	  ooo = data;
		expression = {};
		intensity = {};
		for (var i = 0; i < data.length; i++) {
			var e = data[i];
			expression[e.structure_id] = e.expression_energy;
			intensity[e.structure_id] = e.intensity;
		}
		processData();
	}

	function processData() {
		if (!structures || !expression || !intensity)
			return;
        $("#useIntensityFromSystem2").css("background", "");
str = structures;exp = expression; int = intensity;
		callback(structures,expression,intensity);
	}

	// Find a structure's child structure, by structure id.

	function findChild(structure, childId) {
		if (structure.id == childId)
			return structure;
		else {
			for (var i=0; i<structure.children.length; i++) {
				var r = findChild(structure.children[i], childId);
				if (r) return r;
			}
			return null;
		}
	}


	function apiError(response, url) {

		var errorHtml =
			"<p>There was an error with the following query:</p>" +
			"<p>" + url + "</p>" +
			"<p>Error message:</p>" +
			"<p>" + response + "</p>";

        $("#useIntensityFromSystem2").css("background", "");

		var dialog = $( "#errorDialog" );

		var existingErrors = dialog.html();

		$( "#errorDialog" )
			.html(existingErrors + errorHtml)
			.dialog({
				width: 500,
				height: 200,
				modal: true
			});
	}




}

  $('#upload').change(upload);
  function upload(){
    o = $("#upload")[0]
    var req=ocpu.call("upload",{
      path:$("#upload")[0].files[0]
    },function(session){
      session.getObject(function(obj){
      data = obj.data
      col = obj.color

{
  long1 = obj.long1;short1 = obj.short1;long2 = obj.long2;short2 = obj.short2;
  var sectionDataSetId = 69855739;

var w = 650;
var h = 650;
var r = Math.min(w, h) / 2;
var outerRadius = r/4.5;
var expDomain = [8.236325e-06,0.1145533];
var expDomain2 = [2.15102e-06,0.2850248];
var expressionHash = {};
var intensityHash = {};


$("#homeButton").button({ icons: { primary: "ui-icon-home" }});

var structureLabel = $("#structureLabel");
var expressionLabel = $("#expressionLabel");
var intensityLabel = $("#intensityLabel");
var scaleButtonContainer = $("#scaleButtons");

var scaleOptions = [{ name: 'expression', id: "#expressionButton", fn: expressionValue },
					{ name: 'uniform', id: "#uniformButton", fn: uniformValue },
					{ name: 'intensity', id: "#intensityButton", fn: intensityValue }];

for (var i = 0; i < scaleOptions.length; i++) {
	var opt = scaleOptions[i].name;
	scaleButtonContainer.append($(document.createElement('input'))
								.attr('type', 'radio')
								.attr('id', opt + 'Button')
								.attr('name', 'scaleRadio')
								.attr('value', opt)
								.attr('checked', opt == 'uniform'));

	scaleButtonContainer.append($(document.createElement('label'))
								.attr('for', opt + 'Button')
								.html(opt));
}

//scaleButtonContainer.buttonset();

function uniformValue(d) { return 1; }
function expressionValue(d) {
  var eout = expressionHash[d.id];
	return eout ? eout : 0;
}
function intensityValue(d){
	var iout = intensityHash[d.id];
	return iout ? iout : 0;
}

function isParent(s) {
    return Boolean(s.children && s.children.length > 0);
}




var rootNode = null;
var x = d3.scale.linear().range([0, 2 * Math.PI]);
var y = d3.scale.linear().range([0, Math.pow((r - .1 * outerRadius) / r, 2) * r]);
var yOut = d3.scale.linear().domain(expDomain).range([r - 1*outerRadius, r]).clamp(true);
var expcolor = d3.scale.linear().domain(expDomain).range(["#eee", "red"]);
var highlightcolor = d3.scale.linear().domain(expDomain).range(["#f9fbe4", "#f9fbe4"]);
importData(1,
long2,
short2,function(structureTree, expression, intensity) {
  var vis = d3.select("#useIntensityFromSystem2").append("svg:svg")
	.attr("id","vis")
	.attr("width", w)
	.attr("height", h)
	.append("svg:g")
	.attr("transform", "translate(" + w / 2 + "," + h / 2 + ")");

var partition = d3.layout.partition()
	.sort(function(a,b) { return b.graph_order - a.graph_order; })
	.value(uniformValue);


var arc = d3.svg.arc()
  .startAngle(function(d) { return Math.max(0, Math.min(2 * Math.PI, x(d.x))); })
  .endAngle(function(d) { return Math.max(0, Math.min(2 * Math.PI, x(d.x + d.dx))); })
  .innerRadius(function(d) { return !isParent(d) ? r - 1.1*outerRadius : Math.max(0, y(d.y)); })
  .outerRadius(function(d) { return !isParent(d) ? yOut(expressionValue(d)) : Math.max(0, y(d.y + d.dy));});


var arcHighlight = d3.svg.arc()
  .startAngle(function(d) { return Math.max(0, Math.min(2 * Math.PI, x(d.x))); })
  .endAngle(function(d) { return Math.max(0, Math.min(2 * Math.PI, x(d.x + d.dx))); })
  .innerRadius(function(d) { return !isParent(d) ? r - 1*outerRadius : Math.max(0, y(d.y + d.dy)); })
  .outerRadius(function(d) { return r; });

	rootNode = structureTree;
	expressionHash = expression;
	intensityHash = intensity;
	$("#useIntensityFromSystem2").css("background", "");
	// The mouseover highlight goes first, so that the actual visualization
	// elements will be drawn on top due to z-ordering.
	 highlight = vis.append("svg:path")
		.attr("d", null)
		.style("fill", "#eee");

	var path = vis.data([structureTree]).selectAll("dataArc")
		.data(partition)//!!!! this is the key of where x is generated
		.enter().append("svg:path")
		.attr("d", arc)
		.style("fill", arcColor)
		.style("stroke", arcStroke)
		.style("stroke-width", arcStrokeWidth)
		.on("click", click)
		.on("mouseover", mouseover)
		.on("mouseout", mouseout)
		.each(stash);

	function mouseover(d) {
		highlight.attr("d", arcHighlight(d));
		highlight.style("fill", isParent(d) ? highlightcolor(expressionValue(d)) : "#eee")
		if("parent" in d){
		  if("parent" in d.parent){
		    if("parent" in d.parent.parent){
		      structureLabel.html(d.parent.parent.name + "-->" + d.parent.name + "-->" + d.name);
		    }else{
		      structureLabel.html(d.parent.name + "-->" + d.name);
		    }
		  }else{
		    structureLabel.html(d.name);
		  }
		}else{
		  structureLabel.html(d.name);
		}
		expressionLabel.html(expressionValue(d));
		intensityLabel.html(intensityValue(d));
	}

	function mouseout(d) {
		highlight.attr("d", null);
		highlight.style("fill", "#fff");
		structureLabel.html("");
		expressionLabel.html("");
		intensityLabel.html("");
	}


	function click(d,i) {
		if (isParent(d)) {
			rootNode = d;
			highlight.attr("d", null);
			structureLabel.html("");
			expressionLabel.html("");
		  intensityLabel.html("");
			path.transition()
				.duration(750)
				.attrTween("d", arcTween(rootNode))
	  ;
		}
	}


	$.each(scaleOptions, function(i,button) {
		$(button.id).click(function() { dataTransition(button.fn);});
	});

	function dataTransition(valueFunction) {
		path.data(partition.value(valueFunction))
			.style("fill", arcColor)
			.style("stroke", arcStroke)
			.style("stroke-width", arcStrokeWidth)
			.transition()
			//.duration(750)
			.attrTween("d", dataTween(rootNode));
	}

	function arcColor(d) {
		return (isParent(d)) ?
		"#" + d.color_hex_triplet :
			expcolor(expressionValue(d));
	}

	function arcStroke(d) {
		return (isParent(d)) ? "#fff" : "none";
	}

	function arcStrokeWidth(d) {
		return "1px";
	}

	function arcTween(root) {
		var xd = d3.interpolate(x.domain(), [root.x, root.x + root.dx]);
		var yd = d3.interpolate(y.domain(), [root.y, 1]);
		var yr = d3.interpolate(y.range(), [root.y ? 20 : 0, r - 0.2*outerRadius]);

		return function(d, i) {
			return i
				? function(t) { return arc(d); }
		    : function(t) { x.domain(xd(t)); y.domain(yd(t)).range(yr(t)); return arc(d); };
		};
	}

	function stash(d) {
		d.x0 = d.x;
		d.dx0 = d.dx;
	}


	function dataTween(r) {
		var xd = d3.interpolate(x.domain(), [r.x, r.x + r.dx]);
		return function(d, i) {
			var arci = d3.interpolate({x: d.x0, dx: d.dx0}, d);

			// the domain only gets updated once.
			if (i == 0) {
				return function(t) {
					x.domain(xd(t));
					var b = arci(t);
					d.x0 = b.x;
					d.dx0 = b.dx;
					return arc(b);
				}
			} else {
				return function(t) {
					var b = arci(t);
					d.x0 = b.x;
					d.dx0 = b.dx;
					return arc(b);
				}
			}
		};
	}

	function computeTextRotation(d) {
		return (x(d.x + d.dx / 2) - Math.PI / 2) / Math.PI * 180;
	}

});

function getUrlVars()
{
	var vars = [], hash;
	var hashes = window.location.href.slice(window.location.href.indexOf('?') + 1).split('&');
	for(var i = 0; i < hashes.length; i++)
	{
		hash = hashes[i].split('=');
		vars.push(hash[0]);
		vars[hash[0]] = hash[1];
	}
	return vars;
}
}




      })
    }).fail(function() {alert("Error: " + req.responseText);})
  }
})


