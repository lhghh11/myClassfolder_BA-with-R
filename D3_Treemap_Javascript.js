if(parent.document.getElementsByTagName("iframe")[0]){
    parent.document.getElementsByTagName("iframe")[0].setAttribute('style','height: 800px !important');
}

var margin = {top: 20, right: 0, bottom: 25, left: 0},
    width = 1420, //1420
    height = 750 - margin.bottom, //775
    formatNumber = d3.format(".2s"),
    transitioning,
    headerHeight = 20,
    headerColor = "#555555";

var x = d3.scale.linear()
          .domain([0, width])
          .range([0, width]);

var y = d3.scale.linear()
          .domain([0, height])
          .range([0, height]);
          
//Create the image container / canvas
var svg = d3.select("#chart")
.append("svg")
.attr("width", width) //+margin.left + margin.right)
.attr("height", height + margin.bottom + margin.top)
.style("margin-left", -margin.left + "px")
.append("g")
.attr("transform", "translate(" + margin.left + "," + margin.top + ")")
.style("shape-rendering", "crispEdges")

// Create a Tree Layout
var treemap = d3.layout.treemap()
.round(false)
.children(function(d, depth) { return depth ? null: d.children;})
.sort(function(a,b){ return a.value - b.value;});

//Append header container in the svg container
var grandparent = svg.append("g")
                    .attr("class","grandparent");
                    
//Create header area
grandparent.append("rect")
            .attr("y", -margin.top)
            .attr("width", width)
            .attr("height", margin.top);
            
// Add Header Text
grandparent.append("text")
            .attr("x", 6)
            .attr("y", 6 - margin.top)
            .attr("dy", ".75em");
