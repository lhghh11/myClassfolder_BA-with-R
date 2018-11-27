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
            
function loadData(root){
    initialize(root);
    accumulate(root);
    layout(root);
    display(root);
}

/* display shows the treemap and writes the embeded transition function */
function display(d){

//create grandparent bar at top
    grandparent
            .datum(d.parent)
            .select("text")
            .text(d.name);
    var g1 = svg.insert("g", ".grandparent");
    
// add in data
    var g = g1.selectAll("g")
        .data(d.children)
        .enter()
            .append("g");
//write parent rectangle
    g.append("rect")
        .attr("class", "parent")
        .call(rect); //Call the rectangle function to assign dimensions to the rectangle
//write children rectangles
    var k = g.selectAll(".child")
                    .data(function(d) { return d.children || [d];})
                      .enter();
//create rectangle for the child nodes
         k.append("rect")
          .attr("class", "child")
          .call(rect)
          .append("title")
          .text(function(d) { return d.name + "-" + formatNumber(d.value);}) /*should be d.value*/
//Add text for the child nodes
         k.append("foreignObject")
          .call(rect)
          .attr("class", "foreignobj")
          .append("xhtml:div")
          .html(function(d) {return d.name;}) //Display child name
          
          .attr("class", function(d){
          //the if condition is used to assign diffrent font sizes to text based the height and width of the rectangel
                if((d.dy <= 25 && d.dx <= 25) ||
                   (d.dy <= 25 && d.dx > 25)  ||
                   (d.dy > 25 && d.dx <= 25)
                   ){
                   return "textdiv_xs";
                   }
                if((d.dy > 25 && d.dy <=50) && (d.dx > 25 && d.dx <=50) ||
                   (d.dy > 50 && d.dy <=75) && (d.dx > 50 && d.dx <=75)
                   ){
                   return "textdiv_s";
                   }
                if(((d.dy >= 25 && d.dx <= 50) && d.dx >25)             ||
                   (d.dy > 25 && (d.dx > 25)  && d.dx <=50))            ||
                   ((d.dy > 50 && d.dx <= 75)  && d.dx >50)             ||
                   (d.dy > 50 &&(d.dx > 50 && d.dx <=75))
                   ){
                   return "textdiv_s";
                   }
                if((d.dy > 75 && d.dy <=100) && (d.dx > 75 && d.dx <=100) ||
                   ((d.dy > 75 && d.dy <=100) && d.dx >75)                ||
                   (d.dy > 75 && (d.dx >75 && d.dx <=100))
                   ){
                   return "textdiv_,";
                   }
                if (d.dy > 100 && d.dx >100 || d.dy < 100&& d.dx > 100 || d.dy >100 && d.dx <100){
                    return "textdiv";
                    }
                });
           return g;
  }//endfunc display

//Initialize the root node dimensions
function initialize(root){
    root.x = root.y = 0;
    root.dx = width;
    root.dy = height;
    root.depth = 0;
}

//Aggregate the values for internal nodes. This is normally done by the treemap layout
function accumulate(d){
    return d.children
    ? d.value = d.children.reduce(function(p, v){ return p + accumulate(v); },0)
    : d.value;
}

//Creates the layout
//Defines how the nodes needs to be displayed in the svg
function layout(d){
    if(d.children){
        treemap.nodes({children: d.children});
        d.children.forEach(function(c){
            c.x = d.x + c.x * d.dx;
            c.y = d.y + c.y * d.dy;
            c.dx *= d.dx;
            c.dy *= d.dy;
            c.parent = d;
            layout(c);
            });
       }
 }
 
 //This function defines the placements of rectangles for each parent/child
 function rect(rect){
    rect.attr("x", function(d) { return x(d.x);})
        .attr("y", function(d) { return y(d.y);})
        .attr("width", function(d) { return x(d.x + d.dx) - x(d.x);})
        .attr("height", function(d) { return y(d.y + d.dy) - y(d.y);})
        .attr("stroke", "white");
 }
 
 function foreign(foreign){ /*added*/
    foreign.attr("x",function(d) {return x(d.x);})
        .attr("y",function(d) { return y(d.y);})
        .attr("width", function(d) { return x(d.x + d.dx) - x(d.x);})
        .attr("height", function(d) { return y(d.y + d.dy) - y(d.y);})
}

//This function is used to prepare the text at the header of the treemap
function name(d){
    return d.parent ? name(d.parent) +"." d.name : d.name;
   
