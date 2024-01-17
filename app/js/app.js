function updateGraph() {
	// Get the information relating to the selected timestamp, making sure that the selected timestamp actually exists
	var current_info = timeline.find(slice => slice.timestamp === current_timestamp);
	if (current_info == undefined) {
		current_info = timeline[0];
		current_timestamp = current_info.timestamp;
	}

	//// Show timeline bar


	//// Update textual representation of this timestamp
	// const displayArea = document.getElementById("info");


	//// Update graphical representation of the relevant fluents

	var graph = new Set();
	current_info.fluents
		// We only want 'trust' fluents with two arguments
		.filter(fluent => fluent.type == "trust" && fluent.args.length == 2)
		.forEach(fluent => {
			// Add each argument as nodes to the graph
			graph.add({
				data : { id : fluent.args[0] }
			});
			graph.add({
				data : { id : fluent.args[1] }
			});
			// Add the edge to the graph
			graph.add({
				data : { 
					id : `trust(${fluent.args[0]}, ${fluent.args[1]})=${fluent.value}`,
					source : fluent.args[0],
					target : fluent.args[1],
					// weight : fluent.value
				}
			})
		});

	var cy = cytoscape({
		container: document.getElementById("graph"), // container to render in
		elements: [...graph],
	
		// the stylesheet for the graph
		style: [{
			selector: 'node',
			style: {
				'background-color': '#666',
				'label': 'data(id)',
				'font-size': 'smaller',
				'text-wrap': 'ellipsis',
				'text-max-width': '200px',
				'text-overflow-wrap': 'anywhere'
			}
		},
		{
			selector: 'edge',
			  style: {
				'width': 2,
				'line-color': '#ccc',
				'target-arrow-color': '#ccc',
				'target-arrow-shape': 'triangle',
				'curve-style': 'bezier',
				// 'label': 'data(weight)'
			  }
		}],
		layout: { 
			name: 'klay',
			klay: {
				spacing: 75,
				addUnnecessaryBendpoints: true,
				aspectRatio: 2,
				nodePlacement: 'LINEAR_SEGMENTS'
			}
		}
	});

	cy.minZoom(0.5);
	cy.maxZoom(2);
}

function updateSlider() {
	slider.max = timeline.length;
	slider.min = 1;
}

function moveSlider(event) {
	if (timeline == undefined) return;
	
	current_timestamp = timeline[event.target.value - 1].timestamp;
	console.log(current_timestamp);

	updateGraph();
}

// Update the EEC without redirecting to another page
function submit(event) {
    var url = "/update_eec";
    var data = {};

    for (let node of event.target.children) {
        if (node.nodeName != "TEXTAREA") continue;

        data[node.id] = node.value;
    };

    fetch(url, {
            method: "POST",
            headers: {'Content-Type': 'application/json'}, 
            body: JSON.stringify(data)
        })
        .then(res => res.json())
        .then(data => {
            timeline = data.timeline;
			updateGraph();
			updateSlider();
        });

    event.preventDefault();
}

// Adding the event listener
document.getElementById("update_eec").addEventListener("submit", submit);

const slider = document.getElementById("timeline");
slider.addEventListener("input", moveSlider);

var timeline = undefined;
var current_timestamp = undefined;
