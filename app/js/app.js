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

	// Collect the nodes and edges which will be contained in the set
	// We also want to display their judgements when nodes are hovered over
	var graph = new Set();
	var judgements = current_info.fluents.filter(fluent => fluent.type == "judgement" && fluent.args.length == 3);

	current_info.fluents
		// We only want nodes involved in 'trust/2' fluents
		.filter(fluent => fluent.type == "trust" && fluent.args.length == 2)
		.forEach(fluent => {

			// Construct the hovertext for a given node, by looking up their current judgements
			const constructHovertext = id => {

				var hovertext = judgements.reduce((text, judgement) => {
					// This judgement doesn't relate to this node, so don't change the text
					if (judgement.args[0] != id) return text;

					// Otherwise, update the text by including this claim (in Prolog style)
					var val = (Math.round(judgement.value * 100) / 100).toFixed(2);
					var claim = jsonToPrologTerm(judgement.args[2]);
					return `• ${claim}=${val}\n${text}`;
				},
				// Initialise the reduce function with the node's id
				`\n${id}`);

				return hovertext;
			};

			const name1 = fluent.args[0];
			const hovertext1 = constructHovertext(name1);
			graph.add({ data : { id : name1, hovertext : hovertext1 }});

			const name2 = fluent.args[1];
			const hovertext2 = constructHovertext(name2);
			graph.add({ data : { id : name2, hovertext : hovertext2 }});
			
			// Add the edge to the graph
			graph.add({
				data : { 
					id : `trust(${name1}, ${name2})`,
					source : name1,
					target : name2,
					weight : fluent.value
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
				'font-size': '14px',
				'text-wrap': 'ellipsis',
				'text-max-width': '200px',
				'text-overflow-wrap': 'anywhere',
				'text-events': 'yes'
			}
		},
		{
			selector: 'node.hover',
			style: {
				'background-color': '#666',
				'label': 'data(hovertext)',
				'color': '#FFF',
				'font-size': '16px',
				'text-background-color': '#000',
				'text-background-opacity': '0.75',
				'text-background-shape': 'round-rectangle',
				'text-background-padding': '6px',
				'text-wrap': 'wrap',
				'text-max-width': '400px',
				'text-overflow-wrap': 'whitespace',
				'text-events': 'yes'
			}
		},
		{
			selector: 'edge',
			  style: {
				'width': 2,
				'line-color': '#444',
				'target-arrow-color': '#444',
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

	// Ensure that the hovertext appears / disappears when the mouse moves onto or away from a node
	cy.on("mouseover", "node", event => event.target.addClass("hover"));
	cy.on("mouseout", "node.hover", event => event.target.removeClass("hover"));
}

function jsonToPrologTerm(json) {
	// Atoms (base case)
	if (json.args == undefined) return json;
	
	// Complex terms (recursive case)
	jsonifiedArguments = json.args.map(arg => jsonToPrologTerm(arg));
	functor = json.type;

	return `${functor}(${jsonifiedArguments})`;
}

function updateSlider() {
	slider.max = timeline.length;
	slider.min = 1;
}

function moveSlider(event) {
	if (timeline == undefined) return;
	
	current_timestamp = timeline[event.target.value - 1].timestamp;

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
