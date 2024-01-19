function updateDisplay() {
	// Get the information relating to the selected timestamp, making sure that the selected timestamp actually exists
	current_info = timeline.find(slice => slice.timestamp === current_timestamp);
	if (current_info == undefined) {
		current_info = timeline[0];
		current_timestamp = current_info.timestamp;
	}

	// Update the timeline slider
	const slider = document.getElementById("timeline");
	slider.max = timeline.length;
	slider.min = 1;

	// Update the display options
	var options = [{ fluent_data : "trust", display : "Trust fluents" }];
	
	current_info.fluents
		.filter(fluent => fluent.type == "judgement" && fluent.args.length == 3)
		// We need to remove duplicates, but can't use a set because strings aren't primitive
		.reduce((acc, judgement) => {
			// Construct the object using Prolog's complex term style
			var claim = jsonToPrologTerm(judgement.args[2]);
			var obj = { fluent_data : claim, display : `Judgements for ${claim}` };

			// Is the object already there?
			const already_present = acc.find(f => f.fluent_data == claim);
			if (!already_present) acc.push(obj);
			return acc;
		}, options);
	
	// Turn the options into one string of html elements
	const options_html = options.reduce((text, option) => {
		// If this option is not already selected, don't give it the 'active' class
		if (fluent_graph != option.fluent_data) return `${text}<button onclick="changeFluentMode(this, \'${option.fluent_data}\')">${option.display}</button>`;
		// Otherwise, it's active
		return `${text}<button class="active" onclick="changeFluentMode(this, \'${option.fluent_data}\')">${option.display}</button>`;
	}, "");

	// Displaying the options
	const display_options_div = document.getElementById("display-options");
	display_options_div.innerHTML = options_html;

	// Do something if the currently selected fluent_graph mode isn't there
	// Maybe display ALL possible fluents, just greyed out to indicate that they aren't in the current timestamp?


	

	// const eventsLabel = document.getElementById("events-label");
	// eventsLabel.innerHTML = `Events at timestamp ${current_timestamp}`;
	// const eventsText = document.getElementById("events");
	// eventsText.value = current_info.events
	// 		.map(event => jsonToPrologTerm(event))
	// 		.join("\n");

	// const fluentsLabel = document.getElementById("fluents-label");
	// fluentsLabel.innerHTML = `Fluents just after timestamp ${current_timestamp}`;
	// const fluentsText = document.getElementById("fluents");
	// fluentsText.value = current_info.fluents
	// 		.map(fluent => jsonToPrologTerm(fluent))
	// 		.join("\n");

	// // Update textarea heights to fit text,
	// // making sure that events + fluents is not bigger than the maximum area
	// // by sharing the height by their ratios
	// maxHeight = window.innerHeight * 0.32;
	// minHeight = 15;

	// var eventsHeight = eventsText.value.split(/\r|\r\n|\n/).length
	// var fluentsHeight = fluentsText.value.split(/\r|\r\n|\n/).length

	// sum = fluentsHeight + eventsHeight;
	// if (sum == 0) sum = 1;
	
	// newFluentsHeight = Math.max((fluentsHeight / sum) * maxHeight, minHeight);
	// newEventsHeight = Math.max((eventsHeight / sum) * maxHeight, minHeight);

	// newFluentsHeight = Math.min(newFluentsHeight, maxHeight - minHeight);
	// newEventsHeight = Math.min(newEventsHeight, maxHeight - minHeight);

	// fluentsText.style.height = `${newFluentsHeight}px`;
	// eventsText.style.height = `${newEventsHeight}px`;

	// Update graph
	updateGraph();
}

//// Update graphical representation of the relevant fluents
function updateGraph() {
	// Get the information relating to the selected timestamp, making sure that the selected timestamp actually exists
	current_info = timeline.find(slice => slice.timestamp === current_timestamp);
	if (current_info == undefined) {
		current_info = timeline[0];
		current_timestamp = current_info.timestamp;
	}

	// If we don't have any information about fluents, return without drawing the graph.
	if (current_info == undefined) return;

	// Default to displaying trust fluents
	if (fluent_graph == undefined) fluent_graph = 'trust';

	if (fluent_graph == 'trust') var obj = calculateTrustGraph();
	// else var obj = calculateJudgementGraph();

	const { graph, display_information } = obj;

	// Display the graph
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
				'text-max-width': '100px',
				'text-overflow-wrap': 'anywhere',
				'text-events': 'yes'
			}
		},
		{
			selector: 'node.hover',
			style: {
				'background-color': '#666',
				'color': '#FFF',
				'font-size': '16px',
				'text-background-color': '#000',
				'text-background-opacity': '0.75',
				'text-background-shape': 'round-rectangle',
				'text-background-padding': '6px',
				'text-wrap': 'wrap',
				'text-max-width': '200px',
				'text-overflow-wrap': 'anywhere',
				'text-events': 'yes'
			}
		},
		{
			selector: 'edge',
			  style: {
				'width': 2,
				'line-color': '#666',
				'target-arrow-color': '#666',
				'target-arrow-shape': 'triangle',
				'curve-style': 'bezier',
				// 'label': 'data(weight)'
			  }
		}],
		layout: { 
			name: 'klay',
			klay: {
				spacing: 80,
				addUnnecessaryBendpoints: true,
				aspectRatio: 2,
				nodePlacement: 'LINEAR_SEGMENTS'
			}
		}
	});

	cy.minZoom(0.5);
	cy.maxZoom(2);

	// Ensure that the display text and hover effects are put into place
	cy.on("mouseover", "node", event => {
		event.target.addClass("hover");

		var current_id = event.target.id();
		var entry = display_information.find(node => node.id == current_id);
		selected_node = current_id;

		const selected_node_label = document.getElementById('selected-node-label');
		const selected_node_text = document.getElementById('selected-node-text');
	
		if (fluent_graph == 'trust') selected_node_label.innerHTML = `Judgements held by ${selected_node}`;
		else selected_node_label.innerHTML = "Proof tree for selected judgement";
	
		// Update the display area text
		var entry = display_information.find(node => node.id == selected_node);
		if (entry != undefined) selected_node_text.value = entry.text;
		else selected_node_text.value = "";
	});
	cy.on("mouseout", "node.hover", event => event.target.removeClass("hover"));


	// Update the display area label and text
	const selected_node_label = document.getElementById('selected-node-label');
	const selected_node_text = document.getElementById('selected-node-text');

	if (selected_node == undefined) {
		selected_node_label.innerHTML = "No node selected";
		selected_node_text.value = "";
		return;
	}
	else if (fluent_graph == 'trust') selected_node_label.innerHTML = `Judgements held by ${selected_node}`;
	else selected_node_label.innerHTML = "Proof tree for selected judgement";

	// Update the display area text
	var entry = display_information.find(node => node.id == selected_node);
	if (entry != undefined) selected_node_text.value = entry.text;
	else selected_node_text.value = "";
}

function calculateTrustGraph() {
	// Collect the nodes and edges in a set
	var graph = new Set();

	// We also want to display judgements when nodes are hovered over
	var display_information = [];
	var judgements = current_info.fluents.filter(fluent => fluent.type == "judgement" && fluent.args.length == 3);

	// Iterate over all the trust/2 fluents to find the relevant nodes and edges
	current_info.fluents
		// We only want nodes involved in 'trust/2' fluents
		.filter(fluent => fluent.type == "trust" && fluent.args.length == 2)
		.forEach(fluent => {

			// Construct the display text for this node
			const constructDisplayText = id => {

				var display_text = judgements.reduce((text, judgement) => {
					// This judgement doesn't relate to this node, so don't change the text
					if (judgement.args[0] != id) return text;
					// Otherwise, update the text by including this judgement (in Prolog style)
					return `${text}\n${jsonToPrologTerm(judgement)}`;
				}, "");

				return display_text;
			};

			const name_1 = fluent.args[0];
			const display_text_1 = constructDisplayText(name_1);
			graph.add({ data : { id : name_1 }});
			display_information.push({
				id : name_1,
				text : display_text_1
			});

			const name_2 = fluent.args[1];
			const display_text_2 = constructDisplayText(name_2);
			graph.add({ data : { id : name_2 }});
			display_information.push({
				id : name_2,
				text : display_text_2
			});
			
			// Add the edge to the graph
			graph.add({
				data : { 
					id : `trust(${name_1}, ${name_2})`,
					source : name_1,
					target : name_2,
					weight : fluent.value
				}
			})
		});
	
	return { graph, display_information };
}

function jsonToPrologTerm(json) {
	// List of json terms (base case)
	//// if (json instanceof Array) return json.map(element => jsonToPrologTerm(element));
	// Printing all of the 'evidence' for a judgement makes it impossible to read!
	// Instead, we note that it has been cut down - the proof tree can be found elsewhere
	if (json instanceof Array) return "(...)";

	// Atoms (base case)
	if (json.args == undefined) return json;
	
	// Complex terms (recursive case)
	jsonifiedArguments = json.args.map(arg => jsonToPrologTerm(arg));
	functor = json.type;
	
	if (json.value == undefined) return `${functor}(${jsonifiedArguments})`;

	value = (Math.round(json.value * 100) / 100).toFixed(2);
	return `${functor}(${jsonifiedArguments})=${value}`;
}

// Callback function for when a button is clicked in order to change
// the fluents being graphically displayed
function changeFluentMode(button, fluent_graph_mode) {
	// If this button is already in the "active" class, return early
	const classList = Array.from(button.classList);
	if (classList.includes('active')) return;

	// Remove all of the sibling elements from the "active" class, and add it to this button
	const siblingNodes = Array.from(button.parentNode.children);
	siblingNodes.forEach(node => node.classList.remove('active'));
	button.classList.add('active');

	// Redraw the graph accordingly
	fluent_graph = fluent_graph_mode;
	updateGraph();
}


function moveSlider(event) {
	if (timeline == undefined) return;
	
	current_timestamp = timeline[event.target.value - 1].timestamp;

	updateDisplay();
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
			updateDisplay();
        });

    event.preventDefault();
}

// Event listeners
// Sending data to the server
document.getElementById("update_eec").addEventListener("submit", submit);
// Moving the timeline slider
document.getElementById("timeline").addEventListener("input", moveSlider);

// Object holding all of the calculated events and fluents for a given narrative and set of rules
var timeline = undefined;
// The timestamp being displayed
var current_timestamp = undefined;
// The type of fluents being displayed in the graph
var fluent_graph = 'trust';
// The node in the graph that is currently selected
// If a node is selected, its information (e.g. judgements) are displayed
var selected_node = undefined;