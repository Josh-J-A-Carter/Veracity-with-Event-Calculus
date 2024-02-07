function updateDisplay() {
	// Get the information relating to the selected timestamp, making sure that the selected timestamp actually exists
	current_info = timeline.find(slice => slice.timestamp === current_timestamp);
	if (current_info == undefined) {
		current_info = timeline[0];
		current_timestamp = current_info.timestamp;
	}

	const indicator = document.getElementById('indicator');
	indicator.style.visibility = 'hidden';

	// Update the timeline slider
	const slider = document.getElementById("timeline");
	slider.max = timeline.length;
	slider.min = 1;

	// Update timestamp indicator
	const timestamp_label = document.getElementById("timestamp");
	timestamp.innerHTML = `T=${current_timestamp}`;

	// Update the graph options
	// Always have the option of trust fluents, but also make it clear if there aren't any trust fluents in this current timestamp
	const trust_fluent_found = current_info.fluents.find(fluent => fluent.type == "trust" && fluent.args.length == 2)
	var options = [{ fluent_data : "trust", display : "Trust fluents", current : trust_fluent_found != undefined }];
	
	current_info.fluents
		.filter(fluent => fluent.type == "judgement" && fluent.args.length == 3)
		// We need to remove duplicates, but can't use a set because strings aren't primitive
		.reduce((acc, judgement) => {
			// Construct the object using Prolog's complex term style
			var claim = jsonToPrologTerm(judgement.args[2]);
			var obj = { fluent_data : claim, display : `Judgements for ${claim}`, current : true };

			// Is the object already there?
			const already_present = acc.find(f => f.fluent_data == claim);
			if (!already_present) acc.push(obj);
			return acc;
		}, options);
	
	// Get the judgement/3 fluents from all other timestamps and add them in as well
	// They should be selectable from any timestamp, but displayed as being not relevant to this timestamp
	timeline
		// We've already processed this time stamp, so ignore them.
		.filter(slice => slice.timestamp != current_timestamp)
		// Turn each timestamp into a list of its fluents, specifically the judgement/3 predicates
		.map(slice => slice.fluents.filter(fluent => fluent.type == "judgement" && fluent.args.length == 3))
		// Turn the list of lists of judgements into one list of judgements
		.flat(1)
		// Put the list of judgements into the 'options' list,
		// being careful to avoid duplicates
		.reduce((acc, judgement) => {
			// Construct the object using Prolog's complex term style
			var claim = jsonToPrologTerm(judgement.args[2]);
			var obj = { fluent_data : claim, display : `Judgements for ${claim}`, current : false };

			// Is the object already there?
			const already_present = acc.find(f => f.fluent_data == claim);
			if (!already_present) acc.push(obj);
			return acc;
		}, options);
	
	// Turn the options into one string of html elements
	const options_html = options.reduce((text, option) => {

		var is_active = graph_mode == option.fluent_data;
		var is_current = option.current == true;
		var classes = `${is_active ? "active" : ""} ${is_current ? "" : "not-current"}`;

		return `${text}<button class="${classes}" onclick="changeFluentMode(this, \'${option.fluent_data}\')">${option.display}</button>`;
	}, "");

	// Displaying the options
	const display_options_div = document.getElementById("display-options");
	display_options_div.innerHTML = options_html;

	// Adding up / down arrow bindings to switch between options
	var buttons = Array.from(display_options_div.children);
	buttons.forEach(button => {
		button.onkeydown = e => {
			switch(e.which) {
				case 38: // up
					var sibling = button.previousSibling;
					if (sibling == undefined) break;
					sibling.focus();
					// Call the sibling's onclick - need this weird syntax to change the execution context
					(function() {eval(sibling.getAttribute('onclick'))}).call(sibling);
					break;

				case 40: // down
					var sibling = button.nextSibling;
					if (sibling == undefined) break;
					sibling.focus();
					// Call the sibling's onclick - need this weird syntax to change the execution context
					(function() {eval(sibling.getAttribute('onclick'))}).call(sibling);
					break;

				default: return; // exit this handler for other keys
			}
			e.preventDefault(); // prevent the default action (scroll / move caret)
		};
	});


	//// Update the fluents / events for this timestamp

	const events_text = document.getElementById("events-text");
	events_text.innerHTML = current_info.events
			.map(event => jsonToPrologTerm(event))
			.join("<br>");

	const fluents_text = document.getElementById("fluents-text");
	fluents_text.innerHTML = current_info.fluents
			.map(fluent => jsonToPrologTerm(fluent))
			.join("<br>");


	//// Update the graph visualisation
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
	if (graph_mode == undefined) graph_mode = 'trust';

	// Calculate the nodes and edges
	if (graph_mode == 'trust') var obj = calculateTrustGraph();
	else var obj = calculateJudgementGraph();
	const { graph, display_information } = obj;

	// Sort the nodes and edges so that they will always be displayed in the same order
	const sorted_data = [...graph];
	sorted_data.sort((n1, n2) => n1.data.id.localeCompare(n2.data.id));

	// Display the graph
	var cy = cytoscape({
		container: document.getElementById("graph"), // container to render in
		elements: sorted_data,
	
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
			selector: 'node.highlight',
			style: {
				'background-color': '#04AA6D',
			}
		},
		{
			selector: 'node.select',
			style: {
				'border-width': 4,
		    	'border-color': '#6666ff'
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
		},
		{
			selector: 'edge.highlight',
			style: {
				'target-arrow-color': '#04AA6D',
				'line-color': '#04AA6D'
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

	// Set all of the elements that are labelled as highlighted to the 'highlight' class
	cy.elements('[highlight]').addClass('highlight');
	cy.getElementById(selected_node).addClass('select');

	// Make sure zooming is controlled and consistent between timestamps
	// i.e. swapping to another timestamp shouldn't reset the zoom level
	cy.minZoom(0.5);
	cy.maxZoom(2);
	cy.zoom(current_zoom);

	cy.on('zoom', _ => {
		current_zoom = cy.zoom();
	})

	// Do the same for screen position / pan
	if (screen_position == undefined) screen_position = cy.pan();
	cy.pan(screen_position);

	cy.on('pan', _ => {
		screen_position = cy.pan();
	})

	// Events for selecting a node, or hovering over it.
	cy.on("tap", "node", event => {
		// Remove selection style from previous node and add to the new one
		cy.getElementById(selected_node).removeClass('select');
		event.target.addClass('select');

		// Store which node is now selected
		selected_node = event.target.id();

		// Update the visual indicator for the current node
		const current_node_label = document.getElementById('current-node');
		current_node_label.innerHTML = `Node '${selected_node}' is selected`;
		
		// Update the display area text
		const selected_node_text = document.getElementById('selected-node-text');
		var entry = display_information.find(node => node.id == selected_node);
		if (entry != undefined) selected_node_text.innerHTML = entry.text;
		else selected_node_text.innerHTML = "";
	});

	cy.on("mouseover", "node", event => event.target.addClass("hover"));
	cy.on("mouseout", "node.hover", event => event.target.removeClass("hover"));


	// Update the display area label and text
	const selected_node_text = document.getElementById('selected-node-text');

	if (selected_node == undefined) {
		selected_node_text.innerHTML = "";
		return;
	}

	// Update the display area text
	var entry = display_information.find(node => node.id == selected_node);
	if (entry != undefined) selected_node_text.innerHTML = entry.text;
	else selected_node_text.innerHTML = "";
}

function calculateJudgementGraph() {
	// Collect the nodes and edges in a set
	var graph = new Set();

	// We also want to display judgements when nodes are hovered over
	var display_information = [];
	var trust = current_info.fluents.filter(fluent => fluent.type == "trust" && fluent.args.length == 2);

	// Iterate over all the trust/2 fluents to find the relevant nodes and edges
	current_info.fluents
		// We only want nodes involved in 'judgement/3' fluents, relating to the selected claim / graph mode
		.filter(fluent => fluent.type == "judgement" && fluent.args.length == 3 && jsonToPrologTerm(fluent.args[2]) == graph_mode)
		.forEach(fluent => {
			const name = fluent.args[0];
			const display_text = buildProofTree(fluent);
			graph.add({ data : { id : name, highlight : true }});
			display_information.push({
				id : name,
				text : display_text
			});
			
			// Go through the trust/2 fluents to find all the nodes which trust this one
			// They will also be involved in this graph, so there should be an edge between them
			trust.forEach(fluent => {
				var trustor = fluent.args[0];
				var trustee = fluent.args[1];
				if (trustee != name) return;

				graph.add({
					data : { 
						id : `trust(${trustor}, ${trustee})`,
						source : trustor,
						target : trustee,
						highlight : true
					}
				})
			});
		});

	// Add the other edges and nodes from the trust graph, but don't highlight them
	trust.forEach(fluent => {
		var list_graph = [...graph];

		// If each node isn't already present, then add it
		var trustor = fluent.args[0];
		var found_trustor = list_graph.find(element => element.id == trustor);
		if (!found_trustor) {
			graph.add({ data : { id : trustor }});
			display_information.push({
				id : trustor,
				text : ""
			});
		}

		var trustee = fluent.args[1];
		var found_trustee = list_graph.find(element => element.id == trustee);
		if (!found_trustee) {
			graph.add({ data : { id : trustee }});
			display_information.push({
				id : trustee,
				text : ""
			});
		}

		// Add the edge between them, unless either of the nodes are already in the graph
		if (found_trustee | found_trustor) return;

		graph.add({
			data : { 
				id : `trust(${trustor}, ${trustee})`,
				source : trustor,
				target : trustee
			}
		})
	});
	
	return { graph, display_information };
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

				var text = judgements
							.filter(judgement => judgement.args[0] == id)
							.map(judgement => jsonToPrologTerm(judgement))
							.join('<br>');

				return text;
			};

			const name_1 = fluent.args[0];
			const display_text_1 = constructDisplayText(name_1);
			graph.add({ data : { id : name_1, highlight : true }});
			display_information.push({
				id : name_1,
				text : display_text_1
			});

			const name_2 = fluent.args[1];
			const display_text_2 = constructDisplayText(name_2);
			graph.add({ data : { id : name_2, highlight : true }});
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
					highlight : true
				}
			})
		});
	
	return { graph, display_information };
}

function buildProofTree(json_judgement) {
	const { pre_text, conclusion } = proofTreeRecurse(json_judgement);

	if (pre_text == undefined) return conclusion;

	return pre_text;
}

function proofTreeRecurse(json_judgement, claim_styles = {}) {
	// Base case: atom
	// We don't need to prove atomic pieces of evidence any further.

	if (json_judgement.type == 'trust') return { 
		pre_text: undefined, 
		conclusion : jsonToPrologTerm(json_judgement),
		atomic : false,
		atomic_evidence : []
	}

	if (json_judgement.args == undefined) return { 
		pre_text : undefined, 
		conclusion : json_judgement,
		atomic : true,
		atomic_evidence : [json_judgement]
	};

	var agent = jsonToPrologTerm(json_judgement.args[0]);
	var evidence_objects = json_judgement.args[1].map(e => proofTreeRecurse(e, claim_styles));
	var claim = jsonToPrologTerm(json_judgement.args[2]);
	var confidence = (Math.round(json_judgement.value * 100) / 100).toFixed(2);

	// Including the proofs for the evidence that this current judgement depends on
	// Only relevant when the pieces of evidence are not atomic
	var previous_proofs = evidence_objects
					.filter(e => e.pre_text != undefined)
					.map(e => e.pre_text)
					.reduce((text, e) => `<br><div class="skip-line"></div>${e}${text}`, '');
	
	// Collect the atomic witnesses which combine to this current judgement, and display them as text
	// Make sure that there are no duplicate atomic witnesses
	var witnesses = evidence_objects
						.map(e => e.atomic_evidence)
						.reduce((acc, list) => acc.concat(list), [])
						// .filter(e => e != 'true' && e != 'false');

	var no_duplicates = new Set(witnesses);
	var witness_text = [...no_duplicates].join(',');
	if (witnesses.length > 1) witness_text = `(${witness_text})`;
	
	// Using the conclusions of each bit of evidence which this judgement depends on,
	// we then format this together in veracity logic style
	var evidence = evidence_objects
					.map(e => e.conclusion)
					.join(', ');
	
	// Style the claim (uniquely) for better readability
	if (claim_styles[claim] == undefined) {
		var taken_styles = Object.keys(claim_styles).length;
		var colour_options = Object.keys(possible_claim_colours).length;
		var decoration_options = Object.keys(possible_claim_decoration).length;

		if (taken_styles >= colour_options * decoration_options) claim_styles[claim] = '';
		
		else {
			// Colour choices wrap around
			var next_colour_index = taken_styles % colour_options;
			// Determine which text decoration is required, based on the number of taken styles
			// We know this won't go past decoration_options because we've already checked that in the if statement
			var next_decoration_index = Math.trunc(taken_styles / colour_options);
			claim_styles[claim] = 
				`color : ${possible_claim_colours[next_colour_index]} ;
				text-decoration : ${possible_claim_decoration[next_decoration_index]}`;
		}
	}

	var claim_element = `<span style="${claim_styles[claim]}">${claim}</span>`;
	
	// Glue it all together with logic symbols, and return it
	var scripts = `<div class="scripts"><span class="super">${agent}</span><span class="sub">${confidence}</span></div>`;

	var atomic_evidence = evidence_objects.find(e => e.atomic == true);
	if (atomic_evidence) var conclusion = `${witness_text}${scripts} ∈ ${claim_element}`;

	else {
		var pre_text = `${evidence} ⊢ ${witness_text}${scripts} ∈ ${claim_element}${previous_proofs}`;
		var conclusion = `${witness_text}${scripts} ∈ ${claim_element}`;
	}

	return { 
		pre_text : pre_text,
		conclusion : conclusion,
		atomic : false,
		atomic_evidence : witnesses,
		claim_colours : claim_styles
	};
}

// The bindings parameter holds the Prolog variable bindings across the entire (complex) term
function jsonToPrologTerm(json, bindings = {}) {
	// List of json terms (base case)
	// Printing all of the 'evidence' for a judgement makes it impossible to read!
	// Instead, we note that it has been cut down - the proof tree can be found elsewhere
	if (json instanceof Array) return "(...)";

	// Atoms (base case)
	if (json.args == undefined) {
		// If it starts with an underscore, it is a Prolog variable and needs to be rebound
		if (json[0] == '_') {
			// This variable is not yet bound, so create a new binding, starting from V1, V2, V3, ...
			if (bindings[json] == undefined) {
				var nth_variable = Object.keys(bindings).length + 1;
				var new_binding = 'V' + nth_variable;
				bindings[json] = new_binding;
			}

			json = bindings[json];
		}

		return json;
	}

	// if (json.type == '{}') {
	// 	console.log(json.args[0]);
	// 	return `{${json.args[0]}}`;
	// }

	if (json.type == 'implies') {
		var conditions = json.args[0]
							.filter(condition => condition.type != '{}')
							.map(condition => jsonToPrologTerm(condition, bindings));
		var conditions_text = conditions.join(', ');
		if (conditions.length > 1) conditions_text = `(${conditions_text})`;
	
		var conclusion = jsonToPrologTerm(json.args[1], bindings);
		return 	`${conditions_text} ⇒ ${conclusion}`;
	}
	
	// Complex terms (recursive case)
	jsonified_arguments = json.args.map(arg => jsonToPrologTerm(arg, bindings));
	functor = json.type;
	
	if (json.value == undefined) return `${functor}(${jsonified_arguments})`;

	value = (Math.round(json.value * 100) / 100).toFixed(2);
	return `${functor}(${jsonified_arguments})=${value}`;
}

// Callback function for when a button is clicked in order to change
// the fluents being graphically displayed
function changeFluentMode(button, mode) {
	// If this button is already in the "active" class, return early
	const classList = Array.from(button.classList);
	if (classList.includes('active')) return;

	// Remove all of the sibling elements from the "active" class, and add it to this button
	const siblingNodes = Array.from(button.parentNode.children);
	siblingNodes.forEach(node => node.classList.remove('active'));
	button.classList.add('active');

	// Redraw the graph accordingly
	graph_mode = mode;
	updateGraph();
}


function toggleMenu(label) {
	// Get the menu which this label refers to, and toggle its state
	const menu_id = label.htmlFor;
	const menu = document.getElementById(menu_id);
	const classList = Array.from(menu.classList);

	if (classList.includes('opened')) menu.classList.remove('opened');
	else menu.classList.add('opened');

	// Recalculate the size that all the menus need to be;
	// this depends on how many menus are open, and the viewport size.

	var total_height = window.innerHeight * 0.3;

	// How many menus are open now?
	var menu_wrappers = Array.from(menu.parentNode.parentNode.children);
	
	var num_open = menu_wrappers.filter(menu_wrapper => {
		const menu = menu_wrapper.querySelector('.menu');

		if (menu == undefined) return false;
		
		const classList = Array.from(menu.classList);
		return classList.includes('opened');
	}).length;

	var individual_height = total_height / num_open;
	var old_height = total_height / (num_open + 1);

	const root = document.querySelector(':root');
	root.style.setProperty('--menu-height', `${individual_height}px`);
	root.style.setProperty('--old-menu-height', `${old_height}px`);
}


function moveSlider(event) {
	if (timeline == undefined) return;
	
	current_timestamp = timeline[event.target.value - 1].timestamp;

	updateDisplay();
}

// Update the EEC without redirecting to another page
function submit(event) {
    var url = "/update_dec";
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
			if (data.success == false) throw new Error(data.error_type);

            timeline = data.timeline;
			updateDisplay();
        })
		.catch(error => handleError(error));

    event.preventDefault();
}

function handleError(error) {
	var message = "Server unresponsive - there may be an infinite loop";

	if (error.message == 'syntax_error') message = "Syntax error(s) reported";
	if (error.message == 'warning') message = "Warning(s) reported";

	const indicator = document.getElementById('indicator');
	indicator.innerHTML = message;
	indicator.style.visibility = 'visible';
}

// Event listeners
// Sending data to the server
document.getElementById("update_dec").addEventListener("submit", submit);
// Moving the timeline slider
document.getElementById("timeline").addEventListener("input", moveSlider);

// Object holding all of the calculated events and fluents for a given narrative and set of rules
var timeline = undefined;
// The timestamp being displayed
var current_timestamp = undefined;
// The type of fluents being displayed in the graph
var graph_mode = 'trust';
// The node in the graph that is currently selected
// If a node is selected, its information (e.g. judgements) are displayed
var selected_node = undefined;

// Current zoom level and screen position
var current_zoom = 1;
var screen_position = undefined;


// Style options for making claims more readable
possible_claim_colours = [
		'#0075DC',	// Blue
		'#FF0010',	// Red
		'#F0A3FF',	// Amethyst
		'#2BCE48',	// Green
		'#740AFF',	// Violet
		'#C22299',	// "Mallow"
		'#00998F',	// Turquoise
		'#FF8405',	// Orange
		'#5EE1E2',	// "Sky"
		'#FFD100',	// Yellow
		'#BB4444'	// "Wine"
	];

possible_claim_decoration = [
	"none",
	"underline",
	"wavy underline",
	"underline dotted"
];
