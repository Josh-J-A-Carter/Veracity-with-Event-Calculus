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
			var success = judgement.args[1].args[0];
			var obj = { fluent_data : claim, display : `Judgements for ${claim}`, current : true, success : success };

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


		// Show visual indicators for successful or failed proofs
		// If this isn't a judgement, then there shouldn't be such an indication
		if (option.success == undefined) {
			return `${text}<button class="${classes}" onclick="changeFluentMode(this, \'${option.fluent_data}\')">
						${option.display}
					</button>`;
		}
		else if (option.success) {
			// Icon from Roundicons - Flaticon
			return `${text}<button class="${classes}" onclick="changeFluentMode(this, \'${option.fluent_data}\')">
						${option.display}
						<img class="verification" src='img/success.png' />
					</button>`;
		}
		else {
			// Icon from kliwir art - Flaticon
			return `${text}<button class="${classes}" onclick="changeFluentMode(this, \'${option.fluent_data}\')">
						${option.display}
						<img class="verification" src='img/failure.png' />
					</button>`;
		}
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
		
		updateDisplayText(display_information);
	});

	cy.on("mouseover", "node", event => event.target.addClass("hover"));
	cy.on("mouseout", "node.hover", event => event.target.removeClass("hover"));

	updateDisplayText(display_information);
}

// Ensure that the display text area shows current information about selected node
function updateDisplayText(display_information) {

	if (selected_node == undefined) return;

	// Update the display area label and text
	const selected_node_text = document.getElementById('selected-node-text');

	// Update the display area text
	var entry = display_information.find(node => node.id == selected_node);
	if (entry != undefined) selected_node_text.innerHTML = entry.text;
	else selected_node_text.innerHTML = "";	

	// Is this a proof?
	var verification_indicator = document.getElementById('verification');
	// No, remove the indicator
	if (entry.success == undefined) {
		if (verification_indicator) verification_indicator.remove();
	// Yes, change the indicator to show success or failure
	} else {
		if (verification_indicator == undefined) {
			verification_indicator = document.createElement('img');
			verification_indicator.setAttribute('id', 'verification');
			verification_indicator.classList.add('verification');
			document.getElementById('selected-node-label')
					.appendChild(verification_indicator);
		}
		// Icon from Roundicons - Flaticon
		if (entry.success) {
			verification_indicator.src = 'img/success.png';
		}
		// Icon from kliwir art - Flaticon
		else verification_indicator.src = 'img/failure.png';
	}
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
			const proof = fluent.args[1];
			const success = proof.args[0];
			const display_text = buildProofTree(fluent);
			// Only show proof if successful
			graph.add({ data : { id : name, highlight : true }});
			display_information.push({
				id : name,
				text : display_text,
				success : success
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
	const p = json_judgement.args[1];

	const success = p.args[0];
	const error_location = p.args[1];
	const tactics = p.args[2];
	
	// Store context about the previous tactic
	// The stack is useful when breaking down tactics with two arguments, e.g. 'impl_elim' or 'and_intro'
	// Each argument has to be dealt with separately, but there may be more tactics before getting to the other one,
	// and so we'll lose the context of the original 'impl_elim' tactic by then
	init_actor = json_judgement.args[0];
	init_claim = json_judgement.args[2];
	init_confidence = json_judgement.value;
	initial_context = { actor : init_actor, claim : init_claim, confidence : init_confidence };
	context_stack = [initial_context];

	// Building proof as a list then calling join() on it
	proof = [];
	// Storing the styles for each claim so that they can be visually differentiated
	claim_styles = {};

	for (let [index, tactic] of tactics.entries()) {
		context = context_stack.pop();
		// This could just be a signal to add a gap element here
		if (context == 'gap') {
			proof.push(`<div class="skip-line"></div>`);
			context = context_stack.pop();
		}
		({ actor : prev_actor, claim : prev_claim, confidence : prev_confidence } = context);
		
		if (tactic.type == 'impl_elim') {
			[actor, left_evidence, right_evidence, left_confidence, right_confidence, left, right] = tactic.args;
			total_evidence = left_evidence.concat(right_evidence);

			left_judgement = judgementFormat(left_evidence, actor, left_confidence, left, undefined, claim_styles);
			right_judgement = judgementFormat(right_evidence, actor, right_confidence, right, undefined, claim_styles);
			prev_judgement = judgementFormat(total_evidence, actor, prev_confidence, prev_claim, undefined, claim_styles);

			proof.push(`${left_judgement}, ${right_judgement} ⊢ ${prev_judgement}
						<div class="skip-line"></div>`);

			context_stack.push({ actor : actor, claim : right, confidence : right_confidence });
			// We want the two arguments to be separated visually...
			context_stack.push('gap');
			context_stack.push({ actor : actor, claim : left, confidence : left_confidence });
		}
		else if (tactic.type == 'and_intro') {
			[actor, left_evidence, right_evidence, left_confidence, right_confidence, left, right] = tactic.args;
			total_evidence = left_evidence.concat(right_evidence);

			left_judgement = judgementFormat(left_evidence, actor, left_confidence, left, undefined, claim_styles);
			right_judgement = judgementFormat(right_evidence, actor, right_confidence, right, undefined, claim_styles);
			prev_judgement = judgementFormat(total_evidence, actor, prev_confidence, prev_claim, undefined, claim_styles);

			proof.push(`${left_judgement}, ${right_judgement} ⊢ ${prev_judgement}
						<div class="skip-line"></div>`);

			context_stack.push({ actor : actor, claim : right, confidence : right_confidence });
			// We want the two arguments to be separated visually...
			context_stack.push('gap');
			context_stack.push({ actor : actor, claim : left, confidence : left_confidence });
		}
		else if (tactic.type == 'or_intro1') {
			[actor, evidence, confidence, claim] = tactic.args;

			result = judgementFormat(evidence, actor, prev_confidence, prev_claim, undefined, claim_styles);
			disjunction = judgementFormat(evidence, actor, confidence, claim, undefined, claim_styles);

			proof.push(`${disjunction} ⊢ ${result}`);

			context_stack.push({ actor : prev_actor, claim : pl_claim, confidence : confidence });
		}
		else if (tactic.type == 'or_intro2') {
			[actor, evidence, confidence, claim] = tactic.args;

			result = judgementFormat(evidence, actor, prev_confidence, prev_claim, undefined, claim_styles);
			disjunction = judgementFormat(evidence, actor, confidence, claim, undefined, claim_styles);

			proof.push(`${disjunction} ⊢ ${result}`);

			context_stack.push({ actor : actor, claim : claim, confidence : confidence });
		}
		else if (tactic.type == 'trust') {
			[trustor, trustee, evidence, claim, weight] = tactic.args;
			
			pl_trustor = jsonToPrologTerm(trustor);
			pl_trustee = jsonToPrologTerm(trustee);

			// Undo the effect of applying T(weight), i.e. Trustor_Conf = Trustee_Conf * weight
			// so confidence = Trustee_Conf = Trustor_Conf / weight = prev_confidence / weight
			confidence = prev_confidence / weight;

			prev_judgement = judgementFormat(evidence, trustee, confidence, prev_claim, undefined, claim_styles);
			resulting_judgement = judgementFormat(evidence, trustor, prev_confidence, prev_claim, undefined, claim_styles);

			proof.push(`${prev_judgement},
					${pl_trustor} 
					T<div class="scripts">
						<span class="super"></span>
						<span class="sub">${weight.toFixed(2)}</span>
					</div> ${pl_trustee}
					⊢ ${resulting_judgement}`);

			context_stack.push({ actor : pl_trustee, claim : claim, confidence : confidence });
		}
		else if (tactic.type == 'assume') {
			[evidence, actor, confidence] = tactic.args;
			
			pl_judgement = judgementFormat(evidence, prev_actor, confidence, prev_claim, undefined, claim_styles);

			proof.push(`Assume ${pl_judgement}`);
			context_stack.push({ actor : prev_actor, claim : prev_claim, confidence : confidence });
		}
		else if (tactic == 'leaf') {
			claim_element = getStyledClaim(prev_claim, claim_styles)
			proof.push(`${claim_element} is a veracity claim`);
		}
		else throw new TypeError("Unknown tactic " + tactic);


		// If this is where a logical error occurred during verification of the judgement, mark it as such
		if (success == false && index == error_location) {
			proof[proof.length - 1] = `<b>Logic Error</b><br>
										<div class="verification-failure">${proof[proof.length - 1]}</div>`;
		}
	}

	return proof.join('<br>');
}

// Style the claim (uniquely) for better readability
function getStyledClaim(claim, claim_styles) {
	pl_claim = jsonToPrologTerm(claim);

	if (claim_styles[pl_claim] == undefined) {
		var taken_styles = Object.keys(claim_styles).length;
		var colour_options = Object.keys(possible_claim_colours).length;
		var decoration_options = Object.keys(possible_claim_decoration).length;

		if (taken_styles >= colour_options * decoration_options) claim_styles[pl_claim] = '';
		
		else {
			// Colour choices wrap around
			var next_colour_index = taken_styles % colour_options;
			// Determine which text decoration is required, based on the number of taken styles
			// We know this won't go past decoration_options because we've already checked that in the if statement
			var next_decoration_index = Math.trunc(taken_styles / colour_options);
			claim_styles[pl_claim] = 
				`color : ${possible_claim_colours[next_colour_index]} ;
				text-decoration : ${possible_claim_decoration[next_decoration_index]}`;
		}
	}

	return `<span style="${claim_styles[pl_claim]}">${pl_claim}</span>`;
}

function judgementFormat(evidence, actor, confidence, claim, tag, claim_styles) {

	pl_actor = jsonToPrologTerm(actor);

	// Style the claim for readability
	var claim_element = getStyledClaim(claim, claim_styles);

	// Parse the evidence into a Prolog term
	has_brackets = false;
	if (Array.isArray(evidence)) {
		pl_list = evidence.map(e => jsonToPrologTerm(e));
		pl_evidence = pl_list.join(', ');
		if (pl_list.length > 1) {
			pl_evidence = `(${pl_evidence})`;
			has_brackets = true;
		}
	} else pl_evidence = jsonToPrologTerm(evidence)

	scripts = `<div class="scripts">
		<span class="super">${pl_actor}</span>
		<span class="sub">${confidence.toFixed(2)}</span>
	</div>`;

	if (tag == undefined) return `${pl_evidence}${scripts} ∈ ${claim_element}`
	// Needed for disjunctions, e.g. i(a) ∈ A \/ B
	else {
		// Don't want two sets of brackets
		if (has_brackets) return `${tag}${pl_evidence}${scripts} ∈ ${claim_element}`
		return `${tag}(${pl_evidence})${scripts} ∈ ${claim_element}`
	}
}

// The bindings parameter holds the Prolog variable bindings across the entire (complex) term
function jsonToPrologTerm(json, bindings = {}) {

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
	
	if (json.type == 'proof') return "(...)";

	// Constraints are better not displayed, due to the infix operators
	if (json.type == '{}') return '{}';
	
	// Complex terms (recursive case)
	if (json.type == 'and') {
		var left = jsonToPrologTerm(json.args[0], bindings);
		var right = jsonToPrologTerm(json.args[1], bindings);

		if (left != '{}' && right != '{}') return `(${left} ∧ ${right})`;
		else if (left != '{}') return left;
		else if (right != '{}') return right;
		else return '{}'
	}

	if (json.type == 'or') {
		var left = jsonToPrologTerm(json.args[0], bindings);
		var right = jsonToPrologTerm(json.args[1], bindings);

		if (left != '{}' && right != '{}') return `(${left} ∨ ${right})`;
		else if (left != '{}') return left;
		else if (right != '{}') return right;
		else return '{}'
	}

	if (json.type == 'implies') {
		var left = jsonToPrologTerm(json.args[0], bindings);
		var right = jsonToPrologTerm(json.args[1], bindings);

		if (left != '{}' && right != '{}') return `(${left} → ${right})`;
		else if (left != '{}') return left;
		else if (right != '{}') return right;
		else return '{}'
	}
	
	// Some other recursive structure
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
	var message = "Server unresponsive - execution may be blocked";

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
