
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
        .then(res => res.text())
        .then(data => {
            const displayArea = document.getElementById("display");
            displayArea.innerHTML = `<p> ${data} </p>`;
        });

    event.preventDefault();
}

// Adding the event listener
document.getElementById("update_eec").addEventListener("submit", submit);

