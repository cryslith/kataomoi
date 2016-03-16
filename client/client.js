var socket = new WebSocket("ws://127.0.0.1:8000"); // for testing
var users = new Map();

socket.onmessage = function (event) {
    console.log(event.data);
    var data = JSON.parse(event.data);
    switch (data["type"]) {
    case "users":
        updateUsers(users, data["users"]);
        var uList = userList(users)
        document.getElementById("users").innerHTML = uList;
        break;
    case "error":
        console.log("server error: " + data["error"]);
        break;
    default:
        console.log("invalid message type");
    }
}

function signIn() {
    var name = document.forms["signin"]["name"].value;
    document.getElementById("name").innerHTML = name;
    socket.send(JSON.stringify({"type": "join", "name": name}));
}

function usernameOK(username) {
    return /^[a-zA-Z0-9]+$/.test(username);
}

function userList(users) {
    return "<ul>" + [...users].map(function([user, data]) {
        return "<li>" + user + "</li>";
    }).join("") + "</ul>";
}

function updateUsers(users, newUsernames) {
    newUsernames.forEach(function(username) {
        if (usernameOK(username)) {
            if (!users.has(username)) {
                users.set(username, {});
            }
        }
    });
    [...users].forEach(function([username, data]) {
        if (newUsernames.indexOf(username) == -1) {
            users.delete(username);
        }
    });
}
