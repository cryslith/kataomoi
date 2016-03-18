var socket = new WebSocket("ws://127.0.0.1:8000"); // for testing
var users = new Map();
var name = undefined;

var states = {INITIAL: "initial", TOLDTHEM: "toldthem", TOLDUS: "toldus",
              DONE: "done"};
var likes = {UNKNOWN: "unknown", DONTLIKE: 0, LIKE: 1};

socket.onmessage = receiveServer_raw;

function receiveServer_raw(event) {
    console.log(event.data);
    receiveServer(JSON.parse(event.data));
}

function receiveServer(data) {
    switch (data["type"]) {
    case "users":
        updateUsers(users, data["users"]);
        var uList = userList(users)
        document.getElementById("users").innerHTML = uList;
        break;
    case "client":
        if (data["recipient"] != name) {
            console.log("spurious message");
            return;
        }
        console.log(data["payload"]);
        receiveClient(data["sender"],
                      decodePayload(data["sender"], data["payload"]));
        break;
    case "error":
        console.log("server error: " + data["error"]);
        break;
    default:
        console.log("invalid message type");
        return;
    }
}

function signIn() {
    name = document.forms["signin"]["name"].value;
    document.getElementById("name").innerHTML = name;
    sendServer({"type": "join", "name": name});
}

function usernameOK(username) {
    return /^[a-zA-Z0-9]+$/.test(username);
}

function userList(users) {
    return "<form name=\"selection\"" +
        "onsubmit=\"selections(); return false\">" +
        [...users].map(function([user, data]) {
            if (user == name) {
                return user + " (you)<br>";
            }
            else {
                return "<label for=\"button_" + user + "\">" +
                    user + "</label>" +
                    "<input type=\"checkbox\" name=\"" + user +
                    "\" id=\"button_" + user + "\">" +
                    "<span id=\"result_" + user + "\"></span>" +
                    "<br>";
            }
        }).join("") +
        "<input type=\"submit\">" +
        "</form>";
}

function updateUsers(users, newUsernames) {
    newUsernames.forEach(function(username) {
        if (usernameOK(username)) {
            if (!users.has(username)) {
                users.set(username, {"state": states.INITIAL,
                                     "like": likes.UNKNOWN,
                                     "likesus": likes.UNKNOWN});
            }
        }
        else {
            console.log("illegal username: " + username);
        }
    });
    [...users].forEach(function([username, data]) {
        if (newUsernames.indexOf(username) == -1) {
            users.delete(username);
        }
    });
}

function selections() {
    userloop:
    for (var [username, data] of users) {
        if (username == name) {
            continue;
        }
        switch(data["state"]) {
        case states.INITIAL:
            data["state"] = states.TOLDTHEM;
            break;
        case states.TOLDUS:
            data["state"] = states.DONE;
            break;
        case states.TOLDTHEM:
        case states.DONE:
            continue userloop;
        default:
            console.log("invalid state " + data["state"]);
            return;
        }
        data["like"] = document.getElementById("button_" + username).checked ?
            likes.LIKE : likes.DONTLIKE;
        sendClient(username, {"type": "choice", "like": data.like});
        displayResult(username);
    }
}

function sendServer(x) {
    socket.send(JSON.stringify(x));
}

function sendClient(recipient, message) {
    sendServer({"type": "client", "sender": name, "recipient": recipient,
                "payload": encodePayload(recipient, message)});
}

function receiveClient(sender, message) {
    switch (message["type"]) {
    case "choice":
        updateLikesUs(sender, message["like"]);
        break;
    default:
        console.log("invalid message type");
        return;
    }
}

function updateLikesUs(sender, likesUs) {
    data = users.get(sender);
    switch (data["state"]) {
    case states.INITIAL:
        data["state"] = states.TOLDUS;
        break;
    case states.TOLDTHEM:
        data["state"] = states.DONE;
        break;
    case states.TOLDUS:
    case states.DONE:
        console.log("already told us that");
        return;
    default:
        console.log("invalid state " + data["state"]);
        return;
    }
    data["likesus"] = likesUs;
    displayResult(sender);
}

function displayResult(username) {
    if (users.get(username)["state"] != states.DONE) {
        return;
    }
    result = users.get(username)["like"] == likes.LIKE &&
        users.get(username)["likesus"] == likes.LIKE;
    document.getElementById("result_" + username).innerHTML = result ?
        "yay" : "nay";
}

// TODO encryption
function encodePayload(recipient, message) {
    return JSON.stringify(message);
}

// TODO encryption
function decodePayload(sender, payload) {
    return JSON.parse(payload);
}
