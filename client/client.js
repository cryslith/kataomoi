var states = {INITIAL: "initial", INITIATED: "initiated",
              THEYINITIATED: "theyinitiated",
              RESPONDED: "responded", DECRYPTED: "decrypted",
              REVEALED: "revealed", CONFIRMED: "confirmed"};
var likes = {UNKNOWN: "unknown", DONTLIKE: 0, LIKE: 1};
var rsa = forge.pki.rsa;
var e64 = forge.util.encode64;
var d64 = forge.util.decode64;
var NBITS = 1024;
var PUBLIC_EXPONENT = 0x10001;
var SENPAI_PUBLIC_EXPONENT = 0x10001;
var eBI = new forge.jsbn.BigInteger("" + SENPAI_PUBLIC_EXPONENT, 10);
var X_LEN = 32; // bytes
var S_LEN = 32; // bytes

var socket = new WebSocket("ws://127.0.0.1:8000"); // for testing
var users = new Map();
var name = undefined;
//var keypair = rsa.generateKeyPair({bits: NBITS, e: PUBLIC_EXPONENT});


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
    if (!usernameOK(name)) {
        document.getElementById("name").innerHTML =
            "<span style=\"color:red;\">" +
            "name must match ^[a-zA-Z0-9]{1,8}$" +
            "</span>";
    }
    document.getElementById("name").innerHTML = "Welcome, " + name + "!";
    sendServer({"type": "join", "name": name});
}

function usernameOK(username) {
    return /^[a-zA-Z0-9]{1,8}$/.test(username);
}

function userList(users) {
    return "<form name=\"selection\" " +
        "onsubmit=\"selections()\" " +
        "action=\"javascript:void(0);\">" +
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
        "<input type=\"submit\" value=\"Send choices\">" +
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
    for (var [username, data] of users) {
        if (username == name) {
            continue;
        }
        if (data["state"] == states.INITIAL ||
            data["state"] == states.THEYINITIATED) {
            data["like"] =
                document.getElementById("button_" + username).checked ?
                likes.LIKE : likes.DONTLIKE;
            sendSelection(username);
        }
    }
}

function sendSelection(username) {
    var state = users.get(username)["state"];
    switch (state) {
    case states.INITIAL:
        initiate(username);
        return;
    case states.THEYINITIATED:
        respond(username);
        return;
    default:
        console.log("invalid state " + state);
        return;
    }
}

function withFirstBit(bytes, b) {
    var buffer = new forge.util.createBuffer();
    buffer.putBytes(bytes);
    buffer.setAt(0, buffer.at(0) & 0xfe | b);
    return buffer.bytes();
}

function getFirstBit(bytes) {
    var buffer = new forge.util.createBuffer();
    buffer.putBytes(bytes);
    return buffer.at(0) & 0x01;
}

function initiate(username) {
    var data = users.get(username);
    var key = rsa.generateKeyPair({bits: NBITS, e: SENPAI_PUBLIC_EXPONENT});
    data["keypair"] = key;
    var s = forge.random.getBytesSync(S_LEN);
    var s_y1 = forge.random.getBytesSync(S_LEN);
    data["s"] = s;
    var x0 = forge.random.getBytesSync(X_LEN);
    x = withFirstBit(x0, 0) + s;
    data["x"] = x;
    var y0 = forge.random.getBytesSync(X_LEN);
    if (data["like"] == likes.LIKE) {
        y = withFirstBit(y0, 1);
        y += s_y1;
    }
    else {
        y = withFirstBit(y0, 0);
        y += s;
    }

    var n = key.publicKey.n;
    var xe = key.publicKey.encrypt(x, "RAW");
    var ye = key.publicKey.encrypt(y, "RAW");
    var sh_md = forge.md.sha256.create();
    sh_md.update(s);
    var sh = sh_md.digest().bytes();

    sendClient(username, {"type": "initiate",
                          "n": e64(bigNumToBytes(n)),
                          "xe": e64(xe),
                          "ye": e64(ye),
                          "sh": e64(sh)});

    data["state"] = states.INITIATED;
}

function respond(username) {
    data = users.get(username);
    var n = data["n"];
    var nbytes = Math.ceil(n.bitLength() / 8)
    var r = bytesToBigNum(forge.random.getBytesSync(nbytes));
    data["r"] = r;
    var re = r.modPow(eBI, n);
    var we = data["like"] == likes.LIKE ? data["ye"] : data["xe"];
    var wre = re.multiply(we).mod(n);

    sendClient(username, {"type": "respond",
                          "wre": e64(bigNumToBytes(wre))});

    data["state"] = states.RESPONDED;
}

function decrypt(username) {
    data = users.get(username);
    var wre = data["wre"];
    var key = data["keypair"];
    var k = Math.ceil(key.publicKey.n.bitLength() / 8);
    var wre_bytes = zero_pad(bigNumToBytes(wre), k);
    var wr_bytes = key.privateKey.decrypt(wre_bytes, "RAW");

    sendClient(username, {"type": "decrypt",
                          "wr": e64(wr_bytes)});

    data["state"] = states.DECRYPTED;
}


function reveal(username) {
    data = users.get(username);
    var wr = data["wr"];
    var rInv = data["r"].modInverse(data["n"]);
    var w = wr.multiply(rInv).mod(data["n"]);
    w_bytes = bigNumToBytes(w);
    w_bytes = zero_pad(w_bytes, X_LEN + S_LEN);
    if (w_bytes.length != X_LEN + S_LEN) {
        console.log("bad w_bytes length " + w_bytes.length);
        return;
    }
    like = getFirstBit(w_bytes) == 1;
    data["likemutual"] = like ? likes.LIKE : likes.DONTLIKE;
    var s = undefined;
    if (!like) {
        s = w_bytes.slice(X_LEN); // TODO check s against sh
    }

    sendClient(username, {"type": "reveal",
                          "result": like ? "true" : false,
                          "s": s});

    data["state"] = states.REVEALED;
}

function confirm(username) {
    data = users.get(username);
    if (data["likemutual"] == likes.DONTLIKE) {
        if (data["so"] != data["s"]) {
            console.log("detected cheating via s");
            data["state"] = states.CHEAT;
            return;
        }
    }

    sendClient(username, {"type": "confirm",
                          "x": e64(data["x"])});

    data["state"] = states.CONFIRMED;
}

function verify(username) {
    data = users.get(username);
    var x = data["x"]; // TODO check s from x against sh
    var pubkey = rsa.setPublicKey(
        data["n"], eBI);
    var xe = pubkey.encrypt(x, "RAW");

    if (data["xe"].toString() != bytesToBigNum(xe).toString()) {
        console.log(data["xe"].toString());
        console.log(bytesToBigNum(xe).toString());
        console.log("detected cheating via x");
        data["state"] = states.CHEAT;
        return;
    }

    data["state"] = states.CONFIRMED;
}

function sendServer(x) {
    socket.send(JSON.stringify(x));
}

function sendClient(recipient, message) {
    sendServer({"type": "client", "sender": name, "recipient": recipient,
                "payload": encodePayload(recipient, message)});
}

function receiveClient(sender, message) {
    data = users.get(sender);
    switch (message["type"]) {
    case "initiate":
        switch (data["state"]) {
        case states.INITIATED:
            // resolve race condition via usernames
            if (name < sender) {
                break; // ignore
            }
            // fall-through
        case states.INITIAL:
            data["n"] = bytesToBigNum(d64(message["n"]));
            data["xe"] = bytesToBigNum(d64(message["xe"]));
            data["ye"] = bytesToBigNum(d64(message["ye"]));
            data["sh"] = d64(message["sh"]);
            data["state"] = states.THEYINITIATED;
            break;
        default:
            console.log("wrong state for initiate");
            return;
        }
        return;
    case "respond":
        if (data["state"] != states.INITIATED) {
            console.log("wrong state for respond");
            return;
        }
        data["wre"] = bytesToBigNum(d64(message["wre"]));
        decrypt(sender);
        return;
    case "decrypt":
        if (data["state"] != states.RESPONDED) {
            console.log("wrong state for decrypt");
            return;
        }
        data["wr"] = bytesToBigNum(d64(message["wr"]));
        reveal(sender);
        return;
    case "reveal":
        if (data["state"] != states.DECRYPTED) {
            console.log("wrong state for reveal");
            return;
        }
        data["likemutual"] = message["result"] == "true"
            ? likes.LIKE : likes.DONTLIKE;
        data["so"] = message["s"];
        confirm(sender);
        displayResult(sender);
        return;
    case "confirm":
        if (data["state"] != states.REVEALED) {
            console.log("wrong state for confirm");
            return;
        }
        data["x"] = d64(message["x"]);
        verify(sender);
        displayResult(sender);
        return;
    default:
        console.log("invalid message type");
        return;
    }
}

function bytesToBigNum(x) {
    eb = forge.util.createBuffer();
    eb.putBytes(x);
    return new forge.jsbn.BigInteger(eb.toHex(), 16);
}

function zero_pad(y, k) {
  var ed = forge.util.createBuffer();
  var zeros = k - y.length;
  while(zeros > 0) {
    ed.putByte(0x00);
    --zeros;
  }
  ed.putBytes(y);
  return ed.getBytes();
}

function bigNumToBytes(y) {
  var yhex = y.toString(16);
  var ed = forge.util.createBuffer();
  ed.putBytes(forge.util.hexToBytes(yhex));
  return ed.getBytes();
}

function displayResult(username) {
    data = users.get(username);
    if (data["state"] == states.CONFIRMED) {
        result = data["likemutual"] == likes.LIKE;
        document.getElementById("result_" + username).innerHTML = result ?
            "yay" : "nay";
    }
    else if (data["state"] == states.CHEAT) {
        document.getElementById("result_" + username).innerHTML =
            "<span style=\"color:red\">detected cheating</span>";
    }
}

// TODO encryption
function encodePayload(recipient, message) {
    return JSON.stringify(message);
}

// TODO encryption
function decodePayload(sender, payload) {
    return JSON.parse(payload);
}
