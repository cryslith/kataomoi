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
var eBI = new forge.jsbn.BigInteger("" + PUBLIC_EXPONENT, 10);
var SENPAI_PUBLIC_EXPONENT = 0x10001;
var seBI = new forge.jsbn.BigInteger("" + SENPAI_PUBLIC_EXPONENT, 10);
var X_LEN = 32; // bytes
var S_LEN = 32; // bytes

var socket = new WebSocket("ws://127.0.0.1:8000"); // for testing
var users = new Map();
var name = undefined;
var keypair = rsa.generateKeyPair({bits: NBITS, e: PUBLIC_EXPONENT});


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
        receiveClient(data["sender"], decodePayload(data));
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

    join();
}

function join() {
    sendServer({"type": "join", "name": name,
                "pubkey": e64(bigNumToBytes(keypair.publicKey.n))});
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
    for (var username in newUsernames) {
        if (usernameOK(username)) {
            if (!users.has(username)) {
                users.set(username,
                          {"pubkey":
                           bytesToBigNum(d64(newUsernames[username])),
                           "state": states.INITIAL,
                           "like": likes.UNKNOWN,
                           "likesus": likes.UNKNOWN});
            }
        }
        else {
            console.log("illegal username: " + username);
        }
    }

    [...users].forEach(function([username, data]) {
        if (!(username in newUsernames)) {
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
    var buffer = forge.util.createBuffer(bytes, "raw");
    buffer.setAt(0, buffer.at(0) & 0xfe | b);
    return buffer.bytes();
}

function getFirstBit(bytes) {
    var buffer = forge.util.createBuffer(bytes, "raw");
    return buffer.at(0) & 0x01;
}

function initiate(username) {
    var data = users.get(username);
    var key = rsa.generateKeyPair({bits: NBITS, e: SENPAI_PUBLIC_EXPONENT});
    data["keypair"] = key;
    var s = forge.random.getBytesSync(S_LEN);
    var s_y1 = forge.random.getBytesSync(S_LEN);
    data["s"] = s;
    // TODO pad x and y correctly, not by appending random bytes
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
    var data = users.get(username);
    var n = data["n"];
    // TODO verify that n is not too small
    var nbytes = Math.ceil(n.bitLength() / 8)
    var r = bytesToBigNum(forge.random.getBytesSync(nbytes));
    data["r"] = r;
    var re = r.modPow(seBI, n);
    var we = data["like"] == likes.LIKE ? data["ye"] : data["xe"];
    var wre = re.multiply(we).mod(n);

    sendClient(username, {"type": "respond",
                          "wre": e64(bigNumToBytes(wre))});

    data["state"] = states.RESPONDED;
}

function decrypt(username) {
    var data = users.get(username);
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
    var data = users.get(username);
    var wr = data["wr"];
    var rInv = data["r"].modInverse(data["n"]);
    var w = wr.multiply(rInv).mod(data["n"]);
    var w_bytes = bigNumToBytes(w);
    var w_bytes = zero_pad(w_bytes, X_LEN + S_LEN);
    if (w_bytes.length != X_LEN + S_LEN) {
        console.log("bad w_bytes length " + w_bytes.length);
        return;
    }
    var like = getFirstBit(w_bytes) == 1;
    data["likemutual"] = like ? likes.LIKE : likes.DONTLIKE;

    if (like) {
        sendClient(username, {"type": "reveal",
                              "result": "true"});
    }
    else {
        var s = w_bytes.slice(X_LEN); // TODO check s against sh
        sendClient(username, {"type": "reveal",
                              "result": "false",
                              "s": e64(s)});
    }


    data["state"] = states.REVEALED;
}

function confirm(username) {
    var data = users.get(username);
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
    var data = users.get(username);
    var x = data["x"]; // TODO check s from x against sh
    var pubkey = rsa.setPublicKey(data["n"], seBI);
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
    var encoded = encodePayload(recipient, message);
    encoded["type"] = "client";
    encoded["sender"] = name;
    encoded["recipient"] = recipient;
    sendServer(encoded);
}

function receiveClient(sender, message) {
    var data = users.get(sender);
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
        if ("s" in message) {
            data["so"] = d64(message["s"]);
        }
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
    var data = users.get(username);
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

function encodePayload(recipient, message) {
    var plaintext = JSON.stringify(message);
    var n = users.get(recipient)["pubkey"];
    var pubkey = rsa.setPublicKey(n, eBI);

    var aeskey = forge.random.getBytesSync(32); // AES-256
    var iv = forge.random.getBytesSync(16);

    var encryptedkey = pubkey.encrypt(aeskey);

    var cipher = forge.cipher.createCipher("AES-CBC", aeskey);
    cipher.start({"iv": iv});
    cipher.update(forge.util.createBuffer(plaintext));
    cipher.finish();
    var ciphertext = cipher.output.bytes();

    var digest = forge.md.sha256.create();
    digest.update(ciphertext);
    var signature = keypair.privateKey.sign(digest);

    return {"key": e64(encryptedkey),
            "iv": e64(iv),
            "payload": e64(ciphertext),
            "signature": e64(signature)};
}

function decodePayload(data) {
    var sender = data["sender"];
    var encryptedkey = d64(data["key"]);
    var iv = d64(data["iv"]);
    var payload = d64(data["payload"]);
    var signature = d64(data["signature"]);

    var n = users.get(sender)["pubkey"];
    var pubkey = rsa.setPublicKey(n, eBI);

    var digest = forge.md.sha256.create();
    digest.update(payload);
    var verified = pubkey.verify(digest.digest().bytes(), signature);
    if (!verified) {
        throw "Invalid signature";
    }

    var aeskey = keypair.privateKey.decrypt(encryptedkey);
    var decipher = forge.cipher.createDecipher("AES-CBC", aeskey);
    decipher.start({"iv": iv});
    decipher.update(forge.util.createBuffer(payload, "raw"));
    decipher.finish();

    var plaintext = decipher.output.bytes();
    console.log(plaintext);
    return JSON.parse(plaintext);
}
