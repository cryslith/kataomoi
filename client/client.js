var states = {INITIAL: "initial", INITIATED: "initiated",
              THEYINITIATED: "theyinitiated",
              RESPONDED: "responded", DECRYPTED: "decrypted",
              REVEALED: "revealed", CONFIRMED: "confirmed",
              CHEAT: "cheat"};
var likes = {UNKNOWN: "unknown", DONTLIKE: 0, LIKE: 1};
var rsa = forge.pki.rsa;
var e64 = forge.util.encode64;
var d64 = forge.util.decode64;
var TUNNEL_BITS = 2048;
var SENPAI_BITS = 1024; // RSA in JS is really slow; the outer layer of crypto protects us from the NSA, and your crush probably doesn't have a supercomputer
var PUBLIC_EXPONENT = 0x10001;
var eBI = new forge.jsbn.BigInteger("" + PUBLIC_EXPONENT, 10);
var SENPAI_PUBLIC_EXPONENT = 0x10001;
var seBI = new forge.jsbn.BigInteger("" + SENPAI_PUBLIC_EXPONENT, 10);
var S_LEN = 32; // bytes

// TODO: use wss so NSA can't spy on the social graph
var socket = new WebSocket("ws://127.0.0.1:8000"); // for testing
var users = new Map();
var name = undefined;
var requestedName = undefined;
var keypair = rsa.generateKeyPair({bits: TUNNEL_BITS, e: PUBLIC_EXPONENT});


socket.onmessage = receiveServer_raw;

function receiveServer_raw(event) {
    console.log(event.data);
    receiveServer(JSON.parse(event.data));
}

function receiveServer(data) {
    switch (data["type"]) {
    case "welcome":
        if (data["name"] == requestedName) {
            name = data["name"];
            users.set(name,
                      {"pubkey": keypair.publicKey.n,
                       "state": states.INITIAL,
                       "like": likes.UNKNOWN,
                       "likesus": likes.UNKNOWN});
            document.getElementById("signin").style = "display:none;";
            document.getElementById("name").innerHTML = "Welcome, " + name + "!";
            document.getElementById("users").style = "";
            showUsers(users); // in case we process "users" message before our own name is confirmed by "welcome"
        } else {
            console.log("server tried to assign us unrequested name: " + data["name"]);
        }
        break;
    case "unavailable":
        if (data["name"] == requestedName) {
            document.getElementById("name").innerHTML =
            "<span style=\"color:red;\">" +
                "The username " + data["name"] + " is unavailable. " +
                "Please choose a different name."
            "</span>";
        } else {
            console.log("server rejected unrequested name: " + data["name"]);
        }
        break;
    case "users":
        updateUsers(users, data["users"]);
        showUsers(users);
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
    requestedName = document.forms["signin"]["name"].value;
    if (!usernameOK(requestedName)) {
        document.getElementById("name").innerHTML =
            "<span style=\"color:red;\">" +
            "name must match ^[a-zA-Z0-9]{1,8}$" +
            "</span>";
        return;
    }
    join();
}

function join() {
    sendServer({"type": "join", "name": requestedName,
                "pubkey": e64(bigNumToBytes(keypair.publicKey.n))});
}

function usernameOK(username) {
    return /^[a-zA-Z0-9]{1,8}$/.test(username);
}

function showUsers(users) {
    document.getElementById("userlist").innerHTML = userList(users);
}

function userList(users) {
    return "<form name=\"selection\" " +
        "onsubmit=\"selections()\" " +
        "action=\"javascript:void(0);\">" +
        "<table>" +
        "<tr><td class=\"user\"><b>User</b></td>" +
        "<td class=\"dtf\"><b>DTF?</b></td>" +
        "<td class=\"result\"><b>Result</b></td></tr>" +
        [...users].map(function([user, data]) {
            if (user == name) {
                return "<tr>" +
                    "<td class=\"user\">" + user + "</td>" +
                    "<td class=\"dtf\">(you)</td>" +
                    "<td class=\"result\"></td>" +
                    "</tr>";
            }
            else {
                return "<tr>" +
                    "<td class=\"user\">" +
                    "<label for=\"button_" + user + "\">" +
                    user + "</label></td>" +
                    "<td class=\"dtf\">" +
                    "<input type=\"checkbox\" name=\"" + user +
                    "\" id=\"button_" + user + "\"></td>" +
                    "<td class=\"result\" id=\"result_"
                    + user + "\"></td>" +
                    "</td></tr>";
            }
        }).join("") +
        "</table>" +
        "<input type=\"submit\" value=\"Send choices\">" +
        "</form>";
}

function updateUsers(users, newUsernames) {
    for (var username in newUsernames) {
        if (usernameOK(username)) {
            if (!users.has(username) && username != name) { // don't let server tell us our key
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
        if (!(username in newUsernames) && username != name) {
            users.delete(username);
        }
    });

    // update verification phrase
    var userKeys = [...users].map(function(currentValue, index, array) {
        return [currentValue[0], e64(bigNumToBytes(currentValue[1]["pubkey"]))];
    });
    userKeys.sort(function(a, b) {
        if (a[0] > b[0]) {
            return 1;
        }
        if (a[0] < b[0]) {
            return -1;
        }
        return 0;
    });
    document.getElementById("phrase").innerHTML = hashPhrase(JSON.stringify(userKeys));
}

function selections() {
    for (var [username, data] of users) {
        if (username == name) {
            continue;
        }
        var checkbox = document.getElementById("button_" + username);
        if (data["state"] == states.INITIAL ||
            data["state"] == states.THEYINITIATED) {
            data["like"] =
                checkbox.checked ? likes.LIKE : likes.DONTLIKE;
            sendSelection(username);
        }
        checkbox.disabled = true;
    }
}

function sendSelection(username) {
    var state = users.get(username)["state"];
    switch (state) {
    case states.INITIAL:
        initiate(username);
        break;
    case states.THEYINITIATED:
        respond(username);
        break;
    default:
        console.log("invalid state " + state);
        break;
    }

    displayResult(username);
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
    var key = rsa.generateKeyPair({bits: SENPAI_BITS, e: SENPAI_PUBLIC_EXPONENT});
    data["keypair"] = key;
    var s = forge.random.getBytesSync(S_LEN);
    var s_y1 = forge.random.getBytesSync(S_LEN);
    data["s"] = s;

    var x_raw = '\x00' + s;
    var x = forge.pkcs1.encode_rsa_oaep(key.publicKey, x_raw, '');
    data["x"] = x;

    var y_raw;
    if (data["like"] == likes.LIKE) {
        y_raw = '\x01' + s_y1;
    }
    else {
        y_raw = '\x00' + s;
    }
    var y = forge.pkcs1.encode_rsa_oaep(key.publicKey, y_raw, '');

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
    if (!(n.gcd(data["ye"]).equals(forge.jsbn.BigInteger.ONE) &&
          n.gcd(data["xe"]).equals(forge.jsbn.BigInteger.ONE))) {
        data["state"] = states.CHEAT;
        return;
    }
    var nbytes = Math.ceil(n.bitLength() / 8);
    var r = bytesToBigNum(forge.random.getBytesSync(nbytes+4)).mod(n);
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
    var n_bytes = Math.ceil(data["n"].bitLength() / 8);
    var w_bytes = zero_pad(w_bytes, n_bytes);
    if (w_bytes.length != n_bytes) {
        console.log("bad w_bytes length " + w_bytes.length);
        return;
    }
    var pubkey = rsa.setPublicKey(data["n"], seBI);
    var w_decode = forge.pkcs1.decode_rsa_oaep(pubkey, w_bytes, '');
    // TODO: catch errors from invalid OAEP
    var like = getFirstBit(w_decode) == 1;
    data["likemutual"] = like ? likes.LIKE : likes.DONTLIKE;

    if (like) {
        sendClient(username, {"type": "reveal",
                              "result": "true_",
                              "s": e64(forge.random.getBytesSync(S_LEN))});
    }
    else {
        var s = w_decode.slice(1);
        var sh_md = forge.md.sha256.create();
        sh_md.update(s);
        var sh = sh_md.digest().bytes();
        if (sh != data["sh"]) {
            console.log("detected cheating via s");
            data["state"] = states.CHEAT;
            return;
        }
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
    var x = data["x"];
    var pubkey = rsa.setPublicKey(data["n"], seBI);
    var xe = pubkey.encrypt(x, "RAW");

    if (data["xe"].toString() != bytesToBigNum(xe).toString()) {
        console.log(data["xe"].toString());
        console.log(bytesToBigNum(xe).toString());
        console.log("detected cheating via x");
        data["state"] = states.CHEAT;
        return;
    }

    var x_decode = forge.pkcs1.decode_rsa_oaep(pubkey, x, '');
    var s = x_decode.slice(1);
    var sh_md = forge.md.sha256.create();
    sh_md.update(s);
    var sh = sh_md.digest().bytes();
    if (sh != data["sh"]) {
        console.log("detected cheating via s");
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
            break;
        }
        break;
    case "respond":
        if (data["state"] != states.INITIATED) {
            console.log("wrong state for respond");
            break;
        }
        data["wre"] = bytesToBigNum(d64(message["wre"]));
        decrypt(sender);
        break;
    case "decrypt":
        if (data["state"] != states.RESPONDED) {
            console.log("wrong state for decrypt");
            break;
        }
        data["wr"] = bytesToBigNum(d64(message["wr"]));
        reveal(sender);
        break;
    case "reveal":
        if (data["state"] != states.DECRYPTED) {
            console.log("wrong state for reveal");
            break;
        }
        data["likemutual"] = message["result"] == "true_"
            ? likes.LIKE : likes.DONTLIKE;
        if ("s" in message) {
            data["so"] = d64(message["s"]);
        }
        confirm(sender);
        break;
    case "confirm":
        if (data["state"] != states.REVEALED) {
            console.log("wrong state for confirm");
            break;
        }
        data["x"] = d64(message["x"]);
        verify(sender);
        break;
    default:
        console.log("invalid message type");
        break;
    }

    displayResult(sender);
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
    var result = "";
    switch(data["state"]) {
    case states.INITIAL:
        result = "";
        break;
    case states.INITIATED:
        result = "Waiting for response...";
        break;
    case states.THEYINITIATED:
        result = "Awaiting your response...";
        break;
    case states.RESPONDED:
    case states.DECRYPTED:
        result = "Processing...";
        break;
    case states.REVEALED:
    case states.CONFIRMED:
        result = data["likemutual"] == likes.LIKE ?
            "<span style=\"color:green;\">Yes</span>" :
            "No";
        if (data["state"] != states.CONFIRMED) {
            result += " (Waiting for final verification...)";
        }
        break;
    case states.CHEAT:
        result = "<span style=\"color:red;\">CHEATING DETECTED</span>";
        break;
    }

    document.getElementById("result_" + username).innerHTML = result;
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

function hashPhrase(data) {
    var digest = forge.md.sha256.create();
    digest.update(data);
    var hashBuffer = digest.digest();
    var hashWords = new Array();
    for (var i = 0; i < hashBuffer.length(); ++i) {
        hashWords.push(wordList[i % 2][hashBuffer.at(i)]);
    }

    return hashWords.join(" ");
}

// PGP word list
var wordList = [
    [
        "aardvark",
        "absurd",
        "accrue",
        "acme",
        "adrift",
        "adult",
        "afflict",
        "ahead",
        "aimless",
        "Algol",
        "allow",
        "alone",
        "ammo",
        "ancient",
        "apple",
        "artist",
        "assume",
        "Athens",
        "atlas",
        "Aztec",
        "baboon",
        "backfield",
        "backward",
        "basalt",
        "beaming",
        "bedlamp",
        "beehive",
        "beeswax",
        "befriend",
        "Belfast",
        "berserk",
        "billiard",
        "bison",
        "blackjack",
        "blockade",
        "blowtorch",
        "bluebird",
        "bombast",
        "bookshelf",
        "brackish",
        "breadline",
        "breakup",
        "brickyard",
        "briefcase",
        "Burbank",
        "button",
        "buzzard",
        "cement",
        "chairlift",
        "chatter",
        "checkup",
        "chisel",
        "choking",
        "chopper",
        "Christmas",
        "clamshell",
        "classic",
        "classroom",
        "cleanup",
        "clockwork",
        "cobra",
        "commence",
        "concert",
        "cowbell",
        "crackdown",
        "cranky",
        "crowfoot",
        "crucial",
        "crumpled",
        "crusade",
        "cubic",
        "deadbolt",
        "deckhand",
        "dogsled",
        "dosage",
        "dragnet",
        "drainage",
        "dreadful",
        "drifter",
        "dropper",
        "drumbeat",
        "drunken",
        "Dupont",
        "dwelling",
        "eating",
        "edict",
        "egghead",
        "eightball",
        "endorse",
        "endow",
        "enlist",
        "erase",
        "escape",
        "exceed",
        "eyeglass",
        "eyetooth",
        "facial",
        "fallout",
        "flagpole",
        "flatfoot",
        "flytrap",
        "fracture",
        "fragile",
        "framework",
        "freedom",
        "frighten",
        "gazelle",
        "Geiger",
        "Glasgow",
        "glitter",
        "glucose",
        "goggles",
        "goldfish",
        "gremlin",
        "guidance",
        "hamlet",
        "highchair",
        "hockey",
        "hotdog",
        "indoors",
        "indulge",
        "inverse",
        "involve",
        "island",
        "Janus",
        "jawbone",
        "keyboard",
        "kickoff",
        "kiwi",
        "klaxon",
        "lockup",
        "merit",
        "minnow",
        "miser",
        "Mohawk",
        "mural",
        "music",
        "Neptune",
        "newborn",
        "nightbird",
        "obtuse",
        "offload",
        "oilfield",
        "optic",
        "orca",
        "payday",
        "peachy",
        "pheasant",
        "physique",
        "playhouse",
        "Pluto",
        "preclude",
        "prefer",
        "preshrunk",
        "printer",
        "profile",
        "prowler",
        "pupil",
        "puppy",
        "python",
        "quadrant",
        "quiver",
        "quota",
        "ragtime",
        "ratchet",
        "rebirth",
        "reform",
        "regain",
        "reindeer",
        "rematch",
        "repay",
        "retouch",
        "revenge",
        "reward",
        "rhythm",
        "ringbolt",
        "robust",
        "rocker",
        "ruffled",
        "sawdust",
        "scallion",
        "scenic",
        "scorecard",
        "Scotland",
        "seabird",
        "select",
        "sentence",
        "shadow",
        "showgirl",
        "skullcap",
        "skydive",
        "slingshot",
        "slothful",
        "slowdown",
        "snapline",
        "snapshot",
        "snowcap",
        "snowslide",
        "solo",
        "spaniel",
        "spearhead",
        "spellbind",
        "spheroid",
        "spigot",
        "spindle",
        "spoilage",
        "spyglass",
        "stagehand",
        "stagnate",
        "stairway",
        "standard",
        "stapler",
        "steamship",
        "stepchild",
        "sterling",
        "stockman",
        "stopwatch",
        "stormy",
        "sugar",
        "surmount",
        "suspense",
        "swelter",
        "tactics",
        "talon",
        "tapeworm",
        "tempest",
        "tiger",
        "tissue",
        "tonic",
        "tracker",
        "transit",
        "trauma",
        "treadmill",
        "Trojan",
        "trouble",
        "tumor",
        "tunnel",
        "tycoon",
        "umpire",
        "uncut",
        "unearth",
        "unwind",
        "uproot",
        "upset",
        "upshot",
        "vapor",
        "village",
        "virus",
        "Vulcan",
        "waffle",
        "wallet",
        "watchword",
        "wayside",
        "willow",
        "woodlark",
        "Zulu",
    ],
    [
        "adroitness",
        "adviser",
        "aggregate",
        "alkali",
        "almighty",
        "amulet",
        "amusement",
        "antenna",
        "applicant",
        "Apollo",
        "armistice",
        "article",
        "asteroid",
        "Atlantic",
        "atmosphere",
        "autopsy",
        "Babylon",
        "backwater",
        "barbecue",
        "belowground",
        "bifocals",
        "bodyguard",
        "borderline",
        "bottomless",
        "Bradbury",
        "Brazilian",
        "breakaway",
        "Burlington",
        "businessman",
        "butterfat",
        "Camelot",
        "candidate",
        "cannonball",
        "Capricorn",
        "caravan",
        "caretaker",
        "celebrate",
        "cellulose",
        "certify",
        "chambermaid",
        "Cherokee",
        "Chicago",
        "clergyman",
        "coherence",
        "combustion",
        "commando",
        "company",
        "component",
        "concurrent",
        "confidence",
        "conformist",
        "congregate",
        "consensus",
        "consulting",
        "corporate",
        "corrosion",
        "councilman",
        "crossover",
        "cumbersome",
        "customer",
        "Dakota",
        "decadence",
        "December",
        "decimal",
        "designing",
        "detector",
        "detergent",
        "determine",
        "dictator",
        "dinosaur",
        "direction",
        "disable",
        "disbelief",
        "disruptive",
        "distortion",
        "divisive",
        "document",
        "embezzle",
        "enchanting",
        "enrollment",
        "enterprise",
        "equation",
        "equipment",
        "escapade",
        "Eskimo",
        "everyday",
        "examine",
        "existence",
        "exodus",
        "fascinate",
        "filament",
        "finicky",
        "forever",
        "fortitude",
        "frequency",
        "gadgetry",
        "Galveston",
        "getaway",
        "glossary",
        "gossamer",
        "graduate",
        "gravity",
        "guitarist",
        "hamburger",
        "Hamilton",
        "handiwork",
        "hazardous",
        "headwaters",
        "hemisphere",
        "hesitate",
        "hideaway",
        "holiness",
        "hurricane",
        "hydraulic",
        "impartial",
        "impetus",
        "inception",
        "indigo",
        "inertia",
        "infancy",
        "inferno",
        "informant",
        "insincere",
        "insurgent",
        "integrate",
        "intention",
        "inventive",
        "Istanbul",
        "Jamaica",
        "Jupiter",
        "leprosy",
        "letterhead",
        "liberty",
        "maritime",
        "matchmaker",
        "maverick",
        "Medusa",
        "megaton",
        "microscope",
        "microwave",
        "midsummer",
        "millionaire",
        "miracle",
        "misnomer",
        "molasses",
        "molecule",
        "Montana",
        "monument",
        "mosquito",
        "narrative",
        "nebula",
        "newsletter",
        "Norwegian",
        "October",
        "Ohio",
        "onlooker",
        "opulent",
        "Orlando",
        "outfielder",
        "Pacific",
        "pandemic",
        "pandora",
        "paperweight",
        "paragon",
        "paragraph",
        "paramount",
        "passenger",
        "pedigree",
        "Pegasus",
        "penetrate",
        "perceptive",
        "performance",
        "pharmacy",
        "phonetic",
        "photograph",
        "pioneer",
        "pocketful",
        "politeness",
        "positive",
        "potato",
        "processor",
        "prophecy",
        "provincial",
        "proximate",
        "puberty",
        "publisher",
        "pyramid",
        "quantity",
        "racketeer",
        "rebellion",
        "recipe",
        "recover",
        "repellent",
        "replica",
        "reproduce",
        "resistor",
        "responsive",
        "retraction",
        "retrieval",
        "retrospect",
        "revenue",
        "revival",
        "revolver",
        "Sahara",
        "sandalwood",
        "sardonic",
        "Saturday",
        "savagery",
        "scavenger",
        "sensation",
        "sociable",
        "souvenir",
        "specialist",
        "speculate",
        "stethoscope",
        "stupendous",
        "supportive",
        "surrender",
        "suspicious",
        "sympathy",
        "tambourine",
        "telephone",
        "therapist",
        "tobacco",
        "tolerance",
        "tomorrow",
        "torpedo",
        "tradition",
        "travesty",
        "trombonist",
        "truncated",
        "typewriter",
        "ultimate",
        "undaunted",
        "underfoot",
        "unicorn",
        "unify",
        "universe",
        "unravel",
        "upcoming",
        "vacancy",
        "vagabond",
        "versatile",
        "vertigo",
        "Virginia",
        "visitor",
        "vocalist",
        "voyager",
        "warranty",
        "Waterloo",
        "whimsical",
        "Wichita",
        "Wilmington",
        "Wyoming",
        "yesteryear",
        "Yucatan",
    ]
];
