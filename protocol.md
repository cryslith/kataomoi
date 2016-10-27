# kataomoi protocol

## Overview

kataomoi allows users to conduct SENPAI protocol exchanges, performing
secure multiparty computation to determine the logical AND of two bits
held by two respective users. kataomoi consists of a server to
coordinate SENPAI exchanges, and various clients that connect to the
server and perform exchanges.

kataomoi is designed so that all interesting operations are performed
by the client; the sole purpose of the server is to establish
connections between clients and then let them talk to each other. This
furthers our goal of minimizing the amount of trust that needs to be
placed in the server.

All communication is performed using WebSockets; clients send JSON
data to the server, which processes it and passes it on to the
appropriate recipient.

## Protocol design

kataomoi's protocol is based around "rooms" containing "users".  A
room is a unique identifying string associated with a non-empty set of
users.  A user is an identifying string associated with a websocket,
an RSA public key, and a connection status (connected or
disconnected).  A user must belong to exactly one room, and a room
must contain at least one connected user.  Note that no two rooms on
the server can have the same name, and no two users within a single
room can have the same name; however, users in different rooms can
have the same name.

A client begins by generating an RSA keypair; they then establish a
websocket connection to the server, and send a message specifying a
room name and username and containing their public key.  If the
requested room does not exist, it is created.  If the requested
username is available within the requested room, the client's
combination of username, websocket, and public key is added to the
room as a new connected user.

Once a user is in a room, they can send the server messages to be
passed on to other connected users in the same room.  The server
provides the client with a list of users in the current room; users in
other rooms are not visible to the client.  Messages between users are
end-to-end encrypted and signed using the provided RSA keys, such that
they can't be read or modified by the server.  In our client
implementation, these messages are used to conduct SENPAI exchanges,
but in theory, non-spec-conformant clients could exchange any type of
message; the kataomoi server simply provides a simple encrypted chat
service.

When a user's websocket becomes disconnected, their connection status
in the room is changed to "disconnected", but they are still listed
among the room's users, and their name can't be reused.  Once all
users are disconnected, the room is destroyed; afterward, a new room
with the same name will be created if a new user requests that room
name.

## Structure of messages

All messages are JSON objects encoded as strings. These objects will
always have a "type" field indicating the type of the message, and
will often have "sender" and "recipient" fields; other fields
may be present depending on the type of the message.

## Client-to-server message types

### join

A "join" message is sent by the client when they sign in to the
server. Its fields are:

* room: the room the user wishes to join. This must be a string 1-20
  characters long, containing only alphanumeric characters.
* name: the name the user wishes to be known by. This must be a string
  1-8 characters long, containing only alphanumeric characters.
* pubkey: an RSA modulus, encoded as a string of base 64 digits. This
  will be the user's public key for wrapping all client-to-client
  messages.

### quit

Sent by the client when they wish to leave the server; has no fields
other than "type". Upon receiving this message, the server will close
the client's websocket and mark them as disconnected in their room. If
every client in the room is disconnected, the room is destroyed.

## Server-to-client message types

### welcome

Acknowledges a "join" message, and indicates that the client has been
added to the list of users in the requested room with the requested
name.

Fields:

* room: the requested room
* name: the requested name

### unavailable

Sent in response to a "join" message, indicating that the requested
username is unavailable in the requested room.

Fields:

* room: the requested room
* name: the requested name

### users

Provides a list of users currently signed in to the client's
room. Sent to all clients in a room whenever a new client joins the
room. A "users" message must only be sent to a client after the client
has been sent a "welcome" message confirming their name and room.

Fields:

* users: a JSON object with keys for each username. Values are JSON
  objects, with a key "pubkey" whose value is a base64 encoded RSA
  modulus; and a key "connected", whose value is a boolean indicating
  whether the user is currently connected to the server.

### error

Indicates an error in a message sent by a client.

Fields:

* error: a string explaining the error
* message: the string the client sent that resulted in an error

## Client-to-client message types

All client-to-client messages have type "client".

Fields:

* sender: the sender of the message
* recipient: the desired recipient of the message
* key: An AES-256 key encrypted with the recipient's RSA key (using
  RSAES PKCS#1 v1.5) and base-64 encoded
* iv: An initialization vector for the encryption, base-64 encoded
* payload: The remaining fields of the message, encrypted using the AES
  key and IV and base-64 encoded
* signature: The SHA256 hash of the encrypted payload signed with the
  sender's RSA key (using RSASSA PKCS#1 v1.5) and base-64 encoded

In the following calculations, e = 0x10001 is the public exponent, and
all calculations on encrypted values are performed mod N, where N is the
modulus of the initiator's SENPAI public key.  All numerical values are
base64 encoded.

### initiate

Initiates a SENPAI exchange with another client.  Requires randomly
generating a new SENPAI public key.

Fields:

* n: the sender's SENPAI public key modulus N.
* xe: The "dummy" bit; concatenated with a verification string, padded
  and encrypted with the sender's SENPAI public key.
* ye: The "response" bit; concatenated with a verification string if 1,
  padded and encrypted with the sender's SENPAI public key.
* sh: The verification string, SHA-256 hashed

### respond

Sent in response to an "initiate" message

* wre: Whichever of xe or ye the client chose, multiplied by r^e

### decrypt

Sent in response to a "respond" message

* wr: wre, decrypted

### reveal

Sent in response to a "decrypt" message

* result: a boolean indicating the result of the protocol
* s: If the result was false, the value of s that was extracted from
  x. Otherwise, a random value.

### confirm

Sent in response to a "reveal" message, ending the SENPAI exchange

* x: The padded value of x
