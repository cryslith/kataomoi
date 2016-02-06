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

## Structure of messages

All messages are JSON objects encoded as strings. These objects will
always have a "type" field indicating the type of the message, and
will often have "sender" and "recipient" fields; other fields
may be present depending on the type of the message.

## Client-to-server message types

### join

A "join" message is sent by the client when they sign in to the
server. Its fields are:

* name: the name the user wishes to be known by. This must be a string
  of nonzero length, containing only alphanumeric characters, spaces,
  hyphens, and underscores.
* pubkey: an RSA modulus, encoded as a string of base 64 digits. This
  will be the user's public key in all later protocol exchanges.

### quit

Sent by the client when they wish to leave the server; has no fields
other than "type". Upon receiving this message, the server will close
the client's websocket and remove them from the list of users.

## Server-to-client message types

### users

Provides a list of users currently signed in. Sent to all clients
whenever a new client joins.

Fields:

* users: a JSON object with keys for each username, and the users'
  public keys as values.

### error

Indicates an error in a message sent by a client.

Fields:

* error: a string explaining the error
* message: the string the client sent that resulted in an error

## Client-to-client message types

All client-to-client messages have "sender" and "recipient" fields
holding the usernames of the sender and desired recipient; these
messages are sent by the client to the server and passed on
accordingly. The only other field is a "payload" field; this consists
of a JSON object containing the remaining fields of the message,
encrypted and signed using the recipient and sender's RSA keys, and
encoded as a string of base 64 digits.

### initiate

Initiates a SENPAI exchange with another client.

Fields:

* x3: The "dummy" bit; concatenated with a verification string, padded,
  encrypted with the user's RSA key, and encoded as a string of base 64
  digits.
* y3: The "response" bit; concatenated with a verification string,
  padded, encrypted, and base 64 encoded
* s3: The verification string; padded, encrypted, and base 64 encoded

### respond

Sent in response to an "initiate" message

* wr3: Whichever of x3 or y3 the client chose, multiplied by r^3 and base64 encoded

### decrypt

Sent in response to a "respond" message

* wr: wr3 decrypted, base64 encoded

### reveal

Sent in response to a "decrypt" message

* result: a boolean indicating the result of the protocol
* s: Only present if the result is false; in that case, the value of s
  that was extracted from x.

### confirm

Sent in response to a "reveal" message, ending the SENPAI exchange

* x: The padded value of x
