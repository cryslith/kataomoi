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
  will be the user's public key for wrapping all messages

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
