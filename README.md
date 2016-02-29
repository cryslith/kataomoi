# kataomoi

A dating service for the paranoid

## About

kataomoi is a system that allows users to discover mutual crushes;
that is, if some user Alice inputs that she has a crush on Bob, and
Bob inputs that he has a crush on Alice, they will each be notified
of the other's interest.

The primary goal of kataomoi is to provide users with as much privacy
as possible and require as little trust as possible. kataomoi
implements secure two-party computation to ensure that users' data is
never on the server in unencrypted form, and that Alice can learn
whether Bob has a crush on her, if and only if she has a crush on
him. The SENPAI protocol implemented by kataomoi provides
cryptographic guarantees of privacy, and prevents cheating to the
maximum extent possible; it is designed so that a malicious server can
never learn users' responses, as long as the client code is correct.

Furthermore, kataomoi is constructed to allow users to verify that the
correct protocol is being carried out, and that their data is
secure. Because all communication with the central server takes place
over WebSockets, the client portion of the kataomoi web app consists
only of static HTML and JavaScript; thus, paranoid users can simply
download the page source and load the web page locally, removing the
need to trust any external server.

For more technical details, see the protocol.md file.

## Components

kataomoi currently consists of a small server program written in
Haskell, and a client written in HTML and JS. There are eventual plans
to write a desktop client in Haskell, but currently the web app is the
only client.
