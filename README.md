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

## Installation

These instructions will set up a kataomoi server on Arch Linux (the
procedure for other Linux distros should be similar), with NGINX
serving the HTML/JS client and acting as a reverse proxy for the
WebSocket server. All communication with the server will be secured
using Let's Encrypt certificates.

First, we install NGINX, Haskell tools, and Let's Encrypt tools:

    $ sudo pacman -S nginx-mainline ghc cabal certbot certbot-nginx

Next, all of the cabal packages we'll need for the server:

    $ cabal update
    $ cabal install json lens text websockets

Now, clone this repo, and tweak the client JS to connect to your
server:

    $ git clone https://github.com/ichung/kataomoi
    $ vi kataomoi/client/client.js

Find the line near the top that says

    var socket = new WebSocket("wss://kataomoi.mit.edu/ws/");

and replace `kataomoi.mit.edu` with your domain name.

Next, write a simple config file for NGINX:

    $ sudo vi /etc/nginx/nginx.conf

All we need is the following:

    user http;
    worker_processes auto;
    
    events {
        worker_connections 1024;
    }
    
    http {
        include mime.types;
    
        server {
            server_name kataomoi.mit.edu;
    
            location /ws/ {
                proxy_pass http://127.0.0.1:9001;
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
            }
    
            location / {
                root /somewhere/kataomoi/client/;
            }
    
        }
    }

As before, replace `kataomoi.mit.edu` with your domain name. Also,
feel free to move the contents of `kataomoi/client` to some other
location such as `/srv/http/`, and adjust the `root` line in your
`nginx.conf` appropriately.

To get Let's Encrypt certificates and automatically configure your
NGINX to use them, just run

    $ certbot --nginx

When asked whether to require HTTPS, select the "Secure" option.

Once that's done, you're ready to start NGINX:

    $ systemctl enable nginx.service
    $ systemctl start nginx.service

Finally, start the kataomoi server:

    $ cd kataomoi/server
    $ cabal run kataomoi-server

Congratulations! Your server should now be up and running!
