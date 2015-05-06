# Homebase

The HTTP API complement to 3rd Base.

## Installation

Put apns and gcm certificates at the following locations: 
homebase/rel/files/apns-cert.pem
homebase/rel/files/apns-key.pem
homebase/rel/files/gcm-key.secret

The keys should be in plain text format in above files.

Compile

```bash
make deps
make rel
```

Make sure CouchDB is up and running at the default address, then

```bash
./make-couchdb-design
```

## Running

To run in foreground:
```
./rel/homebase/bin/homebase console
```
To run in background
```
./rel/homebase/bin/homebase start
```
