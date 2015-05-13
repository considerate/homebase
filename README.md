# Homebase

The HTTP API complement to 3rd Base.

## Installation


### Dependencies

Install these applications using your favorite
package manager. Below is an example using aptitude. 

```bash
apt-get install git
apt-get install erlang
apt-get install rebar
apt-get install couchdb
```

Then `cd` into the cloned directory of this repository and fetch all erlang depency libraries.

```bash
cd homebase
make deps
```

Put apns and gcm certificates at the following locations:

```bash
rel/files/apns-cert.pem
rel/files/apns-key.pem
rel/files/gcm-key.secret
```
The keys should be in plain text format in above files.

Compile

```bash
make rel
```

Make sure CouchDB is up and running at the default address, then

```bash
./make-couchdb-design
```

## Running

To run in foreground:

```bash
./rel/homebase/bin/homebase console
```
To run in background
```
./rel/homebase/bin/homebase start
```
