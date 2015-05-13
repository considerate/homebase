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
Start the couchdb service.

```bash
service couchdb start
```

### Build Homebase

Then `cd` into the cloned directory of this repository and fetch all erlang depenency libraries.

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

This will create a release in the `rel/master` directory.

Make sure CouchDB is up and running at the default address, then run

```bash
./make-couchdb-design
```
Note: this command will overwrite the views in CouchDB.

## Run Homebase

To run in foreground:

```bash
./rel/master/bin/homebase console
```
To run in background
```
./rel/master/bin/homebase start
```
