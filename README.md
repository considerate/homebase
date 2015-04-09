# Homebase

The HTTP API complement to 3rd Base.

## Installation

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
