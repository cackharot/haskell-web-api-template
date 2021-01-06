# Web Api Template

A REST web api server template in Haskell.

This serves as a base to create API projects that comes with the following features to avoid repeated bolierplate in each one.

- Light weight & super fast Warp http server
- Supports HTTP 1.1 & HTTP/2 TLS
- JSON as input/output serialization using Aeson library
- JSON logging
- Define the REST interface using Servant library
- JWT (Bearer) token based authentication
- Application config via ENVIRONMENT variables (via dotenv & envy)
- Health & Info endpoints
- Prometheus metrics endpoint

## TODO

[x] Integrate with RIO
[x] Integrate with Servant
[x] Integrate with FastLogger
[x] Integrate with doenv & envy
[x] Integrate with Prometheus
[x] Integrate with wai-util
[] Setup JWT Authentication
[] Setup Stack template
[] Setup HTTPS

## Getting started

_WIP_

## Build & Run

```bash
make build
PORT=3000 make run

open http://localhost:3000/health
```

_Endpoints_

Info: http://localhost:3000/info

Health: http://localhost:3000/health

Metrics: http://localhost:3000/metrics

_Sample REST Resource:_

GET http://localhost:3000/samples        # list all samples

GET http://localhost:3000/samples/123    # fetch the given sample by id

POST http://localhost:3000/samples/-1    # create new sample resource 

PUT http://localhost:3000/samples/123    # Modify sample by id

DELETE http://localhost:3000/samples/123 # Delete sample by id

## Run tests

```bash
make test
```
