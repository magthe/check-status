A simple server that reports status.

The easiest way to run it is with [Docker Compose](https://docs.docker.com/compose/):

```shell
docker-compose up
```

Then it's possible to contact the outer service at `http://localhost:3000/status`.

```shell
curl -X GET http://localhost:3000/status | jq .
```

```json
{
    "branch": "master",
    "build-id": "15",
    "commit": "a2417e10ad10b072e4bfd08dd7bbb6270a6869b1",
    "srv-id": "9da5884d-874f-4048-aa5c-db6db06fc169",
    "system": "check-status"
}
```

To include the inner service use `POST` and put a list of URLs in the body of the request:

```shell
curl -X POST \
    -d '["http://cs0.inner:3000/status"]' \
    http://localhost:3000/status | jq .
```

```json
[
  {
    "Right": {
      "srv-id": "9da5884d-874f-4048-aa5c-db6db06fc169",
      "system": "check-status",
      "branch": "master",
      "build-id": "15",
      "commit": "a2417e10ad10b072e4bfd08dd7bbb6270a6869b1"
    }
  },
  {
    "Right": {
      "srv-id": "db4b3614-a759-424a-ba39-d9c9148a3c45",
      "system": "check-status",
      "branch": "master",
      "build-id": "15",
      "commit": "a2417e10ad10b072e4bfd08dd7bbb6270a6869b1"
    }
  }
]
```
