Cardano SL Report Server
========================

[![Build Status](https://travis-ci.org/input-output-hk/cardano-report-server.svg?branch=master)](https://travis-ci.org/input-output-hk/cardano-report-server)

Server for collecting CSL and related logs.

```
Usage: cardano-report-server [-p|--port INTEGER] [--logsdir FILEPATH]
                             [--size-limit BYTES] [--store-custom]
                             [--zd-send-logs] [--zd-email STRING]
                             [--zd-token STRING] [--zd-account NAME] [--version]
  CardanoSL reporting server daemon

Available options:
  -p,--port INTEGER        Port server is running on
  --logsdir FILEPATH       Directory server will be saving logs in
  --size-limit BYTES       Maximum body size allowed (will send 413 responses if
                           bigger)
  --store-custom           Store custom reports
  --zd-send-logs           Send logs from custom reports to Zendesk
  --zd-email STRING        Email to access zendesk
  --zd-token STRING        Zendesk api token
  --zd-account NAME        Zendesk account name (first part of account URL)
  -h,--help                Show this help text
  --version                Show version
```

Zendesk Custom Reports
----------------------

These are bug reports sent by users directly through the Daedalus
wallet frontend. This feature can be enabled using the `--zd-`
options:

 * `--zd-account` -- The first part of `https://ACCOUNT.zendesk.com`.
 * `--zd-email` -- E-mail of a user registered in the Zendesk
   account. This user must have role "Agent" or higher.
 * `--zd-token` -- API access token which can be obtained at
   `https://ACCOUNT.zendesk.com/agent/admin/api/settings`.
 * `--zd-send-logs` -- Optional switch which enables uploading of logs
   from Daedalus.

To register a user for this purpose, the [Zendesk API][1] can be used:

```
curl https://ACCOUNT.zendesk.com/api/v2/users/create_or_update.json \
  -d '{"user": {"name": "Report Server Agent", "email": "report-server@iohk.io", "role": "agent" }}' \
  -H "Content-Type: application/json" -X POST \
  -v -u 'EMAIL/token:BASE64_TOKEN'
```

[1]: https://developer.zendesk.com/rest_api/docs/core/users#create-user
