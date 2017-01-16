http2smtp
=========

A simple service that allows mail delivery of HTTP POSTed content. The allowed
content types are either `application/x-www-form-urlencoded` or
`multipart/form-data`. While URL-encoded uploads can only handle one file
attachment the multipart variant allows an unlimited number of attachments.

Configuration
-------------

The _http2smtp_ project allows a lot of customization. We'll only list the most
important things here. For further customisation you'll need to read the source.

* `{host_match, Match :: string() | binary() | '_'}`

  Configures `cowboy`'s host match. Default is _'_'_.

* `{host_port, pos_integer()}`

  The port the web server listens on. The included systemd service assumes that
  an unprivileged user can open this port. Default is _8080_.

* `{rate_limit, Limit :: pos_integer()}`

  Rate limit per context per minute. Do not accept more that X `mails/min` on a
  context. Default is _2 mails/min_.

* `{from, binary()}`

  Default `From` address value, used if not part of the POST. Default is
  _http2smtp@hostname_.

* `{subject, binary()}`

  Default `Subject` value, used if not part of the POST. Default is _http2smtp_.

* `{timezone, string() | auto}`

  Default timezone to use for the SMTP `Date` field. Default is _auto_.

* `{body_opts, Opts :: proplists:proplist()}`

  Options passed to `cowboy`'s body read functions. Defaults to `length` and
  `read_length` set to 8MiB.

Additionally, you'll need the mandatory SMTP options (e.g. `relay`) that will be
passed to the `gen_smtp`'s client send function. These can be directly (plain)
embedded into the application environment of _http2smtp_.

The values for the `To` and `Cc` SMTP headers may be set per context or globally.
E.g. if you have special mail destinations for the context `/custom` you could
configure the application like to following:

```erlang
[
 {http2smtp,
  [
   {<<"custom">>,
    [
     {to, <<"custom@example.org">>},
     {cc, []}
    ]},
   {to, <<"default@example.org">>},
   {cc, [<<"default-cc@example.org">>]},
   {relay, "smtp.example.org"},
   ...
  ]}
]
```

Deployment
----------

This project is easiest to deploy when packaged as an RPM package. To package
the project simply call `make rpm` (you'll need
[fpm](https://github.com/jordansissel/fpm) for this to work). Please notice that
_http2smtp_ assumes [systemd](https://www.freedesktop.org/wiki/Software/systemd/)
to run the target distribution.

The configuration for the service is located at `/etc/http2smtp.config` which
default to a miformated file on purpose. The service should not be able to start
until you configured it properly.

Examples
--------

The following shows some client-side examples using [cURL](https://curl.haxx.se/):

* Send mail with one attachment using content type `application/x-www-form-urlencoded`

```bash
curl -d from="http2smtp@example.org" -d subject="Test" -d body="Body" -d filename="filename.png" --data-urlencode data@filename.png http://localhost:8080/context
```

* Send mail with two attachments using content type `multipart/form-data`

```bash
curl -F from="http2smtp@example.org" -F subject="Test" -F body="Body" -F file1=@filename.png -F file2=@filename.txt http://localhost:8080/context
```
