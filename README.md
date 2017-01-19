http2smtp
=========

A simple service that allows mail delivery via HTTP POST (REST-like). The
allowed content types are either `application/x-www-form-urlencoded` or
`multipart/form-data`. While URL-encoded uploads can only handle one file
attachment the multipart variant allows an unlimited number of attachments.

Features
--------

* Small, robust Erlang-based service
* Inspectable at runtime using the Erlang remote shell
* Server-side request rate limiting per context
* Simple HTTP API with support for the content types
  `application/x-www-form-urlencoded` and `multipart/form-data`

Configuration
-------------

The _http2smtp_ project allows a lot of customization. The RPM distribution
expects the configuration in `/etc/http2smtp.config`. The format of the file is
Erlang's [sys.config](http://erlang.org/doc/man/config.html) style. The
following options can/must be applied.

* `{host_match, Match :: string() | binary() | '_'}`

  Configures [cowboy](https://github.com/ninenines/cowboy/)'s host match.
  Default is '_'.

* `{host_port, pos_integer()}`

  The port the web server listens on. The included systemd service assumes that
  an unprivileged user can open this port. Default is _8080_.

* `{rate_limit, Limit :: pos_integer()}`

  Rate limit per context per minute. Do not accept more that X `mails/min` on a
  context. Default is _2 mails/min_.

* `{from, binary()}`

  Default `From` address value, used if not part of the POST. Default is
  _http2smtp@ ++ inet:gethostname()_.

* `{subject, binary()}`

  Default `Subject` value, used if not part of the POST. Default is _http2smtp_.

* `{timezone, string() | auto}`

  Default timezone to use for the SMTP `Date` field. Default is _auto_. For more
  information refer to the [qdate](https://github.com/choptastic/qdate) project.

* `{body_opts, [cowboy_req:body_opt()]}`

  Options passed to [cowboy](https://github.com/ninenines/cowboy/)'s body read
  functions. Defaults to `length` and `read_length` set to 8MiB.

* `{smtp_opts, [proplists:proplist()]}`

  Options passed to [gen_smtp](https://github.com/Vagabond/gen_smtp)'s
  `gen_smtp_client:send/2` function. There is no default for this configuration
  which will result in a crash of the main HTTP handler. This list *must* at
  least contain the `relay` option.

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
   {smtp_opts,
    [
     {relay, "smtp.example.org"}
    ]},
   ...
  ]}
]
```

This would relay requests POSTed to `/custom` to the address `custom@example.org`
while requests POSTed to all other contexts would be relayed to
`default@example.org` (with the appropriate Cc).

Build
-----

The project is built using [rebar3](http://rebar3.org/). You'll need to have
[Erlang](http://erlang.org/) installed on the build machine. If you want to
build the RPM package you'll also need [fpm](https://github.com/jordansissel/fpm).

Just issue `make release`. The resulting package is self-sufficient and has no
special dependencies (the Erlang runtime will be included).

Please note that the RPM assumes that
[systemd](https://www.freedesktop.org/wiki/Software/systemd/) is used on the
target distribution. The configuration for the service is located at
`/etc/http2smtp.config`. It is malformed by default which is intended because
the application will not work without custom configuration. The minimal
configuration consists of an SMTP relay and a default _To_ address.

As for any decent Erlang application it is possible to connect to the runtime
dynamically using the Erlang remote shell. The RPM installs a handy alias into
`/etc/profile.d` which circumvents typing the necessary boilerplate. All you
need to type is `remsh_http2smtp`. To customize the used cookie just place a
Systemd override in `/etc/systemd/system/http2smtp.service.d/` with a content
similar to

```
[Service]
Environment=COOKIE=my_custom_cookie
```

HTTP API
--------

All fields are optional. However, a request without either a custom `body` or an
attachment will not be forwarded via SMTP.

* POST with `application/x-www-form-urlencoded`

  The content type of the attachment (if any) will be guessed automatically
  based on its filename.

  Fields:
  * from: The sender of email address.
  * subject: The email's subject line.
  * body: The email body (will be relayed with content type `text/plain`)
  * filename: The filename of the attachment
  * data: The content of the attachment (must be URL-encoded)

* POST with `multipart/form-data`

  If the content type of an attachment is `application/octet-stream` the server
  will try to guess the content type based on the filename.

  Fields:
  * from: The sender of email address.
  * subject: The email's subject line.
  * body: The email body (will be relayed with content type `text/plain`)
  * any additional file uploads will be converted to attachments

Client Examples
---------------

The following shows some client-side examples using [cURL](https://curl.haxx.se/):

* Send mail with one attachment using content type `application/x-www-form-urlencoded`

```bash
curl -d subject="Test" -d body="Body" -d filename="filename.png" --data-urlencode data@filename.png http://localhost:8080/context
```

* Send mail with two attachments using content type `multipart/form-data`

```bash
curl -F subject="Test" -F body="Body" -F file1=@filename.png -F file2=@filename.txt http://localhost:8080/context
```
