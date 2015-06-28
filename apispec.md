# API Spec
All API endpoints can be addressed by `GET` or `POST`. All parameters are fetched by
appending all results from `POST` and `GET` together. For single-value parameters,
the first of the resulting list is picked as the value. If the resulting list is
empty, the parameter receives the value `NIL`.

All API endpoints must return responses in the JSON format. All JSON objects must
contain at least the fields that are specified, but may contain additional ones not
covered by this document. In the case of an array, the specification only denotes a
single array element, whereas the API endpoint may return an arbitrary number (zero
or more) of instances of the element within the array. Each response is wrapped in
the following JSON object:

    {"status": status-code,
     "message": message,
     "data": data}

* `status-code` -- Must be the same as the actual HTTP status code sent back.
* `message`     -- A human-readable version of the status or error.
* `data`        -- The actual payload of the individual API endpoint.

The Content-Type header is always `application/json`. The HTTP status code is `200`
in every situation except for errors.  If required parameters are not given, the
status code must be `400` and the `data` must be an array of strings naming the
required parameters. For any other kind of error, the respective API endpoint is
free to pick an appropriate status code in the range of `400` and upwards.

### /api/shutdown ()
Shuts down the server.

Response:

    {}

### /api/transform/ (transform)
* `transform` -- Required, names the transform to fetch data of.

Return metadata information about a given data transform.

Response:

    {"identifier": identifier
     "description": description}

### /api/transform/list ()
Return a list of available data transforms.

This does not include all metadata on a transformer. Use /transform/ explicitly
on a transformer to fetch that information.

Response:

    [{"identifier": identifier,
      "description": description}]

### /api/transform/load (path source identifier)
* `path`         -- Optional, names a server-local source path to load the transform spec from.
* `source`       -- Optional, the direct source code to construct the transform from.
* `identifier`   -- Optional, names the transform.

Dynamically load a new data transform into the system.

If `identifier` is not given, it is automatically recognised from the source or path. If no
identifier can be automatically recognised, a random UUID is chosen instead. One of `path`
or `source` must be given, where `path` must be completable to a pathidentifier that points
to a Lisp source file to `CL:LOAD`. If `source` is given instead, it must be string that
is `CL:READ`able to be subsequently `CL:COMPILE`d.

Response:

    {"identifier": identifier,
     "description": description}

### /api/transform/remove (transform)
* `transform` -- Required, names the transform to remove.

Remove a data transformer from the system.

This will /not/ delete any files from disc.

Response:

    {}

### /api/transform/update (transform source)
* `transform` -- Required, names the transform to update.
* `source`    -- Required, the direct source code to construct the transform from.

Update a data transformer with new source code.

Response:

    {}

### /api/visualizer/ (visualizer)
* `visualizer` -- Required, names the visualizer to fetch data of.

Return all available data on a visualizer.

This includes the visualizer payload itself. In order to use a visualizer, the
payload data must be fed into the DOM.

Response:

    {"identifier": identifier,
     "description": description,
     "payload": {"html": html,
                 "css": css,
                 "js": js}}

### /api/visualizer/list ()
Return a list of all available visualizers.

This does not include all metadata on a visualizer, nor the actual visualizer
payload itself. Use /visualizer/ instead for that.

Response:

    [{"identifier": identifier,
      "description": description}]

### /api/visualizer/load (path source identifier)
* `path`         -- Optional, names a server-local source path to load the visualizer from.
* `source`       -- Optional, a JSON object that contains the source files.
* `identifier`   -- Optional, names the visualizer.

Dynamically load a new visualizer.

One of `path` or `source` must be given. `path` must be completable to a pathidentifier that
points to a directory containing files of the pathidentifier's identifier and with the pathidentifier
types HTML, CSS, and JS. If `source` is given, it must be a JSON object with the
fields "html", "css", and "js" which contain the respective payloads. In the
case when `source` is given, the payloads are saved to disc in the server's default
visualizer directory.

Response:

    {"identifier": identifier,
     "description": description}

### /api/visualizer/remove (visualizer)
* `visualizer` -- Required, names the visualizer to remove.

Remove a visualizer from the system.

This will /not/ delete any files from disc.

Response:

    {}

### /api/visualizer/update (visualizer source)
* `visualizer` -- Required, names the visualizer to update.
* `source`     -- Required, a JSON object that contains the visualizer parts to
                  update.

Update a visualizer with the given `source`.

See /visualizer/load for the necessary structure and consequent effects of `source`.

Response:

    {}

### /api/source/ (source)
* `source` -- Required, names the source to fetch data of.

Return metadata information of a source.

Response:

    {"identifier": identifier,
     "description": description}

### /api/source/list ()
Return a list of all available sources.

This will not return all available metadata on sources. Use /source/ instead.

Response:

    [{"identifier": identifier,
      "description": description}]

### /api/source/add/file (path identifier)
* `path`       -- Required, a server local path that points to a TIDE file.
* `identifier` -- Optional, names the source.

Add a new file source to the system.

`path` must be completable to a pathidentifier pointing to a file with pathidentifier-type TIDE. 
If `identifier` is not given, it is automatically determined by the pathidentifier-identifier.

Response:

    {"identifier": identifier,
     "description": description}

### /api/source/remove (source)
`source` -- Required, names the source to remove.

Remove a source from the system.

This will /not/ delete any files from disc.

Response:

    {}

### /api/source/channel/ (source channel)
* `source`  -- Required, names the source to inspect.
* `channel` -- Required, identifies the channel to fetch data of.

Return metadata information on a `source`'s `channel`.

Response:

    {"identifier": identifier,
     "length": length,
     "start": start-timestamp,
     "end": end-timestamp,
     "resolution": avg-events/sec}

### /api/source/channel/list (source)
`source` -- Required, names the source to inspect.

Return a list of all channels available for a given SOURCE.

This will not return all available channel metadata. Use /source/channel/ instead.

### /api/source/channel/event (source channel transform skip amount from to)
* `source`    -- Required, names the source to fetch data from.
* `channel`   -- One or more, names the channels to fetch events from.
* `transform` -- Optional, names the transform to feed the events through.
                 Defaults to the `direct` transform.
* `skip`      -- Optional, the amount of events to skip. Defaults to `0`.
* `amount`    -- Optional, the upper limit of events to return.
* `from`      -- Optional, events must have a greater timestamp than this.
                 Given in ISO 8601 format or as a UNIX timestamp.
* `to`        -- Optional, events must have a lower timestamp than this.
                 Given in ISO 8601 format or as a UNIX timestamp.

Return event objects from a given `source`'s `channel`/s.

The events are transformed by the identifierd `transform` and the resulting data is used to fill
in the `data` field in the response. If the transform is unknown to the system, an error is
returned instead. `skip` has to be a positive integer that denotes from which event in the
channels on events should be delivered. If `skip` is greater than the length of the
channels, no events are returned. `amount` has to be a positive integer or `NIL`. If it is
not `NIL`, it sets the upper limit on the number of events to return.

If an error occurs during the transformation, the HTTP status code must be `600`.

Response:

    [{"id": id,
      "timestamp": timestamp,
      "data", data}]
