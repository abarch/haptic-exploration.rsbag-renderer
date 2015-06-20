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

    {"name": name
     "description": description}

### /api/transform/list ()
Return a list of available data transforms.

This does not include all metadata on a transformer. Use /transform/ explicitly
on a transformer to fetch that information.

Response:

    [{"name": name,
      "description": description}]

### /api/transform/load (path source name)
* `path`   -- Optional, names a server-local source path to load the transform spec from.
* `source` -- Optional, the direct source code to construct the transform from.
* `name`   -- Optional, names the transform.

Dynamically load a new data transform into the system.

If `name` is not given, it is automatically recognised from the source or path. If no
name can be automatically recognised, a random UUID is chosen instead. One of `path`
or `source` must be given, where `path` must be completable to a pathname that points
to a Lisp source file to `CL:LOAD`. If `source` is given instead, it must be string that
is `CL:READ`able to be subsequently `CL:COMPILE`d.

Response:

    {"name": name,
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

### /api/visualization/ (visualization)
* `visualization` -- Required, names the visualization to fetch data of.

Return all available data on a visualization.

This includes the visualization payload itself. In order to use a visualization, the
payload data must be fed into the DOM.

Response:

    {"name": name,
     "description": description,
     "payload": {"html": html,
                 "css": css,
                 "js": js}}

### /api/visualization/list ()
Return a list of all available visualizations.

This does not include all metadata on a visualization, nor the actual visualization
payload itself. Use /visualization/ instead for that.

Response:

    [{"name": name,
      "description": description}]

### /api/visualization/load (path source name)
* `path`   -- Optional, names a server-local source path to load the visualization from.
* `source` -- Optional, a JSON object that contains the source files.
* `name`   -- Optional, names the visualization.

Dynamically load a new visualization.

One of `path` or `source` must be given. `path` must be completable to a pathname that
points to a directory containing files of the pathname's name and with the pathname
types HTML, CSS, and JS. If `source` is given, it must be a JSON object with the
fields "html", "css", and "js" which contain the respective payloads. In the
case when `source` is given, the payloads are saved to disc in the server's default
visualization directory.

Response:

    {"name": name,
     "description": description}

### /api/visualization/remove (visualization)
* `visualization` -- Required, names the visualization to remove.

Remove a visualization from the system.

This will /not/ delete any files from disc.

Response:

    {}

### /api/visualization/update (visualization source)
* `visualization` -- Required, names the visualization to update.
* `source`        -- Required, a JSON object that contains the visualization parts to
                     update.

Update a visualization with the given `source`.

See /visualization/load for the necessary structure and consequent effects of `source`.

Response:

    {}

### /api/source/ (source)
* `source` -- Required, names the source to fetch data of.

Return metadata information of a source.

Response:

    {"name": name,
     "description": description}

### /api/source/list ()
Return a list of all available sources.

This will not return all available metadata on sources. Use /source/ instead.

Response:

    [{"name": name,
      "description": description}]

### /api/source/add/file (path name)
* `path` -- Required, a server local path that points to a TIDE file.
* `name` -- Optional, names the source.

Add a new file source to the system.

`path` must be completable to a pathname pointing to a file with pathname-type TIDE. 
If `name` is not given, it is automatically determined by the pathname-name.

Response:

    {"name": name,
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

    {"name": name,
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

The events are transformed by the named `transform` and the resulting data is used to fill
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