{EQL
  :INTERFACES {EQL
    :LOGGER "-i-verbose",
    :SERVER "-i-hunchentoot"}
  :SERVER {EQL
    :DOMAINS ("-localhost"),
    :INSTANCES ({EQL
                  :PORT 8080})}}
