# mimir

A zepto native extension.

Mímir is one of the nordic Gods of knowledge. This library aims
to be a database wrapper. It currently only works with PostgreSQL
and it is not even done. Nonetheless, I release it to the world
right now as a RFC.

## Installation

```
zeps install hellerve/mimir
```

## Usage

mimir exposes functions that allow for connecting to and disconnecting
from databases, and executing statements.

```clojure
(load "mimir")
(import-all "mimir")

; connect takes a hashmap that contains the keyvValue pairs as described in
; https://www.postgresql.org/docs/8.1/static/libpq.html#LIBPQ-CONNECT
(define conn (mimir:connect #{"host" "127.0.0.1" "port" "5432" "dbname" "mydb"}))

; alternatively, you can specify a config file
(define conn (mimir:connect-from-config "tests/conf.zp"))
(mimir:connection? conn) ; => true

; you can query the connection for a bit of information
; it is always of this form
(mimir:connection-info conn) ; => #{:driver "somedriver" :server-version "0.1.0"
                             ;      :client-version "0.1.0" :transaction-support #t}
```

After a connection has been obtained, statements can be executed.

```clojure
(mimir:get-tables conn) ; will return a list of tables currently visible
(mimir:execute conn "select * from my-table") ; will return a list of rows

; we can also interpolate values into the statement.
; Question marks will be replaced by the values found in the list
(mimir:execute conn "insert into my-table (id) values (?)" [12])
```

After we're done (especially updating the DB), we should commit using
`mimir:commit` to ensure the results are written to the database.

```clojure
(mimir:commit conn) ; => nil
```

If something goes wrong and the pending transactions cannot complete
(only supported on backends that support transactions), we can rollback
the current commit.

```clojure
(mimir:rollback conn) ; => nil
```

If we do not need the connection anymore, it is recommended to call
`mimir:disconnect` for it to garbage-collect. The connection should
not be used anymore after that.

```clojure
(mimir:disconnect conn)
```

To simplify things, mimir provides an abstraction called `mimir:with-connection`.
It will take either a connection object or a file path and a function in which
to use the connection. It will be created for the function to use and the results
of the function will be automatically commited, so that the user does not have to
bother with that any more.

```clojure
(mimir:with-connection "tests/conf.zp"
  (lambda (conn)
    (write (mimir:execute conn "select * from my-table"))))
```

<hr/>
Have fun!
