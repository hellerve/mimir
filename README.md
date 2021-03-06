# mimir

Many a foe of the Allfather fell prey to he cunning wisdom of Mímir.
<br/>
The One-eyed One himself sacrificed his body to the will of the All-Knowing,
<br/>
for wisdom is greater than what is carnal.

And so we submit to the Great and Powerful in the pursuit of that
which is true.

## (mimir:connect zepto databases)

This library aims to be a database wrapper that brings its own
little DSL. It currently only works with PostgreSQL, MySQL, and
Sqlite and it is not done yet. Nonetheless, I release it to the
world right now as a RFC.

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

**NOTE: For now the transactions are autocomitted. This might change
in the future, so commiting it anyway is probably a good idea.**

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

### The DSL

The DSL is a thin functional layer over SQL that makes working with databases
a bit less string-heavy and compositional.

The DSL lives in the `mimsql` namespace. It is self-contained and does not depend
on the rest of mimir in any way.

First, we might want to define the entites we use.

```clojure
(load "mimir/mimsql")
(import-all "mimsql" "s")

; Entity definition follows the definition:
; (s:entity <tablename:atom> <felds and reationships:mimsql types>)
; Fields are required, relationships are optional; order does not matter.
(define user
  (s:entity :user
    (s:fields :id :group_id :username :firstname :lastname :password)))

(define group
  (s:entity :group
    (s:fields :id :group_name)
    (s:has-many user)))
```

Once we have defined the entites and their relationships (`has-many`, `has-one`,
and `belongs-to` are currently supported), we can query them. All of the functions
emit mimsql types that implement the stringify protocol. Stringification turns
them into SQL queries.

```clojure
(s:select user)
; => SELECT id, group_id username, firstname, lastname, password FROM users
(s:select user :id)
; => SELECT id FROM users
(s:select group (s:with user))
; => SELECT id, group_name FROM group JOIN user ON user.id=group_id
```

As the last line showcases, the default join value is via the ID; you can
specify a join column (or a pair of columns), though.

```clojure
(s:select group :group_name (s:with user [:name :group_name]))
; => SELECT group_name FROM group JOIN user ON user.name=group_name
```

There are a few more advanced queries and filters, such as `ORDER BY` (`s:order`),
`HAVING` (`s:having`), `LIMIT` (`s:limit`), `OFFSET` (`s:offset`), `WHERE`(`s:where`),
subqueries (`s:sub`) and raw joins (on tables that are not entites).

Let's see that in action then.

```clojure
(s:select user
  (s:where :id [> 10])
  (s:limit 10)
  (s:order :id :desc)
  (s:having :count :followers [> 100])
  (s:join :group [:id :group_id]))
; => SELECT id, group_id, username, firstname, lastname, password FROM users
;      WHERE id > 10
;      LIMIT 10
;      ORDER BY id DESC
;      HAVING count(followers) > 100
;      JOIN group ON group.id=group_id
```

Now for the big caveat emptor: this is really fresh, naive, and probably broken.
I have yet to test this in a real world setting.

I hope you like it anyway.

<hr/>
Have fun!
