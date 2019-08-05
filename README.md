# casa

Running

    $ DBCONN="host='localhost' port='5432' dbname=casa user=casa password=casa" PORT=3000 AUTHORIZED_PORT=3031 casa-server

Syntax is https://www.postgresql.org/docs/9.3/libpq-connect.html#AEN39681

Spin up a postgres server:

    $ docker run --rm --name casa-pg -p 6432:5432 -e POSTGRES_PASSWORD=password -d postgres:9.5 -c "log_statement=all"

Testing

    $ DBCONN="host='localhost' port=6432 user=postgres password=password" stack test

Stop the postgres server:

    $ docker stop casa-pg

Can be a handy way to trivially reset the database.
