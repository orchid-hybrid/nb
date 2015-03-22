## Compiling the program

`rm nb.db`
`urweb -dbms sqlite -db nb.db nb`

builds nb.exe and schema.sql


## Creating the database from the schema

`sqlite3 nb.db < schema.sql`

set up the database from the schema


## Setting up nginx

Setting up Nginx so we can serve css and js

`/etc/nginx/nginx.conf`:
```
        #location / {
        #    root   /usr/share/nginx/html;
        #    index  index.html index.htm;
        #}

        location / { # Can't just do /DiffExample/ because of app.js
            proxy_pass http://localhost:8080;
        }

        location /scripts/ {
            root  /usr/share/nginx;
        }
```

`cp mock/style.css /usr/share/nginx/scripts/`
`systemctl restart nginx`


## Inspecting and dumping the sql database

One way to inspect the SQL database itself is:

```
$ sqlite3 nb.db
sqlite> .dump
```

this can be useful to detect a corrupt database, salvage data..


## Automatically starting nb with systemd

To make systemd (on arch linux or similar) automatically start the server

edit the paths in nb.service and then copy it here 

```
mkdir -p ~/.config/systemd/user/
cp nb.service ~/.config/systemd/user/

systemctl --user enable nb
systemctl --user start nb
```
