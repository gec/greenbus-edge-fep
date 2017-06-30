#!/usr/bin/env bash

sudo su postgres -c "psql -c 'drop database edge_modules'"
sudo su postgres -c psql < sqlscript/setup_dbs.sql
sudo su postgres -c "psql -d edge_modules" < sqlscript/moduledb.sql