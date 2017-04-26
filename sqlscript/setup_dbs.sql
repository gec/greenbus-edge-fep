CREATE USER core WITH PASSWORD 'core';
-- CREATE USER gb_test WITH PASSWORD 'gb_test';
CREATE DATABASE edge_modules;
CREATE DATABASE edge_modules_test;
GRANT ALL PRIVILEGES ON DATABASE edge_modules TO core;
GRANT ALL PRIVILEGES ON DATABASE edge_modules_test TO core;