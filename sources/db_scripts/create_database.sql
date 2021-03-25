CREATE DATABASE guestbookdb;

\c guestbookdb

DROP TABLE IF EXISTS guest_entry;

CREATE TABLE guest_entry (
    id SERIAL NOT NULL,
    guest_name VARCHAR NOT NULL,
    guest_email VARCHAR NULL,
    guest_comment VARCHAR NOT NULL,
    create_dt TIMESTAMP DEFAULT NOW(),
    PRIMARY KEY (id)
);
