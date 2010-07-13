CREATE TABLE IF NOT EXISTS users
(
    name        TEXT NOT NULL CHECK (length(name) > 0) PRIMARY KEY,
    password    TEXT NOT NULL CHECK (length(password) > 0),
    key         TEXT NOT NULL CHECK (length(key) > 0) UNIQUE,
    email       TEXT DEFAULT "",

    ctime       DATETIME NOT NULL,
    mtime       DATETIME NOT NULL
);

CREATE TABLE IF NOT EXISTS marks
(
    key         TEXT NOT NULL CHECK (length(key) > 0) REFERENCES users (key),
    url         TEXT NOT NULL CHECK (length(url) > 0),

    left        INTEGER DEFAULT 0 CHECK (left >= 0),
    top         INTEGER DEFAULT 0 CHECK (top >= 0),

    title       TEXT DEFAULT "",
    tags        TEXT DEFAULT "",
    pub_marks   TEXT DEFAULT "",
    pri_marks   TEXT DEFAULT "",
    pub_comments    TEXT DEFAULT "",
    pri_comments    TEXT DEFAULT "",

    ctime       DATETIME NOT NULL,
    mtime       DATETIME NOT NULL,

    PRIMARY KEY (key, url)
);

