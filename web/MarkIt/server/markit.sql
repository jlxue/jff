CREATE TABLE IF NOT EXISTS marks
(
    key     string NOT NULL CHECK (length(key) > 0),
    url     string NOT NULL CHECK (length(url) > 0),
    title   string DEFAULT "",
    marks   string DEFAULT "",
    tags    string DEFAULT "",
    left    integer DEFAULT 0 CHECK (left >= 0),
    top     integer DEFAULT 0 CHECK (top >= 0),

    PRIMARY KEY (key, url)
);

