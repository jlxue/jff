CREATE TABLE IF NOT EXISTS marks
(
    left    integer DEFAULT 0 CHECK (left >= 0),
    top     integer DEFAULT 0 CHECK (top >= 0),
    key     string NOT NULL,
    url     string NOT NULL,
    title   string DEFAULT "",
    marks   string DEFAULT "",

    PRIMARY KEY (key, url)
);

