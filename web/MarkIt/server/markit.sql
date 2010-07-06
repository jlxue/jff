CREATE TABLE IF NOT EXISTS marks
(
    key     string NOT NULL,
    url     string NOT NULL,
    title   string DEFAULT "",
    marks   string DEFAULT "",
    targs   string DEFAULT "",
    left    integer DEFAULT 0 CHECK (left >= 0),
    top     integer DEFAULT 0 CHECK (top >= 0),

    PRIMARY KEY (key, url)
);

