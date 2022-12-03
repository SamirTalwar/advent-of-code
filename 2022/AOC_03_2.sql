CREATE TABLE priority(
  priority_value SERIAL PRIMARY KEY NOT NULL,
  item_code CHAR NOT NULL
);
INSERT INTO priority (
  SELECT *
    FROM unnest(
           array(SELECT i FROM generate_series(1, 52) AS i),
           string_to_array('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ', NULL)
         )
);

CREATE TABLE input(
  rucksack_no SERIAL PRIMARY KEY NOT NULL,
  item_codes TEXT NOT NULL
);
COPY input(item_codes) FROM :'input' WITH (FORMAT csv, HEADER false);

CREATE TABLE item AS
  SELECT rucksack_no,
         string_to_table(item_codes, NULL) AS item_code
    FROM input;

CREATE TABLE badge AS
  SELECT DISTINCT
         a.rucksack_no,
         a.item_code
    FROM item AS a
    JOIN item AS b
          ON b.rucksack_no = a.rucksack_no + 1
         AND a.item_code = b.item_code
    JOIN item AS c
          ON c.rucksack_no = b.rucksack_no + 1
         AND a.item_code = c.item_code
   WHERE a.rucksack_no % 3 = 1;

\echo
\t on
\o

SELECT SUM(priority_value)
  FROM badge
  JOIN priority
       ON badge.item_code = priority.item_code;
