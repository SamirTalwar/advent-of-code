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
         0 AS half,
         string_to_table(substring(item_codes FOR length(item_codes) / 2), NULL) AS item_code
    FROM input
  UNION ALL
  SELECT rucksack_no,
         1 AS half,
         string_to_table(substring(item_codes FROM length(item_codes) / 2 + 1 FOR length(item_codes) / 2), NULL) AS item_code
    FROM input;

CREATE TABLE misplaced_item AS
  SELECT DISTINCT
         a.rucksack_no,
         a.item_code
    FROM item AS a
    JOIN item AS b
          ON a.rucksack_no = b.rucksack_no
         AND a.half = 0
         AND b.half = 1
         AND a.item_code = b.item_code;

\echo
\t on
\o

SELECT SUM(priority_value)
  FROM misplaced_item
  JOIN priority
       ON misplaced_item.item_code = priority.item_code;
