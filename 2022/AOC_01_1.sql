CREATE TABLE input(
  line SERIAL NOT NULL PRIMARY KEY,
  calories INT NULL
);
INSERT INTO input VALUES (0, NULL); -- so we always get a match for constructing `input_elf`
COPY input(calories) FROM :'input' WITH (FORMAT csv, HEADER false);

CREATE MATERIALIZED VIEW input_elf AS
     SELECT input.line, COUNT(grouping.*) AS elf
       FROM input
  LEFT JOIN input AS grouping ON grouping.line < input.line
      WHERE input.calories IS NOT NULL
        AND grouping.calories IS NULL
   GROUP BY input.line
   ORDER BY input.line;

CREATE MATERIALIZED VIEW calories AS
    SELECT elf, SUM(calories) AS total
      FROM input
      JOIN input_elf ON input.line = input_elf.line
  GROUP BY elf;

\echo
\t on
\o
SELECT MAX(total) FROM calories;
