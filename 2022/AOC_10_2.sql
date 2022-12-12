CREATE TABLE input(
  line_no serial NOT NULL PRIMARY KEY,
  contents text NOT NULL
);
COPY input(contents) FROM :'input' WITH (FORMAT csv, HEADER false);

CREATE TYPE code AS ENUM ('noop', 'addx');

CREATE TABLE instruction(
  id int NOT NULL PRIMARY KEY,
  code code NOT NULL,
  value int NULL
);
INSERT INTO instruction (
  SELECT
     line_no AS id,
     split_part(contents, ' ', 1) :: code,
     nullif(split_part(contents, ' ', 2), '') :: int
  FROM input
);

CREATE VIEW run AS
  WITH RECURSIVE cycle(id, program_counter, x) AS (
    SELECT 0, 0, 1
    UNION ALL
    SELECT
      cycle.id + CASE instruction.code
        WHEN 'noop' THEN 1
        WHEN 'addx' THEN 2
      END,
      cycle.program_counter + 1,
      CASE instruction.code
        WHEN 'noop' THEN cycle.x
        WHEN 'addx' THEN cycle.x + instruction.value
      END
    FROM cycle
    JOIN instruction ON cycle.program_counter + 1 = instruction.id
  ) SELECT * FROM cycle;

CREATE VIEW interpolated_run AS
  SELECT seq.id, current_run.program_counter, current_run.x
  FROM
    generate_series((SELECT MIN(id) FROM run), (SELECT MAX(id) FROM run)) seq(id),
    LATERAL (SELECT * FROM run WHERE run.id <= seq.id ORDER BY run.id DESC FETCH FIRST 1 ROW ONLY) current_run;

CREATE VIEW crt AS
  SELECT
    i,
    x IN (i % 40 - 1, i % 40, i % 40 + 1) lit
  FROM generate_series((SELECT MIN(id) FROM interpolated_run), (SELECT MAX(id) - 1 FROM interpolated_run)) i
  JOIN interpolated_run ON i = id;

\echo
\t on
\o

SELECT string_agg(CASE lit WHEN true THEN '#' ELSE '.' END, null)
FROM crt
GROUP BY i / 40;
