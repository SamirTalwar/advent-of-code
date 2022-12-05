CREATE TABLE input(
  line_no SERIAL NOT NULL PRIMARY KEY,
  contents TEXT NULL
);
COPY input(contents) FROM :'input' WITH (FORMAT csv, HEADER false);

CREATE TABLE input_stacks(
  line_no SERIAL NOT NULL PRIMARY KEY,
  contents TEXT NULL
);
INSERT INTO input_stacks (
  SELECT input.*
    FROM input
   WHERE input.line_no < (SELECT line_no - 1 FROM input WHERE contents IS NULL)
);

CREATE TABLE input_instructions(
  line_no SERIAL NOT NULL PRIMARY KEY,
  contents TEXT NULL
);
INSERT INTO input_instructions (
  SELECT input.*
    FROM input
   WHERE input.line_no > (SELECT line_no FROM input WHERE contents IS NULL)
);

CREATE TABLE position AS
  WITH maximum AS (
    SELECT MAX(line_no) AS value FROM input_stacks
  )
  SELECT line.stack / 4 + 1 AS stack,
         maximum.value - line_no AS height,
         line.crate
    FROM input_stacks
    JOIN unnest(
          array(SELECT i FROM generate_series(1, length(input_stacks.contents)) AS i),
          string_to_array(input_stacks.contents, NULL)
        ) AS line(stack, crate)
        ON true
    JOIN maximum ON true
    WHERE stack % 4 = 2
      AND crate <> ' ';

CREATE TABLE instruction(
  i SERIAL NOT NULL PRIMARY KEY,
  source INT NOT NULL,
  destination INT NOT NULL
);

CREATE FUNCTION update_positions()
  RETURNS trigger
  AS $$
  BEGIN
    WITH next_instruction AS (
        DELETE
          FROM instruction
          WHERE i = (SELECT MIN(i) FROM instruction)
      RETURNING *
    )
    UPDATE position
       SET stack = next_instruction.destination,
           height = (SELECT COALESCE(MAX(height) + 1, 0) FROM position WHERE stack = next_instruction.destination)
      FROM next_instruction
     WHERE stack = next_instruction.source
       AND height = (SELECT MAX(height) FROM position WHERE stack = next_instruction.source);
    RETURN NULL;
  END
  $$
  LANGUAGE plpgsql;

CREATE TRIGGER on_new_instruction_update_positions
     AFTER INSERT
        ON instruction
  FOR EACH ROW
   EXECUTE FUNCTION update_positions();

INSERT INTO instruction(source, destination) (
  WITH
    matches(matches) AS (
      SELECT regexp_match(contents, '^move (\d+) from (\d+) to (\d+)')
        FROM input_instructions
    ),
    nested(sources, destinations) AS (
      SELECT array_fill(matches[2]::INT, ARRAY[matches[1]::INT]) AS sources,
             array_fill(matches[3]::INT, ARRAY[matches[1]::INT]) AS destinations
        FROM matches
    )
  SELECT unnested.source, unnested.destination
    FROM nested
    JOIN unnest(nested.sources, nested.destinations) AS unnested(source, destination) ON true
);

CREATE MATERIALIZED VIEW stack_top AS
    SELECT position.stack, position.crate
      FROM position
      JOIN (SELECT stack, MAX(height) AS height FROM position GROUP BY stack) AS stack_height
           ON position.stack = stack_height.stack AND position.height = stack_height.height
  ORDER BY position.stack;

\echo
\t on
\o

SELECT string_agg(crate, NULL)
  FROM stack_top;
