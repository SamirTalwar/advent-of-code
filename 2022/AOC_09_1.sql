CREATE TYPE direction AS ENUM ('U', 'R', 'D', 'L');

CREATE FUNCTION direction_to_point(direction direction)
  RETURNS point
  LANGUAGE SQL
  RETURN CASE direction
    WHEN 'U' THEN point(0, 1)
    WHEN 'R' THEN point(1, 0)
    WHEN 'D' THEN point(0, -1)
    WHEN 'L' THEN point(-1, 0)
  END;

CREATE TABLE input(
  line_no serial NOT NULL PRIMARY KEY,
  direction direction NOT NULL,
  amount int NOT NULL
);
COPY input(direction, amount) FROM :'input' WITH (FORMAT csv, HEADER false, DELIMITER ' ');

CREATE TABLE position(
  generation int NOT NULL,
  head point NOT NULL,
  tail point NOT NULL
);
INSERT INTO position VALUES (
  0,
  point(0, 0),
  point(0, 0)
);

CREATE TABLE instruction(
  generation serial NOT NULL PRIMARY KEY,
  direction point NOT NULL
);

CREATE FUNCTION follow(head point, movement point, tail point)
  RETURNS point
  LANGUAGE SQL
  RETURN CASE
    WHEN ((head + movement - tail)[0] BETWEEN -1 AND 1) AND ((head + movement - tail)[1] BETWEEN -1 AND 1)
      THEN tail
    ELSE head
  END;

CREATE FUNCTION process_instruction()
  RETURNS trigger
  AS $$
  BEGIN
    INSERT INTO position (
      SELECT
        new.generation,
        position.head + new.direction,
        follow(position.head, new.direction, position.tail)
      FROM position
      WHERE position.generation + 1 = new.generation
    );
    RETURN NULL;
  END
  $$
  LANGUAGE plpgsql;

CREATE TRIGGER process_instruction
  AFTER INSERT
  ON instruction
  FOR EACH ROW
  EXECUTE FUNCTION process_instruction();

INSERT INTO instruction(direction) (
  SELECT unnest(array_fill(direction_to_point(direction), ARRAY[amount]))
  FROM input
  ORDER BY line_no
);

\echo
\t on
\o

SELECT count(*) FROM (SELECT DISTINCT position.tail[0], position.tail[1] FROM position) result;
