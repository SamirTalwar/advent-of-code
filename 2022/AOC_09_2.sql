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
  knot_1 point NOT NULL,
  knot_2 point NOT NULL,
  knot_3 point NOT NULL,
  knot_4 point NOT NULL,
  knot_5 point NOT NULL,
  knot_6 point NOT NULL,
  knot_7 point NOT NULL,
  knot_8 point NOT NULL,
  tail point NOT NULL
);
INSERT INTO position VALUES (
  0,
  point(0, 0),
  point(0, 0),
  point(0, 0),
  point(0, 0),
  point(0, 0),
  point(0, 0),
  point(0, 0),
  point(0, 0),
  point(0, 0),
  point(0, 0)
);

CREATE TABLE instruction(
  generation serial NOT NULL PRIMARY KEY,
  direction point NOT NULL
);

CREATE FUNCTION follow(head point, tail point)
  RETURNS point
  LANGUAGE SQL
  RETURN CASE ((head - tail)[0] :: int, (head - tail)[1] :: int)
    WHEN (-2, -2)
      THEN tail + point(-1, -1)
    WHEN (-1, -2)
      THEN tail + point(-1, -1)
    WHEN ( 0, -2)
      THEN tail + point( 0, -1)
    WHEN ( 1, -2)
      THEN tail + point( 1, -1)
    WHEN ( 2, -2)
      THEN tail + point( 1, -1)
    WHEN (-2, -1)
      THEN tail + point(-1, -1)
    WHEN ( 2, -1)
      THEN tail + point( 1, -1)
    WHEN (-2,  0)
      THEN tail + point(-1,  0)
    WHEN ( 2,  0)
      THEN tail + point( 1,  0)
    WHEN (-2,  1)
      THEN tail + point(-1,  1)
    WHEN ( 2,  1)
      THEN tail + point( 1,  1)
    WHEN (-2,  2)
      THEN tail + point(-1,  1)
    WHEN (-1,  2)
      THEN tail + point(-1,  1)
    WHEN ( 0,  2)
      THEN tail + point( 0,  1)
    WHEN ( 1,  2)
      THEN tail + point( 1,  1)
    WHEN ( 2,  2)
      THEN tail + point( 1,  1)
    ELSE tail
  END;

CREATE FUNCTION process_instruction()
  RETURNS trigger
  AS $$
  DECLARE
    old_position position%rowtype;
    new_head point;
    new_knot_1 point;
    new_knot_2 point;
    new_knot_3 point;
    new_knot_4 point;
    new_knot_5 point;
    new_knot_6 point;
    new_knot_7 point;
    new_knot_8 point;
    new_tail point;
  BEGIN
    SELECT *
      INTO old_position
      FROM position
      WHERE position.generation + 1 = new.generation;
    new_head := old_position.head + new.direction;
    new_knot_1 := follow(new_head, old_position.knot_1);
    new_knot_2 := follow(new_knot_1, old_position.knot_2);
    new_knot_3 := follow(new_knot_2, old_position.knot_3);
    new_knot_4 := follow(new_knot_3, old_position.knot_4);
    new_knot_5 := follow(new_knot_4, old_position.knot_5);
    new_knot_6 := follow(new_knot_5, old_position.knot_6);
    new_knot_7 := follow(new_knot_6, old_position.knot_7);
    new_knot_8 := follow(new_knot_7, old_position.knot_8);
    new_tail := follow(new_knot_8, old_position.tail);
    INSERT INTO position (
      SELECT
        new.generation,
        new_head,
        new_knot_1,
        new_knot_2,
        new_knot_3,
        new_knot_4,
        new_knot_5,
        new_knot_6,
        new_knot_7,
        new_knot_8,
        new_tail
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

SELECT count(*) FROM (
  SELECT DISTINCT
    position.tail[0],
    position.tail[1]
  FROM position
) result;
