CREATE TABLE input(
  line serial NOT NULL PRIMARY KEY,
  contents text NULL
);
COPY input(contents) FROM :'input' WITH (FORMAT text, HEADER false);

CREATE VIEW grouped AS
  SELECT array_agg(contents) lines FROM (
    SELECT
      input.*,
      max(line) FILTER (where contents ^@ 'Monkey ') OVER (ORDER BY line RANGE UNBOUNDED PRECEDING) AS grouping
    FROM input
  ) grouped
  GROUP BY grouping;

CREATE TYPE operator AS ENUM('+', '*');

CREATE TABLE monkey(
  id int NOT NULL PRIMARY KEY,
  operation_operator operator NOT NULL,
  operation_operand int NULL, -- `NULL` means `old`
  test_divisible_by int NOT NULL,
  if_true_throw_to int NOT NULL,
  if_false_throw_to int NOT NULL
);

INSERT INTO monkey(
  SELECT
    regexp_substr(lines[1], '^Monkey ([0-9]+):$', 1, 1, '', 1) :: int,
    split_part(trim(lines[3]), ' ', 5) :: operator,
    nullif(split_part(trim(lines[3]), ' ', 6), 'old') :: int,
    split_part(trim(lines[4]), ' ', 4) :: int,
    split_part(trim(lines[5]), ' ', 6) :: int,
    split_part(trim(lines[6]), ' ', 6) :: int
  FROM grouped
  ORDER BY regexp_substr(lines[1], '^Monkey ([0-9]+):$', 1, 1, '', 1) :: int
);

CREATE TABLE item(
  round int NOT NULL,
  turn int NOT NULL,
  monkey_id int NOT NULL,
  ordering int NOT NULL,
  worry_level int NOT NULL
);

CREATE INDEX item_index ON item(round, turn, monkey_id, ordering);

CREATE FUNCTION operate(a int, operator operator, b int)
  RETURNS int
  LANGUAGE SQL
  RETURN
    CASE operator
      WHEN '+' THEN a + coalesce(b, a)
      WHEN '*' THEN a * coalesce(b, a)
    END;

CREATE FUNCTION throw_items(round int, turn int)
  RETURNS TABLE(
    source_monkey_id int,
    target_monkey_id int,
    ordering int,
    worry_level int
  )
  AS $$
  SELECT
    id AS source_monkey_id,
    CASE
      WHEN operate(worry_level, operation_operator, operation_operand) / 3 % test_divisible_by = 0
        THEN if_true_throw_to
      ELSE if_false_throw_to
    END AS target_monkey_id,
    row_number() OVER (PARTITION BY monkey_id ORDER BY ordering) AS new_ordering,
    operate(worry_level, operation_operator, operation_operand) / 3 AS worry_level
  FROM monkey
  JOIN item
    ON round = $1 AND turn = $2
      AND monkey.id = item.monkey_id
  ORDER BY ordering
  $$
  LANGUAGE SQL;

CREATE FUNCTION next_turn(round int, turn int, max_id int)
  RETURNS SETOF item
  AS $$
  SELECT *
  FROM
    (
      SELECT
        CASE $2
          WHEN $3
            THEN $1 + 1
          ELSE $1
        END,
        CASE $2
          WHEN $3
            THEN 0
          ELSE $2 + 1
        END
    ) AS next_turn,
    (
      SELECT
        item.monkey_id,
        item.ordering,
        item.worry_level
      FROM item
      WHERE round = $1
        AND turn = $2
        AND monkey_id <> turn
      UNION ALL
      SELECT
        thrown.target_monkey_id,
        thrown.ordering + coalesce((SELECT max(item.ordering) FROM item WHERE round = $1 AND turn = $2 AND monkey_id = thrown.target_monkey_id), 0),
        thrown.worry_level
      FROM throw_items($1, $2) AS thrown
      WHERE thrown.source_monkey_id = $2
    ) AS new_item
  $$
  LANGUAGE SQL;

-- I really tried doing this with recursive CTEs, with rules, with triggers… none of it worked.
CREATE FUNCTION iterate(rounds int)
  RETURNS void
  AS $$
  DECLARE
    max_id int;
  BEGIN
    SELECT MAX(id) INTO max_id FROM monkey;
    FOR round IN 1 .. rounds LOOP
      FOR turn IN 0 .. max_id LOOP
        INSERT INTO item (SELECT * FROM next_turn(round, turn, max_id));
      END LOOP;
    END LOOP;
  END
  $$
  LANGUAGE PLpgSQL;

INSERT INTO item(
  SELECT
    1,
    0,
    regexp_substr(lines[1], '^Monkey ([0-9]+):$', 1, 1, '', 1) :: int,
    row_number() over(),
    monkey_item :: int
  FROM
    grouped,
    LATERAL string_to_table(regexp_substr(lines[2], '^ +Starting items: (.+)', 1, 1, '', 1), ', ') WITH ORDINALITY AS monkey_item
  ORDER BY regexp_substr(lines[1], '^Monkey ([0-9]+):$', 1, 1, '', 1) :: int
);

SELECT iterate(20);

CREATE VIEW activity AS
  SELECT
    monkey_id,
    count(*) AS activity
  FROM item
  WHERE round <= 20
    AND turn = monkey_id
  GROUP BY monkey_id
  ORDER BY activity DESC;

CREATE AGGREGATE product(NUMERIC) (
   sfunc = numeric_mul,
   stype = NUMERIC
);

\echo
\t on
\o

SELECT product(activity) FROM (SELECT * FROM activity FETCH FIRST 2 ROWS ONLY) result;
