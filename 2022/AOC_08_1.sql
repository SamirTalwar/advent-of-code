CREATE TABLE input(
  row_no SERIAL NOT NULL PRIMARY KEY,
  contents TEXT NOT NULL
);
COPY input(contents) FROM :'input' WITH (FORMAT csv, HEADER false);

CREATE TABLE tree_height(
  y INT NOT NULL,
  x INT NOT NULL,
  height INT NOT NULL
);
INSERT INTO tree_height(y, x, height) (
  SELECT
    row_no AS y,
    tree_row.x AS x,
    tree_row.digit :: INT AS height
  FROM input
  JOIN unnest(
      array(SELECT i FROM generate_series(1, length(contents)) AS i),
      string_to_array(contents, NULL)
    ) AS tree_row(x, digit)
    ON true
);

CREATE MATERIALIZED VIEW visible_from_north AS
  SELECT
    tree_height.y,
    tree_height.x,
    count(*) FILTER (WHERE other.height IS NOT NULL) AS expected,
    count(*) FILTER (WHERE tree_height.height > other.height) AS actual
  FROM tree_height
  LEFT JOIN tree_height AS other
    ON tree_height.y > other.y AND tree_height.x = other.x
  GROUP BY tree_height.y, tree_height.x
  ORDER BY tree_height.y, tree_height.x;

CREATE MATERIALIZED VIEW visible_from_south AS
  SELECT
    tree_height.y,
    tree_height.x,
    count(*) FILTER (WHERE other.height IS NOT NULL) AS expected,
    count(*) FILTER (WHERE tree_height.height > other.height) AS actual
  FROM tree_height
  LEFT JOIN tree_height AS other
    ON tree_height.y < other.y AND tree_height.x = other.x
  GROUP BY tree_height.y, tree_height.x
  ORDER BY tree_height.y, tree_height.x;

CREATE MATERIALIZED VIEW visible_from_west AS
  SELECT
    tree_height.y,
    tree_height.x,
    count(*) FILTER (WHERE other.height IS NOT NULL) AS expected,
    count(*) FILTER (WHERE tree_height.height > other.height) AS actual
  FROM tree_height
  LEFT JOIN tree_height AS other
    ON tree_height.y = other.y AND tree_height.x > other.x
  GROUP BY tree_height.y, tree_height.x
  ORDER BY tree_height.y, tree_height.x;

CREATE MATERIALIZED VIEW visible_from_east AS
  SELECT
    tree_height.y,
    tree_height.x,
    count(*) FILTER (WHERE other.height IS NOT NULL) AS expected,
    count(*) FILTER (WHERE tree_height.height > other.height) AS actual
  FROM tree_height
  LEFT JOIN tree_height AS other
    ON tree_height.y = other.y AND tree_height.x < other.x
  GROUP BY tree_height.y, tree_height.x
  ORDER BY tree_height.y, tree_height.x;

CREATE MATERIALIZED VIEW visible AS
  SELECT DISTINCT y, x
  FROM (
      SELECT * FROM visible_from_north
      UNION ALL
      SELECT * FROM visible_from_south
      UNION ALL
      SELECT * FROM visible_from_west
      UNION ALL
      SELECT * FROM visible_from_east
    ) AS visible
  WHERE expected = actual;

\echo
\t on
\o

SELECT count(*) FROM visible;
