CREATE TABLE input(
  row_no SERIAL NOT NULL PRIMARY KEY,
  contents TEXT NOT NULL
);
COPY input(contents) FROM :'input' WITH (FORMAT csv, HEADER false);

CREATE TABLE tree_height(
  y INT NOT NULL,
  x INT NOT NULL,
  height INT NOT NULL,
  PRIMARY KEY (y, x)
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

CREATE MATERIALIZED VIEW view_north AS
  SELECT
    this.y,
    this.x,
    coalesce(
      array_agg(this.height > other.height ORDER BY other.y DESC)
        FILTER (WHERE other.height IS NOT NULL),
      '{}'
    ) AS taller_than_neighbor
  FROM tree_height AS this
  LEFT JOIN tree_height AS other
    ON this.y > other.y AND this.x = other.x
  GROUP BY this.y, this.x
  ORDER BY this.y, this.x;

CREATE MATERIALIZED VIEW view_south AS
  SELECT
    this.y,
    this.x,
    coalesce(
      array_agg(this.height > other.height ORDER BY other.y ASC)
        FILTER (WHERE other.height IS NOT NULL),
      '{}'
    ) AS taller_than_neighbor
  FROM tree_height AS this
  LEFT JOIN tree_height AS other
    ON this.y < other.y AND this.x = other.x
  GROUP BY this.y, this.x
  ORDER BY this.y, this.x;

CREATE MATERIALIZED VIEW view_west AS
  SELECT
    this.y,
    this.x,
    coalesce(
      array_agg(this.height > other.height ORDER BY other.x DESC)
        FILTER (WHERE other.height IS NOT NULL),
      '{}'
    ) AS taller_than_neighbor
  FROM tree_height AS this
  LEFT JOIN tree_height AS other
    ON this.y = other.y AND this.x > other.x
  GROUP BY this.y, this.x
  ORDER BY this.y, this.x;

CREATE MATERIALIZED VIEW view_east AS
  SELECT
    this.y,
    this.x,
    coalesce(
      array_agg(this.height > other.height ORDER BY other.x ASC)
        FILTER (WHERE other.height IS NOT NULL),
      '{}'
    ) AS taller_than_neighbor
  FROM tree_height AS this
  LEFT JOIN tree_height AS other
    ON this.y = other.y AND this.x < other.x
  GROUP BY this.y, this.x
  ORDER BY this.y, this.x;

CREATE MATERIALIZED VIEW view AS
  SELECT * FROM view_north
  UNION ALL
  SELECT * FROM view_south
  UNION ALL
  SELECT * FROM view_west
  UNION ALL
  SELECT * FROM view_east;

CREATE AGGREGATE product(NUMERIC) (
   sfunc = numeric_mul,
   stype = NUMERIC
);

CREATE MATERIALIZED VIEW scenic_score AS
  SELECT
    y,
    x,
    product(
      coalesce(
        array_position(taller_than_neighbor, false),
        array_length(taller_than_neighbor, 1),
        0
      )
    ) AS score
  FROM view
  GROUP BY y, x
  ORDER BY y, x;

\echo
\t on
\o

SELECT MAX(score) FROM scenic_score;
