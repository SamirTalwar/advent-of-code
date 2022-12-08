CREATE TABLE input(
  id SERIAL NOT NULL PRIMARY KEY,
  command TEXT NOT NULL
);
COPY input(command) FROM :'input' WITH (FORMAT csv, HEADER false);

CREATE FUNCTION path_append(current_path TEXT[], element TEXT)
  RETURNS TEXT[]
  LANGUAGE SQL
  RETURN
    CASE element
      WHEN '/' THEN '{}'
      WHEN '..' THEN current_path[: array_length(current_path, 1) - 1]
      ELSE array_append(current_path, element)
    END;

CREATE AGGREGATE normalized_path (TEXT) (
  sfunc = path_append,
  stype = TEXT[]
);

CREATE MATERIALIZED VIEW input_with_path AS
  SELECT
    *,
    normalized_path(substring(command FROM 6))
      FILTER (WHERE command LIKE '$ cd %')
      OVER (ROWS UNBOUNDED PRECEDING)
      AS file_path
  FROM input;

CREATE MATERIALIZED VIEW size AS
  SELECT
    *,
    split_part(command, ' ', 1) :: INT AS file_size
  FROM input_with_path
  WHERE regexp_like(command, '^\d+ ')
  UNION
  SELECT
    *,
    0
  FROM input_with_path
  WHERE regexp_like(command, '^dir ');

CREATE MATERIALIZED VIEW directory_size AS
  SELECT
    file_path AS directory_path,
    SUM(file_size) AS directory_size
  FROM size
  GROUP BY file_path
  ORDER BY file_path;

CREATE MATERIALIZED VIEW total_size AS
  SELECT
    total.directory_path AS directory_path,
    SUM(current.directory_size) AS total_size
  FROM directory_size AS total
  JOIN directory_size AS current
    ON current.directory_path[: coalesce(array_length(total.directory_path, 1), 0)] = total.directory_path
  GROUP BY total.directory_path
  ORDER BY total.directory_path;

\echo
\t on
\o

SELECT MIN(total_size)
FROM total_size
WHERE (SELECT total_size FROM total_size WHERE directory_path = '{}') - total_size <= 70000000 - 30000000;
