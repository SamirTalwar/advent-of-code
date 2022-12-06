CREATE TABLE input(
  datastream TEXT NOT NULL
);
COPY input(datastream) FROM :'input' WITH (FORMAT csv, HEADER false);

CREATE TABLE data_values(
  i SERIAL NOT NULL PRIMARY KEY,
  c CHAR NOT NULL
);
INSERT INTO data_values(c) (SELECT string_to_table(datastream, NULL) FROM input);

CREATE MATERIALIZED VIEW marker_end AS
  SELECT d.i
    FROM data_values AS a
    JOIN data_values AS b on a.i + 1 = b.i
    JOIN data_values AS c on a.i + 2 = c.i
    JOIN data_values AS d on a.i + 3 = d.i
  WHERE a.c NOT IN (b.c, c.c, d.c)
    AND b.c NOT IN (a.c, c.c, d.c)
    AND c.c NOT IN (a.c, b.c, d.c);

\echo
\t on
\o

SELECT MIN(i) FROM marker_end;
