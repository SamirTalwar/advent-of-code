CREATE TABLE input(
  datastream TEXT NOT NULL
);
COPY input(datastream) FROM :'input' WITH (FORMAT csv, HEADER false);

CREATE TABLE data_values(
  i SERIAL NOT NULL PRIMARY KEY,
  c CHAR NOT NULL
);
INSERT INTO data_values(c) (SELECT string_to_table(datastream, NULL) FROM input);

CREATE MATERIALIZED VIEW potential AS
    SELECT initial.i, array_agg(values.c) segment
      FROM data_values initial
      JOIN data_values values
           ON values.i BETWEEN initial.i AND initial.i + 3
  GROUP BY initial.i;

CREATE MATERIALIZED VIEW marker AS
  SELECT i AS marker_start,
         i + array_length(segment, 1) - 1 AS marker_end
    FROM potential
   WHERE array_length(array(SELECT DISTINCT x FROM unnest(segment) x), 1) = array_length(segment, 1);

\echo
\t on
\o

SELECT MIN(marker_end) FROM marker;
