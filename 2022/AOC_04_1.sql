CREATE TABLE input(
  pair SERIAL NOT NULL PRIMARY KEY,
  elf_0_range TEXT NOT NULL,
  elf_1_range TEXT NOT NULL
);

COPY input(elf_0_range, elf_1_range) FROM :'input' WITH (FORMAT csv, HEADER false);

CREATE TABLE assignment(
  pair INT NOT NULL,
  elf INT NOT NULL,
  range_start INT NOT NULL,
  range_end INT NOT NULL
);
INSERT INTO assignment (
  SELECT pair,
         0 AS elf,
         split_part(elf_0_range, '-', 1)::INT AS range_start,
         split_part(elf_0_range, '-', 2)::INT AS range_end
    FROM input
);
INSERT INTO assignment (
  SELECT pair,
         1 AS elf,
         split_part(elf_1_range, '-', 1)::INT AS range_start,
         split_part(elf_1_range, '-', 2)::INT AS range_end
    FROM input
);

CREATE MATERIALIZED VIEW fully_overlapped AS
  SELECT DISTINCT a.pair AS pair
    FROM assignment AS a
    JOIN assignment AS b
          ON a.pair = b.pair
          AND a.elf <> b.elf
    WHERE a.range_start <= b.range_start
      AND a.range_end >= b.range_end;

\echo
\t on
\o

SELECT COUNT(*) FROM fully_overlapped;
