CREATE TABLE shape_score(
  shape CHAR NOT NULL,
  score INT NOT NULL
);
INSERT INTO shape_score VALUES
  ('X', 1),
  ('Y', 2),
  ('Z', 3);

CREATE TABLE outcome_score(
  opponent CHAR NOT NULL,
  player CHAR NOT NULL,
  score INT NOT NULL
);
INSERT INTO outcome_score VALUES
  ('A', 'X', 3),
  ('B', 'X', 0),
  ('C', 'X', 6),
  ('A', 'Y', 6),
  ('B', 'Y', 3),
  ('C', 'Y', 0),
  ('A', 'Z', 0),
  ('B', 'Z', 6),
  ('C', 'Z', 3);

CREATE TABLE guide(
  opponent CHAR NOT NULL,
  player CHAR NOT NULL
);
COPY guide FROM :'input' WITH (FORMAT csv, HEADER false, DELIMITER ' ');

CREATE MATERIALIZED VIEW outcome_round AS
  SELECT guide.opponent, guide.player, shape_score.score AS shape_score, outcome_score.score AS outcome_score
    FROM guide
    JOIN shape_score
          ON guide.player = shape_score.shape
    JOIN outcome_score
          ON guide.opponent = outcome_score.opponent
         AND guide.player = outcome_score.player;

\echo
\t on
\o
SELECT SUM(shape_score) + SUM(outcome_score) FROM outcome_round;
