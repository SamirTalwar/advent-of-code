CREATE TABLE shape_score(
  shape CHAR NOT NULL,
  score INT NOT NULL
);
INSERT INTO shape_score VALUES
  ('A', 1),
  ('B', 2),
  ('C', 3);

CREATE TABLE outcome_score(
  instruction CHAR NOT NULL,
  score INT NOT NULL
);
INSERT INTO outcome_score VALUES
  ('X', 0),
  ('Y', 3),
  ('Z', 6);

CREATE TABLE choice(
  opponent CHAR NOT NULL,
  instruction CHAR NOT NULL,
  player CHAR NOT NULL
);
INSERT INTO choice VALUES
  ('A', 'X', 'C'),
  ('A', 'Y', 'A'),
  ('A', 'Z', 'B'),
  ('B', 'X', 'A'),
  ('B', 'Y', 'B'),
  ('B', 'Z', 'C'),
  ('C', 'X', 'B'),
  ('C', 'Y', 'C'),
  ('C', 'Z', 'A');

CREATE TABLE guide(
  opponent CHAR NOT NULL,
  instruction CHAR NOT NULL
);
COPY guide FROM :'input' WITH (FORMAT csv, HEADER false, DELIMITER ' ');

CREATE MATERIALIZED VIEW outcome_round AS
  SELECT choice.opponent, choice.player, shape_score.score AS shape_score, outcome_score.score AS outcome_score
    FROM guide
    JOIN choice
          ON guide.opponent = choice.opponent
         AND guide.instruction = choice.instruction
    JOIN shape_score
          ON choice.player = shape_score.shape
    JOIN outcome_score
          ON choice.instruction = outcome_score.instruction;

\echo
\t on
\o
SELECT SUM(shape_score) + SUM(outcome_score) FROM outcome_round;
