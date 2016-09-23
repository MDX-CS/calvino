CREATE TABLE exercise_inputs (
    exercise_id integer NOT NULL,
    input_line text
);

CREATE TABLE exercises (
    id integer NOT NULL,
    description text,
    code text
);

CREATE SEQUENCE exercises_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE exercises_id_seq OWNED BY exercises.id;

ALTER TABLE ONLY exercises ALTER COLUMN id SET DEFAULT nextval('exercises_id_seq'::regclass);

ALTER TABLE ONLY exercises
    ADD CONSTRAINT exercises_pkey PRIMARY KEY (id);

ALTER TABLE ONLY exercise_inputs
    ADD CONSTRAINT exercise_inputs_exercise_id_fkey FOREIGN KEY (exercise_id) REFERENCES exercises(id);
