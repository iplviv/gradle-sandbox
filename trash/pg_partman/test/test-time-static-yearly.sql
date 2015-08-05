-- ########## TIME STATIC TESTS ##########
-- Other tests: UNLOGGED

\set ON_ERROR_ROLLBACK 1
\set ON_ERROR_STOP true

BEGIN;
SELECT set_config('search_path','partman, public',false);

SELECT plan(137);
CREATE SCHEMA partman_test;
CREATE SCHEMA partman_retention_test;
CREATE ROLE partman_basic;
CREATE ROLE partman_revoke;
CREATE ROLE partman_owner;

CREATE UNLOGGED TABLE partman_test.time_static_table (col1 int primary key, col2 text, col3 timestamptz NOT NULL DEFAULT now());
INSERT INTO partman_test.time_static_table (col1, col3) VALUES (generate_series(1,10), CURRENT_TIMESTAMP);
GRANT SELECT,INSERT,UPDATE ON partman_test.time_static_table TO partman_basic;
GRANT ALL ON partman_test.time_static_table TO partman_revoke;

SELECT create_parent('partman_test.time_static_table', 'col3', 'time-static', 'yearly');
SELECT has_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'), 'Check time_static_table_'||to_char(CURRENT_TIMESTAMP, 'YYYY')||' exists');
SELECT has_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY')||' exists');
SELECT has_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY')||' exists');
SELECT has_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY')||' exists');
SELECT has_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY')||' exists');
SELECT hasnt_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY')||' does not exist');
SELECT has_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY')||' exists');
SELECT has_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY')||' exists');
SELECT has_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY')||' exists');
SELECT has_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY')||' exists');
SELECT hasnt_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'5 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP-'5 years'::interval, 'YYYY')||' does not exist');

SELECT results_eq('SELECT relpersistence::text FROM pg_catalog.pg_class WHERE oid::regclass = ''partman_test.time_static_table''::regclass', ARRAY['u'], 'Check that parent table is unlogged');
SELECT results_eq('SELECT relpersistence::text FROM pg_catalog.pg_class WHERE oid::regclass = ''partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY')||'''::regclass', ARRAY['u'], 'Check that partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY')||' is unlogged');
SELECT results_eq('SELECT relpersistence::text FROM pg_catalog.pg_class WHERE oid::regclass = ''partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY')||'''::regclass', ARRAY['u'], 'Check that partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY')||' is unlogged');
SELECT results_eq('SELECT relpersistence::text FROM pg_catalog.pg_class WHERE oid::regclass = ''partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY')||'''::regclass', ARRAY['u'], 'Check that partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY')||' is unlogged');
SELECT results_eq('SELECT relpersistence::text FROM pg_catalog.pg_class WHERE oid::regclass = ''partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY')||'''::regclass', ARRAY['u'], 'Check that partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY')||' is unlogged');
SELECT results_eq('SELECT relpersistence::text FROM pg_catalog.pg_class WHERE oid::regclass = ''partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY')||'''::regclass', ARRAY['u'], 'Check that partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY')||' is unlogged');

SELECT col_is_pk('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'), ARRAY['col1'], 
    'Check for primary key in time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'));
SELECT col_is_pk('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'), ARRAY['col1'], 
    'Check for primary key in time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'));
SELECT col_is_pk('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'), ARRAY['col1'], 
    'Check for primary key in time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'));
SELECT col_is_pk('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'), ARRAY['col1'], 
    'Check for primary key in time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'));
SELECT col_is_pk('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'), ARRAY['col1'], 
    'Check for primary key in time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'));
SELECT col_is_pk('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'), ARRAY['col1'], 
    'Check for primary key in time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'));
SELECT col_is_pk('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'), ARRAY['col1'], 
    'Check for primary key in time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'));
SELECT col_is_pk('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'), ARRAY['col1'], 
    'Check for primary key in time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'));
SELECT col_is_pk('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'), ARRAY['col1'], 
    'Check for primary key in time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'));
SELECT col_is_pk('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'), ARRAY['col1'], 
    'Check for primary key in time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'));

SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'));

SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'), 'partman_revoke', 
    ARRAY['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'TRUNCATE', 'REFERENCES', 'TRIGGER'], 
    'Check partman_revoke privileges of time_static_table_'||to_char(CURRENT_TIMESTAMP, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'), 'partman_revoke', 
    ARRAY['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'TRUNCATE', 'REFERENCES', 'TRIGGER'], 
    'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'), 'partman_revoke', 
    ARRAY['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'TRUNCATE', 'REFERENCES', 'TRIGGER'], 
    'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'), 'partman_revoke', 
    ARRAY['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'TRUNCATE', 'REFERENCES', 'TRIGGER'], 
    'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'), 'partman_revoke', 
    ARRAY['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'TRUNCATE', 'REFERENCES', 'TRIGGER'], 
    'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'), 'partman_revoke', 
    ARRAY['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'TRUNCATE', 'REFERENCES', 'TRIGGER'], 
    'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'), 'partman_revoke', 
    ARRAY['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'TRUNCATE', 'REFERENCES', 'TRIGGER'], 
    'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'), 'partman_revoke', 
    ARRAY['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'TRUNCATE', 'REFERENCES', 'TRIGGER'], 
    'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'), 'partman_revoke', 
    ARRAY['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'TRUNCATE', 'REFERENCES', 'TRIGGER'], 
    'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'));

SELECT results_eq('SELECT partition_data_time(''partman_test.time_static_table'')::int', ARRAY[10], 'Check that partitioning function returns correct count of rows moved');
SELECT is_empty('SELECT * FROM ONLY partman_test.time_static_table', 'Check that parent table has had data moved to partition');
SELECT results_eq('SELECT count(*)::int FROM partman_test.time_static_table', ARRAY[10], 'Check count from parent table');
SELECT results_eq('SELECT count(*)::int FROM partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'), 
    ARRAY[10], 'Check count from time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'));

REVOKE INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER ON partman_test.time_static_table FROM partman_revoke;
INSERT INTO partman_test.time_static_table (col1, col3) VALUES (generate_series(11,20), CURRENT_TIMESTAMP + '1 year'::interval);
INSERT INTO partman_test.time_static_table (col1, col3) VALUES (generate_series(21,25), CURRENT_TIMESTAMP + '2 years'::interval);
INSERT INTO partman_test.time_static_table (col1, col3) VALUES (generate_series(26,30), CURRENT_TIMESTAMP + '3 years'::interval);
INSERT INTO partman_test.time_static_table (col1, col3) VALUES (generate_series(31,37), CURRENT_TIMESTAMP + '4 years'::interval);
INSERT INTO partman_test.time_static_table (col1, col3) VALUES (generate_series(40,49), CURRENT_TIMESTAMP - '1 years'::interval);
INSERT INTO partman_test.time_static_table (col1, col3) VALUES (generate_series(50,70), CURRENT_TIMESTAMP - '2 years'::interval);
INSERT INTO partman_test.time_static_table (col1, col3) VALUES (generate_series(71,85), CURRENT_TIMESTAMP - '3 years'::interval);
INSERT INTO partman_test.time_static_table (col1, col3) VALUES (generate_series(86,100), CURRENT_TIMESTAMP - '4 years'::interval);

SELECT is_empty('SELECT * FROM ONLY partman_test.time_static_table', 'Check that parent table has had no data inserted to it');
SELECT results_eq('SELECT count(*)::int FROM partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'), 
    ARRAY[10], 'Check count from time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'));
SELECT results_eq('SELECT count(*)::int FROM partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'), 
    ARRAY[5], 'Check count from time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'));
SELECT results_eq('SELECT count(*)::int FROM partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'), 
    ARRAY[5], 'Check count from time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'));
SELECT results_eq('SELECT count(*)::int FROM partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'), 
    ARRAY[7], 'Check count from time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'));
SELECT results_eq('SELECT count(*)::int FROM partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'), 
    ARRAY[10], 'Check count from time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'));
SELECT results_eq('SELECT count(*)::int FROM partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'), 
    ARRAY[21], 'Check count from time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'));
SELECT results_eq('SELECT count(*)::int FROM partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'), 
    ARRAY[15], 'Check count from time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'));
SELECT results_eq('SELECT count(*)::int FROM partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'), 
    ARRAY[15], 'Check count from time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'));

UPDATE part_config SET premake = 5 WHERE parent_table = 'partman_test.time_static_table';
SELECT run_maintenance();
INSERT INTO partman_test.time_static_table (col1, col3) VALUES (generate_series(101,122), CURRENT_TIMESTAMP + '5 years'::interval);

SELECT has_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY')||' exists');
SELECT results_eq('SELECT relpersistence::text FROM pg_catalog.pg_class WHERE oid::regclass = ''partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY')||'''::regclass', ARRAY['u'], 'Check that partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY')||' is unlogged');
SELECT hasnt_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY')||' exists');
SELECT col_is_pk('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'), ARRAY['col1'], 
    'Check for primary key in time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'));

SELECT is_empty('SELECT * FROM ONLY partman_test.time_static_table', 'Check that parent table has had no data inserted to it');
SELECT results_eq('SELECT count(*)::int FROM partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'), 
    ARRAY[22], 'Check count from time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'));

SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'), 'partman_basic', ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'), 'partman_basic', 
ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'), 'partman_basic', 
ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'));

SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'), 'partman_revoke', 
    ARRAY['SELECT'], 'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'));

GRANT DELETE ON partman_test.time_static_table TO partman_basic;
REVOKE ALL ON partman_test.time_static_table FROM partman_revoke;
ALTER TABLE partman_test.time_static_table OWNER TO partman_owner;

UPDATE part_config SET premake = 6 WHERE parent_table = 'partman_test.time_static_table';
SELECT run_maintenance();
INSERT INTO partman_test.time_static_table (col1, col3) VALUES (generate_series(123,150), CURRENT_TIMESTAMP + '6 years'::interval);

SELECT is_empty('SELECT * FROM ONLY partman_test.time_static_table', 'Check that parent table has had no data inserted to it');
SELECT results_eq('SELECT count(*)::int FROM partman_test.time_static_table', ARRAY[148], 'Check count from parent table');
SELECT results_eq('SELECT count(*)::int FROM partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'), 
    ARRAY[28], 'Check count from time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'));

SELECT has_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'), 
    'Check time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY')||' exists');
SELECT results_eq('SELECT relpersistence::text FROM pg_catalog.pg_class WHERE oid::regclass = ''partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY')||'''::regclass', ARRAY['u'], 'Check that partman_test.time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY')||' is unlogged');
SELECT hasnt_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'7 years'::interval, 'YYYY'), 
    'Check time_static_table_p'||to_char(CURRENT_TIMESTAMP+'7 years'::interval, 'YYYY')||' exists');
SELECT col_is_pk('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'), ARRAY['col1'], 
    'Check for primary key in time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'), 'partman_basic', ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'), 'partman_revoke', 
    ARRAY['SELECT'], 'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'), 'partman_revoke', 
    '{}'::text[], 'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'));

SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE', 'DELETE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'));
SELECT table_owner_is ('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'), 'partman_owner', 
    'Check that ownership change worked for time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'));

INSERT INTO partman_test.time_static_table (col1, col3) VALUES (generate_series(200,210), CURRENT_TIMESTAMP + '20 years'::interval);
SELECT results_eq('SELECT count(*)::int FROM ONLY partman_test.time_static_table', ARRAY[11], 'Check that data outside trigger scope goes to parent');

SELECT reapply_privileges('partman_test.time_static_table');
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE', 'DELETE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE', 'DELETE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE', 'DELETE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE', 'DELETE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE', 'DELETE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE', 'DELETE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE', 'DELETE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE', 'DELETE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE', 'DELETE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE', 'DELETE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'), 'partman_basic', 
    ARRAY['SELECT','INSERT','UPDATE', 'DELETE'], 
    'Check partman_basic privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'));

SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'), 'partman_revoke', 
    '{}'::text[], 'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'), 'partman_revoke', 
    '{}'::text[], 'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'), 'partman_revoke', 
    '{}'::text[], 'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'), 'partman_revoke', 
    '{}'::text[], 'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'), 'partman_revoke', 
    '{}'::text[], 'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'), 'partman_revoke', 
    '{}'::text[], 'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'));
SELECT table_privs_are('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'), 'partman_revoke', 
    '{}'::text[], 'Check partman_revoke privileges of time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'));

SELECT table_owner_is ('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'), 'partman_owner', 
    'Check that ownership change worked for time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'));
SELECT table_owner_is ('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'), 'partman_owner', 
    'Check that ownership change worked for time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'));
SELECT table_owner_is ('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'), 'partman_owner', 
    'Check that ownership change worked for time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'));
SELECT table_owner_is ('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'), 'partman_owner', 
    'Check that ownership change worked for time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'));
SELECT table_owner_is ('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'), 'partman_owner', 
    'Check that ownership change worked for time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'));
SELECT table_owner_is ('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'), 'partman_owner', 
    'Check that ownership change worked for time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'));
SELECT table_owner_is ('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'), 'partman_owner', 
    'Check that ownership change worked for time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'));
SELECT table_owner_is ('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'), 'partman_owner', 
    'Check that ownership change worked for time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'));
SELECT table_owner_is ('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'), 'partman_owner', 
    'Check that ownership change worked for time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'));
SELECT table_owner_is ('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'), 'partman_owner', 
    'Check that ownership change worked for time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'));
SELECT table_owner_is ('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'), 'partman_owner', 
    'Check that ownership change worked for time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'));

SELECT drop_partition_time('partman_test.time_static_table', '3 years', p_keep_table := false);
SELECT hasnt_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP-'4 years'::interval, 'YYYY')||' does not exist');

UPDATE part_config SET retention = '2 years'::interval WHERE parent_table = 'partman_test.time_static_table';
SELECT drop_partition_time('partman_test.time_static_table', p_retention_schema := 'partman_retention_test');
SELECT hasnt_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY')||' does not exist');
/* This test may fail around the end of the year. If so, swap to the commented out one below */
SELECT has_table('partman_retention_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP-'3 years'::interval, 'YYYY')||' got moved to new schema. This test may fail around the year boundary. See comment in tests for different test to try.');

/* This test may fail around the beginning of the year. If so, swap to the commented out one above */
--SELECT has_table('partman_retention_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'), 
--    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY')||' got moved to new schema. This test may fail around the year boundary. See comment in tests for different test to try.');

SELECT undo_partition_time('partman_test.time_static_table', 20, p_keep_table := false);
/* This test may fail around the end of the year. If so, swap to the commented out one below */
SELECT results_eq('SELECT count(*)::int FROM ONLY partman_test.time_static_table', ARRAY[129], 'Check count from parent table after undo. This test may fail around the year boundary. See comment in tests for different test to try.');

/* This test may fail around the beginning of the year. If so, swap to the commented out one above */
--SELECT results_eq('SELECT count(*)::int FROM ONLY partman_test.time_static_table', ARRAY[108], 'Check count from parent table after undo. This test may fail around the year boundary. See comment in tests for different test to try.');
SELECT hasnt_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP, 'YYYY')||' does not exist');
SELECT hasnt_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP+'1 year'::interval, 'YYYY')||' does not exist');
SELECT hasnt_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP+'2 years'::interval, 'YYYY')||' does not exist');
SELECT hasnt_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP+'3 years'::interval, 'YYYY')||' does not exist');
SELECT hasnt_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP+'4 years'::interval, 'YYYY')||' does not exist');
SELECT hasnt_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP+'5 years'::interval, 'YYYY')||' does not exist');
SELECT hasnt_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP+'6 years'::interval, 'YYYY')||' does not exist');
SELECT hasnt_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP-'1 year'::interval, 'YYYY')||' does not exist');
SELECT hasnt_table('partman_test', 'time_static_table_p'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY'), 
    'Check time_static_table_'||to_char(CURRENT_TIMESTAMP-'2 years'::interval, 'YYYY')||' does not exist');

SELECT * FROM finish();
ROLLBACK;
