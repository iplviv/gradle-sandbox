-- ########## pg_jobmon extension table definitions ##########
-- Recommended to make job_log and job_detail tables partitioned on start_time 
--  if you see high logging traffic or don't need to keep the data indefinitely
CREATE TABLE job_log (
    job_id bigint NOT NULL,
    owner text NOT NULL,
    job_name text NOT NULL,
    start_time timestamp with time zone NOT NULL,
    end_time timestamp with time zone,
    status text,
    pid integer NOT NULL,
    CONSTRAINT job_log_job_id_pkey PRIMARY KEY (job_id)
);
CREATE INDEX job_log_job_name_idx ON job_log (job_name);
CREATE INDEX job_log_start_time_idx ON job_log (start_time);
CREATE INDEX job_log_status_idx ON job_log (status);
CREATE INDEX job_log_pid_idx ON job_log (pid);
CREATE SEQUENCE job_log_job_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE job_log_job_id_seq OWNED BY job_log.job_id;
ALTER TABLE job_log ALTER COLUMN job_id SET DEFAULT nextval('job_log_job_id_seq'::regclass);


CREATE TABLE job_detail (
    job_id bigint NOT NULL,
    step_id bigint NOT NULL,
    action text NOT NULL,
    start_time timestamp with time zone NOT NULL,
    end_time timestamp with time zone,
    elapsed_time real,
    status text,
    message text,
    CONSTRAINT job_detail_step_id_pkey PRIMARY KEY (step_id),
    CONSTRAINT job_detail_job_id_fkey FOREIGN KEY (job_id) REFERENCES job_log(job_id) ON DELETE CASCADE
);
CREATE INDEX job_detail_job_id_idx ON job_detail (job_id);
CREATE SEQUENCE job_detail_step_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE job_detail_step_id_seq OWNED BY job_detail.step_id;
ALTER TABLE job_detail ALTER COLUMN step_id SET DEFAULT nextval('job_detail_step_id_seq'::regclass);


CREATE TABLE job_check_log (
    job_id bigint NOT NULL,
    job_name text NOT NULL,
    alert_code int DEFAULT 3 NOT NULL
);
SELECT pg_catalog.pg_extension_config_dump('job_check_log', '');


CREATE TABLE dblink_mapping (
    username text,
    port text,
    pwd text
);
SELECT pg_catalog.pg_extension_config_dump('dblink_mapping', '');


CREATE TABLE job_check_config (
    job_name text NOT NULL,
    warn_threshold interval NOT NULL,
    error_threshold interval NOT NULL,
    active boolean DEFAULT false NOT NULL,
    sensitivity smallint DEFAULT 0 NOT NULL,
    escalate int,
    CONSTRAINT job_check_config_job_name_pkey PRIMARY KEY (job_name)
);
SELECT pg_catalog.pg_extension_config_dump('job_check_config', '');


CREATE TABLE job_status_text (
    alert_code  integer NOT NULL,
    alert_text  text NOT NULL,
    CONSTRAINT job_status_text_alert_code_pkey PRIMARY KEY (alert_code)
);
SELECT pg_catalog.pg_extension_config_dump('job_status_text', '');
INSERT INTO job_status_text (alert_code, alert_text) VALUES (1, 'OK');
INSERT INTO job_status_text (alert_code, alert_text) VALUES (2, 'WARNING');
INSERT INTO job_status_text (alert_code, alert_text) VALUES (3, 'CRITICAL');
/*
 *  Add Job Autonmous
 */
CREATE FUNCTION _autonomous_add_job(p_owner text, p_job_name text, p_pid integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_job_id bigint;
BEGIN
    SELECT nextval('@extschema@.job_log_job_id_seq') INTO v_job_id;

    INSERT INTO @extschema@.job_log (job_id, owner, job_name, start_time, pid)
    VALUES (v_job_id, p_owner, upper(p_job_name), current_timestamp, p_pid); 

    RETURN v_job_id; 
END
$$;

/*
 *  Add Job
 */
CREATE OR REPLACE FUNCTION add_job(p_job_name text) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE 
    v_job_id bigint;
    v_remote_query text;
    v_dblink_schema text;
BEGIN
    SELECT nspname INTO v_dblink_schema FROM pg_namespace n, pg_extension e WHERE e.extname = 'dblink' AND e.extnamespace = n.oid;
    
    v_remote_query := 'SELECT @extschema@._autonomous_add_job (' ||
        quote_literal(current_user) || ',' ||
        quote_literal(p_job_name) || ',' ||
        pg_backend_pid() || ')';

    EXECUTE 'SELECT job_id FROM ' || v_dblink_schema || '.dblink('||quote_literal(@extschema@.auth())||
        ','|| quote_literal(v_remote_query) || ',TRUE) t (job_id int)' INTO v_job_id;      

    IF v_job_id IS NULL THEN
        RAISE EXCEPTION 'Job creation failed';
    END IF;

    RETURN v_job_id;
END
$$;
/*
 *  Add Step Autonomous
 */
CREATE FUNCTION _autonomous_add_step(p_job_id bigint, p_action text) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_step_id bigint;
BEGIN
    SELECT nextval('@extschema@.job_detail_step_id_seq') INTO v_step_id;

    INSERT INTO @extschema@.job_detail (job_id, step_id, action, start_time)
    VALUES (p_job_id, v_step_id, p_action, current_timestamp);

    RETURN v_step_id;
END
$$;

/*
 *  Add Step
 */
CREATE OR REPLACE FUNCTION add_step(p_job_id bigint, p_action text) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE 
    v_step_id bigint;
    v_remote_query text;
    v_dblink_schema text;
    
BEGIN

    SELECT nspname INTO v_dblink_schema FROM pg_namespace n, pg_extension e WHERE e.extname = 'dblink' AND e.extnamespace = n.oid;
    
    v_remote_query := 'SELECT @extschema@._autonomous_add_step (' ||
        p_job_id || ',' ||
        quote_literal(p_action) || ')';

    EXECUTE 'SELECT step_id FROM ' || v_dblink_schema || '.dblink('||quote_literal(@extschema@.auth())||
        ','|| quote_literal(v_remote_query) || ',TRUE) t (step_id int)' INTO v_step_id;      

    IF v_step_id IS NULL THEN
        RAISE EXCEPTION 'Job creation failed';
    END IF;

    RETURN v_step_id;
END
$$;
/*
 *  dblink Authentication mapping
 */
CREATE FUNCTION auth() RETURNS text
    LANGUAGE plpgsql STABLE
    AS $$
DECLARE
 
    v_auth          text = '';
    v_port          text;
    v_password      text; 
    v_username      text;
 
BEGIN
    SELECT username, port, pwd INTO v_username, v_port, v_password FROM @extschema@.dblink_mapping;

    IF v_port IS NULL THEN
        v_auth = 'dbname=' || current_database();
    ELSE
        v_auth := 'port='||v_port||' dbname=' || current_database();
    END IF;

    IF v_username IS NOT NULL THEN
        v_auth := v_auth || ' user='||v_username;
    END IF;

    IF v_password IS NOT NULL THEN
        v_auth := v_auth || ' password='||v_password;
    END IF;
    RETURN v_auth;    
END
$$;
/*
 *  Cancel Job
 */
CREATE FUNCTION cancel_job(p_job_id bigint) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_current_role  text;
    v_pid           integer;
    v_step_id       bigint;
    v_status        text;
BEGIN
    EXECUTE 'SELECT alert_text FROM @extschema@.job_status_text WHERE alert_code = 3'
        INTO v_status;
    SELECT pid INTO v_pid FROM @extschema@.job_log WHERE job_id = p_job_id;
    SELECT current_user INTO v_current_role;
    PERFORM pg_cancel_backend(v_pid);
    SELECT max(step_id) INTO v_step_id FROM @extschema@.job_detail WHERE job_id = p_job_id;
    PERFORM @extschema@._autonomous_update_step(v_step_id, v_status, 'Manually cancelled via call to @extschema@.cancel_job() by '||v_current_role);
    PERFORM @extschema@._autonomous_fail_job(p_job_id, 3);
    RETURN true;
END
$$;
/*
 *  Check Job status
 *
 * p_history is how far into job_log's past the check will go. Don't go further back than the longest job's interval that is contained
 *      in job_check_config to keep check efficient
 * Return code 1 means a successful job run
 * Return code 2 is for use with jobs that support a warning indicator. Not critical, but someone should look into it
 * Return code 3 is for use with a critical job failure 
 */
CREATE FUNCTION check_job_status(p_history interval, OUT alert_code int, OUT alert_status text, OUT job_name text, OUT alert_text text) RETURNS SETOF record 
LANGUAGE plpgsql
    AS $$
DECLARE
    v_count                 int = 1;
    v_longest_period        interval;
    v_row                   record;
    v_rowcount              int;
    v_problem_count         int := 0;
    v_version               int;
BEGIN

-- Leave this check here in case helper function isn't used and this is called directly with an interval argument
SELECT greatest(max(error_threshold), max(warn_threshold)) INTO v_longest_period FROM @extschema@.job_check_config;
IF v_longest_period IS NOT NULL THEN
    IF p_history < v_longest_period THEN
        RAISE EXCEPTION 'Input argument must be greater than or equal to the longest threshold in job_check_config table';
    END IF;
END IF;
    
SELECT current_setting('server_version_num')::int INTO v_version;

CREATE TEMP TABLE jobmon_check_job_status_temp (alert_code int, alert_status text, job_name text, alert_text text, pid int);

-- Check for jobs with three consecutive errors and not set for any special configuration
INSERT INTO jobmon_check_job_status_temp (alert_code, alert_status, job_name, alert_text)
SELECT l.alert_code, 'FAILED_RUN' AS alert_status, l.job_name, '3 consecutive '||t.alert_text||' runs' AS alert_text
FROM @extschema@.job_check_log l 
JOIN @extschema@.job_status_text t ON l.alert_code = t.alert_code
WHERE l.job_name NOT IN (
    SELECT c.job_name FROM @extschema@.job_check_config c
) GROUP BY l.job_name, l.alert_code, t.alert_text HAVING count(*) > 2;

GET DIAGNOSTICS v_rowcount = ROW_COUNT;
IF v_rowcount IS NOT NULL AND v_rowcount > 0 THEN
    v_problem_count := v_problem_count + 1;
END IF;

-- Check for jobs with specially configured sensitivity
INSERT INTO jobmon_check_job_status_temp (alert_code, alert_status, job_name, alert_text)
SELECT l.alert_code, 'FAILED_RUN' as alert_status, l.job_name, count(*)||' '||t.alert_text||' run(s)' AS alert_text 
FROM @extschema@.job_check_log l
JOIN @extschema@.job_check_config c ON l.job_name = c.job_name
JOIN @extschema@.job_status_text t ON l.alert_code = t.alert_code
GROUP BY l.job_name, l.alert_code, t.alert_text, c.sensitivity HAVING count(*) > c.sensitivity;

GET DIAGNOSTICS v_rowcount = ROW_COUNT;
IF v_rowcount IS NOT NULL AND v_rowcount > 0 THEN
    v_problem_count := v_problem_count + 1;
END IF;

-- Check for missing jobs that have configured time thresholds. Jobs that have not run since before the p_history will return pid as NULL
INSERT INTO jobmon_check_job_status_temp (alert_code, alert_status, job_name, alert_text, pid)
SELECT CASE WHEN l.max_start IS NULL AND l.end_time IS NULL THEN 3
    WHEN (CURRENT_TIMESTAMP - l.max_start) > c.error_threshold THEN 3
    WHEN (CURRENT_TIMESTAMP - l.max_start) > c.warn_threshold THEN 2
    ELSE 3
  END AS ac
, CASE WHEN (CURRENT_TIMESTAMP - l.max_start) > c.warn_threshold OR l.end_time IS NULL THEN 'MISSING' 
    ELSE l.status 
  END AS alert_status
, c.job_name
, COALESCE('Last completed run: '||l.max_end, 'Has not completed a run since highest configured monitoring time period') AS alert_text
, l.pid
FROM @extschema@.job_check_config c
LEFT JOIN (
    WITH max_start_time AS (
        SELECT w.job_name, max(w.start_time) as max_start, max(w.end_time) as max_end FROM @extschema@.job_log w WHERE start_time > (CURRENT_TIMESTAMP - p_history) GROUP BY w.job_name)
    SELECT a.job_name, a.end_time, a.status, a.pid, m.max_start, m.max_end
    FROM @extschema@.job_log a
    JOIN max_start_time m ON a.job_name = m.job_name and a.start_time = m.max_start
    WHERE start_time > (CURRENT_TIMESTAMP - p_history)
) l ON c.job_name = l.job_name
WHERE c.active
AND (CURRENT_TIMESTAMP - l.max_start) > c.warn_threshold OR l.max_start IS NULL
ORDER BY ac, l.job_name, l.max_start;

GET DIAGNOSTICS v_rowcount = ROW_COUNT;
IF v_rowcount IS NOT NULL AND v_rowcount > 0 THEN
    v_problem_count := v_problem_count + 1;
END IF;

-- Check for BLOCKED after RUNNING to ensure blocked jobs are labelled properly   
IF v_version >= 90200 THEN
    -- Jobs currently running that have not run before within their configured monitoring time period
    FOR v_row IN SELECT j.job_name
        FROM @extschema@.job_log j
        JOIN @extschema@.job_check_config c ON j.job_name = c.job_name
        JOIN pg_catalog.pg_stat_activity a ON j.pid = a.pid
        WHERE j.start_time > (CURRENT_TIMESTAMP - p_history)
        AND (CURRENT_TIMESTAMP - j.start_time) >= least(c.warn_threshold, c.error_threshold)
        AND j.end_time IS NULL 
    LOOP
        UPDATE jobmon_check_job_status_temp t 
        SET alert_status = 'RUNNING'
            , alert_text = (SELECT COALESCE('Currently running. Last completed run: '||max(end_time),
                        'Currently running. Job has not had a completed run within configured monitoring time period.') 
                FROM @extschema@.job_log 
                WHERE job_log.job_name = v_row.job_name 
                AND job_log.start_time > (CURRENT_TIMESTAMP - p_history))
        WHERE t.job_name = v_row.job_name;
     END LOOP;
    
    -- Jobs blocked by locks 
    FOR v_row IN SELECT j.job_name
        FROM @extschema@.job_log j
        JOIN pg_catalog.pg_locks l ON j.pid = l.pid
        JOIN pg_catalog.pg_stat_activity a ON j.pid = a.pid
        WHERE j.start_time > (CURRENT_TIMESTAMP - p_history)
        AND NOT l.granted
    LOOP
        UPDATE jobmon_check_job_status_temp t 
        SET alert_status = 'BLOCKED'
            , alert_text = COALESCE('Another transaction has a lock that blocking this job from completing') 
        WHERE t.job_name = v_row.job_name;
     END LOOP;  

ELSE -- version less than 9.2 with old procpid column

    -- Jobs currently running that have not run before within their configured monitoring time period
    FOR v_row IN SELECT j.job_name
        FROM @extschema@.job_log j
        JOIN @extschema@.job_check_config c ON j.job_name = c.job_name
        JOIN pg_catalog.pg_stat_activity a ON j.pid = a.procpid
        WHERE j.start_time > (CURRENT_TIMESTAMP - p_history)
        AND (CURRENT_TIMESTAMP - j.start_time) >= least(c.warn_threshold, c.error_threshold)
        AND j.end_time IS NULL 
    LOOP
        UPDATE jobmon_check_job_status_temp t 
        SET alert_status = 'RUNNING'
            , alert_text = (SELECT COALESCE('Currently running. Last completed run: '||max(end_time),
                        'Currently running. Job has not had a completed run within configured monitoring time period.') 
                FROM @extschema@.job_log 
                WHERE job_log.job_name = v_row.job_name 
                AND job_log.start_time > (CURRENT_TIMESTAMP - p_history))
        WHERE t.job_name = v_row.job_name;
   END LOOP;  

   -- Jobs blocked by locks 
    FOR v_row IN SELECT j.job_name
        FROM @extschema@.job_log j
        JOIN pg_catalog.pg_locks l ON j.pid = l.pid
        JOIN pg_catalog.pg_stat_activity a ON j.pid = a.procpid
        WHERE j.start_time > (CURRENT_TIMESTAMP - p_history)
        AND NOT l.granted
    LOOP
        UPDATE jobmon_check_job_status_temp t 
        SET alert_status = 'BLOCKED'
            , alert_text = COALESCE('Another transaction has a lock that blocking this job from completing') 
        WHERE t.job_name = v_row.job_name;
    END LOOP;  

END IF; -- end version check IF

IF v_problem_count > 0 THEN
    FOR v_row IN SELECT t.alert_code, t.alert_status, t.job_name, t.alert_text FROM jobmon_check_job_status_temp t ORDER BY alert_code DESC, job_name ASC, alert_status ASC
    LOOP
        alert_code := v_row.alert_code;
        alert_status := v_row.alert_status;
        job_name := v_row.job_name;
        alert_text := v_row.alert_text;
        RETURN NEXT;
    END LOOP;
ELSE
        alert_code := 1;
        alert_status := 'OK'; 
        job_name := NULL;
        alert_text := 'All jobs run successfully';
        RETURN NEXT;
END IF;

DROP TABLE IF EXISTS jobmon_check_job_status_temp;

END
$$;


/*
 * Helper function to allow calling without an argument.
 */
CREATE FUNCTION check_job_status(OUT alert_code int, OUT alert_status text, OUT job_name text, OUT alert_text text) RETURNS SETOF record 
    LANGUAGE plpgsql STABLE
    AS $$
DECLARE
    v_longest_period    interval;
    v_row               record;
BEGIN

-- Interval doesn't matter if nothing is in job_check_config. Just give default of 1 week. 
-- Still monitors for any 3 consecutive failures.
SELECT COALESCE(greatest(max(error_threshold), max(warn_threshold)), '1 week') INTO v_longest_period FROM @extschema@.job_check_config;

FOR v_row IN SELECT q.alert_code, q.alert_status, q.job_name, q.alert_text FROM @extschema@.check_job_status(v_longest_period) q
LOOP
        alert_code := v_row.alert_code;
        alert_status := v_row.alert_status;
        job_name := v_row.job_name;
        alert_text := v_row.alert_text;
        RETURN NEXT;
END LOOP;

END
$$;
/*
 *  Close Job Autonomous
 */
CREATE FUNCTION _autonomous_close_job(p_job_id bigint) RETURNS integer
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_numrows integer;
    v_status text;
BEGIN    
    EXECUTE 'SELECT alert_text FROM @extschema@.job_status_text WHERE alert_code = 1'
        INTO v_status;
    UPDATE @extschema@.job_log SET
        end_time = current_timestamp,
        status = v_status
    WHERE job_id = p_job_id;
    GET DIAGNOSTICS v_numrows = ROW_COUNT;
    RETURN v_numrows;
END
$$;

/*
 *  Close Job
 */
CREATE OR REPLACE FUNCTION close_job(p_job_id bigint) RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_remote_query text;
    v_dblink_schema text;
BEGIN

    SELECT nspname INTO v_dblink_schema FROM pg_namespace n, pg_extension e WHERE e.extname = 'dblink' AND e.extnamespace = n.oid;
    
    v_remote_query := 'SELECT @extschema@._autonomous_close_job('||p_job_id||')'; 

    EXECUTE 'SELECT devnull FROM ' || v_dblink_schema || '.dblink('||quote_literal(@extschema@.auth())||
        ',' || quote_literal(v_remote_query) || ',TRUE) t (devnull int)';  
END
$$;
/*
 *  Fail Job Autonomous
 */
CREATE FUNCTION _autonomous_fail_job(p_job_id bigint, p_fail_level int) RETURNS integer
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_numrows integer;
    v_status text;
BEGIN
    EXECUTE 'SELECT alert_text FROM @extschema@.job_status_text WHERE alert_code = '||p_fail_level
        INTO v_status;
    UPDATE @extschema@.job_log SET
        end_time = current_timestamp,
        status = v_status
    WHERE job_id = p_job_id;
    GET DIAGNOSTICS v_numrows = ROW_COUNT;
    RETURN v_numrows;
END
$$;

/*
 *  Fail Job
 */
CREATE FUNCTION fail_job(p_job_id bigint, p_fail_level int DEFAULT 3) RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_remote_query text;
    v_dblink_schema text;
BEGIN
    
    SELECT nspname INTO v_dblink_schema FROM pg_namespace n, pg_extension e WHERE e.extname = 'dblink' AND e.extnamespace = n.oid;
    
    v_remote_query := 'SELECT @extschema@._autonomous_fail_job('||p_job_id||', '||p_fail_level||')'; 

    EXECUTE 'SELECT devnull FROM ' || v_dblink_schema || '.dblink('||quote_literal(@extschema@.auth())||
        ',' || quote_literal(v_remote_query) || ',TRUE) t (devnull int)';  

END
$$;
CREATE FUNCTION job_check_log_escalate_trig() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE

v_count                         int;
v_escalate                      int;
v_highest_logged_alert_code     int;
v_max_alert_code                int;
v_step_id                       bigint;

BEGIN

SELECT escalate INTO v_escalate FROM @extschema@.job_check_config WHERE job_name = NEW.job_name;
IF v_escalate IS NOT NULL THEN
    -- Only get the count of the highest number of failures for an alert code for a specific job
    SELECT count(job_name)
        , alert_code 
    INTO v_count
        , v_highest_logged_alert_code 
    FROM @extschema@.job_check_log 
    WHERE job_name = NEW.job_name 
    GROUP BY job_name, alert_code 
    ORDER BY alert_code DESC 
    LIMIT 1 ;

    -- Ensure new alert codes are always equal to at least the last escalated value
    IF v_highest_logged_alert_code > NEW.alert_code THEN
       NEW.alert_code = v_highest_logged_alert_code; 
    END IF;

    -- +1 to ensure the insertion that matches the v_escalate value triggers the escalation, not the next insertion
    IF v_count + 1 >= v_escalate THEN
        SELECT max(alert_code) INTO v_max_alert_code FROM @extschema@.job_status_text;
        IF NEW.alert_code < v_max_alert_code THEN -- Don't exceed the highest configured alert code
            NEW.alert_code = NEW.alert_code + 1;
            -- Log that alert code was escalated by the last job that failed
            EXECUTE 'SELECT @extschema@.add_step('||NEW.job_id||', ''ALERT ESCALATION'')' INTO v_step_id;
            EXECUTE 'SELECT @extschema@.update_step('||v_step_id||', ''ESCALATE'', 
                ''Job has alerted at level '||NEW.alert_code - 1 ||' in excess of the escalate value configured for this job ('||v_escalate||
                    '). Alert code value has been escaleted to: '||NEW.alert_code||''')';
            EXECUTE 'UPDATE @extschema@.job_log SET status = ''ESCALATED'' WHERE job_id = '||NEW.job_id;
        END IF;
    END IF;
END IF; 

RETURN NEW;
END
$$;


CREATE TRIGGER job_check_log_escalate_trig
BEFORE INSERT ON @extschema@.job_check_log
FOR EACH ROW EXECUTE PROCEDURE job_check_log_escalate_trig();


/*
 *  Delete jobs from job_log and job_detail table older than a given interval.
 *  Also logs this job purging task.
 */
CREATE FUNCTION job_log_clear(p_interval interval) RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
    
    v_boundary      timestamptz;
    v_job_id        bigint;
    v_rowcount      bigint;
    v_step_id       bigint;

BEGIN

v_boundary := now() - p_interval;
v_job_id := @extschema@.add_job('Purging pg_jobmon job logs older than '|| v_boundary);
v_step_id := @extschema@.add_step(v_job_id,'Purging pg_jobmon job logs older than '|| v_boundary);

DELETE FROM @extschema@.job_log WHERE start_time <= v_boundary;

GET DIAGNOSTICS v_rowcount = ROW_COUNT;
IF v_rowcount > 0 THEN
    RAISE NOTICE 'Deleted % rows from job_log and associated rows in job_detail', v_rowcount;
    PERFORM @extschema@.update_step(v_step_id, 'OK', 'Deleted '||v_rowcount||' rows from job_log and associated rows in job_detail');
ELSE
    RAISE NOTICE 'No jobs logged older than %', v_boundary;
    PERFORM @extschema@.update_step(v_step_id, 'OK', 'No jobs logged older than '|| v_boundary);
END IF;
PERFORM @extschema@.close_job(v_job_id);
RETURN;

EXCEPTION
    WHEN OTHERS THEN
        IF v_step_id IS NULL THEN
            v_step_id := @extschema@.add_step(v_job_id, 'EXCEPTION before first step logged');
        END IF;
        PERFORM @extschema@.update_step(v_step_id, 'CRITICAL', 'ERROR: '||coalesce(SQLERRM,'unknown'));
        PERFORM @extschema@.fail_job(v_job_id);
        RAISE EXCEPTION '%', SQLERRM;
END
$$;
/*
 *  Job Monitor Trigger
 */
CREATE OR REPLACE FUNCTION job_monitor() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_alert_code    int;
BEGIN

    SELECT alert_code INTO v_alert_code FROM @extschema@.job_status_text WHERE alert_text = NEW.status;
    IF v_alert_code IS NOT NULL THEN
        IF v_alert_code = 1 THEN
            DELETE FROM @extschema@.job_check_log WHERE job_name = NEW.job_name;
        ELSE
            INSERT INTO @extschema@.job_check_log (job_id, job_name, alert_code) VALUES (NEW.job_id, NEW.job_name, v_alert_code);
        END IF;
    END IF;

    RETURN NULL;
END
$$;

CREATE TRIGGER trg_job_monitor 
AFTER UPDATE ON @extschema@.job_log 
FOR EACH ROW EXECUTE PROCEDURE @extschema@.job_monitor();
/*
 *  Show Job Details By Job ID
 */
CREATE FUNCTION show_detail(p_id bigint) RETURNS SETOF @extschema@.job_detail
    LANGUAGE plpgsql STABLE
    AS $$
DECLARE
    v_job_detail     @extschema@.job_detail%ROWTYPE;
BEGIN
    FOR v_job_detail IN SELECT job_id, step_id, action, start_time, end_time, elapsed_time, status, message
        FROM @extschema@.job_detail
        WHERE job_id = p_id
        ORDER BY step_id ASC
    LOOP
        RETURN NEXT v_job_detail; 
    END LOOP;

    RETURN;
END
$$;


/*
 *  Show Job Details By Exact Name
 */
CREATE FUNCTION show_detail(p_name text, int default 1) RETURNS SETOF @extschema@.job_detail
    LANGUAGE plpgsql STABLE
    AS $$
DECLARE
    v_job_list      @extschema@.job_log%ROWTYPE;
    v_job_detail     @extschema@.job_detail%ROWTYPE;
BEGIN

    FOR v_job_list IN SELECT job_id, owner, job_name, start_time, end_time, status, pid  
        FROM @extschema@.job_log
        WHERE job_name = upper(p_name)
        ORDER BY job_id DESC
        LIMIT $2
    LOOP
        FOR v_job_detail IN SELECT job_id, step_id, action, start_time, end_time, elapsed_time, status, message
            FROM @extschema@.job_detail
            WHERE job_id = v_job_list.job_id
            ORDER BY step_id ASC
        LOOP
            RETURN NEXT v_job_detail; 
        END LOOP;
    END LOOP;

    RETURN;
END
$$;
/*
 *  Show Jobs Like Name
 */
CREATE FUNCTION show_job_like(p_name text, int default 10) RETURNS SETOF @extschema@.job_log
    LANGUAGE plpgsql STABLE
    AS $$
DECLARE
    v_job_list      @extschema@.job_log%ROWTYPE;
BEGIN
    FOR v_job_list IN SELECT job_id, owner, job_name, start_time, end_time, status, pid  
        FROM @extschema@.job_log
        WHERE job_name ~ upper(p_name)
        ORDER BY job_id DESC
        LIMIT $2
    LOOP
        RETURN NEXT v_job_list; 
    END LOOP;

    RETURN;
END
$$;
/*
 *  Show Jobs By Exact Name
 */
CREATE FUNCTION show_job(p_name text, int default 10) RETURNS SETOF @extschema@.job_log
    LANGUAGE plpgsql STABLE
    AS $$
DECLARE
    v_job_list      @extschema@.job_log%ROWTYPE;
BEGIN
    FOR v_job_list IN SELECT job_id, owner, job_name, start_time, end_time, status, pid  
        FROM @extschema@.job_log
        WHERE job_name = upper(p_name)
        ORDER BY job_id DESC
        LIMIT $2
    LOOP
        RETURN NEXT v_job_list; 
    END LOOP;

    RETURN;
END
$$;
/*
 *  Show Jobs By Status
 */
CREATE FUNCTION show_job_status(p_status text, int default 10) RETURNS SETOF @extschema@.job_log
    LANGUAGE plpgsql STABLE
    AS $$
DECLARE
    v_job_list      @extschema@.job_log%ROWTYPE;
BEGIN
    FOR v_job_list IN SELECT job_id, owner, job_name, start_time, end_time, status, pid  
        FROM @extschema@.job_log
        WHERE status = p_status
        ORDER BY job_id DESC
        LIMIT $2
    LOOP
        RETURN NEXT v_job_list; 
    END LOOP;

    RETURN;
END
$$;

/*
 *  Show Jobs By Exact Name and Status
 */
CREATE FUNCTION show_job_status(p_name text, p_status text, int default 10) RETURNS SETOF @extschema@.job_log
    LANGUAGE plpgsql STABLE
    AS $$
DECLARE
    v_job_list      @extschema@.job_log%ROWTYPE;
BEGIN
    FOR v_job_list IN SELECT job_id, owner, job_name, start_time, end_time, status, pid  
        FROM @extschema@.job_log
        WHERE job_name = upper(p_name)
        AND status = p_status
        ORDER BY job_id DESC
        LIMIT $3
    LOOP
        RETURN NEXT v_job_list; 
    END LOOP;

    RETURN;
END
$$;
/*
 *  Show Currently Running Jobs
 */
CREATE FUNCTION show_running(int default 10) RETURNS SETOF @extschema@.job_log
    LANGUAGE plpgsql STABLE
    AS $$
DECLARE
    v_job_list      @extschema@.job_log%ROWTYPE;
    v_version       int;
BEGIN

SELECT current_setting('server_version_num')::int INTO v_version;

IF v_version >= 90200 THEN
    FOR v_job_list IN SELECT j.job_id, j.owner, j.job_name, j.start_time, j.end_time, j.status, j.pid  
        FROM @extschema@.job_log j
        JOIN pg_stat_activity p ON j.pid = p.pid
        WHERE p.state <> 'idle'
        AND j.status IS NULL
        ORDER BY j.job_id DESC
        LIMIT $1
    LOOP
        RETURN NEXT v_job_list; 
    END LOOP;
ELSE 
    FOR v_job_list IN SELECT j.job_id, j.owner, j.job_name, j.start_time, j.end_time, j.status, j.pid  
        FROM @extschema@.job_log j
        JOIN pg_stat_activity p ON j.pid = p.procpid
        WHERE p.current_query <> '<IDLE>' 
        AND j.status IS NULL
        ORDER BY j.job_id DESC
        LIMIT $1
    LOOP
        RETURN NEXT v_job_list; 
    END LOOP;
END IF;

RETURN;

END
$$;
/*
 *  Log a complete, single query job
 */
CREATE FUNCTION sql_job(p_job_name text, p_sql text) RETURNS text
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_job_id    bigint;
    v_step_id   bigint;
    v_numrows   bigint;
    v_adv_lock  boolean;
    v_return    text;
BEGIN
    v_job_id := @extschema@.add_job(p_job_name);
    
    -- Take advisory lock to prevent multiple calls to function overlapping
    v_adv_lock := pg_try_advisory_lock(hashtext('sql_log'), hashtext(p_job_name));
    IF v_adv_lock = 'false' THEN
        v_step_id := @extschema@.add_step(v_job_id,'Obtaining advisory lock for job: '||v_job_name);
        PERFORM @extschema@.update_step(v_step_id, 'OK','Found concurrent job. Exiting gracefully');
        PERFORM @extschema@.close_job(v_job_id);
        RETURN 'Concurrent job found. Obtaining advisory lock FAILED for job: %', v_job_name;
    END IF;

    v_step_id := @extschema@.add_step(v_job_id, 'Running sql: ' || quote_literal(p_sql));
    EXECUTE p_sql;
    GET DIAGNOSTICS v_numrows = ROW_COUNT;
    PERFORM @extschema@.update_step(v_step_id, 'OK', 'Rows affected: ' || v_numrows);
    PERFORM @extschema@.close_job(v_job_id);
    
    PERFORM pg_advisory_unlock(hashtext('sql_log'), hashtext(p_job_name));

    RETURN 'Job logged with job id: ' || v_job_id;

EXCEPTION
    WHEN OTHERS THEN 
        PERFORM @extschema@.update_step(v_step_id, 'CRITICAL', 'ERROR: '||coalesce(SQLERRM,'unknown'));
        PERFORM @extschema@.fail_job(v_job_id);
        PERFORM pg_advisory_unlock(hashtext('sql_log'), hashtext(p_job_name));
        RETURN 'Job ID ' || v_job_id || ' failed. See job_detail table for more details';
END
$$;
/*
 *  Log a single query step
 */
CREATE FUNCTION sql_step(p_job_id bigint, p_action text, p_sql text) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_step_id   bigint;
    v_numrows   bigint;
BEGIN
    v_step_id := @extschema@.add_step(p_job_id, p_action);
    EXECUTE p_sql;
    GET DIAGNOSTICS v_numrows = ROW_COUNT;
    PERFORM @extschema@.update_step(v_step_id, 'OK', 'Rows affected: ' || v_numrows);
    PERFORM @extschema@.close_job(p_job_id);

    RETURN true;
EXCEPTION
    WHEN OTHERS THEN
        PERFORM @extschema@.update_step(v_step_id, 'CRITICAL', 'ERROR: '||coalesce(SQLERRM,'unknown'));
        RETURN false;
END
$$;
/*
 *  Update Step Autonomous
 */
CREATE FUNCTION _autonomous_update_step(p_step_id bigint, p_status text, p_message text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_numrows integer;
BEGIN
    UPDATE @extschema@.job_detail SET 
        end_time = current_timestamp,
        elapsed_time = date_part('epoch',now() - start_time)::real,
        status = p_status,
        message = p_message
    WHERE step_id = p_step_id; 
    GET DIAGNOSTICS v_numrows = ROW_COUNT;
    RETURN v_numrows;
END
$$;

/*
 *  Update Step
 */
CREATE OR REPLACE FUNCTION update_step(p_step_id bigint, p_status text, p_message text) RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_remote_query text;
    v_dblink_schema text;
BEGIN
    SELECT nspname INTO v_dblink_schema FROM pg_namespace n, pg_extension e WHERE e.extname = 'dblink' AND e.extnamespace = n.oid;
    
    v_remote_query := 'SELECT @extschema@._autonomous_update_step ('||
    p_step_id || ',' ||
    quote_literal(p_status) || ',' ||
    quote_literal(p_message) || ')';

    EXECUTE 'SELECT devnull FROM ' || v_dblink_schema || '.dblink('||quote_literal(@extschema@.auth())||
        ','|| quote_literal(v_remote_query) || ',TRUE) t (devnull int)';  
END
$$;
