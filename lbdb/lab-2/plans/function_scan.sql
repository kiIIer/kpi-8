CREATE OR REPLACE FUNCTION get_high_salary_emps(min_sal INTEGER)
RETURNS TABLE (id INT, name TEXT, sal INT)
AS $$
BEGIN
    RETURN QUERY
      SELECT e.id, e.first_name || ' ' || e.last_name, e.salary::INT
      FROM employees e
      WHERE e.salary > min_sal;
END;
$$ LANGUAGE plpgsql;

EXPLAIN ANALYZE SELECT * FROM get_high_salary_emps(1500);
