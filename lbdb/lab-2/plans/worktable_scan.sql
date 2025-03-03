EXPLAIN ANALYZE
WITH RECURSIVE emp_tree AS (
    SELECT id, manager_id, first_name
    FROM employees
    WHERE manager_id IS NULL

    UNION ALL

    SELECT e.id, e.manager_id, e.first_name
    FROM employees e
    JOIN emp_tree et ON e.manager_id = et.id
)
SELECT * FROM emp_tree;
