CREATE INDEX emp_department_idx ON employees(department_id);
EXPLAIN ANALYZE SELECT * FROM employees WHERE department_id IN (1, 2, 3, 4, 5);
