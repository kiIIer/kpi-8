EXPLAIN ANALYZE
SELECT * FROM (
    SELECT * FROM LATERAL (
        SELECT id, salary FROM employees WHERE salary > 1000 OFFSET 0
    ) AS subq1
) AS subq2;
