EXPLAIN ANALYZE
SELECT *
FROM (VALUES (1, 'Test'), (2, 'Hello')) AS t(col1, col2);
