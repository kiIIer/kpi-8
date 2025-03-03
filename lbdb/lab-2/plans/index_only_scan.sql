CREATE INDEX emp_email_idx ON employees(email);
EXPLAIN ANALYZE SELECT email FROM employees WHERE email = 'someone@example.com';
