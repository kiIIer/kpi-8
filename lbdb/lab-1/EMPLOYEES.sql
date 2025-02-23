CREATE TABLE EMPLOYEES (
    id             INTEGER GENERATED ALWAYS AS IDENTITY,
    first_name     VARCHAR(20),
    last_name      VARCHAR(25),
    email          VARCHAR(25),
    phone_number   VARCHAR(20),
    hire_date      DATE,
    job_id         VARCHAR(10),
    salary         DECIMAL(10,2),
    manager_id     INTEGER,
    department_id  INTEGER
);


alter table EMPLOYEES
  add constraint EMP_EMP_ID_PK primary key (ID);
alter table EMPLOYEES
  add constraint EMP_EMAIL_UK unique (EMAIL);
alter table EMPLOYEES
  add constraint EMP_MANAGER_FK foreign key (MANAGER_ID)
  references EMPLOYEES (ID);
-- Create/Recreate check constraints 
alter table EMPLOYEES
  add constraint EMP_EMAIL_NN
  check (EMPLOYEES.email IS NOT NULL);
alter table EMPLOYEES
  add constraint EMP_HIRE_DATE_NN
  check (EMPLOYEES.hire_date IS NOT NULL);
alter table EMPLOYEES
  add constraint EMP_JOB_NN
  check (EMPLOYEES.job_id IS NOT NULL);
alter table EMPLOYEES
  add constraint EMP_LAST_NAME_NN
  check (EMPLOYEES.last_name IS NOT NULL);
alter table EMPLOYEES
  add constraint EMP_SALARY_MIN
  check (salary > 0);