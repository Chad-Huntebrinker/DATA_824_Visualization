-- Exercise 1
-- problem 1
select count(*)
from payment
where amount > 5.00;

-- problem 2
select *
from actor
where first_name like 'P%';

-- problem 3
select distinct count(*)
from address;

-- problem 4
select distinct *
from address;

-- problem 5
select *
from film
where rating = 'R' and
replacement_cost between 5 and 15;

-- problem 6
select count(*)
from film
where title like '%Truman%';