-- Exercise 2
select *
from cd.facilities;

-- Exercise 3
select name, membercost
from cd.facilities;

-- Exercise 4
select *
from cd.facilities
where membercost > 0;

-- Exercise 5
select facid, name, membercost, monthlymaintenance
from cd.facilities
where membercost > 0 and membercost < (monthlymaintenance/50);

-- Exercise 6
select *
from cd.facilities
where name like '%Tennis%';

-- Exercise 7
select *
from cd.facilities
where facid in (1, 5);

-- Exercise 8
select memid, firstname, joindate
from cd.members
where joindate >= '2012-09-01';

-- Exercise 9
select distinct surname
from cd.members
order by surname
limit 10;

-- Exercise 10
select max(joindate)
from cd.members;

-- Exercise 11
select count(*)
from cd.facilities
where guestcost >= 10;

-- Exercise 12
select facid, sum(slots) as "Total Slots"
from cd.bookings
where starttime between '2012-07-01' and '2012-07-31'
group by facid
order by "Total Slots";

-- Exercise 13
select facid, sum(slots) as "Total Slots"
from cd.bookings
group by facid
having sum(slots) > 1000
order by facid;

-- Exercise 14
select bks.starttime as start, facs.name as name
from cd.facilities facs inner join cd.bookings bks on facs.facid = bks.facid
where facs.name in ('Tennis Court 2','Tennis Court 1') and
bks.starttime >= '2012-09-21' and
bks.starttime < '2012-09-22'
order by bks.starttime; 

-- Exercise 15
select bks.starttime 
from cd.bookings bks inner join cd.members mems on mems.memid = bks.memid
where mems.firstname='David' and mems.surname='Farrell'; 