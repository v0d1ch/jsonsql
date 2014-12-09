# jsonsql

Interpolates JSON data into SQL strings from the command line. For generating
SQL statements to pass to DB client programs like `psql`, `mysql`, and
`sqlite3` via Unix pipelines or shell scripts. A faster, lighter-weight
alternative to writing ad-hoc, monolithic programs with database and ORM
libraries. 

Interpolation syntax

  INSERT INTO titles (title, rating, year) VALUES (:title, :ratings.imdb, :year);

  With input JSON

generates

  INSERT INTO titles (title, rating, year) VALUES ('Interstellar', 8.9, 2014);
  

## Array joining

If a key path evaluates to an array of values, the values are converted into strings, joined by a delimiter, and then output as a string. The default delimiter is a comma:

```
INSERT into titles (title, year, rating, stars, created) 
VALUES (:title, :year, :ratings.imdb, :stars.name, DEFAULT);
```

```
INSERT into titles (title, year, rating, stars, created)
VALUES ('Terminator 2: ''Judgment Day''', 1991, 8.5, 'Arnold Schwarzenegger,Linda Hamilton', DEFAULT);
INSERT into titles (title, year, rating, stars, created)
VALUES ('Interstellar', 2014, 8.9, 'Matthew McConaughey,Anne Hathaway', DEFAULT);
```

A key path can be followed by an array formatting expression:

```
{delimiter-string!prefix-sring!postfix-string}
```


template
```
INSERT into titles (title, year, rating, stars, created) 
VALUES (:title, :year, :ratings.imdb, :stars.name{;!$!}, DEFAULT);
```

output:
```
INSERT into titles (title, year, rating, stars, created) 
VALUES ('Terminator 2: ''Judgment Day''', 1991, 8.5, 
'$Arnold Schwarzenegger;$Linda Hamilton', DEFAULT);
INSERT into titles (title, year, rating, stars, created) 
VALUES ('Interstellar', 2014, 8.9, 
'$Matthew McConaughey;$Anne Hathaway', DEFAULT);
```

