# jsonsql

Interpolates JSON data into SQL strings from the command line. For generating
SQL statements to pass to DB client programs like `psql`, `mysql`, and
`sqlite3` via Unix pipelines or shell scripts. A faster, lighter-weight
alternative to writing ad-hoc, monolithic programs with database and ORM
libraries. 

A template file with this interpolation syntax:

    INSERT into titles (title, year, rating, created) 
    VALUES (:title, :year, :ratings.imdb, DEFAULT);

combined with this JSON stream on STDIN

```json
{
  "title": "Terminator 2: 'Judgment Day'",
  "year": 1991,
  "stars": [
    {"name": "Arnold Schwarzenegger"},
    {"name": "Linda Hamilton"}
  ],
  "ratings": {
    "imdb": 8.5
  },
  "created": "2014-12-04T10:10:10Z"
  
}
{
  "title": "Interstellar",
  "year": 2014,
  "stars": [
    {"name":"Matthew McConaughey"},
    {"name":"Anne Hathaway"}
  ],
  "ratings": {
    "imdb": 8.9
  }
}
```

generates this output:

    INSERT into titles (title, year, rating, created)
    VALUES ('Terminator 2: ''Judgment Day''', 1991, 8.5, DEFAULT);
    INSERT into titles (title, year, rating, created)
    VALUES ('Interstellar', 2014, 8.9, DEFAULT);

## Usage


```
jsonsql

Usage: jsonsql (TEMPLATE | -f FILE)
  Inject JSON into SQL template strings

Available options:
  -h,--help                Show this help text
  -f FILE                  Template file
```

## Array joining

If a key path evaluates to an array of values, specify a `<delimiter>` to have
the the values converted into strings, joined by the delimiter, and then
output as a string. 

```
INSERT into titles (title, year, rating, stars<,>, created) 
VALUES (:title, :year, :ratings.imdb, :stars.name, DEFAULT);
```

```
INSERT into titles (title, year, rating, stars, created)
VALUES ('Terminator 2: ''Judgment Day''', 1991, 8.5, 'Arnold Schwarzenegger,Linda Hamilton', DEFAULT);
INSERT into titles (title, year, rating, stars, created)
VALUES ('Interstellar', 2014, 8.9, 'Matthew McConaughey,Anne Hathaway', DEFAULT);
```

## JSON literal interpolation

If a key path terminates in a JSON object or array (without a array delimiter
specified), a JSON literal will be interpolated:

template:
```
INSERT into titles (title, stars) 
VALUES (:title, :stars);
```

output:
```
INSERT into titles (title, stars)
VALUES ('Terminator 2: ''Judgment Day''', '[{"name":"Arnold Schwarzenegger"},{"name":"Linda Hamilton"}]');
INSERT into titles (title, stars) 
VALUES ('Interstellar', '[{"name":"Matthew McConaughey"},{"name":"Anne Hathaway"}]');
```

To interpolate the base object as a literal JSON string, use `:.` as the keypath.

Do NOT put quotes around the placeholder for a literal JSON interpolation.


## Author

* Daniel Choi <dhchoi@gmail.com>
