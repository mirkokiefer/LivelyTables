#HTTP Interface

Its always:

    %database/%table/%row
    
As tables are rows of table "table" we can create them with:

    PUT %database/table/%newtable
    
But to add new rows to it we go:

    PUT %database/%newtable/%newrow
    

Example:

    PUT testdb/table/person
    {
      "label": "Person",
      "parent": "testdb/table/row",
      "required_columns": ["testdb/column/age"]
    }
    
    PUT testdb/table/employee
    {
      "label": "Employee",
      "parent": "testdb/table/person"
      "required_columns": ["testdb/column/salary"]
    }
    
    PUT testdb/table/shareholder
    {
      "label": "Shareholder",
      "parent": "testdb/table/person"
      "required_columns": ["testdb/column/owns"]
    }

Adding a row to "table/employee" requires you to provide the "age" column of "table/person" and the "label" column defined by "table/row":

    PUT testdb/employee/jack
    {
      "label": "Jack",
      "age": 40,
      "salary": 4000
    }
   
All legal columns are stored in

    %database/column/%column_name

An example definition of age:

    PUT testdb/column/age
    {
      "label": "age",
      "range": "meta/literal/number"
      "arity": "one"
    }

So a "cleaner" description of jack would actually be:

    PUT testdb/employee/jack
    {
      "testdb/column/label": "Jack",
      "testdb/column/age": 40,
      "testdb/column/salary": 4000
    }

But LivelyTables automatically resolves a column named "age" to its full URI in the current database. You only need to provide it explicitly when using properties defined in a different database.
Core columns like "label", "parent", or "required_columns" are provided in every database - conceptually they are actually subcolumns (or subproperties like in RDFS) of a global "meta database".