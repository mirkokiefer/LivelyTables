assert = require 'assert'
vows = require 'vows'
client = require '../lib/main'

server = 'http://localhost:8080'
database = 'meta'
tableURI = (tableName) -> [server, database, tableName].join '/'
rowURI = (rowid, tableName) -> [server, database, tableName, rowid].join '/'

salaryColumn =
  'uri': rowURI 'salary', 'column'
  'label':'salary'
  'range':'number'

personTable =
  'uri': tableURI 'person'
  'label':'Person'

employeeTable =
  'uri': tableURI 'employee'
  'label': 'Employee'
  'parents': personTable.uri
  'legal_columns': [salaryColumn.uri]

samplePerson =
  'uri': rowURI 'paul', 'person'
  'label': 'Paul'

sampleEmployee =
  'uri': rowURI 'mike', 'employee'
  'label':'Mike'
  'salary':2000

badItem =
  'uri': rowURI 'john', 'employee'
  'label':'John'


vows.describe('stats').addBatch(
  'write column':
    topic: () ->
      that = this
      response = (err, body) ->
        that.callback null, body
      client.writeItem salaryColumn, response 
    'succeeds': (err, response) -> assert.equal response, {ok: "success"}
).export module