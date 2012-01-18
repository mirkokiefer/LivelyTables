request = require 'request'

write = (data, URI, cb) ->
  options =
    method: 'PUT'
    url: URI
    body: JSON.stringify data
  console.log options
  response = (err, res, body) ->
    cb(err, JSON.parse body)
  request(options, response)

writeItem = (item, cb) ->
  write item, item.uri, cb

module.exports = 
  write: write
  writeItem: writeItem