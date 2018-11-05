## Spec
– Accepts a “database operation” for find or insert (from terminal/command line)
  – “Find customer column1=value1, column7=value2”
  – “Insert order (value1, value2,,value3,,,value4,,,,)”
– Examples
  – Find order employeeID=5, shipCountry=“Brazil”
  – Insert customer (“ALFKI","Alfreds Futterkiste","Maria Anders","Sales Representative","Obere Str.57","Berlin","","12209","Germany","030-0074321","030-0076545”)

## Code Organization
- `/src/index.js` - Main function
- `/src/csvDB.js` - Database drivers
- `/src/parser.js` - Query parser
- `/src/termAsync.js` - Promisifed UI module
- `/src/ConverterAsync.js` - Promisifed CSV & JSON converter module
- `/data` - CSV files as database storage

## NPM Modules
- Terminal-kit
  A UI framework for terminal.
- Parsimmon
  A Parsimmon parser is a lazy action monad, which returns either an object yielded by that action on success or a message in case of failure. 
  It's way easier to compose than native Javascript RegExp.
- json-2-csv
  Very convenient to handle data in CSV and JSON format with this module. However, its asynchronous nature is primarily for web servers which forces the rest of the code to live inside the asynchronous world. To better organize the code in a literally synchronous way, I wrap the functions that takes a callback in a Promise monad and use async/await syntax to chain the continuation.
- Ramda
  A library of general functions to manipulate functors such as array. Though Javascript is by default a imperative language, I try to write codes as functional as possible to emphasis the data flow.

## Design Decisions
- The number of actions and tables are relatively small so I let the user to directly select them in the UI, which saves some complexity from the parser.
- JSON object as the native data structure in Javascript is easier to manipulate than strings in CSV format. So I choose to read and convert CSV files to database objects in memory and then persist database objects in CSV files after the query process.