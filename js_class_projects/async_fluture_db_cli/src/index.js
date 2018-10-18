const toString = require('ramda/src/toString')
const findParser = require('./parser').findParser
const insertParser = require('./parser').insertParser
const db = require('./csvDB')
const term = require('./TermAsync').term
const gridMenuAsync = require('./TermAsync').gridMenuAsync
const singleColumnMenuAsync = require('./TermAsync').singleColumnMenuAsync
const inputFieldAsync = require('./TermAsync').inputFieldAsync

// Render loop
//TODO: Introduce IO monad to handle side effects.
//TODO: Replace Promise with Fluture(Future monad) to embrace laziness.
const main = async () => {
  // Table schema.
  const headers = {
    'orders': ["OrderID","CustomerID","EmployeeID","OrderDate","RequiredDate","ShippedDate","ShipVia","Freight","ShipName","ShipAddress","ShipCity","ShipRegion","ShipPostalCode","ShipCountry"],
    'customers': ["CustomerID","CompanyName","ContactName","ContactTitle","Address","City","Region","PostalCode","Country","Phone","Fax"]
  }

  // Parser for each type of action.
  const parsers = {
    Find: findParser,
    Insert: insertParser
  }

  // Primary key for each table.
  const PKs = {
    orders: 'OrderID',
    customers: 'CustomerID'
  }

  while (true) {
    // Get user action.
    term('\n')
    const action = await singleColumnMenuAsync(['Find', 'Insert', 'Exit'])
    // Terminate the program if Exit action is received; otherwise, conduct the query process.
    if (action === 'Exit') { process.exit() }

    // Get the table which user wants to query.
    term('\n Select table:')
    const table = await gridMenuAsync(['orders', 'customers'])

    // Print query format.
    term('\nFormat:')
    switch(action) {
      case 'Find':
        term('\nkey1 = value1, key2 = value2, ..., key_n = value_n')
        break
      case 'Insert':
        term('\nvalue1, value2, ..., value_n')
      default:
    }

    // Print table schema.
    term('\nSchema:')
    term(`\n${headers[table]}`)

    // Read and parse each line of input until receive a valid query statement.
    let query
    while (true) {
      // Read query statement.
      term('\n>')
      let conditions = await inputFieldAsync()
      //TODO: wrap following try-catch block in Either monad to handle error
      try {
        query = parsers[action](headers[table], conditions)
      } catch (err) {
        term.red.bold(`\n${err.message}`)
      }
      if (query) { break }
    }

    // Load table from CSV file and transform it into JSON format
    const json = await db.readFile(table)

    // Dispatch query to database drivers.
    // TODO: rewrite conditional expressions in Monoids
    // Concat Logical Monoids (All, Any)
    const All = x => ({
      x,
      concat: ({x:y}) => All(x && y)
    })
    const Fn = f => ({
      fold: f,
      concat: o => Fn( x => f(x).concat(o.fold(x)) )
    })
    const isFind = x => x === 'Find'
    const isInsert = x => x === 'Insert'
    const both = Fn(Compose(All, isFind)).concat(Compose(All, isInsert))

    switch (action) {
      case 'Find':
        const result = db.find(json, query)
        //TODO: wrap the following null check in Maybe monad
        if (result.length === 0) {
          term.green('\nEmpty')
        } else {
          term.green(`\n${toString(result)}`)
        }
        break
      case 'Insert':
        const pk = PKs[table]
        let csv
        //TODO: wrap following try-catch block in Either monad to handle error
        try {
          csv = await db.insert(json, query, pk, table)
        } catch (err) {
          term.red(`\n${err}`)
        }
        if (csv) {
          term.green('\nSuccessfully inserted.')
        }
      default:
    }
  }
}

// Execute render loop.
main()
  .catch( (err) => { console.log(`\n${err}`) })
