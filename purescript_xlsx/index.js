const path = require( "path" )
const fs = require( "fs" )
const xlsx = require( "xlsx" )

const tableName = "Sheet1"
const inputFilePath = path.resolve( __dirname, './source/product-feed.xlsx' )
const fileContent = fs.readFileSync( inputFilePath, { encoding: "base64" } )
const table = xlsx.read( fileContent, { type: "base64" } )
const csv = xlsx.utils.sheet_to_csv( table.Sheets[ tableName ] )
