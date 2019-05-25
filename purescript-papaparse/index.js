const fs = require( "fs" )
const path = require( "path" )
const xlsx = require( "xlsx" )
const papaparse = require( "papaparse" )

const sourceDirectory = "./source/"
const sourceFileName = "marketing.xlsx"
const sourceFilePath = path.resolve( __dirname, sourceDirectory + sourceFileName )

const sourceXLSX = fs.readFileSync( sourceFilePath, { encoding: "base64" } )
const table = xlsx.read( sourceXLSX, { type: "base64" } )

var sheets = []
for ( var sheetName in table.Sheets ) {
  sheets.push(
    [ sheetName, xlsx.utils.sheet_to_csv( table.Sheets[ sheetName ] ) ]
  )
}


// console.log( papaparse.parse( sourceCSV ) )
