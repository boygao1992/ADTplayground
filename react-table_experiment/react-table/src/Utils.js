import React from "react";
import namor from "namor";
import "./App.css";

const range = len => {
  const arr = [];
  for ( let i = 0; i < len; i++ ) {
    arr.push( i );
  }
  return arr;
};

const newPerson = () => {
  const statusChance = Math.random();
  return {
    firstName: namor.generate( { words: 1, numbers: 0 } ),
    lastName: namor.generate( { words: 1, numbers: 0 } ),
    age: Math.floor( Math.random() * 30 ),
    visits: Math.floor( Math.random() * 100 ),
    progress: Math.floor( Math.random() * 100 ),
    status: statusChance > 0.66 ?
      "relationship" :
      statusChance > 0.33 ? "complicated" : "single"
  };
};

const makes = ["BMW", "Leusx", "ZIL"]

const newVehicle = ({ id }) => {
  return {
    id,
    make: makes[Math.floor( Math.random() * makes.length )],
    model: namor.generate( { words: 1, numbers: 0 } ),
    year: 1950 + Math.floor( Math.random() * 50 ),
    trim: namor.generate( { words: 4, numbers: 1 } ),
  }
}

export function makeData( len = 5553 ) {
  return range( len )
    .map( d => {
      return {
        ...newPerson(),
        children: range( 10 )
          .map( newPerson )
      };
    } );
}

export function randomData( len = 3 ) {
  return range(len)
  .map(id => newVehicle({ id }))
}

export const Logo = () =>
<div style = { { margin: '1rem auto', display: 'flex', flexWrap: 'wrap', alignItems: 'center', justifyContent: 'center' } } >
  For more examples,
  visit { '' }
  <br / >
  <a href = "https://github.com/react-tools/react-table" target = "_blank" >
    <img src = "https://github.com/react-tools/media/raw/master/logo-react-table.png" style = { { width: `150px`, margin: ".5em auto .3em" } } />
  </a>
</div>;

export const Tips = () =>
<div style = { { textAlign: "center" } } >
  <em> Tip: Hold shift when sorting to multi - sort! < /em>
</div>;
