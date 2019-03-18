import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import * as serviceWorker from './serviceWorker';
import App from "./App.jsx"

const css_anchor = "my-vehicles-root"
const rootElem = document.getElementById(css_anchor)

ReactDOM.render(<App />, rootElem)

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

