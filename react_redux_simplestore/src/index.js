import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import { App, store } from './App';
import registerServiceWorker from './registerServiceWorker';

const render = () => { ReactDOM.render(<App />, document.getElementById('root')) }
store.subscribe( render )
store.subscribe( () => { console.log(`current state: ${store.getState()}`)} )
render()
registerServiceWorker();
