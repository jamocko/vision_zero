import React from 'react';
import {BrowserRouter, Route} from "react-router-dom";
import HomepagePage from './pages/HomePage';
import ProductPage from './pages/ProductPage';
import TeamPage from './pages/TeamPage';

function App() {
  return (
    <div>
        <BrowserRouter>
            <div>
                <Route path="/" exact component={HomepagePage}/>
                <Route path="/Product" exact component={ProductPage}/>
                <Route path="/OurTeam" exact component={TeamPage}/>
            </div>
        </BrowserRouter>
    </div>
  );
}

export default App;
