import React from "react";
import {BrowserRouter, Switch, Route} from "react-router-dom";
import CreatePoll from "./pages/CreatePoll";
import NotFound from "./pages/NotFound";
import Poll from "./pages/Poll";

export default function App() {
  return (
    <React.StrictMode>
      <BrowserRouter>
        <Switch>
          <Route exact path="/">
            <CreatePoll />
          </Route>
          <Route exact path="/:pollId">
            <Poll />
          </Route>
          <Route path="*">
            <NotFound />
          </Route>
        </Switch>
      </BrowserRouter>
    </React.StrictMode>
  );
}
