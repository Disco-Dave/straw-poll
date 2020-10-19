import "@fortawesome/fontawesome-free/css/solid.css";
import "./index.css";
import React from "react";
import ReactDOM from "react-dom";
import App from "./App";

const app = document.getElementById("app");
ReactDOM.render(<App />, app);

// eslint-disable-next-line no-undef
if (module.hot) {
  // eslint-disable-next-line no-undef
  module.hot.accept();
}
