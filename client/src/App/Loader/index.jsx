import React from "react";
import "./index.css";

export default function Loader() {
  return (
    <div className="loader">
      <div className="loader__spinner">
        <div></div>
        <div></div>
        <div></div>
        <div></div>
      </div>
    </div>
  );
}
