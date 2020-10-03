import React from "react";
import PropTypes from "prop-types";

export default function Control({ children, error, htmlFor, info, label }) {
  return (
    <div className={`control ${error ? "control--invalid" : ""}`}>
      <label className="control__label" htmlFor={htmlFor}>
        {label}
      </label>

      {info ? <span className="control__info">{info}</span> : null}

      {children}

      {error ? <span className="control__help">{error}</span> : null}
    </div>
  );
}

Control.propTypes = {
  children: PropTypes.node.isRequired,
  error: PropTypes.string,
  htmlFor: PropTypes.string.isRequired,
  info: PropTypes.node,
  label: PropTypes.string.isRequired,
};
