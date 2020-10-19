import React from "react";
import PropTypes from "prop-types";
import Control from "../../form/Control";

export default function Answer({
  value,
  error,
  onChange,
  onBlur,
  onRemove,
  showRemove,
  answerNumber,
}) {
  return (
    <Control
      error={error}
      htmlFor={`answer${answerNumber}`}
      info={
        showRemove ? (
          <button
            name={`Remove Answer ${answerNumber}`}
            type="button"
            className="button button--link"
            onClick={onRemove}
          >
            Remove
          </button>
        ) : null
      }
      label={`Answer ${answerNumber}`}
    >
      <input
        id={`answer${answerNumber}`}
        name={`answer-${answerNumber}`}
        type="text"
        className="control__input"
        value={value}
        onChange={onChange}
        onBlur={onBlur}
      />
    </Control>
  );
}

Answer.propTypes = {
  value: PropTypes.string.isRequired,
  error: PropTypes.string,
  onChange: PropTypes.func.isRequired,
  onBlur: PropTypes.func.isRequired,
  onRemove: PropTypes.func.isRequired,
  showRemove: PropTypes.bool.isRequired,
  answerNumber: PropTypes.number.isRequired,
};
