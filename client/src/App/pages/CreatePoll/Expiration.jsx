import React from "react";
import PropTypes from "prop-types";
import Control from "../../form/Control";

function DateInput({ onChange, value }) {
  return (
    <Control htmlFor="expiration-date" info="optional" label="Expiration Date">
      <input
        type="date"
        className="control__input"
        id="expiration-date"
        name="expiration-date"
        min={new Date()}
        value={value}
        onChange={onChange}
      />
    </Control>
  );
}

DateInput.propTypes = {
  onChange: PropTypes.func.isRequired,
  value: PropTypes.instanceOf(Date).isRequired,
};

function TimeInput({ onChange, value }) {
  return (
    <Control htmlFor="expiration-time" info="optional" label="Expiration Time">
      <input
        type="time"
        className="control__input"
        id="expiration-time"
        name="expiration-time"
        min={new Date()}
        value={value}
        onChange={onChange}
      />
    </Control>
  );
}

TimeInput.propTypes = {
  onChange: PropTypes.func.isRequired,
  value: PropTypes.instanceOf(Date).isRequired,
};

export default function Expiration({ onChange, value }) {
  return (
    <>
      <DateInput />
      <TimeInput />
    </>
  );
}
