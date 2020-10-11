import React from "react";
import PropTypes from "prop-types";
import Control from "../../form/Control";

function getDate(value) {
  if (value) {
    const year = value.getFullYear();
    const month = (value.getMonth() + 1).toString().padStart(2, "0");
    const date = value.getDate().toString().padStart(2, "0");
    return `${year}-${month}-${date}`;
  } else {
    return "";
  }
}

export default function Expiration({ value, onChange, min }) {
  function handleDateChange(e) {
    if (e?.target?.value) {
      const date = new Date();

      const [year, month, day] = e.target.value.split("-");
      date.setYear(year);
      date.setMonth(month - 1);
      date.setDate(day);
      date.setHours(0);
      date.setMinutes(0);
      date.setSeconds(0);

      onChange(date);
    } else {
      onChange(null);
    }
  }

  return (
    <Control htmlFor="expiration-date" info="optional" label="Expiration Date">
      <input
        type="date"
        className="control__input"
        id="expiration-date"
        name="expiration-date"
        min={min ? getDate(min) : null}
        value={getDate(value)}
        onChange={handleDateChange}
      />
    </Control>
  );
}

Expiration.propTypes = {
  onChange: PropTypes.func.isRequired,
  value: PropTypes.instanceOf(Date),
  min: PropTypes.instanceOf(Date),
};
