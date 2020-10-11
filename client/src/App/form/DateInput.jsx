import React from "react";
import PropTypes from "prop-types";

function displayDate(value) {
  if (value) {
    const year = value.getFullYear();
    const month = (value.getMonth() + 1).toString().padStart(2, "0");
    const date = value.getDate().toString().padStart(2, "0");
    return `${year}-${month}-${date}`;
  } else {
    return "";
  }
}

function convertToDate(value) {
  if (value) {
    const date = new Date();

    const [year, month, day] = value.split("-");
    date.setYear(year);
    date.setMonth(month - 1);
    date.setDate(day);
    date.setHours(0);
    date.setMinutes(0);
    date.setSeconds(0);

    return date;
  } else {
    return "";
  }
}

export default function DateInput({
  className,
  id,
  max,
  min,
  name,
  onBlur,
  onChange,
  value,
}) {
  function handleOnChange(e) {
    onChange({
      ...e,
      target: {
        ...e.target,
        value: convertToDate(e.target.value),
      },
    });
  }

  return (
    <input
      type="date"
      className={className}
      id={id}
      name={name}
      min={min ? displayDate(min) : null}
      max={max ? displayDate(max) : null}
      value={value ? displayDate(value) : null}
      onChange={handleOnChange}
      onBlur={onBlur}
    />
  );
}

DateInput.defaultProps = {
  className: null,
  id: null,
  max: null,
  min: null,
  name: null,
  onBlur: () => {},
  onChange: () => {},
  value: null,
};

DateInput.propTypes = {
  className: PropTypes.string,
  id: PropTypes.string,
  max: PropTypes.instanceOf(Date),
  min: PropTypes.instanceOf(Date),
  name: PropTypes.string,
  onBlur: PropTypes.func,
  onChange: PropTypes.func,
  value: PropTypes.instanceOf(Date),
};
