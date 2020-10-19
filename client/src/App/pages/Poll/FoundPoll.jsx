import React from "react";
import PropTypes from "prop-types";

export default function FoundPoll({ poll }) {
  return (
    <main className="container">
      <h1>{poll.question}</h1>
    </main>
  );
}

FoundPoll.propTypes = {
  poll: PropTypes.shape({
    question: PropTypes.string.isRequired,
  }),
};
