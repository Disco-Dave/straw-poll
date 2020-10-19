import React from "react";

export default function VotedPoll({ poll }) {
  const expiration = poll.expiration ? new Date(poll.expiration) : null;

  const totalVotes = poll.answers.reduce(
    (sum, { votes }) => votes + sum,
    0,
  );

  return (
    <main className="container">
      <h1 className="container__title">{poll.question}</h1>

      {expiration && (
        <h2 className="container__subtitle">
          Expires: {expiration.toLocaleDateString()}
        </h2>
      )}

      {poll.answers.map(({id, text, votes}) => (
        <div key={id} className="vote">
          <label 
            htmlFor={`answer_${id}`}
            className="vote__label"
          >
            {text}
          </label>
          <progress 
            id={`answer_${id}`} 
            max={totalVotes} 
            value={votes}
            className="vote__progress"
          >
            {votes}
          </progress>
        </div>
      ))}

    </main>
  );
}

VotedPoll.propTypes = {
  poll: PropTypes.shape({
    question: PropTypes.string.isRequired,
    expiration: PropTypes.string,
    answers: PropTypes.arrayOf(
      PropTypes.shape({
        id: PropTypes.number.isRequired,
        text: PropTypes.string.isRequired,
        votes: PropTypes.number.isRequired,
      })
    ).isRequired,
  }),
};
import PropTypes from "prop-types";
