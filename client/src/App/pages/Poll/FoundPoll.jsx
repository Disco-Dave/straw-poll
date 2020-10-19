import React from "react";
import PropTypes from "prop-types";
import VotedPoll from "./VotedPoll";
import { vote } from "../../../api";

export default function FoundPoll({ poll }) {
  const [selectedVote, setSelectedVote] = React.useState(poll.answers[0]?.id);
  const [votedPoll, setVotedPoll] = React.useState(null);

  const expiration = poll.expiration ? new Date(poll.expiration) : null;

  function handleOnSubmit(e) {
    e?.preventDefault?.();

    vote(poll.id, selectedVote).then((newlyVotedPoll) => {
      setVotedPoll(newlyVotedPoll);
    });
  }

  if (votedPoll || (expiration && expiration <= new Date())) {
    return <VotedPoll poll={votedPoll ?? poll} />;
  } else {
    return (
      <main className="container">
        <h1 className="container__title">{poll.question}</h1>

        {expiration && (
          <h2 className="container__subtitle">
            Expires: {expiration.toLocaleDateString()}
          </h2>
        )}

        <form className="form" onSubmit={handleOnSubmit}>
          {poll.answers.map(({ id, text }) => (
            <div className="radio" key={id}>
              <input
                type="radio"
                id={`answer_${id}`}
                name="vote"
                value={id}
                onChange={() => setSelectedVote(id)}
                checked={id === selectedVote}
                className="radio__input"
              />
              <label className="radio__label" htmlFor={`answer_${id}`}>
                {text}
              </label>
            </div>
          ))}

          <div className="action-buttons">
            <button className="button button--primary" type="submit">
              Vote
            </button>
          </div>
        </form>
      </main>
    );
  }
}

FoundPoll.propTypes = {
  poll: PropTypes.shape({
    question: PropTypes.string.isRequired,
    expiration: PropTypes.string,
    answers: PropTypes.arrayOf(
      PropTypes.shape({
        id: PropTypes.number.isRequired,
        text: PropTypes.string.isRequired,
      })
    ).isRequired,
  }),
};
