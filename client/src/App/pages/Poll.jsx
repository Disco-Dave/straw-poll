import React from "react";
import PropTypes from "prop-types";
import * as ReactRouterDom from "react-router-dom";
import NotFound from "./NotFound";

function FoundPoll({ pollId }) {
  return <h1> Poll {pollId} </h1>;
}

FoundPoll.propTypes = {
  pollId: PropTypes.number.isRequired,
};

export default function Poll() {
  const params = ReactRouterDom.useParams();
  const pollId = Number.parseInt(params.pollId);

  if (Number.isNaN(pollId)) {
    return <NotFound />;
  } else {
    return <FoundPoll pollId={pollId} />;
  }
}
