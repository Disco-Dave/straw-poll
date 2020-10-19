import React from "react";
import * as ReactRouterDom from "react-router-dom";
import NotFound from "../NotFound";
import FoundPoll from "./FoundPoll";
import Loader from "../../Loader";
import { getPoll } from "../../../api";

export default function Poll() {
  const params = ReactRouterDom.useParams();
  const pollId = Number.parseInt(params.pollId);

  const [requestedPoll, setRequestedPoll] = React.useState({
    status: "not loaded",
    poll: null,
  });

  React.useEffect(() => {
    if (Number.isNaN(pollId)) {
      setRequestedPoll((p) => ({
        ...p,
        status: "not found",
      }));
    } else {
      getPoll(pollId)
        .then((poll) => {
          setRequestedPoll({
            status: "found",
            poll,
          });
        })
        .catch(() => {
          setRequestedPoll((p) => ({
            ...p,
            status: "not found",
          }));
        });
    }
  }, [pollId, setRequestedPoll]);

  switch (requestedPoll.status) {
    case "not found":
      return <NotFound />;

    case "found":
      return <FoundPoll poll={requestedPoll.poll} />;

    default:
      return <Loader />;
  }
}
